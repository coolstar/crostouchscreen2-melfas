#define DESCRIPTOR_DEF
#include "mlfs.h"

static ULONG MlfsDebugLevel = 100;
static ULONG MlfsDebugCatagories = DBG_INIT || DBG_PNP || DBG_IOCTL;

NTSTATUS
DriverEntry(
	__in PDRIVER_OBJECT  DriverObject,
	__in PUNICODE_STRING RegistryPath
)
{
	NTSTATUS               status = STATUS_SUCCESS;
	WDF_DRIVER_CONFIG      config;
	WDF_OBJECT_ATTRIBUTES  attributes;

	MlfsPrint(DEBUG_LEVEL_INFO, DBG_INIT,
		"Driver Entry\n");

	WDF_DRIVER_CONFIG_INIT(&config, MlfsEvtDeviceAdd);

	WDF_OBJECT_ATTRIBUTES_INIT(&attributes);

	//
	// Create a framework driver object to represent our driver.
	//

	status = WdfDriverCreate(DriverObject,
		RegistryPath,
		&attributes,
		&config,
		WDF_NO_HANDLE
	);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_INIT,
			"WdfDriverCreate failed with status 0x%x\n", status);
	}

	return status;
}

static NTSTATUS mip4_i2c_xfer(PMLFS_CONTEXT pDevice, uint8_t *write_buf, unsigned int write_len,
	uint8_t *read_buf, unsigned int read_len) {
	NTSTATUS status;

	status = SpbXferDataSynchronously(&pDevice->I2CContext, write_buf, write_len, read_buf, read_len);
	return status;
}

NTSTATUS BOOTTOUCHSCREEN(
	_In_  PMLFS_CONTEXT  devContext
)
{
	NTSTATUS status;

	if (!devContext->TouchScreenBooted) {
		uint8_t cmd[2];
		uint8_t buf[14];
		/* Resolution */

		cmd[0] = MIP4_R0_INFO;
		cmd[1] = MIP4_R1_INFO_RESOLUTION_X;

		status = mip4_i2c_xfer(devContext, cmd, 2, buf, 14);

		if (!NT_SUCCESS(status)) {
			DbgPrint("Unable to get resolution %d", status);
			return status;
		}

		uint16_t max_x = buf[0] | (buf[1] << 8);
		uint16_t max_y = buf[2] | (buf[3] << 8);

		devContext->max_x = max_x;
		devContext->max_y = max_y;

		devContext->node_x = buf[4];
		devContext->node_y = buf[5];
		devContext->node_key = buf[6];

		//XXX: Wrong Max X
		max_x = devContext->node_x * (devContext->max_y / devContext->node_y);
		devContext->max_x = max_x;

		devContext->max_x_hid[0] = max_x;
		devContext->max_x_hid[1] = max_x >> 8;

		devContext->max_y_hid[0] = max_y;
		devContext->max_y_hid[1] = max_y >> 8;

		devContext->ppm_x = buf[12];
		devContext->ppm_y = buf[13];

		for (int i = 0; i < 14; i++) {
			buf[i] = 0;
		}

		cmd[0] = MIP4_R0_EVENT;
		cmd[1] = MIP4_R1_EVENT_SUPPORTED_FUNC;

		status = mip4_i2c_xfer(devContext, cmd, sizeof(cmd), buf, 7);

		if (!NT_SUCCESS(status)) {
			DbgPrint("Unable to get packet format %d", status);
			return status;
		}

		devContext->event_format = buf[4] | (buf[5] << 8);
		devContext->event_size = buf[6];

		if (devContext->event_format == 2 || devContext->event_format > 3) {
			DbgPrint("Unknown Event Format %d\n", devContext->event_format);
		}
		devContext->TouchScreenBooted = true;
	}
	return STATUS_SUCCESS;
}

NTSTATUS
OnPrepareHardware(
	_In_  WDFDEVICE     FxDevice,
	_In_  WDFCMRESLIST  FxResourcesRaw,
	_In_  WDFCMRESLIST  FxResourcesTranslated
)
/*++

Routine Description:

This routine caches the SPB resource connection ID.

Arguments:

FxDevice - a handle to the framework device object
FxResourcesRaw - list of translated hardware resources that
the PnP manager has assigned to the device
FxResourcesTranslated - list of raw hardware resources that
the PnP manager has assigned to the device

Return Value:

Status

--*/
{
	PMLFS_CONTEXT pDevice = GetDeviceContext(FxDevice);
	BOOLEAN fSpbResourceFound = FALSE;
	NTSTATUS status = STATUS_INSUFFICIENT_RESOURCES;

	UNREFERENCED_PARAMETER(FxResourcesRaw);

	//
	// Parse the peripheral's resources.
	//

	ULONG resourceCount = WdfCmResourceListGetCount(FxResourcesTranslated);

	for (ULONG i = 0; i < resourceCount; i++)
	{
		PCM_PARTIAL_RESOURCE_DESCRIPTOR pDescriptor;
		UCHAR Class;
		UCHAR Type;

		pDescriptor = WdfCmResourceListGetDescriptor(
			FxResourcesTranslated, i);

		switch (pDescriptor->Type)
		{
		case CmResourceTypeConnection:
			//
			// Look for I2C or SPI resource and save connection ID.
			//
			Class = pDescriptor->u.Connection.Class;
			Type = pDescriptor->u.Connection.Type;
			if (Class == CM_RESOURCE_CONNECTION_CLASS_SERIAL &&
				Type == CM_RESOURCE_CONNECTION_TYPE_SERIAL_I2C)
			{
				if (fSpbResourceFound == FALSE)
				{
					status = STATUS_SUCCESS;
					pDevice->I2CContext.I2cResHubId.LowPart = pDescriptor->u.Connection.IdLowPart;
					pDevice->I2CContext.I2cResHubId.HighPart = pDescriptor->u.Connection.IdHighPart;
					fSpbResourceFound = TRUE;
				}
				else
				{
				}
			}
			break;
		default:
			//
			// Ignoring all other resource types.
			//
			break;
		}
	}

	//
	// An SPB resource is required.
	//

	if (fSpbResourceFound == FALSE)
	{
		status = STATUS_NOT_FOUND;
	}

	status = SpbTargetInitialize(FxDevice, &pDevice->I2CContext);

	BOOTTOUCHSCREEN(pDevice);

	if (!NT_SUCCESS(status))
	{
		return status;
	}

	return status;
}

NTSTATUS
OnReleaseHardware(
	_In_  WDFDEVICE     FxDevice,
	_In_  WDFCMRESLIST  FxResourcesTranslated
)
/*++

Routine Description:

Arguments:

FxDevice - a handle to the framework device object
FxResourcesTranslated - list of raw hardware resources that
the PnP manager has assigned to the device

Return Value:

Status

--*/
{
	PMLFS_CONTEXT pDevice = GetDeviceContext(FxDevice);
	NTSTATUS status = STATUS_SUCCESS;

	UNREFERENCED_PARAMETER(FxResourcesTranslated);

	SpbTargetDeinitialize(FxDevice, &pDevice->I2CContext);

	return status;
}

NTSTATUS
OnD0Entry(
	_In_  WDFDEVICE               FxDevice,
	_In_  WDF_POWER_DEVICE_STATE  FxPreviousState
)
/*++

Routine Description:

This routine allocates objects needed by the driver.

Arguments:

FxDevice - a handle to the framework device object
FxPreviousState - previous power state

Return Value:

Status

--*/
{
	UNREFERENCED_PARAMETER(FxPreviousState);

	PMLFS_CONTEXT pDevice = GetDeviceContext(FxDevice);
	NTSTATUS status = STATUS_SUCCESS;

	WdfTimerStart(pDevice->Timer, WDF_REL_TIMEOUT_IN_MS(10));

	for (int i = 0; i < 20; i++) {
		pDevice->Flags[i] = 0;
	}

	pDevice->RegsSet = false;
	pDevice->ConnectInterrupt = true;

	return status;
}

NTSTATUS
OnD0Exit(
	_In_  WDFDEVICE               FxDevice,
	_In_  WDF_POWER_DEVICE_STATE  FxPreviousState
)
/*++

Routine Description:

This routine destroys objects needed by the driver.

Arguments:

FxDevice - a handle to the framework device object
FxPreviousState - previous power state

Return Value:

Status

--*/
{
	UNREFERENCED_PARAMETER(FxPreviousState);

	PMLFS_CONTEXT pDevice = GetDeviceContext(FxDevice);

	WdfTimerStop(pDevice->Timer, TRUE);

	pDevice->ConnectInterrupt = false;

	return STATUS_SUCCESS;
}

void MlfsProcessInput(PMLFS_CONTEXT pDevice) {
	struct _MLFS_MULTITOUCH_REPORT report;
	report.ReportID = REPORTID_MTOUCH;

	int count = 0, i = 0;
	while (count < 10 && i < 20) {
		if (pDevice->Flags[i] != 0) {
			report.Touch[count].ContactID = i;
			report.Touch[count].Height = pDevice->AREA[i];
			report.Touch[count].Width = pDevice->AREA[i];

			report.Touch[count].XValue = pDevice->XValue[i];
			report.Touch[count].YValue = pDevice->YValue[i];

			uint8_t flags = pDevice->Flags[i];
			if (flags & MXT_T9_DETECT) {
				report.Touch[count].Status = MULTI_CONFIDENCE_BIT | MULTI_TIPSWITCH_BIT;
			}
			else if (flags & MXT_T9_PRESS) {
				report.Touch[count].Status = MULTI_CONFIDENCE_BIT | MULTI_TIPSWITCH_BIT;
			}
			else if (flags & MXT_T9_RELEASE) {
				report.Touch[count].Status = MULTI_CONFIDENCE_BIT;
				pDevice->Flags[i] = 0;
			}
			else
				report.Touch[count].Status = 0;

			count++;
		}
		i++;
	}

	report.ActualCount = count;

	if (count > 0) {
		size_t bytesWritten;
		MlfsProcessVendorReport(pDevice, &report, sizeof(report), &bytesWritten);
	}
}

static void mip4_report_touch(PMLFS_CONTEXT pDevice, uint8_t* packet) {
	int id;
	bool hover;
	bool palm;
	bool state;
	uint16_t x, y;
	uint8_t pressure_stage;
	uint8_t pressure;
	uint8_t size;
	uint8_t touch_major;
	uint8_t touch_minor;

	switch (pDevice->event_format) {
	case 0:
	case 1:
		/* Touch only */
		state = packet[0] & BIT(7);
		hover = packet[0] & BIT(5);
		palm = packet[0] & BIT(4);
		id = (packet[0] & 0x0F) - 1;
		x = ((packet[1] & 0x0F) << 8) | packet[2];
		y = (((packet[1] >> 4) & 0x0F) << 8) |
			packet[3];
		pressure = packet[4];
		size = packet[5];
		if (pDevice->event_format == 0) {
			touch_major = packet[5];
			touch_minor = packet[5];
		}
		else {
			touch_major = packet[6];
			touch_minor = packet[7];
		}
		break;

	case 3:
	default:
		/* Touch + Force(Pressure) */
		id = (packet[0] & 0x0F) - 1;
		hover = packet[1] & BIT(2);
		palm = packet[1] & BIT(1);
		state = packet[1] & BIT(0);
		x = ((packet[2] & 0x0F) << 8) | packet[3];
		y = (((packet[2] >> 4) & 0x0F) << 8) |
			packet[4];
		size = packet[6];
		pressure_stage = (packet[7] & 0xF0) >> 4;
		pressure = ((packet[7] & 0x0F) << 8) |
			packet[8];
		touch_major = packet[9];
		touch_minor = packet[10];
		break;
	}

	if (state) {
		pDevice->Flags[id] = MXT_T9_DETECT;
		pDevice->XValue[id] = x;
		pDevice->YValue[id] = y;
		pDevice->AREA[id] = touch_major;
	}
	else {
		pDevice->Flags[id] = MXT_T9_RELEASE;
	}
}

static void mip4_report_keys(PMLFS_CONTEXT pDevice, uint8_t* packet)
{
	uint8_t key;
	bool down;

	switch (pDevice->event_format) {
	case 0:
	case 1:
		key = packet[0] & 0x0F;
		down = packet[0] & 0x80;
		break;

	case 3:
	default:
		key = packet[0] & 0x0F;
		down = packet[1] & 0x01;
		break;
	}

	/* Report key event */
	if (key >= 1 && key <= pDevice->node_key) {
		unsigned short keycode = pDevice->key_code[key - 1];

		DbgPrint("Key - ID: %d, keycode: %d, state: %d\n",
			key, keycode, down);
	}
	else {
		DbgPrint("Unknown key: %d\n", key);
	}
}

static NTSTATUS mip4_handle_packet(PMLFS_CONTEXT pDevice, uint8_t* packet)
{
	uint8_t type;

	switch (pDevice->event_format) {
	case 0:
	case 1:
		type = (packet[0] & 0x40) >> 6;
		break;

	case 3:
		type = (packet[0] & 0xF0) >> 4;
		break;

	default:
		/* Should not happen unless we have corrupted firmware */
		return STATUS_DEVICE_PROTOCOL_ERROR;
	}

	/* Report input event */
	switch (type) {
	case MIP4_EVENT_INPUT_TYPE_KEY:
		mip4_report_keys(pDevice, packet);
		break;

	case MIP4_EVENT_INPUT_TYPE_SCREEN:
		mip4_report_touch(pDevice, packet);
		break;

	default:
		DbgPrint("Unknown event type: %d\n", type);
		break;
	}

	return STATUS_SUCCESS;
}

BOOLEAN OnInterruptIsr(
	WDFINTERRUPT Interrupt,
	ULONG MessageID) {
	UNREFERENCED_PARAMETER(MessageID);

	WDFDEVICE Device = WdfInterruptGetDevice(Interrupt);
	PMLFS_CONTEXT pDevice = GetDeviceContext(Device);

	NTSTATUS status;

	if (!pDevice->ConnectInterrupt)
		return true;

	if (!pDevice->TouchScreenBooted)
		return true;

	uint8_t cmd[2];
	uint8_t buf[MIP4_BUF_SIZE];
	uint8_t size;
	bool alert;

	cmd[0] = MIP4_R0_EVENT;
	cmd[1] = MIP4_R1_EVENT_PACKET_INFO;

	status = mip4_i2c_xfer(pDevice, cmd, sizeof(cmd), buf, 1);

	if (!NT_SUCCESS(status)) {
		return true;
	}

	size = buf[0] & 0x7F;
	alert = buf[0] & 0x80;

	if (!size) {
		return true;
	}

	/* Read packet data */
	cmd[0] = MIP4_R0_EVENT;
	cmd[1] = MIP4_R1_EVENT_PACKET_DATA;

	status = mip4_i2c_xfer(pDevice, cmd, sizeof(cmd), buf, size);
	if (!NT_SUCCESS(status)) {
		return true;
	}

	if (alert) {
		DbgPrint("Alert: %d\n", buf[0]);
	}
	else {
		for (int i = 0; i < size; i += pDevice->event_size) {
			status = mip4_handle_packet(pDevice, &buf[i]);
			if (!NT_SUCCESS(status)) {
				break;
			}
		}
	}

	MlfsProcessInput(pDevice);

	return true;
}

void MlfsTimerFunc(_In_ WDFTIMER hTimer) {
	WDFDEVICE Device = (WDFDEVICE)WdfTimerGetParentObject(hTimer);
	PMLFS_CONTEXT pDevice = GetDeviceContext(Device);

	if (!pDevice->ConnectInterrupt)
		return;

	if (!pDevice->RegsSet)
		return;

	MlfsProcessInput(pDevice);
	return;
}

NTSTATUS
MlfsEvtDeviceAdd(
	IN WDFDRIVER       Driver,
	IN PWDFDEVICE_INIT DeviceInit
)
{
	NTSTATUS                      status = STATUS_SUCCESS;
	WDF_IO_QUEUE_CONFIG           queueConfig;
	WDF_OBJECT_ATTRIBUTES         attributes;
	WDFDEVICE                     device;
	WDF_INTERRUPT_CONFIG interruptConfig;
	WDFQUEUE                      queue;
	UCHAR                         minorFunction;
	PMLFS_CONTEXT               devContext;

	UNREFERENCED_PARAMETER(Driver);

	PAGED_CODE();

	MlfsPrint(DEBUG_LEVEL_INFO, DBG_PNP,
		"MlfsEvtDeviceAdd called\n");

	//
	// Tell framework this is a filter driver. Filter drivers by default are  
	// not power policy owners. This works well for this driver because
	// HIDclass driver is the power policy owner for HID minidrivers.
	//

	WdfFdoInitSetFilter(DeviceInit);

	{
		WDF_PNPPOWER_EVENT_CALLBACKS pnpCallbacks;
		WDF_PNPPOWER_EVENT_CALLBACKS_INIT(&pnpCallbacks);

		pnpCallbacks.EvtDevicePrepareHardware = OnPrepareHardware;
		pnpCallbacks.EvtDeviceReleaseHardware = OnReleaseHardware;
		pnpCallbacks.EvtDeviceD0Entry = OnD0Entry;
		pnpCallbacks.EvtDeviceD0Exit = OnD0Exit;

		WdfDeviceInitSetPnpPowerEventCallbacks(DeviceInit, &pnpCallbacks);
	}

	//
	// Setup the device context
	//

	WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&attributes, MLFS_CONTEXT);

	//
	// Create a framework device object.This call will in turn create
	// a WDM device object, attach to the lower stack, and set the
	// appropriate flags and attributes.
	//

	status = WdfDeviceCreate(&DeviceInit, &attributes, &device);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_PNP,
			"WdfDeviceCreate failed with status code 0x%x\n", status);

		return status;
	}

	WDF_IO_QUEUE_CONFIG_INIT_DEFAULT_QUEUE(&queueConfig, WdfIoQueueDispatchParallel);

	queueConfig.EvtIoInternalDeviceControl = MlfsEvtInternalDeviceControl;

	status = WdfIoQueueCreate(device,
		&queueConfig,
		WDF_NO_OBJECT_ATTRIBUTES,
		&queue
	);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_PNP,
			"WdfIoQueueCreate failed 0x%x\n", status);

		return status;
	}

	//
	// Create manual I/O queue to take care of hid report read requests
	//

	devContext = GetDeviceContext(device);

	devContext->TouchScreenBooted = false;

	devContext->FxDevice = device;

	WDF_IO_QUEUE_CONFIG_INIT(&queueConfig, WdfIoQueueDispatchManual);

	queueConfig.PowerManaged = WdfFalse;

	status = WdfIoQueueCreate(device,
		&queueConfig,
		WDF_NO_OBJECT_ATTRIBUTES,
		&devContext->ReportQueue
	);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_PNP,
			"WdfIoQueueCreate failed 0x%x\n", status);

		return status;
	}

	//
	// Create an interrupt object for hardware notifications
	//
	WDF_INTERRUPT_CONFIG_INIT(
		&interruptConfig,
		OnInterruptIsr,
		NULL);
	interruptConfig.PassiveHandling = TRUE;

	status = WdfInterruptCreate(
		device,
		&interruptConfig,
		WDF_NO_OBJECT_ATTRIBUTES,
		&devContext->Interrupt);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_PNP,
			"Error creating WDF interrupt object - %!STATUS!",
			status);

		return status;
	}

	WDF_TIMER_CONFIG              timerConfig;
	WDFTIMER                      hTimer;

	WDF_TIMER_CONFIG_INIT_PERIODIC(&timerConfig, MlfsTimerFunc, 10);

	WDF_OBJECT_ATTRIBUTES_INIT(&attributes);
	attributes.ParentObject = device;
	status = WdfTimerCreate(&timerConfig, &attributes, &hTimer);
	devContext->Timer = hTimer;
	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_PNP, "(%!FUNC!) WdfTimerCreate failed status:%!STATUS!\n", status);
		return status;
	}

	//
	// Initialize DeviceMode
	//

	devContext->DeviceMode = DEVICE_MODE_MOUSE;

	return status;
}

VOID
MlfsEvtInternalDeviceControl(
	IN WDFQUEUE     Queue,
	IN WDFREQUEST   Request,
	IN size_t       OutputBufferLength,
	IN size_t       InputBufferLength,
	IN ULONG        IoControlCode
)
{
	NTSTATUS            status = STATUS_SUCCESS;
	WDFDEVICE           device;
	PMLFS_CONTEXT     devContext;
	BOOLEAN             completeRequest = TRUE;

	UNREFERENCED_PARAMETER(OutputBufferLength);
	UNREFERENCED_PARAMETER(InputBufferLength);

	device = WdfIoQueueGetDevice(Queue);
	devContext = GetDeviceContext(device);

	MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
		"%s, Queue:0x%p, Request:0x%p\n",
		DbgHidInternalIoctlString(IoControlCode),
		Queue,
		Request
	);

	//
	// Please note that HIDCLASS provides the buffer in the Irp->UserBuffer
	// field irrespective of the ioctl buffer type. However, framework is very
	// strict about type checking. You cannot get Irp->UserBuffer by using
	// WdfRequestRetrieveOutputMemory if the ioctl is not a METHOD_NEITHER
	// internal ioctl. So depending on the ioctl code, we will either
	// use retreive function or escape to WDM to get the UserBuffer.
	//

	switch (IoControlCode)
	{

	case IOCTL_HID_GET_DEVICE_DESCRIPTOR:
		//
		// Retrieves the device's HID descriptor.
		//
		status = MlfsGetHidDescriptor(device, Request);
		break;

	case IOCTL_HID_GET_DEVICE_ATTRIBUTES:
		//
		//Retrieves a device's attributes in a HID_DEVICE_ATTRIBUTES structure.
		//
		status = MlfsGetDeviceAttributes(Request);
		break;

	case IOCTL_HID_GET_REPORT_DESCRIPTOR:
		//
		//Obtains the report descriptor for the HID device.
		//
		status = MlfsGetReportDescriptor(device, Request);
		break;

	case IOCTL_HID_GET_STRING:
		//
		// Requests that the HID minidriver retrieve a human-readable string
		// for either the manufacturer ID, the product ID, or the serial number
		// from the string descriptor of the device. The minidriver must send
		// a Get String Descriptor request to the device, in order to retrieve
		// the string descriptor, then it must extract the string at the
		// appropriate index from the string descriptor and return it in the
		// output buffer indicated by the IRP. Before sending the Get String
		// Descriptor request, the minidriver must retrieve the appropriate
		// index for the manufacturer ID, the product ID or the serial number
		// from the device extension of a top level collection associated with
		// the device.
		//
		status = MlfsGetString(Request);
		break;

	case IOCTL_HID_WRITE_REPORT:
	case IOCTL_HID_SET_OUTPUT_REPORT:
		//
		//Transmits a class driver-supplied report to the device.
		//
		status = MlfsWriteReport(devContext, Request);
		break;

	case IOCTL_HID_READ_REPORT:
	case IOCTL_HID_GET_INPUT_REPORT:
		//
		// Returns a report from the device into a class driver-supplied buffer.
		// 
		status = MlfsReadReport(devContext, Request, &completeRequest);
		break;

	case IOCTL_HID_SET_FEATURE:
		//
		// This sends a HID class feature report to a top-level collection of
		// a HID class device.
		//
		status = MlfsSetFeature(devContext, Request, &completeRequest);
		break;

	case IOCTL_HID_GET_FEATURE:
		//
		// returns a feature report associated with a top-level collection
		//
		status = MlfsGetFeature(devContext, Request, &completeRequest);
		break;

	case IOCTL_HID_ACTIVATE_DEVICE:
		//
		// Makes the device ready for I/O operations.
		//
	case IOCTL_HID_DEACTIVATE_DEVICE:
		//
		// Causes the device to cease operations and terminate all outstanding
		// I/O requests.
		//
	default:
		status = STATUS_NOT_SUPPORTED;
		break;
	}

	if (completeRequest)
	{
		WdfRequestComplete(Request, status);

		MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
			"%s completed, Queue:0x%p, Request:0x%p\n",
			DbgHidInternalIoctlString(IoControlCode),
			Queue,
			Request
		);
	}
	else
	{
		MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
			"%s deferred, Queue:0x%p, Request:0x%p\n",
			DbgHidInternalIoctlString(IoControlCode),
			Queue,
			Request
		);
	}

	return;
}

NTSTATUS
MlfsGetHidDescriptor(
	IN WDFDEVICE Device,
	IN WDFREQUEST Request
)
{
	NTSTATUS            status = STATUS_SUCCESS;
	size_t              bytesToCopy = 0;
	WDFMEMORY           memory;

	UNREFERENCED_PARAMETER(Device);

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetHidDescriptor Entry\n");

	//
	// This IOCTL is METHOD_NEITHER so WdfRequestRetrieveOutputMemory
	// will correctly retrieve buffer from Irp->UserBuffer. 
	// Remember that HIDCLASS provides the buffer in the Irp->UserBuffer
	// field irrespective of the ioctl buffer type. However, framework is very
	// strict about type checking. You cannot get Irp->UserBuffer by using
	// WdfRequestRetrieveOutputMemory if the ioctl is not a METHOD_NEITHER
	// internal ioctl.
	//
	status = WdfRequestRetrieveOutputMemory(Request, &memory);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfRequestRetrieveOutputMemory failed 0x%x\n", status);

		return status;
	}

	//
	// Use hardcoded "HID Descriptor" 
	//
	bytesToCopy = DefaultHidDescriptor.bLength;

	if (bytesToCopy == 0)
	{
		status = STATUS_INVALID_DEVICE_STATE;

		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"DefaultHidDescriptor is zero, 0x%x\n", status);

		return status;
	}

	status = WdfMemoryCopyFromBuffer(memory,
		0, // Offset
		(PVOID)&DefaultHidDescriptor,
		bytesToCopy);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfMemoryCopyFromBuffer failed 0x%x\n", status);

		return status;
	}

	//
	// Report how many bytes were copied
	//
	WdfRequestSetInformation(Request, bytesToCopy);

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetHidDescriptor Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsGetReportDescriptor(
	IN WDFDEVICE Device,
	IN WDFREQUEST Request
)
{
	NTSTATUS            status = STATUS_SUCCESS;
	ULONG_PTR           bytesToCopy;
	WDFMEMORY           memory;

	PMLFS_CONTEXT devContext = GetDeviceContext(Device);

	UNREFERENCED_PARAMETER(Device);

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetReportDescriptor Entry\n");

#define MT_TOUCH_COLLECTION												\
			MT_TOUCH_COLLECTION0 \
			0x26, devContext->max_x_hid[0], devContext->max_x_hid[1],                   /*       LOGICAL_MAXIMUM (WIDTH)    */ \
			MT_TOUCH_COLLECTION1 \
			0x26, devContext->max_y_hid[0], devContext->max_y_hid[1],                   /*       LOGICAL_MAXIMUM (HEIGHT)    */ \
			MT_TOUCH_COLLECTION2 \

	HID_REPORT_DESCRIPTOR ReportDescriptor[] = {
		//
		// Multitouch report starts here
		//
		0x05, 0x0d,                         // USAGE_PAGE (Digitizers)
		0x09, 0x04,                         // USAGE (Touch Screen)
		0xa1, 0x01,                         // COLLECTION (Application)
		0x85, REPORTID_MTOUCH,              //   REPORT_ID (Touch)
		0x09, 0x22,                         //   USAGE (Finger)
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		MT_TOUCH_COLLECTION
		USAGE_PAGE
		0xc0,                               // END_COLLECTION
	};

	//
	// This IOCTL is METHOD_NEITHER so WdfRequestRetrieveOutputMemory
	// will correctly retrieve buffer from Irp->UserBuffer. 
	// Remember that HIDCLASS provides the buffer in the Irp->UserBuffer
	// field irrespective of the ioctl buffer type. However, framework is very
	// strict about type checking. You cannot get Irp->UserBuffer by using
	// WdfRequestRetrieveOutputMemory if the ioctl is not a METHOD_NEITHER
	// internal ioctl.
	//
	status = WdfRequestRetrieveOutputMemory(Request, &memory);
	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfRequestRetrieveOutputMemory failed 0x%x\n", status);

		return status;
	}

	//
	// Use hardcoded Report descriptor
	//
	bytesToCopy = DefaultHidDescriptor.DescriptorList[0].wReportLength;

	if (bytesToCopy == 0)
	{
		status = STATUS_INVALID_DEVICE_STATE;

		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"DefaultHidDescriptor's reportLength is zero, 0x%x\n", status);

		return status;
	}

	status = WdfMemoryCopyFromBuffer(memory,
		0,
		(PVOID)ReportDescriptor,
		bytesToCopy);
	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfMemoryCopyFromBuffer failed 0x%x\n", status);

		return status;
	}

	//
	// Report how many bytes were copied
	//
	WdfRequestSetInformation(Request, bytesToCopy);

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetReportDescriptor Exit = 0x%x\n", status);

	return status;
}


NTSTATUS
MlfsGetDeviceAttributes(
	IN WDFREQUEST Request
)
{
	NTSTATUS                 status = STATUS_SUCCESS;
	PHID_DEVICE_ATTRIBUTES   deviceAttributes = NULL;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetDeviceAttributes Entry\n");

	//
	// This IOCTL is METHOD_NEITHER so WdfRequestRetrieveOutputMemory
	// will correctly retrieve buffer from Irp->UserBuffer. 
	// Remember that HIDCLASS provides the buffer in the Irp->UserBuffer
	// field irrespective of the ioctl buffer type. However, framework is very
	// strict about type checking. You cannot get Irp->UserBuffer by using
	// WdfRequestRetrieveOutputMemory if the ioctl is not a METHOD_NEITHER
	// internal ioctl.
	//
	status = WdfRequestRetrieveOutputBuffer(Request,
		sizeof(HID_DEVICE_ATTRIBUTES),
		(PVOID *)&deviceAttributes,
		NULL);
	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfRequestRetrieveOutputBuffer failed 0x%x\n", status);

		return status;
	}

	//
	// Set USB device descriptor
	//

	deviceAttributes->Size = sizeof(HID_DEVICE_ATTRIBUTES);
	deviceAttributes->VendorID = MLFS_VID;
	deviceAttributes->ProductID = MLFS_PID;
	deviceAttributes->VersionNumber = MLFS_VERSION;

	//
	// Report how many bytes were copied
	//
	WdfRequestSetInformation(Request, sizeof(HID_DEVICE_ATTRIBUTES));

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetDeviceAttributes Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsGetString(
	IN WDFREQUEST Request
)
{

	NTSTATUS status = STATUS_SUCCESS;
	PWSTR pwstrID;
	size_t lenID;
	WDF_REQUEST_PARAMETERS params;
	void *pStringBuffer = NULL;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetString Entry\n");

	WDF_REQUEST_PARAMETERS_INIT(&params);
	WdfRequestGetParameters(Request, &params);

	switch ((ULONG_PTR)params.Parameters.DeviceIoControl.Type3InputBuffer & 0xFFFF)
	{
	case HID_STRING_ID_IMANUFACTURER:
		pwstrID = L"Mlfs.\0";
		break;

	case HID_STRING_ID_IPRODUCT:
		pwstrID = L"MaxTouch Touch Screen\0";
		break;

	case HID_STRING_ID_ISERIALNUMBER:
		pwstrID = L"123123123\0";
		break;

	default:
		pwstrID = NULL;
		break;
	}

	lenID = pwstrID ? wcslen(pwstrID) * sizeof(WCHAR) + sizeof(UNICODE_NULL) : 0;

	if (pwstrID == NULL)
	{

		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"MlfsGetString Invalid request type\n");

		status = STATUS_INVALID_PARAMETER;

		return status;
	}

	status = WdfRequestRetrieveOutputBuffer(Request,
		lenID,
		&pStringBuffer,
		&lenID);

	if (!NT_SUCCESS(status))
	{

		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"MlfsGetString WdfRequestRetrieveOutputBuffer failed Status 0x%x\n", status);

		return status;
	}

	RtlCopyMemory(pStringBuffer, pwstrID, lenID);

	WdfRequestSetInformation(Request, lenID);

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetString Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsWriteReport(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request
)
{
	NTSTATUS status = STATUS_SUCCESS;
	WDF_REQUEST_PARAMETERS params;
	PHID_XFER_PACKET transferPacket = NULL;
	size_t bytesWritten = 0;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsWriteReport Entry\n");

	WDF_REQUEST_PARAMETERS_INIT(&params);
	WdfRequestGetParameters(Request, &params);

	if (params.Parameters.DeviceIoControl.InputBufferLength < sizeof(HID_XFER_PACKET))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"MlfsWriteReport Xfer packet too small\n");

		status = STATUS_BUFFER_TOO_SMALL;
	}
	else
	{

		transferPacket = (PHID_XFER_PACKET)WdfRequestWdmGetIrp(Request)->UserBuffer;

		if (transferPacket == NULL)
		{
			MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
				"MlfsWriteReport No xfer packet\n");

			status = STATUS_INVALID_DEVICE_REQUEST;
		}
		else
		{
			//
			// switch on the report id
			//

			switch (transferPacket->reportId)
			{
			default:

				MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
					"MlfsWriteReport Unhandled report type %d\n", transferPacket->reportId);

				status = STATUS_INVALID_PARAMETER;

				break;
			}
		}
	}

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsWriteReport Exit = 0x%x\n", status);

	return status;

}

NTSTATUS
MlfsProcessVendorReport(
	IN PMLFS_CONTEXT DevContext,
	IN PVOID ReportBuffer,
	IN ULONG ReportBufferLen,
	OUT size_t* BytesWritten
)
{
	NTSTATUS status = STATUS_SUCCESS;
	WDFREQUEST reqRead;
	PVOID pReadReport = NULL;
	size_t bytesReturned = 0;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsProcessVendorReport Entry\n");

	status = WdfIoQueueRetrieveNextRequest(DevContext->ReportQueue,
		&reqRead);

	if (NT_SUCCESS(status))
	{
		status = WdfRequestRetrieveOutputBuffer(reqRead,
			ReportBufferLen,
			&pReadReport,
			&bytesReturned);

		if (NT_SUCCESS(status))
		{
			//
			// Copy ReportBuffer into read request
			//

			if (bytesReturned > ReportBufferLen)
			{
				bytesReturned = ReportBufferLen;
			}

			RtlCopyMemory(pReadReport,
				ReportBuffer,
				bytesReturned);

			//
			// Complete read with the number of bytes returned as info
			//

			WdfRequestCompleteWithInformation(reqRead,
				status,
				bytesReturned);

			MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
				"MlfsProcessVendorReport %d bytes returned\n", bytesReturned);

			//
			// Return the number of bytes written for the write request completion
			//

			*BytesWritten = bytesReturned;

			MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
				"%s completed, Queue:0x%p, Request:0x%p\n",
				DbgHidInternalIoctlString(IOCTL_HID_READ_REPORT),
				DevContext->ReportQueue,
				reqRead);
		}
		else
		{
			MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
				"WdfRequestRetrieveOutputBuffer failed Status 0x%x\n", status);
		}
	}
	else
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfIoQueueRetrieveNextRequest failed Status 0x%x\n", status);
	}

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsProcessVendorReport Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsReadReport(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
)
{
	NTSTATUS status = STATUS_SUCCESS;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsReadReport Entry\n");

	//
	// Forward this read request to our manual queue
	// (in other words, we are going to defer this request
	// until we have a corresponding write request to
	// match it with)
	//

	status = WdfRequestForwardToIoQueue(Request, DevContext->ReportQueue);

	if (!NT_SUCCESS(status))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"WdfRequestForwardToIoQueue failed Status 0x%x\n", status);
	}
	else
	{
		*CompleteRequest = FALSE;
	}

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsReadReport Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsSetFeature(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
)
{
	NTSTATUS status = STATUS_SUCCESS;
	WDF_REQUEST_PARAMETERS params;
	PHID_XFER_PACKET transferPacket = NULL;
	MlfsFeatureReport* pReport = NULL;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsSetFeature Entry\n");

	WDF_REQUEST_PARAMETERS_INIT(&params);
	WdfRequestGetParameters(Request, &params);

	if (params.Parameters.DeviceIoControl.InputBufferLength < sizeof(HID_XFER_PACKET))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"MlfsSetFeature Xfer packet too small\n");

		status = STATUS_BUFFER_TOO_SMALL;
	}
	else
	{

		transferPacket = (PHID_XFER_PACKET)WdfRequestWdmGetIrp(Request)->UserBuffer;

		if (transferPacket == NULL)
		{
			MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
				"MlfsWriteReport No xfer packet\n");

			status = STATUS_INVALID_DEVICE_REQUEST;
		}
		else
		{
			//
			// switch on the report id
			//

			switch (transferPacket->reportId)
			{
			case REPORTID_FEATURE:

				if (transferPacket->reportBufferLen == sizeof(MlfsFeatureReport))
				{
					pReport = (MlfsFeatureReport*)transferPacket->reportBuffer;

					DevContext->DeviceMode = pReport->DeviceMode;

					MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
						"MlfsSetFeature DeviceMode = 0x%x\n", DevContext->DeviceMode);
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;

					MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
						"MlfsSetFeature Error transferPacket->reportBufferLen (%d) is different from sizeof(MlfsFeatureReport) (%d)\n",
						transferPacket->reportBufferLen,
						sizeof(MlfsFeatureReport));
				}

				break;

			default:

				MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
					"MlfsSetFeature Unhandled report type %d\n", transferPacket->reportId);

				status = STATUS_INVALID_PARAMETER;

				break;
			}
		}
	}

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsSetFeature Exit = 0x%x\n", status);

	return status;
}

NTSTATUS
MlfsGetFeature(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
)
{
	NTSTATUS status = STATUS_SUCCESS;
	WDF_REQUEST_PARAMETERS params;
	PHID_XFER_PACKET transferPacket = NULL;

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetFeature Entry\n");

	WDF_REQUEST_PARAMETERS_INIT(&params);
	WdfRequestGetParameters(Request, &params);

	if (params.Parameters.DeviceIoControl.OutputBufferLength < sizeof(HID_XFER_PACKET))
	{
		MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
			"MlfsGetFeature Xfer packet too small\n");

		status = STATUS_BUFFER_TOO_SMALL;
	}
	else
	{

		transferPacket = (PHID_XFER_PACKET)WdfRequestWdmGetIrp(Request)->UserBuffer;

		if (transferPacket == NULL)
		{
			MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
				"MlfsGetFeature No xfer packet\n");

			status = STATUS_INVALID_DEVICE_REQUEST;
		}
		else
		{
			//
			// switch on the report id
			//

			switch (transferPacket->reportId)
			{
			case REPORTID_MTOUCH:
			{

				MlfsMaxCountReport* pReport = NULL;

				if (transferPacket->reportBufferLen == sizeof(MlfsMaxCountReport))
				{
					pReport = (MlfsMaxCountReport*)transferPacket->reportBuffer;

					pReport->MaximumCount = MULTI_MAX_COUNT;

					MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
						"MlfsGetFeature MaximumCount = 0x%x\n", MULTI_MAX_COUNT);
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;

					MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
						"MlfsGetFeature Error transferPacket->reportBufferLen (%d) is different from sizeof(MlfsMaxCountReport) (%d)\n",
						transferPacket->reportBufferLen,
						sizeof(MlfsMaxCountReport));
				}

				break;
			}

			case REPORTID_FEATURE:
			{

				MlfsFeatureReport* pReport = NULL;

				if (transferPacket->reportBufferLen == sizeof(MlfsFeatureReport))
				{
					pReport = (MlfsFeatureReport*)transferPacket->reportBuffer;

					pReport->DeviceMode = DevContext->DeviceMode;

					pReport->DeviceIdentifier = 0;

					MlfsPrint(DEBUG_LEVEL_INFO, DBG_IOCTL,
						"MlfsGetFeature DeviceMode = 0x%x\n", DevContext->DeviceMode);
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;

					MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
						"MlfsGetFeature Error transferPacket->reportBufferLen (%d) is different from sizeof(MlfsFeatureReport) (%d)\n",
						transferPacket->reportBufferLen,
						sizeof(MlfsFeatureReport));
				}

				break;
			}

			default:

				MlfsPrint(DEBUG_LEVEL_ERROR, DBG_IOCTL,
					"MlfsGetFeature Unhandled report type %d\n", transferPacket->reportId);

				status = STATUS_INVALID_PARAMETER;

				break;
			}
		}
	}

	MlfsPrint(DEBUG_LEVEL_VERBOSE, DBG_IOCTL,
		"MlfsGetFeature Exit = 0x%x\n", status);

	return status;
}

PCHAR
DbgHidInternalIoctlString(
	IN ULONG IoControlCode
)
{
	switch (IoControlCode)
	{
	case IOCTL_HID_GET_DEVICE_DESCRIPTOR:
		return "IOCTL_HID_GET_DEVICE_DESCRIPTOR";
	case IOCTL_HID_GET_REPORT_DESCRIPTOR:
		return "IOCTL_HID_GET_REPORT_DESCRIPTOR";
	case IOCTL_HID_READ_REPORT:
		return "IOCTL_HID_READ_REPORT";
	case IOCTL_HID_GET_DEVICE_ATTRIBUTES:
		return "IOCTL_HID_GET_DEVICE_ATTRIBUTES";
	case IOCTL_HID_WRITE_REPORT:
		return "IOCTL_HID_WRITE_REPORT";
	case IOCTL_HID_SET_FEATURE:
		return "IOCTL_HID_SET_FEATURE";
	case IOCTL_HID_GET_FEATURE:
		return "IOCTL_HID_GET_FEATURE";
	case IOCTL_HID_GET_STRING:
		return "IOCTL_HID_GET_STRING";
	case IOCTL_HID_ACTIVATE_DEVICE:
		return "IOCTL_HID_ACTIVATE_DEVICE";
	case IOCTL_HID_DEACTIVATE_DEVICE:
		return "IOCTL_HID_DEACTIVATE_DEVICE";
	case IOCTL_HID_SEND_IDLE_NOTIFICATION_REQUEST:
		return "IOCTL_HID_SEND_IDLE_NOTIFICATION_REQUEST";
	case IOCTL_HID_SET_OUTPUT_REPORT:
		return "IOCTL_HID_SET_OUTPUT_REPORT";
	case IOCTL_HID_GET_INPUT_REPORT:
		return "IOCTL_HID_GET_INPUT_REPORT";
	default:
		return "Unknown IOCTL";
	}
}