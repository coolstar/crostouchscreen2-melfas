#if !defined(_MLFS_H_)
#define _MLFS_H_

#pragma warning(disable:4200)  // suppress nameless struct/union warning
#pragma warning(disable:4201)  // suppress nameless struct/union warning
#pragma warning(disable:4214)  // suppress bit field types other than int warning
#include <initguid.h>
#include <wdm.h>

#pragma warning(default:4200)
#pragma warning(default:4201)
#pragma warning(default:4214)
#include <wdf.h>

#pragma warning(disable:4201)  // suppress nameless struct/union warning
#pragma warning(disable:4214)  // suppress bit field types other than int warning
#include <hidport.h>

#include "hidcommon.h"
#include "spb.h"

#include "mip4_ts.h"

//
// String definitions
//

#define DRIVERNAME                 "crostouchscreen2.sys: "

#define MLFS_POOL_TAG            (ULONG) 'sflM'

#define NTDEVICE_NAME_STRING       L"\\Device\\MLFS0000"
#define SYMBOLIC_NAME_STRING       L"\\DosDevices\\MLFS0000"

#define MT_TOUCH_COLLECTION0                                                    \
    0xa1, 0x02,                         /*     COLLECTION (Logical)         */ \
    0x09, 0x42,                         /*       USAGE (Tip Switch)         */ \
    0x15, 0x00,                         /*       LOGICAL_MINIMUM (0)        */ \
    0x25, 0x01,                         /*       LOGICAL_MAXIMUM (1)        */ \
    0x75, 0x01,                         /*       REPORT_SIZE (1)            */ \
    0x95, 0x01,                         /*       REPORT_COUNT (1)           */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0x09, 0x47,                         /*       USAGE (Confidence)          */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0x95, 0x06,                         /*       REPORT_COUNT (6)           */ \
    0x81, 0x03,                         /*       INPUT (Cnst,Ary,Abs)       */ \
    0x75, 0x08,                         /*       REPORT_SIZE (8)            */ \
    0x09, 0x51,                         /*       USAGE (Contact Identifier) */ \
    0x95, 0x01,                         /*       REPORT_COUNT (1)           */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0x05, 0x01,                         /*       USAGE_PAGE (Generic Desk.. */ \
    0x75, 0x10,                         /*       REPORT_SIZE (16)           */ \
    0x55, 0x00,                         /*       UNIT_EXPONENT (0)          */ \
    0x65, 0x00,                         /*       UNIT (None)                */ \
    0x35, 0x00,                         /*       PHYSICAL_MINIMUM (0)       */ \
    0x46, 0x00, 0x00,                   /*       PHYSICAL_MAXIMUM (0)       */ 


//0x26, 0x56, 0x05,                   /*       LOGICAL_MAXIMUM (1366)    */

#define MT_TOUCH_COLLECTION1												\
    0x09, 0x30,                         /*       USAGE (X)                  */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ 

//0x26, 0x00, 0x03,                   /*       LOGICAL_MAXIMUM (768)    */ 

#define MT_TOUCH_COLLECTION2												\
    0x09, 0x31,                         /*       USAGE (Y)                  */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0x05, 0x0d,                         /*       USAGE PAGE (Digitizers)    */ \
    0x09, 0x48,                         /*       USAGE (Width)              */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0x09, 0x49,                         /*       USAGE (Height)             */ \
    0x81, 0x02,                         /*       INPUT (Data,Var,Abs)       */ \
    0xc0,                               /*    END_COLLECTION                */

#if 0
0x26, 0x56, 0x05,                   /*       LOGICAL_MAXIMUM (1366)    */
0x26, 0x00, 0x03,                   /*       LOGICAL_MAXIMUM (768)    */
#endif

#define MT_REF_TOUCH_COLLECTION												\
	MT_TOUCH_COLLECTION0 \
	0x26, 0x00, 0x00,                   /*       LOGICAL_MAXIMUM (1366)    */ \
	MT_TOUCH_COLLECTION1 \
	0x26, 0x00, 0x00,                   /*       LOGICAL_MAXIMUM (768)    */ \
	MT_TOUCH_COLLECTION2 \

#define USAGE_PAGE \
	0x05, 0x0d,                         /*    USAGE_PAGE (Digitizers) */  \
	0x09, 0x54,                         /*    USAGE (Contact Count) */  \
	0x95, 0x01,                         /*    REPORT_COUNT (1) */  \
	0x75, 0x08,                         /*    REPORT_SIZE (8) */  \
	0x15, 0x00,                         /*    LOGICAL_MINIMUM (0) */  \
	0x25, 0x08,                         /*    LOGICAL_MAXIMUM (8) */  \
	0x81, 0x02,                         /*    INPUT (Data,Var,Abs) */  \
	0x09, 0x55,                         /*    USAGE(Contact Count Maximum) */  \
	0xb1, 0x02,                         /*    FEATURE (Data,Var,Abs) */  \

									//
									// This is the default report descriptor for the Hid device provided
									// by the mini driver in response to IOCTL_HID_GET_REPORT_DESCRIPTOR.
									// 

	typedef UCHAR HID_REPORT_DESCRIPTOR, *PHID_REPORT_DESCRIPTOR;

#ifdef DESCRIPTOR_DEF
HID_REPORT_DESCRIPTOR DefaultReportDescriptor[] = {
	//
	// Multitouch report starts here
	//
	0x05, 0x0d,                         // USAGE_PAGE (Digitizers)
	0x09, 0x04,                         // USAGE (Touch Screen)
	0xa1, 0x01,                         // COLLECTION (Application)
	0x85, REPORTID_MTOUCH,              //   REPORT_ID (Touch)
	0x09, 0x22,                         //   USAGE (Finger)
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	MT_REF_TOUCH_COLLECTION
	USAGE_PAGE
	0xc0,                               // END_COLLECTION
};


//
// This is the default HID descriptor returned by the mini driver
// in response to IOCTL_HID_GET_DEVICE_DESCRIPTOR. The size
// of report descriptor is currently the size of DefaultReportDescriptor.
//

CONST HID_DESCRIPTOR DefaultHidDescriptor = {
	0x09,   // length of HID descriptor
	0x21,   // descriptor type == HID  0x21
	0x0100, // hid spec release
	0x00,   // country code == Not Specified
	0x01,   // number of HID class descriptors
	{ 0x22,   // descriptor type 
	sizeof(DefaultReportDescriptor) }  // total length of report descriptor
};
#endif

#define true 1
#define false 0

typedef struct _MLFS_CONTEXT
{

	WDFDEVICE FxDevice;

	WDFQUEUE ReportQueue;

	WDFQUEUE IdleQueue;

	BYTE DeviceMode;

	SPB_CONTEXT I2CContext;

	WDFINTERRUPT Interrupt;

	BOOLEAN ConnectInterrupt;

	BOOLEAN TouchScreenBooted;

	BOOLEAN RegsSet;

	WDFTIMER Timer;

	UINT32 TouchCount;

	uint8_t      Flags[20];

	USHORT    XValue[20];

	USHORT    YValue[20];

	USHORT    AREA[20];

	uint16_t max_x;
	uint16_t max_y;

	uint8_t node_x;
	uint8_t node_y;
	uint8_t node_key;

	uint8_t ppm_x;
	uint8_t ppm_y;

	uint16_t event_size;
	uint8_t event_format;

	unsigned short key_code[MIP4_MAX_KEYS];

	uint8_t max_x_hid[2];
	uint8_t max_y_hid[2];

} MLFS_CONTEXT, *PMLFS_CONTEXT;

WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(MLFS_CONTEXT, GetDeviceContext)

//
// Power Idle Workitem context
// 
typedef struct _IDLE_WORKITEM_CONTEXT
{
	// Handle to a WDF device object
	WDFDEVICE FxDevice;

	// Handle to a WDF request object
	WDFREQUEST FxRequest;

} IDLE_WORKITEM_CONTEXT, * PIDLE_WORKITEM_CONTEXT;
WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(IDLE_WORKITEM_CONTEXT, GetIdleWorkItemContext)

//
// Function definitions
//

extern "C" {
	DRIVER_INITIALIZE DriverEntry;
}

EVT_WDF_DRIVER_UNLOAD MlfsDriverUnload;

EVT_WDF_DRIVER_DEVICE_ADD MlfsEvtDeviceAdd;

EVT_WDFDEVICE_WDM_IRP_PREPROCESS MlfsEvtWdmPreprocessMnQueryId;

EVT_WDF_IO_QUEUE_IO_INTERNAL_DEVICE_CONTROL MlfsEvtInternalDeviceControl;

NTSTATUS
MlfsGetHidDescriptor(
	IN WDFDEVICE Device,
	IN WDFREQUEST Request
);

NTSTATUS
MlfsGetReportDescriptor(
	IN WDFDEVICE Device,
	IN WDFREQUEST Request
);

NTSTATUS
MlfsGetDeviceAttributes(
	IN WDFREQUEST Request
);

NTSTATUS
MlfsGetString(
	IN WDFREQUEST Request
);

NTSTATUS
MlfsWriteReport(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request
);

NTSTATUS
MlfsProcessVendorReport(
	IN PMLFS_CONTEXT DevContext,
	IN PVOID ReportBuffer,
	IN ULONG ReportBufferLen,
	OUT size_t* BytesWritten
);

NTSTATUS
MlfsReadReport(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
);

NTSTATUS
MlfsSetFeature(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
);

NTSTATUS
MlfsGetFeature(
	IN PMLFS_CONTEXT DevContext,
	IN WDFREQUEST Request,
	OUT BOOLEAN* CompleteRequest
);

PCHAR
DbgHidInternalIoctlString(
	IN ULONG        IoControlCode
);

VOID
MlfsCompleteIdleIrp(
	IN PMLFS_CONTEXT FxDeviceContext
);

//
// Helper macros
//

#define DEBUG_LEVEL_ERROR   1
#define DEBUG_LEVEL_INFO    2
#define DEBUG_LEVEL_VERBOSE 3

#define DBG_INIT  1
#define DBG_PNP   2
#define DBG_IOCTL 4

#if 0
#define MlfsPrint(dbglevel, dbgcatagory, fmt, ...) {          \
    if (MlfsDebugLevel >= dbglevel &&                         \
        (MlfsDebugCatagories && dbgcatagory))                 \
	    {                                                           \
        DbgPrint(DRIVERNAME);                                   \
        DbgPrint(fmt, __VA_ARGS__);                             \
	    }                                                           \
}
#else
#define MlfsPrint(dbglevel, fmt, ...) {                       \
}
#endif

#endif