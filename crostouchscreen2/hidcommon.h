#if !defined(_MLFS_COMMON_H_)
#define _MLFS_COMMON_H_

//
//These are the device attributes returned by vmulti in response
// to IOCTL_HID_GET_DEVICE_ATTRIBUTES.
//

#define MLFS_PID              0xBACC
#define MLFS_VID              0x00FF
#define MLFS_VERSION          0x0001

//
// These are the report ids
//

#define REPORTID_MTOUCH         0x01
#define REPORTID_FEATURE        0x02

//
// Multitouch specific report information
//

#define MULTI_TIPSWITCH_BIT    1
#define MULTI_CONFIDENCE_BIT     2

#define MULTI_MIN_COORDINATE   0x0000
#define MULTI_MAX_COORDINATE   0x7FFF

#define MULTI_MAX_COUNT        10

#pragma pack(1)
typedef struct
{

	BYTE      Status;

	BYTE      ContactID;

	USHORT    XValue;

	USHORT    YValue;

	USHORT    Width;

	USHORT    Height;

}
TOUCH, *PTOUCH;

typedef struct _MLFS_MULTITOUCH_REPORT
{

	BYTE      ReportID;

	TOUCH     Touch[10];

	BYTE      ActualCount;

} MlfsMultiTouchReport;
#pragma pack()

//
// Feature report infomation
//

#define DEVICE_MODE_MOUSE        0x00
#define DEVICE_MODE_SINGLE_INPUT 0x01
#define DEVICE_MODE_MULTI_INPUT  0x02

#pragma pack(1)
typedef struct _MLFS_FEATURE_REPORT
{

	BYTE      ReportID;

	BYTE      DeviceMode;

	BYTE      DeviceIdentifier;

} MlfsFeatureReport;

typedef struct _MLFS_MAXCOUNT_REPORT
{

	BYTE         ReportID;

	BYTE         MaximumCount;

} MlfsMaxCountReport;
#pragma pack()

#endif
