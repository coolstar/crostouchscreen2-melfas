#include "stdint.h"

#define MIP4_DEVICE_NAME	"mip4_ts"

/*****************************************************************
 * Protocol
 * Version : MIP 4.0 Rev 5.4
 *****************************************************************/

 /* Address */
#define MIP4_R0_BOOT				0x00
#define MIP4_R1_BOOT_MODE			0x01
#define MIP4_R1_BOOT_BUF_ADDR			0x10
#define MIP4_R1_BOOT_STATUS			0x20
#define MIP4_R1_BOOT_CMD			0x30
#define MIP4_R1_BOOT_TARGET_ADDR		0x40
#define MIP4_R1_BOOT_SIZE			0x44

#define MIP4_R0_INFO				0x01
#define MIP4_R1_INFO_PRODUCT_NAME		0x00
#define MIP4_R1_INFO_RESOLUTION_X		0x10
#define MIP4_R1_INFO_RESOLUTION_Y		0x12
#define MIP4_R1_INFO_NODE_NUM_X			0x14
#define MIP4_R1_INFO_NODE_NUM_Y			0x15
#define MIP4_R1_INFO_KEY_NUM			0x16
#define MIP4_R1_INFO_PRESSURE_NUM		0x17
#define MIP4_R1_INFO_LENGTH_X			0x18
#define MIP4_R1_INFO_LENGTH_Y			0x1A
#define MIP4_R1_INFO_PPM_X			0x1C
#define MIP4_R1_INFO_PPM_Y			0x1D
#define MIP4_R1_INFO_VERSION_BOOT		0x20
#define MIP4_R1_INFO_VERSION_CORE		0x22
#define MIP4_R1_INFO_VERSION_APP		0x24
#define MIP4_R1_INFO_VERSION_PARAM		0x26
#define MIP4_R1_INFO_SECT_BOOT_START		0x30
#define MIP4_R1_INFO_SECT_BOOT_END		0x31
#define MIP4_R1_INFO_SECT_CORE_START		0x32
#define MIP4_R1_INFO_SECT_CORE_END		0x33
#define MIP4_R1_INFO_SECT_APP_START		0x34
#define MIP4_R1_INFO_SECT_APP_END		0x35
#define MIP4_R1_INFO_SECT_PARAM_START		0x36
#define MIP4_R1_INFO_SECT_PARAM_END		0x37
#define MIP4_R1_INFO_BUILD_DATE			0x40
#define MIP4_R1_INFO_BUILD_TIME			0x44
#define MIP4_R1_INFO_CHECKSUM_PRECALC		0x48
#define MIP4_R1_INFO_CHECKSUM_REALTIME		0x4A
#define MIP4_R1_INFO_PROTOCOL_NAME		0x50
#define MIP4_R1_INFO_PROTOCOL_VERSION		0x58
#define MIP4_R1_INFO_IC_ID			0x70
#define MIP4_R1_INFO_IC_NAME			0x71
#define MIP4_R1_INFO_IC_VENDOR_ID		0x75
#define MIP4_R1_INFO_IC_HW_CATEGORY		0x77
#define MIP4_R1_INFO_CONTACT_THD_SCR		0x78
#define MIP4_R1_INFO_CONTACT_THD_KEY		0x7A
#define MIP4_R1_INFO_PID				0x7C
#define MIP4_R1_INFO_VID				0x7E
#define MIP4_R1_INFO_SLAVE_ADDR			0x80

#define MIP4_R0_EVENT				0x02
#define MIP4_R1_EVENT_SUPPORTED_FUNC		0x00
#define MIP4_R1_EVENT_FORMAT			0x04
#define MIP4_R1_EVENT_SIZE			0x06
#define MIP4_R1_EVENT_PACKET_INFO		0x10
#define MIP4_R1_EVENT_PACKET_DATA		0x11

#define MIP4_R0_CTRL				0x06
#define MIP4_R1_CTRL_READY_STATUS		0x00
#define MIP4_R1_CTRL_EVENT_READY		0x01
#define MIP4_R1_CTRL_MODE			0x10
#define MIP4_R1_CTRL_EVENT_TRIGGER_TYPE		0x11
#define MIP4_R1_CTRL_RECALIBRATE		0x12
#define MIP4_R1_CTRL_POWER_STATE		0x13
#define MIP4_R1_CTRL_GESTURE_TYPE		0x14
#define MIP4_R1_CTRL_DISABLE_ESD_ALERT		0x18
#define MIP4_R1_CTRL_CHARGER_MODE		0x19
#define MIP4_R1_CTRL_HIGH_SENS_MODE		0x1A
#define MIP4_R1_CTRL_WINDOW_MODE		0x1B
#define MIP4_R1_CTRL_PALM_REJECTION		0x1C
#define MIP4_R1_CTRL_EDGE_CORRECTION		0x1D
#define MIP4_R1_CTRL_ENTER_GLOVE_MODE		0x1E
#define MIP4_R1_CTRL_I2C_ON_LPM			0x1F
#define MIP4_R1_CTRL_GESTURE_DEBUG		0x20
#define MIP4_R1_CTRL_PALM_EVENT			0x22
#define MIP4_R1_CTRL_PROXIMITY_SENSING		0x23

/* Value */
#define MIP4_BOOT_MODE_BOOT			0x01
#define MIP4_BOOT_MODE_APP			0x02

#define MIP4_BOOT_STATUS_BUSY			0x05
#define MIP4_BOOT_STATUS_ERROR			0x0E
#define MIP4_BOOT_STATUS_DONE			0xA0

#define MIP4_BOOT_CMD_MASS_ERASE		0x15
#define MIP4_BOOT_CMD_PROGRAM			0x54
#define MIP4_BOOT_CMD_ERASE			0x8F
#define MIP4_BOOT_CMD_WRITE			0xA5
#define MIP4_BOOT_CMD_READ			0xC2

#define MIP4_EVENT_INPUT_TYPE_KEY		0
#define MIP4_EVENT_INPUT_TYPE_SCREEN		1
#define MIP4_EVENT_INPUT_TYPE_PROXIMITY		2

#define I2C_RETRY_COUNT				3	/* 2~ */

#define MIP4_BUF_SIZE				128
#define MIP4_MAX_FINGERS			10
#define MIP4_MAX_KEYS				4

#define MIP4_TOUCH_MAJOR_MIN			0
#define MIP4_TOUCH_MAJOR_MAX			255
#define MIP4_TOUCH_MINOR_MIN			0
#define MIP4_TOUCH_MINOR_MAX			255
#define MIP4_PRESSURE_MIN			0
#define MIP4_PRESSURE_MAX			255

#define MIP4_FW_UPDATE_DEBUG		0	/* 0 (default) or 1 */

struct mip4_fw_version {
	uint16_t boot;
	uint16_t core;
	uint16_t app;
	uint16_t param;
};

#define MXT_T9_RELEASE		(1 << 5)
#define MXT_T9_PRESS		(1 << 6)
#define MXT_T9_DETECT		(1 << 7)