
/** Should be at least 16 bit. */
typedef unsigned short tc_value_part;

/** Should be at least 32 bit */
typedef long tc_temp;

/**
 * We need 64 + 2 bit for floating point support
 */
#define TC_VALUE_SIZE          80
#define TC_VALUE_PART_SIZE     16
#define TC_VALUE_PART_MASK     0xFFFF
#define TC_VALUE_PART_MAX_UINT 0xFFFF
#define TC_VALUE_PART_SIGN_BIT 0x8000

#define NUM_VALUE_PARTS     (TC_VALUE_SIZE / TC_VALUE_PART_SIZE)

/**
 * A two complement value. Note that the parts are store in BIG-Endian.
 * this makes most loops counting towards zero and allows simple access
 * to the sign bit.
 */
typedef struct tc_value {
	tc_value_part part[NUM_VALUE_PARTS];
} tc_value;


/**
 * Every operation produces two bits. Carry and Overflow.
 * Carry means unsigned overflow, Overflow means signed overflow.
 */
extern int tc_Carry, tc_Overflow;
