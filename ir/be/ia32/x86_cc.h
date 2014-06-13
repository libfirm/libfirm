#ifndef X86_CC_H
#define X86_CC_H

/** x86 condition codes (the numbers correspond to the real encoding order) */
typedef enum x86_condition_code_t {
	x86_cc_negated       = 0x01, /**< negates condition */

	x86_cc_overflow      = 0x00,                              /**< OF=1 */
	x86_cc_below         = 0x02,                              /**< CF=1 */
	x86_cc_equal         = 0x04,                              /**< ZF=1 */
	x86_cc_below_equal   = 0x06,                              /**< ZF=1 or CF=1 */
	x86_cc_sign          = 0x08,                              /**< SF=1 */
	x86_cc_parity        = 0x0A,                              /**< PF=1 */
	x86_cc_less          = 0x0C,                              /**< SF!=OF */
	x86_cc_less_equal    = 0x0E,                              /**< ZF=1 or SF!=OF */
	x86_cc_not_overflow  = x86_cc_negated|x86_cc_overflow,    /**< OF=0 */
	x86_cc_above_equal   = x86_cc_negated|x86_cc_below,       /**< CF=0 */
	x86_cc_not_equal     = x86_cc_negated|x86_cc_equal,       /**< ZF=0 */
	x86_cc_above         = x86_cc_negated|x86_cc_below_equal, /**< ZF=0 and CF=0 */
	x86_cc_not_sign      = x86_cc_negated|x86_cc_sign,        /**< SF=0 */
	x86_cc_not_parity    = x86_cc_negated|x86_cc_parity,      /**< PF=0 */
	x86_cc_greater_equal = x86_cc_negated|x86_cc_less,        /**< SF=OF */
	x86_cc_greater       = x86_cc_negated|x86_cc_less_equal,  /**< ZF=0 and SF=OF */

	/* the following codes are (unfortunately) NOT real hardware codes but
	 * simplify our backend as you need these combinations for some
	 * floatingpoint compares (the emitter will split them into multiple
	 * instructions) */
	x86_cc_float_parity_cases = 0x20,
	/* we need even more cases as inversing the cc is different for float
	 * comparisons (though for the following we need no special
	 * parity+x combinations) */
	x86_cc_additional_float_cases = 0x10,

	/* make sure that the lower 4 bit correspond to the real encoding
	 * (of the comparison not involving the parity special) */
	x86_cc_float_equal        = 0x34,                              /**< PF=0 and ZF=1 */
	x86_cc_float_below        = 0x32,                              /**< PF=0 and CF=1 */
	x86_cc_float_below_equal  = 0x36,                              /**< PF=0 and (ZF=1 or CF=1) */
	x86_cc_float_not_equal    = x86_cc_negated|x86_cc_float_equal, /**< PF=1 or ZF=0 */
	x86_cc_float_unordered_above_equal
		= x86_cc_negated|x86_cc_float_below,                       /**< PF=1 or CF=0 */
	x86_cc_float_unordered_above
		= x86_cc_negated|x86_cc_float_below_equal,                 /**< PF=1 or (ZF=0 and CF=0) */

	x86_cc_float_unordered_below_equal = 0x16,                     /**< ZF=1 or CF=1 */
	x86_cc_float_unordered_below       = 0x12,                     /**< CF=1 */
	x86_cc_float_above
		= x86_cc_negated|x86_cc_float_unordered_below_equal,       /**< ZF=0 and CF=0 */
	x86_cc_float_above_equal
		= x86_cc_negated|x86_cc_float_unordered_below,             /**< CF=0 */
} x86_condition_code_t;
ENUM_BITSET(x86_condition_code_t)

static inline x86_condition_code_t x86_negate_condition_code(
		x86_condition_code_t code)
{
	return code ^ x86_cc_negated;
}

static inline x86_condition_code_t x86_invert_condition_code(
		x86_condition_code_t code)
{
	/* doesn't appear to have any systematic, so use a table */
	switch (code) {
	case x86_cc_below:             return x86_cc_above;
	case x86_cc_below_equal:       return x86_cc_above_equal;
	case x86_cc_above:             return x86_cc_below;
	case x86_cc_above_equal:       return x86_cc_below_equal;
	case x86_cc_less:              return x86_cc_greater;
	case x86_cc_less_equal:        return x86_cc_greater_equal;
	case x86_cc_greater:           return x86_cc_less;
	case x86_cc_greater_equal:     return x86_cc_less_equal;
	case x86_cc_float_below:       return x86_cc_float_above;
	case x86_cc_float_below_equal: return x86_cc_float_above_equal;
	case x86_cc_float_above:       return x86_cc_float_below;
	case x86_cc_float_above_equal: return x86_cc_float_below_equal;
	case x86_cc_float_unordered_below:       return x86_cc_float_unordered_above;
	case x86_cc_float_unordered_below_equal: return x86_cc_float_unordered_above_equal;
	case x86_cc_float_unordered_above:       return x86_cc_float_unordered_below;
	case x86_cc_float_unordered_above_equal: return x86_cc_float_unordered_below_equal;
	default:                       return code;
	}
}

x86_condition_code_t ir_relation_to_x86_condition_code(ir_relation relation,
                                                       ir_mode *mode,
                                                       bool overflow_possible);

void x86_emit_condition_code(x86_condition_code_t cc);

#endif
