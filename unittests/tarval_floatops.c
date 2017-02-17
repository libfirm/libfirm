#include "firm.h"
#include "tv_t.h"
#include "util.h"
#include <assert.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>

static void check_mode(ir_mode *mode)
{
	ir_tarval *zero       = get_mode_null(mode);
	ir_tarval *minus_zero = tarval_neg(zero);
	ir_tarval *min        = get_mode_min(mode);
	ir_tarval *max        = get_mode_max(mode);
	ir_tarval *inf        = get_mode_infinite(mode);
	ir_tarval *minus_inf  = tarval_neg(inf);
	ir_tarval *one        = get_mode_one(mode);
	ir_tarval *minus_one  = tarval_neg(one);

	/* some random arithmetics */
	ir_tarval *int_zero      = get_mode_null(mode_Is);
	ir_tarval *int_one       = get_mode_one(mode_Is);
	ir_tarval *int_minus_one = get_mode_all_one(mode_Is);
	ir_tarval *int_min       = get_mode_min(mode_Is);
	ir_tarval *int_max       = get_mode_max(mode_Is);
	assert(tarval_convert_to(zero, mode_Is) == int_zero);
	assert(tarval_convert_to(minus_zero, mode_Is) == int_zero);
	assert(tarval_convert_to(one, mode_Is) == int_one);
	assert(tarval_convert_to(minus_one, mode_Is) == int_minus_one);
	assert(tarval_convert_to(min, mode_Is) == int_min);
	assert(tarval_convert_to(max, mode_Is) == int_max);
	assert(tarval_convert_to(inf, mode_Is) == int_max);
	assert(tarval_convert_to(minus_inf, mode_Is) == int_min);

	static const char *const ints[] = {
		"0", "1", "-1", "12345", "2", "4", "8", "16", "32", "64", "128", "256",
		"512", "1024", "2048", "127", "2047"
	};
	for (unsigned i = 0; i < ARRAY_SIZE(ints); ++i) {
		const char *str = ints[i];
		ir_tarval *flt = new_tarval_from_str(str, strlen(str), mode);
		ir_tarval *intt = new_tarval_from_str(str, strlen(str), mode_Is);
		assert(tarval_convert_to(flt, mode_Is) == intt);
		assert(tarval_convert_to(intt, mode) == flt);
	}
}

int main(void)
{
	ir_init();

	check_mode(mode_F);
	check_mode(mode_D);
#if LDBL_MANT_DIG == 64
	ir_mode *mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                                 ir_overflow_min_max);
	check_mode(mode_E);
#endif
}
