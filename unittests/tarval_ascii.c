#include "tv_t.h"
#include "irprintf.h"
#include "firm.h"
#include <assert.h>
#include <float.h>

static char buf[1024];

static void test_tv(ir_tarval *tv)
{
	const char *res = ir_tarval_to_ascii(buf, sizeof(buf), tv);
	ir_tarval *back = ir_tarval_from_ascii(res, get_tarval_mode(tv));
	if (tv != back) {
		ir_printf("%T [mode %s]\n", tv, get_mode_name(get_tarval_mode(tv)));
		printf("     encoded: %s\n", res);
		assert(tv == back);
	}
}

static void test_mode(ir_mode *mode)
{
	test_tv(get_mode_null(mode));
	test_tv(get_mode_one(mode));
	if (mode_is_signed(mode))
		test_tv(get_mode_minus_one(mode));
	test_tv(get_mode_min(mode));
	test_tv(get_mode_max(mode));
	if (mode_is_float(mode)) {
		test_tv(get_mode_infinite(mode));
		test_tv(tarval_neg(get_mode_infinite(mode)));
		test_tv(get_mode_NAN(mode));
		if (mode == mode_F) {
			test_tv(new_tarval_from_double(FLT_MIN, mode));
			test_tv(new_tarval_from_double(FLT_EPSILON, mode));
		}
		if (mode == mode_D) {
			test_tv(new_tarval_from_double(DBL_MIN, mode));
			test_tv(new_tarval_from_double(DBL_EPSILON, mode));
		}
		test_tv(new_tarval_from_str("-4.3", 4, mode));
		test_tv(new_tarval_from_str("84.1", 4, mode));
	} else {
		test_tv(get_mode_all_one(mode));
	}
}

int main(void)
{
	ir_init();

	test_mode(mode_Bs);
	test_mode(mode_Bu);
	test_mode(mode_Hs);
	test_mode(mode_Hu);
	test_mode(mode_Is);
	test_mode(mode_Iu);
	test_mode(mode_Ls);
	test_mode(mode_Lu);
	test_mode(mode_F);
	test_mode(mode_D);
	test_mode(mode_P);

	ir_mode *mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                                 ir_overflow_indefinite);
	test_mode(mode_E);

	test_tv(tarval_bad);
	test_tv(tarval_undefined);
	test_tv(tarval_reachable);
	test_tv(tarval_unreachable);
	return 0;
}
