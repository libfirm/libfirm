#include "firm.h"
#include "fltcalc.h"
#include "irprintf.h"
#include "panic.h"
#include "strcalc.h"
#include "tv_t.h"
#include "util.h"
#include <assert.h>
#include <float.h>
#include <limits.h>

static int res = 0;

static void test_from_to_bytes(ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	unsigned bits = get_mode_size_bits(mode);
	unsigned buffer_size = bits/CHAR_BIT + (bits % CHAR_BIT != 0);
	unsigned char buffer[buffer_size+4];
	for (unsigned i = 0; i < buffer_size; ++i) {
		buffer[i] = 0xaa ^ i*7;
	}
	buffer[buffer_size+0] = 0xDE;
	buffer[buffer_size+1] = 0xAD;
	buffer[buffer_size+2] = 0xBE;
	buffer[buffer_size+3] = 0xEF;

	tarval_to_bytes(buffer, tv);
	assert(buffer[buffer_size+0] == 0xDE);
	assert(buffer[buffer_size+1] == 0xAD);
	assert(buffer[buffer_size+2] == 0xBE);
	assert(buffer[buffer_size+3] == 0xEF);
	ir_tarval *back = new_tarval_from_bytes(buffer, mode);

	if (tv != back) {
		ir_printf("from_bytes(to_bytes(x)) failed: %T, expected %T [mode %s]\n", back, tv, get_mode_name(get_tarval_mode(tv)));
		res = 1;
	}
}

static void test_from_to_ascii(ir_tarval *tv)
{
	static char buf[128];
	const char *ascii = ir_tarval_to_ascii(buf, sizeof(buf), tv);
	ir_tarval *back = ir_tarval_from_ascii(ascii, get_tarval_mode(tv));
	if (tv != back) {
		ir_printf("from_ascii(to_ascii(x)) failed: %T, expected %T [mode %s]\n", tv, back, get_mode_name(get_tarval_mode(tv)));
		printf("     encoded: %s\n", ascii);
		res = 1;
	}
}

static void test_tv(ir_tarval *tv)
{
	test_from_to_bytes(tv);
	test_from_to_ascii(tv);
}

static ir_tarval *get_mode_minus_one(ir_mode *mode)
{
	return tarval_neg(get_mode_one(mode));
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
		test_tv(new_tarval_nan(mode, true, NULL));
		test_tv(new_tarval_nan(mode, false, NULL));
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

	ir_mode *modes[] = {
		mode_Bs, mode_Bu, mode_Hs, mode_Hu, mode_Is, mode_Iu,
		mode_Ls, mode_Lu, mode_F, mode_D,
		new_reference_mode("p32", 32, 32),
		new_float_mode("E", irma_x86_extended_float, 15, 64, ir_overflow_indefinite),
		new_int_mode("uint6", 6,  false, 0),
		new_int_mode("uint13", 13, false, 0),
		new_int_mode("int6", 6,  true, 0),
		new_int_mode("int13", 13, true, 0),
	};

	for (unsigned i = 0, n = ARRAY_SIZE(modes); i < n; ++i) {
		ir_mode *mode = modes[i];
		test_mode(mode);
	}

	test_from_to_ascii(tarval_bad);
	test_from_to_ascii(tarval_unknown);
	return res;
}
