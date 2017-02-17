#include "ident_t.h"
#include "irmode_t.h"
#include "irprog_t.h"
#include "tv_t.h"
#include "type_t.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>

static int64_t nan_payload(double d) {
	int64_t result;
	memcpy(&result, &d, 8);
	assert(result & 0x0008000000000000); // quiet NaN
	return result & 0x0007ffffffffffff;
}

static double new_nan(int64_t payload) {
	double result;
	payload |= 0xfff8000000000000;
	memcpy(&result, &payload, 8);
	return result;
}

int main(void)
{
	init_ident();
	init_tarval_1();
	init_irprog_1();
	init_mode();
	init_tarval_2();

	double d_nan = 0.0 / 0.0;
	assert(d_nan != d_nan);

	d_nan = new_nan(1234);
	assert(d_nan != d_nan);
	assert(nan_payload(d_nan) == 1234);

	d_nan *= 2;
	assert(d_nan != d_nan);

	ir_tarval* tv_nan = new_tarval_from_double(d_nan, mode_D);
	assert(tarval_is_nan(tv_nan));
	d_nan = get_tarval_double(tv_nan);
	assert(d_nan != d_nan);
	assert(nan_payload(d_nan) == 1234);

	tv_nan = new_tarval_nan(mode_D, false, NULL);
	assert(tarval_is_nan(tv_nan));
	d_nan = get_tarval_double(tv_nan);
	assert(d_nan != d_nan);

	d_nan = new_nan(0);
	assert(nan_payload(d_nan) == 0);
	assert(d_nan != d_nan);
	tv_nan = new_tarval_from_double(d_nan, mode_D);
	assert(tarval_is_nan(tv_nan));
	d_nan = get_tarval_double(tv_nan);
	assert(d_nan != d_nan);
	assert(nan_payload(d_nan) == 0);

	finish_tarval();
	finish_mode();
	finish_ident();
	return 0;
}
