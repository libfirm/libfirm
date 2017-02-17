#include "firm.h"
#include "irmode.h"
#include "tv_t.h"
#include <assert.h>
#include <float.h>
#include <math.h>

static void test_float(float v, ir_tarval *known)
{
	ir_tarval *tv = new_tarval_from_double(v, mode_F);
	assert(tarval_is_double(tv));
	float back = get_tarval_double(tv);
	assert(v == back || (isnan(v) && isnan(back)));
	assert(known == NULL || tv == known);
}

static void test_double(double v, ir_tarval *known)
{
	ir_tarval *tv = new_tarval_from_double(v, mode_D);
	assert(tarval_is_double(tv));
	double back = get_tarval_double(tv);
	assert(v == back || (isnan(v) && isnan(back)));
	assert(known == NULL || tv == known);
}

static ir_mode *mode_E;

static void test_ldouble(long double v, ir_tarval *known)
{
	ir_tarval *tv = new_tarval_from_long_double(v, mode_E);
	long double back = get_tarval_long_double(tv);
	assert(v == back || (isnan(v) && isnan(back)));
	assert(known == NULL || tv == known);
}

int main(void)
{
	ir_init();

	test_float(0, get_mode_null(mode_F));
	test_float(1, get_mode_one(mode_F));
	test_float(-1, tarval_neg(get_mode_one(mode_F)));
	test_float(FLT_MAX, get_mode_max(mode_F));
	test_float(-FLT_MAX, get_mode_min(mode_F));
	test_float(FLT_EPSILON, NULL);
	test_float(FLT_MIN, NULL);
	test_float(INFINITY, get_mode_infinite(mode_F));
	test_float(HUGE_VALF, NULL);
	test_float(FLT_MIN, get_tarval_small(mode_F));
	test_float(FLT_EPSILON, get_tarval_epsilon(mode_F));
	test_float(FLT_MIN * FLT_EPSILON, NULL); // subnormal

	test_double(0, get_mode_null(mode_D));
	test_double(1, get_mode_one(mode_D));
	test_double(-1, tarval_neg(get_mode_one(mode_D)));
	test_double(DBL_MAX, get_mode_max(mode_D));
	test_double(-DBL_MAX, get_mode_min(mode_D));
	test_double(DBL_EPSILON, NULL);
	test_double(DBL_MIN, NULL);
	test_double(HUGE_VAL, NULL);
	test_double(INFINITY, get_mode_infinite(mode_D));
	test_double(DBL_MIN, get_tarval_small(mode_D));
	test_double(DBL_EPSILON, get_tarval_epsilon(mode_D));
	test_double(DBL_MIN * DBL_EPSILON, NULL); // subnormal

#if LDBL_MANT_DIG == 64
	mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                        ir_overflow_indefinite);
#elif LDBL_MANT_DIG == 53
	mode_E = mode_D;
#else
	printf("long double tests skipped\n");
	return 0;
#endif

	test_ldouble(0, get_mode_null(mode_E));
	test_ldouble(1, get_mode_one(mode_E));
	test_ldouble(-1, tarval_neg(get_mode_one(mode_E)));
	test_ldouble(LDBL_MAX, get_mode_max(mode_E));
	test_ldouble(-LDBL_MAX, get_mode_min(mode_E));
	test_ldouble(LDBL_EPSILON, NULL);
	test_ldouble(LDBL_MIN, NULL);
	test_ldouble(HUGE_VAL, NULL);
	test_ldouble(INFINITY, get_mode_infinite(mode_E));
	test_ldouble(LDBL_MIN, get_tarval_small(mode_E));
	test_ldouble(LDBL_EPSILON, get_tarval_epsilon(mode_E));
	test_ldouble(LDBL_MIN * LDBL_EPSILON, NULL); // subnormal

	return 0;
}
