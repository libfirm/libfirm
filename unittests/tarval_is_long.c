#include "firm.h"
#include "irmode.h"
#include "panic.h"
#include "tv.h"
#include <assert.h>
#include <limits.h>

int main(void)
{
	ir_init();

	ir_mode *big_s = new_int_mode("big signed", 66, 1, 0);
	ir_mode *big_u = new_int_mode("big signed", 66, 0, 0);

	ir_mode *long_u;
	ir_mode *long_s;
	if (sizeof(long) == 4) {
		long_u = mode_Iu;
		long_s = mode_Is;
	} else if (sizeof(long) == 8) {
		long_u = mode_Lu;
		long_s = mode_Ls;
	} else {
		panic("unexpected long size");
	}
	ir_tarval *ulongmax = get_mode_max(long_u);
	assert(tarval_is_long(ulongmax));
	ir_tarval *ulongmax_s = tarval_convert_to(ulongmax, long_s);
	assert(tarval_is_long(ulongmax_s));
	ir_tarval *ulongmax_se = tarval_convert_to(ulongmax_s, big_s);
	assert(tarval_is_long(ulongmax_se));
	ir_tarval *ulongmax_z = tarval_convert_to(ulongmax_s, big_u);
	assert(!tarval_is_long(ulongmax_z));

	ir_tarval *longmax = new_tarval_from_long(LONG_MAX, big_s);
	ir_tarval *longmin = new_tarval_from_long(LONG_MIN, big_s);
	assert(tarval_is_long(longmin));
	assert(tarval_is_long(longmax));

	ir_tarval *one      = new_tarval_from_long(1, big_s);
	ir_tarval *longmaxp = tarval_add(longmax, one);
	assert(!tarval_is_long(longmaxp));
	ir_tarval *longmax3 = tarval_sub(longmaxp, one);
	assert(tarval_is_long(longmax3));

	ir_tarval *longmax2 = sizeof(long) == 8 ? get_mode_max(mode_Ls)
	                    : sizeof(long) == 4 ? get_mode_max(mode_Is)
	                    : (panic("unexpected long size"), NULL);
	assert(tarval_is_long(longmax2));
	ir_tarval *longmin2 = sizeof(long) == 8 ? get_mode_min(mode_Ls)
	                    : sizeof(long) == 4 ? get_mode_min(mode_Is)
	                    : (panic("unexpected long size"), NULL);
	assert(tarval_is_long(longmin2));

	return 0;
}
