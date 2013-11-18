#include <limits.h>
#include <assert.h>

#include "firm.h"
#include "error.h"
#include "irmode.h"
#include "tv.h"

int main(void)
{
	ir_init();

	ir_tarval *ulongmax
		= sizeof(long) == 8 ? get_mode_max(mode_Lu)
		: sizeof(long) == 4 ? get_mode_max(mode_Iu)
		: (panic("unexpected long size"), NULL);
	assert(tarval_is_long(ulongmax));
	ir_tarval *ulongmax_s = tarval_convert_to(ulongmax, mode_Ls);
	assert(tarval_is_long(ulongmax_s));
	ir_tarval *ulongmax_se = tarval_convert_to(ulongmax_s, mode_LLs);
	assert(tarval_is_long(ulongmax_se));
	ir_tarval *ulongmax_z = tarval_convert_to(ulongmax_s, mode_LLu);
	assert(!tarval_is_long(ulongmax_z));

	ir_tarval *longmax = new_tarval_from_long(LONG_MAX, mode_LLs);
	ir_tarval *longmin = new_tarval_from_long(LONG_MIN, mode_LLs);
	assert(tarval_is_long(longmin));
	assert(tarval_is_long(longmax));

	ir_tarval *one      = new_tarval_from_long(1, mode_LLs);
	ir_tarval *longmaxp = tarval_add(longmax, one);
	assert(!tarval_is_long(longmaxp));
	ir_tarval *longmax3 = tarval_sub(longmaxp, one, mode_LLs);
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
