#include "firm.h"
#include "irprintf.h"
#include "util.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

int main(void)
{
	char buf[16];
	int  r;

	ir_init();

	r = ir_snprintf(buf, sizeof(buf), "hello");
	assert(streq(buf, "hello"));
	assert(r == 5);

	r = ir_snprintf(buf, sizeof(buf), "nums: %d %u!", 32, 46);
	assert(streq(buf, "nums: 32 46!"));
	assert(r == 12);

	ir_tarval *tv = new_tarval_from_long(123, mode_Iu);
	r = ir_snprintf(buf, sizeof(buf), "tv: %+F\n", tv);
	assert(streq(buf, "tv: 0x7B\n"));
	assert(r == 9);

	r = ir_snprintf(buf, sizeof(buf), "%d %d %d %d %d %d %d", 1234, 1234, 1234, 1234, 1234, 1234, 1234);
	assert(streq(buf, "1234 1234 1234 "));
	assert(r == 34);

	r = ir_snprintf(buf, 4, "%+F\n", tv);
	assert(streq(buf, "0x7"));
	assert(r == 5);

	r = ir_snprintf(buf, 8, "%I", new_id_from_str("Hello World"));
	assert(streq(buf, "Hello W"));
	assert(r == 11);

	return 0;
}
