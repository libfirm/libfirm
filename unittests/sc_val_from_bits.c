#include "strcalc.h"
#include "util.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool fine = true;

static void test(const unsigned char *bytes, unsigned from, unsigned to,
                 const char *expected)
{
	size_t val_len = sc_get_value_length();
	sc_word val0[val_len];
	/* write some vals in the buffer */
	for (sc_word *c = val0; c < val0+val_len; ++c)
		*c = 0xa5 ^ (c-val0);
	sc_val_from_bits(bytes, from, to, val0);

	char print_buf[32];
	char *res = sc_print_buf(print_buf, sizeof(print_buf), val0, 32, SC_HEX, false);
	if (!streq(res, expected)) {
		printf("Failed: 0x%02X%02X%02X%02X [%u:%u) => %s (expected %s)\n",
			   bytes[3], bytes[2], bytes[1], bytes[0], from, to, res, expected);
		fine = false;
	}
}

int main(void)
{
	init_strcalc(68);

	unsigned char bytes[4] = { 0xBE, 0xBA, 0xFE, 0xCA };
	test(bytes, 24, 32, "CA");
	test(bytes, 16, 24, "FE");
	test(bytes,  8, 16, "BA");
	test(bytes,  0,  8, "BE");
	test(bytes,  0, 32, "CAFEBABE");
	const char *bits = "11001010111111101011101010111110";
	for (unsigned i = 0; i < 32; ++i) {
		const char ref[2] = { bits[31-i], '\0' };
		test(bytes, i, i+1, ref);
	}
	test(bytes,  7, 23, "FD75");

	unsigned char bytes2[4] = { 0x00, 0x00, 0x80, 0x3F };
	test(bytes2, 0, 23, "0");
	test(bytes2, 23, 31, "7F");
	test(bytes2, 31, 32, "0");

	uint32_t uval  = 0xCAFEBABE;
	uint32_t uval2 = 0x3F800000;
	for (unsigned from = 0; from < 32; ++from) {
		for (unsigned to = from+1; to < 32; ++to) {
			unsigned dval = (uval << (32-to)) >> (32-to+from);
			char buf[9];
			snprintf(buf, sizeof(buf), "%"PRIX32, dval);
			test(bytes, from, to, buf);

			unsigned dval2 = (uval2 << (32-to)) >> (32-to+from);
			snprintf(buf, sizeof(buf), "%"PRIX32, dval2);
			test(bytes2, from, to, buf);
		}
	}

	if (!fine) {
		printf("*** Some tests failed\n");
		abort();
	}
	return 0;
}
