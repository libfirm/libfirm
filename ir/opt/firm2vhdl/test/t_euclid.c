#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t a, uint32_t b)
{
	if (a == 0) {
		return b;
	}

	while (b > 0) {
		if (a > b) {
			a = a - b;
		} else {
			b = b - a;
		}
	}

	return a;
}

pattern patterns[] = {
	{42, 1, 1},
	{42, 4, 5},
	{42, 16, 20},
	{42, 20, 16},
	{42, 5, 10},
	{42, 10, 5},
	{42, 11, 12},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
