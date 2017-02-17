#include "raw_bitset.h"
#include <assert.h>
#include <stdio.h>

int main(void)
{
	unsigned *field0 = rbitset_malloc(66);
	unsigned *field1 = rbitset_alloca(66);
	unsigned field2[BITSET_SIZE_ELEMS(66)];
	memset(&field2, 0, sizeof(field2));

	assert(rbitset_is_empty(field0, 66));

	rbitset_set(field0, 14);
	assert(!rbitset_is_empty(field0, 66));
	rbitset_set_all(field1, 66);
	rbitset_clear(field1, 14);
	rbitset_flip_all(field1, 66);
	assert(rbitsets_equal(field0, field1, 66));
	assert(rbitset_is_set(field1, 14));
	assert(!rbitset_is_set(field1, 15));
	assert(!rbitset_is_set(field1, 44));

	rbitset_set_range(field2, 23, 55, true);
	rbitset_set_all(field0, 66);
	rbitset_set_range(field0, 0, 23, false);
	rbitset_set_range(field0, 54, 66, false);
	rbitset_flip(field0, 54);
	assert(rbitsets_equal(field2, field0, 66));
	rbitset_flip(field2, 13);
	rbitset_flip(field2, 64);
	assert(rbitset_popcount(field2, 66) == 34);

	rbitset_clear_all(field1, 66);
	assert(rbitset_is_empty(field1, 66));
	rbitset_set(field1, 3);
	rbitset_set(field1, 59);
	assert(rbitset_next(field1, 0, true) == 3);
	assert(rbitset_next(field1, 3, true) == 3);
	assert(rbitset_next(field1, 4, true) == 59);
	assert(rbitset_next(field1, 34, true) == 59);
	assert(rbitset_next(field1, 0, false) == 0);
	assert(rbitset_next(field1, 3, false) == 4);
	assert(rbitset_next_max(field1, 3, 66, false) == 4);
	assert(rbitset_next_max(field1, 60, 66, true) == (size_t)-1);
	assert(rbitset_next_max(field1, 3, 4, false) == (size_t)-1);
	assert(rbitset_prev(field1, 0, true) == (size_t)-1);
	assert(rbitset_prev(field1, 3, true) == (size_t)-1);
	assert(rbitset_prev(field1, 4, true) == 3);
	assert(rbitset_prev(field1, 59, true) == 3);
	assert(rbitset_prev(field1, 60, true) == 59);
	assert(rbitset_prev(field1, 34, true) == 3);
	assert(rbitset_prev(field1, 0, false) == (size_t)-1);
	assert(rbitset_prev(field1, 3, false) == 2);
	assert(rbitset_prev(field1, 1, false) == 0);

	unsigned *null = (unsigned*)0;
	rbitset_flip_all(null, 0);
	rbitset_set_all(null, 0);
	rbitset_flip_all(null, 0);
	rbitset_minus1(null, 0);
	rbitset_copy(null, NULL, 0);
	rbitset_xor(null, 0, 0);
	rbitset_and(null, 0, 0);
	rbitset_or(null, 0, 0);
	rbitset_andnot(null, 0, 0);
	assert(rbitsets_equal(null, NULL, 0));
	assert(rbitset_contains(null, NULL, 0));
	assert(!rbitsets_have_common(null, NULL, 0));
	assert(rbitset_next_max(null, 0, 0, true) == (size_t)-1);
	assert(rbitset_next_max(null, 0, 0, false) == (size_t)-1);
	assert(rbitset_popcount(null, 0) == 0);
	assert(rbitset_is_empty(null, 0));

	return 0;
}
