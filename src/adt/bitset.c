/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */
#include "bitset.h"

#include "irprintf.h"

void bitset_fprint(FILE *const file, bitset_t const *const bs)
{
	putc('{', file);
	char const *prefix = "";
	for (size_t i = bitset_next_set(bs, 0); i != (size_t)-1; i = bitset_next_set(bs, i + 1)) {
		ir_fprintf(file, "%s%zu", prefix, i);
		prefix = ",";
	}
	putc('}', file);
}
