/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Helper functions for calling conventions
 */
#ifndef FIRM_BE_BECCONV_H
#define FIRM_BE_BECCONV_H

#include "obstack.h"
#include "raw_bitset.h"

static inline unsigned *be_cconv_alloc_all_regs(struct obstack *const obst, size_t const n)
{
	unsigned *const res = rbitset_obstack_alloc(obst, n);
	rbitset_set_all(res, n);
	return res;
}

static inline void be_cconv_add_regs(unsigned *const dst, unsigned const *const regs, size_t const n)
{
	for (size_t i = 0; i != n; ++i) {
		rbitset_set(dst, regs[i]);
	}
}

static inline void be_cconv_rem_regs(unsigned *const dst, unsigned const *const regs, size_t const n)
{
	for (size_t i = 0; i != n; ++i) {
		rbitset_clear(dst, regs[i]);
	}
}

#endif
