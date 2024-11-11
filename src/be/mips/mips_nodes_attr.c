/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_nodes_attr.h"

#include <panic.h>

char const *mips_get_cond_name(mips_cond_t const cond)
{
	switch (cond) {
	case mips_cc_eq:  return "eq";
	case mips_cc_ne:  return "ne";
	case mips_cc_ltz: return "ltz";
	case mips_cc_gez: return "gez";
	case mips_cc_lez: return "lez";
	case mips_cc_gtz: return "gtz";
	}
	panic("invalid cond");
}
