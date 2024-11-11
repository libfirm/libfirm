/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "riscv_nodes_attr.h"

#include <panic.h>

char const *riscv_get_cond_name(riscv_cond_t const cond)
{
	switch (cond) {
	case riscv_cc_eq:  return "eq";
	case riscv_cc_ne:  return "ne";
	case riscv_cc_lt:  return "lt";
	case riscv_cc_ge:  return "ge";
	case riscv_cc_ltu: return "ltu";
	case riscv_cc_geu: return "geu";
	}
	panic("invalid cond");
}
