/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_new_nodes_t.h"

#include "bearch.h"
#include "gen_vhdl_new_nodes.h"
#include "ircons_t.h"
#include <inttypes.h>

static int vhdl_attrs_equal_(vhdl_attr_t const *const a_attr, vhdl_attr_t const *const b_attr)
{
#if 0
	return except_attrs_equal(&a_attr->exc, &b_attr->exc); // TODO all other backends do not check this either
#else
	(void)a_attr, (void)b_attr;
	return 1;
#endif
}

int vhdl_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	vhdl_attr_t const *const a_attr = get_vhdl_attr_const(a);
	vhdl_attr_t const *const b_attr = get_vhdl_attr_const(b);
	return vhdl_attrs_equal_(a_attr, b_attr);
}

void vhdl_dump_node(FILE *const F, ir_node const *const n, dump_reason_t const reason)
{
}
