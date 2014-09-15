/*
 * Copyright (C) 1995-2012 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 */
#include "amd64_common_transform.h"

#include "debug.h"
#include "panic.h"
#include "heights.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "tv_t.h"
#include "util.h"

#include "benode.h"
#include "bearch_amd64_t.h"
#include "beirg.h"

#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"

static const arch_register_req_t *mem_reqs[] = {
	&arch_no_requirement,
};

amd64_insn_mode_t get_insn_mode_from_mode(const ir_mode *mode)
{
	switch (get_mode_size_bits(mode)) {
	case  8: return INSN_MODE_8;
	case 16: return INSN_MODE_16;
	case 32: return INSN_MODE_32;
	case 64: return INSN_MODE_64;
	}
	panic("unexpected mode");
}

ir_entity *create_float_const_entity(ir_graph *const irg,
                                     ir_tarval *const tv)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);
	amd64_isa_t      *isa      = (amd64_isa_t*) arch_env;
	ir_entity        *entity   = pmap_get(ir_entity, isa->constants, tv);
	if (entity != NULL)
		return entity;

	ir_mode *mode = get_tarval_mode(tv);
	ir_type *type = get_type_for_mode(mode);
	ir_type *glob = get_glob_type();

	entity = new_entity(glob, id_unique("C%u"), type);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	ir_initializer_t *initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(isa->constants, tv, entity);
	return entity;
}

ir_node *create_float_const(dbg_info *dbgi, ir_node *block,
                                   ir_tarval *tv)
{
	ir_graph  *irg     = get_Block_irg(block);
	ir_mode   *tv_mode = get_tarval_mode(tv);
	ir_entity *entity  = create_float_const_entity(irg, tv);
	ir_node   *nomem   = get_irg_no_mem(irg);

	ir_node *in[] = { nomem };
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	addr.immediate.entity       = entity;
	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(tv_mode);

	addr.base_input  = NO_INPUT;
	addr.index_input = NO_INPUT;

	ir_node *load = new_bd_amd64_xMovs(dbgi, block, ARRAY_SIZE(in), in,
	                                   insn_mode, AMD64_OP_ADDR, addr);

	arch_set_irn_register_reqs_in(load, mem_reqs);
	set_irn_pinned(load, op_pin_state_floats);

	return new_r_Proj(load, tv_mode, pn_amd64_xMovs_res);
}

ir_tarval *create_sign_tv(ir_mode *mode) {
	assert(!mode_is_float(mode));
	const char *sign_str;
	if (get_mode_size_bits(mode) <= 32) {
		sign_str = "0x80000000";
	} else {
		sign_str = "0x8000000000000000";
	}

	ir_tarval *tv = new_tarval_from_str(sign_str, strlen(sign_str), mode);
	return tv;
}
