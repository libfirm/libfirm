/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Implements several optimizations for ARM.
 * @author      Michael Beck
 * @version     $Id: $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irgmod.h"
#include "ircons.h"
#include "error.h"

#include "benode_t.h"
#include "bepeephole.h"
#include "besched.h"

#include "arm_optimize.h"
#include "gen_arm_regalloc_if.h"
#include "gen_arm_new_nodes.h"

static const arch_env_t *arch_env;
static arm_code_gen_t  *cg;

/** Execute ARM ROL. */
static unsigned arm_rol(unsigned v, unsigned rol) {
	return (v << rol) | (v >> (32 - rol));
}

/*
 * construct 8bit values and rot amounts for a value.
 */
void arm_gen_vals_from_word(unsigned int value, arm_vals *result)
{
	int initial = 0;

	memset(result, 0, sizeof(*result));

	/* special case: we prefer shift amount 0 */
	if (value < 0x100) {
		result->values[0] = value;
		result->ops       = 1;
		return;
	}

	while (value != 0) {
		if (value & 0xFF) {
			unsigned v = arm_rol(value, 8) & 0xFFFFFF;
			int shf = 0;
			for (;;) {
				if ((v & 3) != 0)
					break;
				shf += 2;
				v >>= 2;
			}
			v  &= 0xFF;
			shf = (initial + shf - 8) & 0x1F;
			result->values[result->ops] = v;
			result->shifts[result->ops] = shf;
			++result->ops;

			value ^= arm_rol(v, shf) >> initial;
		}
		else {
			value >>= 8;
			initial += 8;
		}
	}
}

/**
 * Encodes an immediate with shifter operand
 */
unsigned int arm_encode_imm_w_shift(unsigned int shift, unsigned int immediate) {
	return immediate | ((shift>>1)<<8);
}

/**
 * Decode an immediate with shifter operand
 */
unsigned int arm_decode_imm_w_shift(long imm_value) {
	unsigned l = (unsigned)imm_value;
	unsigned rol = (l & ~0xFF) >> 7;

	return arm_rol(l & 0xFF, rol);
}

/**
 * Returns non.zero if the given offset can be directly encoded into an ARM instruction.
 */
static int allowed_arm_immediate(int offset, arm_vals *result) {
	arm_gen_vals_from_word(offset, result);
	return result->ops <= 1;
}

/**
 * Fix an IncSP node if the offset gets too big
 */
static void peephole_be_IncSP(ir_node *node) {
	ir_graph *irg;
	ir_node  *block;
	int      offset, cnt, align, sign = 1;
	arm_vals v;

	/* first optimize incsp->incsp combinations */
	be_peephole_IncSP_IncSP(node);

	offset = be_get_IncSP_offset(node);
	/* can be transformed into Add OR Sub */
	if (offset < 0) {
		sign = -1;
		offset = -offset;
	}
	if (allowed_arm_immediate(offset, &v))
		return;

	be_set_IncSP_offset(node, (int)arm_rol(v.values[0], v.shifts[0]) * sign);

	irg   = current_ir_graph;
	block = get_nodes_block(node);
	align = be_get_IncSP_align(node);
	for (cnt = 1; cnt < v.ops; ++cnt) {
		int value = (int)arm_rol(v.values[cnt], v.shifts[cnt]);
		ir_node *next = be_new_IncSP(&arm_gp_regs[REG_SP], irg, block, node, value * sign, align);
		sched_add_after(node, next);
		node = next;
	}
}

/**
 * creates the address by Adds
 */
static ir_node *gen_ptr_add(ir_node *node, ir_node *frame, arm_vals *v)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbg   = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	int     cnt;
	ir_node *ptr;

	ptr = new_rd_arm_Add_i(dbg, irg, block, frame, mode_Iu, arm_encode_imm_w_shift(v->shifts[0], v->values[0]));
	arch_set_irn_register(arch_env, ptr, &arm_gp_regs[REG_R12]);
	sched_add_before(node, ptr);

	for (cnt = 1; cnt < v->ops; ++cnt) {
		long value = arm_encode_imm_w_shift(v->shifts[cnt], v->values[cnt]);
		ir_node *next = new_rd_arm_Add_i(dbg, irg, block, ptr, mode_Iu, value);
		arch_set_irn_register(arch_env, next, &arm_gp_regs[REG_R12]);
		sched_add_before(node, next);
		ptr = next;
	}
	return ptr;
}

/**
* creates the address by Subs
*/
static ir_node *gen_ptr_sub(ir_node *node, ir_node *frame, arm_vals *v)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbg   = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	int     cnt;
	ir_node *ptr;

	ptr = new_rd_arm_Sub_i(dbg, irg, block, frame, mode_Iu, arm_encode_imm_w_shift(v->shifts[0], v->values[0]));
	arch_set_irn_register(arch_env, ptr, &arm_gp_regs[REG_R12]);
	sched_add_before(node, ptr);

	for (cnt = 1; cnt < v->ops; ++cnt) {
		long value = arm_encode_imm_w_shift(v->shifts[cnt], v->values[cnt]);
		ir_node *next = new_rd_arm_Sub_i(dbg, irg, block, ptr, mode_Iu, value);
		arch_set_irn_register(arch_env, next, &arm_gp_regs[REG_R12]);
		sched_add_before(node, next);
		ptr = next;
	}
	return ptr;
}

/**
 * Fix an be_Spill node if the offset gets too big
 */
static void peephole_be_Spill(ir_node *node) {
	ir_entity *ent   = be_get_frame_entity(node);
	int       use_add = 1, offset = get_entity_offset(ent);
	ir_node   *block, *ptr, *frame, *value, *store;
	ir_mode   *mode;
	dbg_info  *dbg;
	ir_graph  *irg;
	arm_vals  v;

	if (allowed_arm_immediate(offset, &v))
		return;
	if (offset < 0) {
		use_add = 0;
		offset = -offset;
	}

	frame = be_get_Spill_frame(node);
	if (use_add) {
		ptr = gen_ptr_add(node, frame, &v);
	} else {
		ptr = gen_ptr_sub(node, frame, &v);
	}

	value = be_get_Spill_val(node);
	mode  = get_irn_mode(value);
	irg   = current_ir_graph;
	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(cg->isa)) {
			/* transform into fpaStf */
			store = new_rd_arm_fpaStf(dbg, irg, block, ptr, value, get_irg_no_mem(irg), mode);
			sched_add_before(node, store);
		} else {
			panic("peephole_be_Spill: spill not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		 /* transform into Store */;
		 store = new_rd_arm_Store(dbg, irg, block, ptr, value, get_irg_no_mem(irg));
		 sched_add_before(node, store);
	} else {
		panic("peephole_be_Spill: spill not supported for this mode");
	}

	be_peephole_before_exchange(node, store);
	sched_remove(node);
	exchange(node, store);
	be_peephole_after_exchange(store);
}

/**
 * Fix an be_Reload node if the offset gets too big
 */
static void peephole_be_Reload(ir_node *node) {
	ir_entity *ent   = be_get_frame_entity(node);
	int       use_add = 1, offset = get_entity_offset(ent);
	ir_node   *block, *ptr, *frame, *load, *mem, *proj;
	ir_mode   *mode;
	dbg_info  *dbg;
	ir_graph  *irg;
	arm_vals  v;
	const arch_register_t *reg;

	if (allowed_arm_immediate(offset, &v))
		return;
	if (offset < 0) {
		use_add = 0;
		offset = -offset;
	}

	frame = be_get_Reload_frame(node);
	if (use_add) {
		ptr = gen_ptr_add(node, frame, &v);
	} else {
		ptr = gen_ptr_sub(node, frame, &v);
	}

	reg   = arch_get_irn_register(arch_env, node);
	mem   = be_get_Reload_mem(node);
	mode  = get_irn_mode(node);
	irg   = current_ir_graph;
	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	if (mode_is_float(mode)) {
		if (USE_FPA(cg->isa)) {
			/* transform into fpaLdf */
			load = new_rd_arm_fpaLdf(dbg, irg, block, ptr, mem, mode);
			sched_add_before(node, load);
			proj = new_rd_Proj(dbg, irg, block, load, mode, pn_arm_fpaLdf_res);
			arch_set_irn_register(arch_env, proj, reg);
		} else {
			panic("peephole_be_Spill: spill not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		/* transform into Store */;
		load = new_rd_arm_Load(dbg, irg, block, ptr, mem);
		sched_add_before(node, load);
		proj = new_rd_Proj(dbg, irg, block, load, mode_Iu, pn_arm_Load_res);
		arch_set_irn_register(arch_env, proj, reg);
	} else {
		panic("peephole_be_Spill: spill not supported for this mode");
	}

	be_peephole_before_exchange(node, proj);
	sched_remove(node);
	exchange(node, proj);
	be_peephole_after_exchange(proj);
}

/**
 * Register a peephole optimization function.
 */
static void register_peephole_optimisation(ir_op *op, peephole_opt_func func) {
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func)func;
}

/* Perform peephole-optimizations. */
void arm_peephole_optimization(arm_code_gen_t *new_cg)
{
	cg       = new_cg;
	arch_env = cg->arch_env;

	/* register peephole optimizations */
	clear_irp_opcodes_generic_func();
	register_peephole_optimisation(op_be_IncSP, peephole_be_IncSP);
	register_peephole_optimisation(op_be_Spill, peephole_be_Spill);
	register_peephole_optimisation(op_be_Reload, peephole_be_Reload);

	be_peephole_opt(cg->birg);
}
