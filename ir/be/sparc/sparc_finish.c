/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief    Peephole optimization and legalization of a sparc function
 * @author   Matthias Braun
 * @version  $Id$
 *
 * A note on sparc stackpointer (sp) behaviour:
 * The ABI expects SPARC_MIN_STACKSIZE bytes to be available at the
 * stackpointer. This space will be used to spill register windows,
 * and for spilling va_arg arguments (maybe we can optimize this away for
 * statically known not-va-arg-functions...)
 * This in effect means that we allocate that extra space at the function begin
 * which is easy. But this space isn't really fixed at the beginning of the
 * stackframe. Instead you should rather imagine the space as always being the
 * last-thing on the stack.
 * So when addressing anything stack-specific we have to account for this
 * area, while our compiler thinks the space is occupied at the beginning
 * of the stack frame. The code here among other things adjusts these offsets
 * accordingly.
 */
#include "config.h"

#include "bearch_sparc_t.h"
#include "gen_sparc_regalloc_if.h"
#include "sparc_new_nodes.h"
#include "sparc_transform.h"
#include "sparc_architecture.h"
#include "icore_lowerperm.h"
#include "irprog.h"
#include "irgmod.h"
#include "ircons.h"
#include "irgwalk.h"

#include "../bepeephole.h"
#include "../benode.h"
#include "../besched.h"
#include "../bespillslots.h"
#include "../bestack.h"
#include "../beirgmod.h"
#include "../belower.h"

static void kill_unused_stacknodes(ir_node *node)
{
	if (get_irn_n_edges(node) > 0)
		return;

	if (be_is_IncSP(node)) {
		sched_remove(node);
		kill_node(node);
	} else if (is_Phi(node)) {
		int       arity = get_irn_arity(node);
		ir_node **ins   = ALLOCAN(ir_node*, arity);
		int       i;
		sched_remove(node);
		memcpy(ins, get_irn_in(node), arity*sizeof(ins[0]));
		kill_node(node);

		for (i = 0; i < arity; ++i)
			kill_unused_stacknodes(ins[i]);
	}
}

static void introduce_epilog(ir_node *ret)
{
	const arch_register_t *sp_reg     = &sparc_registers[REG_SP];
	ir_graph              *irg        = get_irn_irg(ret);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *block      = get_nodes_block(ret);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	int                    sp_idx     = be_find_return_reg_input(ret, sp_reg);
	ir_node               *sp         = get_irn_n(ret, sp_idx);

	if (!layout->sp_relative) {
		const arch_register_t *fp_reg = &sparc_registers[REG_FRAME_POINTER];
		ir_node *fp      = be_get_initial_reg_value(irg, fp_reg);
		ir_node *restore = new_bd_sparc_RestoreZero(NULL, block, fp);
		sched_add_before(ret, restore);
		arch_set_irn_register(restore, sp_reg);
		set_irn_n(ret, sp_idx, restore);

		kill_unused_stacknodes(sp);
	} else {
		ir_node *incsp  = be_new_IncSP(sp_reg, block, sp, -frame_size, 0);
		set_irn_n(ret, sp_idx, incsp);
		sched_add_before(ret, incsp);
	}
}

void sparc_introduce_prolog_epilog(ir_graph *irg)
{
	const arch_register_t *sp_reg     = &sparc_registers[REG_SP];
	ir_node               *start      = get_irg_start(irg);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_node               *initial_sp = be_get_initial_reg_value(irg, sp_reg);
	ir_node               *sp         = initial_sp;
	ir_node               *schedpoint = start;
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);

	/* introduce epilog for every return node */
	{
		ir_node *end_block = get_irg_end_block(irg);
		int      arity     = get_irn_arity(end_block);
		int      i;

		for (i = 0; i < arity; ++i) {
			ir_node *ret = get_irn_n(end_block, i);
			assert(is_sparc_Return(ret));
			introduce_epilog(ret);
		}
	}

	while (be_is_Keep(sched_next(schedpoint)))
		schedpoint = sched_next(schedpoint);

	if (!layout->sp_relative) {
		ir_node *save = new_bd_sparc_Save_imm(NULL, block, sp, NULL,
		                                      -SPARC_MIN_STACKSIZE-frame_size);
		arch_set_irn_register(save, sp_reg);
		sched_add_after(schedpoint, save);
		schedpoint = save;

		edges_reroute(initial_sp, save);
		set_irn_n(save, n_sparc_Save_stack, initial_sp);

		/* we still need the Save even if noone is explicitely using the
		 * value. (TODO: this isn't 100% correct yet, something at the end of
		 * the function should hold the Save, even if we use a restore
		 * which just overrides it instead of using the value)
		 */
		if (get_irn_n_edges(save) == 0) {
			ir_node *in[] = { save };
			ir_node *keep = be_new_Keep(block, 1, in);
			sched_add_after(schedpoint, keep);
		}
	} else {
		ir_node *incsp = be_new_IncSP(sp_reg, block, sp, frame_size, 0);
		edges_reroute(initial_sp, incsp);
		be_set_IncSP_pred(incsp, sp);
		sched_add_after(schedpoint, incsp);
	}
}

static void finish_sparc_Save(ir_node *node)
{
	sparc_attr_t *attr = get_sparc_attr(node);
	int offset = attr->immediate_value;
	ir_node  *schedpoint = node;
	dbg_info *dbgi;
	ir_node  *block;
	ir_node  *new_save;
	ir_node  *stack;
	ir_entity *entity;

	if (sparc_is_value_imm_encodeable(offset))
		return;

	/* uhh only works for the imm variant yet */
	assert(get_irn_arity(node) == 1);

	block = get_nodes_block(node);
	dbgi = get_irn_dbg_info(node);
	stack = get_irn_n(node, n_sparc_Save_stack);
	entity = attr->immediate_value_entity;
	new_save = new_bd_sparc_Save_imm(dbgi, block, stack, entity, 0);
	arch_set_irn_register(new_save, &sparc_registers[REG_SP]);
	stack = new_save;

	sched_add_after(node, new_save);
	schedpoint = new_save;
	while (offset > SPARC_IMMEDIATE_MAX || offset < SPARC_IMMEDIATE_MIN) {
		if (offset > 0) {
			stack = be_new_IncSP(&sparc_registers[REG_SP], block, stack,
			                     SPARC_IMMEDIATE_MIN, 0);
			offset -= -SPARC_IMMEDIATE_MIN;
		} else {
			stack = be_new_IncSP(&sparc_registers[REG_SP], block, stack,
			                     -SPARC_IMMEDIATE_MIN, 0);
			offset -= SPARC_IMMEDIATE_MIN;
		}
		sched_add_after(schedpoint, stack);
		schedpoint = stack;
	}
	attr = get_sparc_attr(new_save);
	attr->immediate_value = offset;
	be_peephole_exchange(node, stack);
}

/**
 * sparc immediates are limited. Split IncSP with bigger immediates if
 * necessary.
 */
static void finish_be_IncSP(ir_node *node)
{
	int      sign   = 1;
	int      offset = be_get_IncSP_offset(node);
	ir_node *sp     = be_get_IncSP_pred(node);
	ir_node *block;

	/* we might have to break the IncSP apart if the constant has become too
	 * big */
	if (offset < 0) {
		offset = -offset;
		sign   = -1;
	}

	if (sparc_is_value_imm_encodeable(-offset))
		return;

	/* split incsp into multiple instructions */
	block = get_nodes_block(node);
	while (offset > -SPARC_IMMEDIATE_MIN) {
		sp = be_new_IncSP(&sparc_registers[REG_SP], block, sp,
		                  sign * -SPARC_IMMEDIATE_MIN, 0);
		sched_add_before(node, sp);
		offset -= -SPARC_IMMEDIATE_MIN;
	}

	be_set_IncSP_pred(node, sp);
	be_set_IncSP_offset(node, sign*offset);
}

/**
 * adjust sp-relative offsets. Split into multiple instructions if offset
 * exceeds sparc immediate range.
 */
static void finish_sparc_FrameAddr(ir_node *node)
{
	/* adapt to sparc stack magic */
	sparc_attr_t *attr   = get_sparc_attr(node);
	int           offset = attr->immediate_value;
	ir_node      *base   = get_irn_n(node, n_sparc_FrameAddr_base);
	dbg_info     *dbgi   = get_irn_dbg_info(node);
	ir_node      *block  = get_nodes_block(node);
	int           sign   = 1;

	if (offset < 0) {
		sign   = -1;
		offset = -offset;
	}

	if (offset > -SPARC_IMMEDIATE_MIN) {
		ir_entity *entity = attr->immediate_value_entity;
		ir_node   *new_frameaddr
			= new_bd_sparc_FrameAddr(dbgi, block, base, entity, 0);
		ir_node   *schedpoint = node;
		const arch_register_t *reg = arch_get_irn_register(node);

		sched_add_after(schedpoint, new_frameaddr);
		schedpoint = new_frameaddr;
		arch_set_irn_register(new_frameaddr, reg);
		base = new_frameaddr;

		while (offset > -SPARC_IMMEDIATE_MIN) {
			if (sign > 0) {
				base = new_bd_sparc_Sub_imm(dbgi, block, base, NULL,
											SPARC_IMMEDIATE_MIN);
			} else {
				base = new_bd_sparc_Add_imm(dbgi, block, base, NULL,
											SPARC_IMMEDIATE_MIN);
			}
			arch_set_irn_register(base, reg);
			sched_add_after(schedpoint, base);
			schedpoint = base;

			offset -= -SPARC_IMMEDIATE_MIN;
		}

		be_peephole_exchange(node, base);
		attr = get_sparc_attr(new_frameaddr);
	}
	attr->immediate_value = sign*offset;
}

static void peephole_be_IncSP(ir_node *node)
{
	ir_node *pred;
	node = be_peephole_IncSP_IncSP(node);
	if (!be_is_IncSP(node))
		return;

	pred = be_get_IncSP_pred(node);
	if (is_sparc_Save(pred) && be_has_only_one_user(pred)) {
		int offset = -be_get_IncSP_offset(node);
		sparc_attr_t *attr = get_sparc_attr(pred);
		attr->immediate_value += offset;
		be_peephole_exchange(node, pred);
	}
}

static void peephole_sparc_FrameAddr(ir_node *node)
{
	/* the peephole code currently doesn't allow this since it changes
	 * the register. Find out why and how to workaround this... */
#if 0
	const sparc_attr_t *attr = get_sparc_attr_const(node);
	if (attr->immediate_value == 0) {
		ir_node *base = get_irn_n(node, n_sparc_FrameAddr_base);
		be_peephole_exchange(node, base);
	}
#endif
	(void) node;
}

static void finish_sparc_Return(ir_node *node)
{
	ir_node *schedpoint = node;
	ir_node *restore;
	/* see that there is no code between Return and restore, if there is move
	 * it in front of the restore */
	while (true) {
		if (!sched_has_prev(schedpoint))
			return;
		schedpoint = sched_prev(schedpoint);
		if (is_sparc_Restore(schedpoint) || is_sparc_RestoreZero(schedpoint))
			break;
	}
	restore = schedpoint;
	schedpoint = sched_prev(node);
	/* move all code between return and restore up */
	while (schedpoint != restore) {
		ir_node *next_schedpoint = sched_prev(schedpoint);
		sched_remove(schedpoint);
		sched_add_before(restore, schedpoint);
		schedpoint = next_schedpoint;
	}
}

static void register_peephole_optimisation(ir_op *op, peephole_opt_func func)
{
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func) func;
}

/**
 * transform reload node => load
 */
static void transform_Reload(ir_node *node)
{
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irn_n(node, n_be_Spill_frame);
	ir_node   *mem    = get_irn_n(node, n_be_Reload_mem);
	ir_mode   *mode   = get_irn_mode(node);
	ir_entity *entity = be_get_frame_entity(node);
	const arch_register_t *reg;
	ir_node   *proj;
	ir_node   *load;

	ir_node  *sched_point = sched_prev(node);

	if (mode_is_float(mode)) {
		load = create_ldf(dbgi, block, ptr, mem, mode, entity, 0, true);
	} else {
		load = new_bd_sparc_Ld_imm(dbgi, block, ptr, mem, mode, entity, 0,
		                           true);
	}
	sched_add_after(sched_point, load);
	sched_remove(node);

	assert((long)pn_sparc_Ld_res == (long)pn_sparc_Ldf_res);
	proj = new_rd_Proj(dbgi, load, mode, pn_sparc_Ld_res);

	reg = arch_get_irn_register(node);
	arch_set_irn_register(proj, reg);

	exchange(node, proj);
}

/**
 * transform spill node => store
 */
static void transform_Spill(ir_node *node)
{
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irn_n(node, n_be_Spill_frame);
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *mem    = get_irg_no_mem(irg);
	ir_node   *val    = get_irn_n(node, n_be_Spill_val);
	ir_mode   *mode   = get_irn_mode(val);
	ir_entity *entity = be_get_frame_entity(node);
	ir_node   *sched_point;
	ir_node   *store;

	sched_point = sched_prev(node);
	if (mode_is_float(mode)) {
		store = create_stf(dbgi, block, val, ptr, mem, mode, entity, 0, true);
	} else {
		store = new_bd_sparc_St_imm(dbgi, block, val, ptr, mem, mode, entity, 0,
		                            true);
	}
	sched_remove(node);
	sched_add_after(sched_point, store);

	exchange(node, store);
}

/**
 * walker to transform be_Spill and be_Reload nodes
 */
static void sparc_after_ra_walker(ir_node *block, void *data)
{
	ir_node *node, *prev;
	(void) data;

	for (node = sched_last(block); !sched_is_begin(node); node = prev) {
		prev = sched_prev(node);

		if (be_is_Reload(node)) {
			transform_Reload(node);
		} else if (be_is_Spill(node)) {
			transform_Spill(node);
		}
	}
}

static void sparc_collect_frame_entity_nodes(ir_node *node, void *data)
{
	be_fec_env_t  *env = (be_fec_env_t*)data;
	const ir_mode *mode;
	int            align;
	ir_entity     *entity;
	const sparc_load_store_attr_t *attr;

	if (be_is_Reload(node) && be_get_frame_entity(node) == NULL) {
		mode  = get_irn_mode(node);
		align = get_mode_size_bytes(mode);
		be_node_needs_frame_entity(env, node, mode, align);
		return;
	}

	if (!is_sparc_Ld(node) && !is_sparc_Ldf(node))
		return;

	attr   = get_sparc_load_store_attr_const(node);
	entity = attr->base.immediate_value_entity;
	mode   = attr->load_store_mode;
	if (entity != NULL)
		return;
	if (!attr->is_frame_entity)
		return;
	if (arch_get_irn_flags(node) & sparc_arch_irn_flag_needs_64bit_spillslot)
		mode = mode_Lu;
	align  = get_mode_size_bytes(mode);
	be_node_needs_frame_entity(env, node, mode, align);
}

static void sparc_set_frame_entity(ir_node *node, ir_entity *entity)
{
	if (is_be_node(node)) {
		be_node_set_frame_entity(node, entity);
	} else {
		/* we only say be_node_needs_frame_entity on nodes with load_store
		 * attributes, so this should be fine */
		sparc_load_store_attr_t *attr = get_sparc_load_store_attr(node);
		assert(attr->is_frame_entity);
		assert(attr->base.immediate_value_entity == NULL);
		attr->base.immediate_value_entity = entity;
	}
}

void sparc_finish(ir_graph *irg)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);
	bool               at_begin     = stack_layout->sp_relative ? true : false;
	be_fec_env_t      *fec_env      = be_new_frame_entity_coalescer(irg);

	if (sparc_cg_config.use_permi)
		icore_lower_nodes_after_ra(irg);
	else
		lower_nodes_after_ra(irg);

	irg_walk_graph(irg, NULL, sparc_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, sparc_set_frame_entity, at_begin);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, sparc_after_ra_walker, NULL);

	sparc_introduce_prolog_epilog(irg);

	/* fix stack entity offsets */
	be_abi_fix_stack_nodes(irg);
	sparc_fix_stack_bias(irg);

	/* perform peephole optimizations */
	clear_irp_opcodes_generic_func();
	register_peephole_optimisation(op_be_IncSP,        peephole_be_IncSP);
	register_peephole_optimisation(op_sparc_FrameAddr, peephole_sparc_FrameAddr);
	be_peephole_opt(irg);

	/* perform legalizations (mostly fix nodes with too big immediates) */
	clear_irp_opcodes_generic_func();
	register_peephole_optimisation(op_be_IncSP,        finish_be_IncSP);
	register_peephole_optimisation(op_sparc_FrameAddr, finish_sparc_FrameAddr);
	register_peephole_optimisation(op_sparc_Return,    finish_sparc_Return);
	register_peephole_optimisation(op_sparc_Save,      finish_sparc_Save);
	be_peephole_opt(irg);

	be_remove_dead_nodes_from_schedule(irg);
}
