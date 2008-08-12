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
 * @brief       Implements several optimizations for IA32.
 * @author      Matthias Braun, Christian Wuerdig
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "irtools.h"
#include "firm_types.h"
#include "iredges.h"
#include "tv.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "height.h"
#include "irbitset.h"
#include "irprintf.h"
#include "error.h"

#include "../be_t.h"
#include "../beabi.h"
#include "../benode_t.h"
#include "../besched_t.h"
#include "../bepeephole.h"

#include "ia32_new_nodes.h"
#include "ia32_optimize.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_common_transform.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_util.h"
#include "ia32_architecture.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static const arch_env_t *arch_env;
static ia32_code_gen_t  *cg;

/**
 * Returns non-zero if the given node produces
 * a zero flag.
 *
 * @param node  the node to check
 * @param pn    if >= 0, the projection number of the used result
 */
static int produces_zero_flag(ir_node *node, int pn)
{
	ir_node                     *count;
	const ia32_immediate_attr_t *imm_attr;

	if (!is_ia32_irn(node))
		return 0;

	if (pn >= 0) {
		if (pn != pn_ia32_res)
			return 0;
	}

	switch (get_ia32_irn_opcode(node)) {
	case iro_ia32_Add:
	case iro_ia32_Adc:
	case iro_ia32_And:
	case iro_ia32_Or:
	case iro_ia32_Xor:
	case iro_ia32_Sub:
	case iro_ia32_Sbb:
	case iro_ia32_Neg:
	case iro_ia32_Inc:
	case iro_ia32_Dec:
		return 1;

	case iro_ia32_ShlD:
	case iro_ia32_ShrD:
	case iro_ia32_Shl:
	case iro_ia32_Shr:
	case iro_ia32_Sar:
		assert(n_ia32_ShlD_count == n_ia32_ShrD_count);
		assert(n_ia32_Shl_count == n_ia32_Shr_count
				&& n_ia32_Shl_count == n_ia32_Sar_count);
		if (is_ia32_ShlD(node) || is_ia32_ShrD(node)) {
			count = get_irn_n(node, n_ia32_ShlD_count);
		} else {
			count = get_irn_n(node, n_ia32_Shl_count);
		}
		/* when shift count is zero the flags are not affected, so we can only
		 * do this for constants != 0 */
		if (!is_ia32_Immediate(count))
			return 0;

		imm_attr = get_ia32_immediate_attr_const(count);
		if (imm_attr->symconst != NULL)
			return 0;
		if ((imm_attr->offset & 0x1f) == 0)
			return 0;
		return 1;

	default:
		break;
	}
	return 0;
}

/**
 * If the given node has not mode_T, creates a mode_T version (with a result Proj).
 *
 * @param node  the node to change
 *
 * @return the new mode_T node (if the mode was changed) or node itself
 */
static ir_node *turn_into_mode_t(ir_node *node)
{
	ir_node               *block;
	ir_node               *res_proj;
	ir_node               *new_node;
	const arch_register_t *reg;

	if(get_irn_mode(node) == mode_T)
		return node;

	assert(get_irn_mode(node) == mode_Iu);

	new_node = exact_copy(node);
	set_irn_mode(new_node, mode_T);

	block    = get_nodes_block(new_node);
	res_proj = new_r_Proj(current_ir_graph, block, new_node, mode_Iu,
	                      pn_ia32_res);

	reg = arch_get_irn_register(arch_env, node);
	arch_set_irn_register(arch_env, res_proj, reg);

	sched_add_before(node, new_node);
	be_peephole_exchange(node, res_proj);
	return new_node;
}

/**
 * Replace Cmp(x, 0) by a Test(x, x)
 */
static void peephole_ia32_Cmp(ir_node *const node)
{
	ir_node                     *right;
	ia32_immediate_attr_t const *imm;
	dbg_info                    *dbgi;
	ir_graph                    *irg;
	ir_node                     *block;
	ir_node                     *noreg;
	ir_node                     *nomem;
	ir_node                     *op;
	ia32_attr_t           const *attr;
	int                          ins_permuted;
	int                          cmp_unsigned;
	ir_node                     *test;
	arch_register_t       const *reg;

	if (get_ia32_op_type(node) != ia32_Normal)
		return;

	right = get_irn_n(node, n_ia32_Cmp_right);
	if (!is_ia32_Immediate(right))
		return;

	imm = get_ia32_immediate_attr_const(right);
	if (imm->symconst != NULL || imm->offset != 0)
		return;

	dbgi         = get_irn_dbg_info(node);
	irg          = current_ir_graph;
	block        = get_nodes_block(node);
	noreg        = ia32_new_NoReg_gp(cg);
	nomem        = get_irg_no_mem(irg);
	op           = get_irn_n(node, n_ia32_Cmp_left);
	attr         = get_irn_generic_attr(node);
	ins_permuted = attr->data.ins_permuted;
	cmp_unsigned = attr->data.cmp_unsigned;

	if (is_ia32_Cmp(node)) {
		test = new_rd_ia32_Test(dbgi, irg, block, noreg, noreg, nomem,
		                        op, op, ins_permuted, cmp_unsigned);
	} else {
		test = new_rd_ia32_Test8Bit(dbgi, irg, block, noreg, noreg, nomem,
		                            op, op, ins_permuted, cmp_unsigned);
	}
	set_ia32_ls_mode(test, get_ia32_ls_mode(node));

	reg = arch_get_irn_register(arch_env, node);
	arch_set_irn_register(arch_env, test, reg);

	sched_add_before(node, test);
	be_peephole_exchange(node, test);
}

/**
 * Peephole optimization for Test instructions.
 * We can remove the Test, if a zero flags was produced which is still
 * live.
 */
static void peephole_ia32_Test(ir_node *node)
{
	ir_node         *left  = get_irn_n(node, n_ia32_Test_left);
	ir_node         *right = get_irn_n(node, n_ia32_Test_right);
	ir_node         *flags_proj;
	ir_node         *block;
	ir_mode         *flags_mode;
	int              pn    = -1;
	ir_node         *schedpoint;
	const ir_edge_t *edge;

	assert(n_ia32_Test_left == n_ia32_Test8Bit_left
			&& n_ia32_Test_right == n_ia32_Test8Bit_right);

	/* we need a test for 0 */
	if(left != right)
		return;

	block = get_nodes_block(node);
	if(get_nodes_block(left) != block)
		return;

	if(is_Proj(left)) {
		pn   = get_Proj_proj(left);
		left = get_Proj_pred(left);
	}

	/* happens rarely, but if it does code will panic' */
	if (is_ia32_Unknown_GP(left))
		return;

	/* walk schedule up and abort when we find left or some other node destroys
	   the flags */
	schedpoint = sched_prev(node);
	while(schedpoint != left) {
		schedpoint = sched_prev(schedpoint);
		if(arch_irn_is(arch_env, schedpoint, modify_flags))
			return;
		if(schedpoint == block)
			panic("couldn't find left");
	}

	/* make sure only Lg/Eq tests are used */
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		int      pnc  = get_ia32_condcode(user);

		if(pnc != pn_Cmp_Eq && pnc != pn_Cmp_Lg) {
			return;
		}
	}

	if(!produces_zero_flag(left, pn))
		return;

	left = turn_into_mode_t(left);

	flags_mode = ia32_reg_classes[CLASS_ia32_flags].mode;
	flags_proj = new_r_Proj(current_ir_graph, block, left, flags_mode,
	                        pn_ia32_flags);
	arch_set_irn_register(arch_env, flags_proj, &ia32_flags_regs[REG_EFLAGS]);

	assert(get_irn_mode(node) != mode_T);

	be_peephole_exchange(node, flags_proj);
}

/**
 * AMD Athlon works faster when RET is not destination of
 * conditional jump or directly preceded by other jump instruction.
 * Can be avoided by placing a Rep prefix before the return.
 */
static void peephole_ia32_Return(ir_node *node) {
	ir_node *block, *irn;

	if (!ia32_cg_config.use_pad_return)
		return;

	block = get_nodes_block(node);

	/* check if this return is the first on the block */
	sched_foreach_reverse_from(node, irn) {
		switch (get_irn_opcode(irn)) {
		case beo_Return:
			/* the return node itself, ignore */
			continue;
		case beo_Barrier:
			/* ignore the barrier, no code generated */
			continue;
		case beo_IncSP:
			/* arg, IncSP 0 nodes might occur, ignore these */
			if (be_get_IncSP_offset(irn) == 0)
				continue;
			return;
		case iro_Phi:
			continue;
		default:
			return;
		}
	}

	/* ensure, that the 3 byte return is generated
	 * actually the emitter tests again if the block beginning has a label and
	 * isn't just a fallthrough */
	be_Return_set_emit_pop(node, 1);
}

/* only optimize up to 48 stores behind IncSPs */
#define MAXPUSH_OPTIMIZE	48

/**
 * Tries to create Push's from IncSP, Store combinations.
 * The Stores are replaced by Push's, the IncSP is modified
 * (possibly into IncSP 0, but not removed).
 */
static void peephole_IncSP_Store_to_push(ir_node *irn)
{
	int      i, maxslot, inc_ofs;
	ir_node  *node;
	ir_node  *stores[MAXPUSH_OPTIMIZE];
	ir_node  *block;
	ir_graph *irg;
	ir_node  *curr_sp;
	ir_mode  *spmode;

	memset(stores, 0, sizeof(stores));

	assert(be_is_IncSP(irn));

	inc_ofs = be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	/*
	 * We first walk the schedule after the IncSP node as long as we find
	 * suitable Stores that could be transformed to a Push.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	maxslot = -1;
	for (node = sched_next(irn); !sched_is_end(node); node = sched_next(node)) {
		ir_node *mem;
		int offset;
		int storeslot;

		/* it has to be a Store */
		if (!is_ia32_Store(node))
			break;

		/* it has to use our sp value */
		if (get_irn_n(node, n_ia32_base) != irn)
			continue;
		/* Store has to be attached to NoMem */
		mem = get_irn_n(node, n_ia32_mem);
		if (!is_NoMem(mem))
			continue;

		/* unfortunately we can't support the full AMs possible for push at the
		 * moment. TODO: fix this */
		if (get_ia32_am_scale(node) > 0 || !is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
			break;

		offset = get_ia32_am_offs_int(node);
		/* we should NEVER access uninitialized stack BELOW the current SP */
		assert(offset >= 0);

		offset = inc_ofs - 4 - offset;

		/* storing at half-slots is bad */
		if ((offset & 3) != 0)
			break;

		if (offset < 0 || offset >= MAXPUSH_OPTIMIZE * 4)
			continue;
		storeslot = offset >> 2;

		/* storing into the same slot twice is bad (and shouldn't happen...) */
		if (stores[storeslot] != NULL)
			break;

		stores[storeslot] = node;
		if (storeslot > maxslot)
			maxslot = storeslot;
	}

	curr_sp = be_get_IncSP_pred(irn);

	/* walk through the Stores and create Pushs for them */
	block  = get_nodes_block(irn);
	spmode = get_irn_mode(irn);
	irg    = cg->irg;
	for (i = 0; i <= maxslot; ++i) {
		const arch_register_t *spreg;
		ir_node *push;
		ir_node *val, *mem, *mem_proj;
		ir_node *store = stores[i];
		ir_node *noreg = ia32_new_NoReg_gp(cg);

		if (store == NULL)
			break;

		val = get_irn_n(store, n_ia32_unary_op);
		mem = get_irn_n(store, n_ia32_mem);
		spreg = arch_get_irn_register(cg->arch_env, curr_sp);

		push = new_rd_ia32_Push(get_irn_dbg_info(store), irg, block, noreg, noreg, mem, val, curr_sp);

		sched_add_before(irn, push);

		/* create stackpointer Proj */
		curr_sp = new_r_Proj(irg, block, push, spmode, pn_ia32_Push_stack);
		arch_set_irn_register(cg->arch_env, curr_sp, spreg);

		/* create memory Proj */
		mem_proj = new_r_Proj(irg, block, push, mode_M, pn_ia32_Push_M);

		/* use the memproj now */
		be_peephole_exchange(store, mem_proj);

		inc_ofs -= 4;
	}

	be_set_IncSP_offset(irn, inc_ofs);
	be_set_IncSP_pred(irn, curr_sp);
}

/**
 * Return true if a mode can be stored in the GP register set
 */
static INLINE int mode_needs_gp_reg(ir_mode *mode) {
        if (mode == mode_fpcw)
                return 0;
        if (get_mode_size_bits(mode) > 32)
                return 0;
        return mode_is_int(mode) || mode_is_reference(mode) || mode == mode_b;
}

/**
 * Tries to create Pops from Load, IncSP combinations.
 * The Loads are replaced by Pops, the IncSP is modified
 * (possibly into IncSP 0, but not removed).
 */
static void peephole_Load_IncSP_to_pop(ir_node *irn)
{
	const arch_register_t *esp = &ia32_gp_regs[REG_ESP];
	int      i, maxslot, inc_ofs, ofs;
	ir_node  *node, *pred_sp, *block;
	ir_node  *loads[MAXPUSH_OPTIMIZE];
	ir_graph *irg;
	unsigned regmask = 0;
	unsigned copymask = ~0;

	memset(loads, 0, sizeof(loads));
	assert(be_is_IncSP(irn));

	inc_ofs = -be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	/*
	 * We first walk the schedule before the IncSP node as long as we find
	 * suitable Loads that could be transformed to a Pop.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	maxslot = -1;
	pred_sp = be_get_IncSP_pred(irn);
	for (node = sched_prev(irn); !sched_is_end(node); node = sched_prev(node)) {
		ir_node *mem;
		int offset;
		int loadslot;
		const arch_register_t *sreg, *dreg;

		/* it has to be a Load */
		if (!is_ia32_Load(node)) {
			if (be_is_Copy(node)) {
				if (!mode_needs_gp_reg(get_irn_mode(node))) {
					/* not a GP copy, ignore */
					continue;
				}
				dreg = arch_get_irn_register(arch_env, node);
				sreg = arch_get_irn_register(arch_env, be_get_Copy_op(node));
				if (regmask & copymask & (1 << sreg->index)) {
					break;
				}
				if (regmask & copymask & (1 << dreg->index)) {
					break;
				}
				/* we CAN skip Copies if neither the destination nor the source
				 * is not in our regmask, ie none of our future Pop will overwrite it */
				regmask |= (1 << dreg->index) | (1 << sreg->index);
				copymask &= ~((1 << dreg->index) | (1 << sreg->index));
				continue;
			}
			break;
		}

		/* we can handle only GP loads */
		if (!mode_needs_gp_reg(get_ia32_ls_mode(node)))
			continue;

		/* it has to use our predecessor sp value */
		if (get_irn_n(node, n_ia32_base) != pred_sp) {
			/* it would be ok if this load does not use a Pop result,
			 * but we do not check this */
			break;
		}
		/* Load has to be attached to Spill-Mem */
		mem = skip_Proj(get_irn_n(node, n_ia32_mem));
		if (!is_Phi(mem) && !is_ia32_Store(mem) && !is_ia32_Push(mem))
			break;

		/* should have NO index */
		if (get_ia32_am_scale(node) > 0 || !is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
			break;

		offset = get_ia32_am_offs_int(node);
		/* we should NEVER access uninitialized stack BELOW the current SP */
		assert(offset >= 0);

		/* storing at half-slots is bad */
		if ((offset & 3) != 0)
			break;

		if (offset < 0 || offset >= MAXPUSH_OPTIMIZE * 4)
			continue;
		/* ignore those outside the possible windows */
		if (offset > inc_ofs - 4)
			continue;
		loadslot = offset >> 2;

		/* loading from the same slot twice is bad (and shouldn't happen...) */
		if (loads[loadslot] != NULL)
			break;

		dreg = arch_get_irn_register(arch_env, node);
		if (regmask & (1 << dreg->index)) {
			/* this register is already used */
			break;
		}
		regmask |= 1 << dreg->index;

		loads[loadslot] = node;
		if (loadslot > maxslot)
			maxslot = loadslot;
	}

	if (maxslot < 0)
		return;

	/* find the first slot */
	for (i = maxslot; i >= 0; --i) {
		ir_node *load = loads[i];

		if (load == NULL)
			break;
	}

	ofs = inc_ofs - (maxslot + 1) * 4;
	inc_ofs = (i+1) * 4;

	/* create a new IncSP if needed */
	block = get_nodes_block(irn);
	irg   = cg->irg;
	if (inc_ofs > 0) {
		pred_sp = be_new_IncSP(esp, irg, block, pred_sp, -inc_ofs, be_get_IncSP_align(irn));
		sched_add_before(irn, pred_sp);
	}

	/* walk through the Loads and create Pops for them */
	for (++i; i <= maxslot; ++i) {
		ir_node *load = loads[i];
		ir_node *mem, *pop;
		const ir_edge_t *edge, *tmp;
		const arch_register_t *reg;

		mem = get_irn_n(load, n_ia32_mem);
		reg = arch_get_irn_register(arch_env, load);

		pop = new_rd_ia32_Pop(get_irn_dbg_info(load), irg, block, mem, pred_sp);
		arch_set_irn_register(arch_env, pop, reg);

		/* create stackpointer Proj */
		pred_sp = new_r_Proj(irg, block, pop, mode_Iu, pn_ia32_Pop_stack);
		arch_set_irn_register(arch_env, pred_sp, esp);

		sched_add_before(irn, pop);

		/* rewire now */
		foreach_out_edge_safe(load, edge, tmp) {
			ir_node *proj = get_edge_src_irn(edge);

			set_Proj_pred(proj, pop);
		}

		/* we can remove the Load now */
		sched_remove(load);
		kill_node(load);
	}

	be_set_IncSP_offset(irn, -ofs);
	be_set_IncSP_pred(irn, pred_sp);
}


/**
 * Find a free GP register if possible, else return NULL.
 */
static const arch_register_t *get_free_gp_reg(void)
{
	int i;

	for(i = 0; i < N_ia32_gp_REGS; ++i) {
		const arch_register_t *reg = &ia32_gp_regs[i];
		if(arch_register_type_is(reg, ignore))
			continue;

		if(be_peephole_get_value(CLASS_ia32_gp, i) == NULL)
			return &ia32_gp_regs[i];
	}

	return NULL;
}

/**
 * Creates a Pop instruction before the given schedule point.
 *
 * @param dbgi        debug info
 * @param irg         the graph
 * @param block       the block
 * @param stack       the previous stack value
 * @param schedpoint  the new node is added before this node
 * @param reg         the register to pop
 *
 * @return the new stack value
 */
static ir_node *create_pop(dbg_info *dbgi, ir_graph *irg, ir_node *block,
                           ir_node *stack, ir_node *schedpoint,
                           const arch_register_t *reg)
{
	const arch_register_t *esp = &ia32_gp_regs[REG_ESP];
	ir_node *pop;
	ir_node *keep;
	ir_node *val;
	ir_node *in[1];

	pop   = new_rd_ia32_Pop(dbgi, irg, block, new_NoMem(), stack);

	stack = new_r_Proj(irg, block, pop, mode_Iu, pn_ia32_Pop_stack);
	arch_set_irn_register(arch_env, stack, esp);
	val   = new_r_Proj(irg, block, pop, mode_Iu, pn_ia32_Pop_res);
	arch_set_irn_register(arch_env, val, reg);

	sched_add_before(schedpoint, pop);

	in[0] = val;
	keep = be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, in);
	sched_add_before(schedpoint, keep);

	return stack;
}

/**
 * Creates a Push instruction before the given schedule point.
 *
 * @param dbgi        debug info
 * @param irg         the graph
 * @param block       the block
 * @param stack       the previous stack value
 * @param schedpoint  the new node is added before this node
 * @param reg         the register to pop
 *
 * @return the new stack value
 */
static ir_node *create_push(dbg_info *dbgi, ir_graph *irg, ir_node *block,
                            ir_node *stack, ir_node *schedpoint)
{
	const arch_register_t *esp = &ia32_gp_regs[REG_ESP];

	ir_node *val   = ia32_new_Unknown_gp(cg);
	ir_node *noreg = ia32_new_NoReg_gp(cg);
	ir_node *nomem = get_irg_no_mem(irg);
	ir_node *push  = new_rd_ia32_Push(dbgi, irg, block, noreg, noreg, nomem, val, stack);
	sched_add_before(schedpoint, push);

	stack = new_r_Proj(irg, block, push, mode_Iu, pn_ia32_Push_stack);
	arch_set_irn_register(arch_env, stack, esp);

	return stack;
}

/**
 * Optimize an IncSp by replacing it with Push/Pop.
 */
static void peephole_be_IncSP(ir_node *node)
{
	const arch_register_t *esp = &ia32_gp_regs[REG_ESP];
	const arch_register_t *reg;
	ir_graph              *irg = current_ir_graph;
	dbg_info              *dbgi;
	ir_node               *block;
	ir_node               *stack;
	int                    offset;

	/* first optimize incsp->incsp combinations */
	node = be_peephole_IncSP_IncSP(node);

	/* transform IncSP->Store combinations to Push where possible */
	peephole_IncSP_Store_to_push(node);

	/* transform Load->IncSP combinations to Pop where possible */
	peephole_Load_IncSP_to_pop(node);

	if (arch_get_irn_register(arch_env, node) != esp)
		return;

	/* replace IncSP -4 by Pop freereg when possible */
	offset = be_get_IncSP_offset(node);
	if ((offset != -8 || ia32_cg_config.use_add_esp_8) &&
	    (offset != -4 || ia32_cg_config.use_add_esp_4) &&
	    (offset != +4 || ia32_cg_config.use_sub_esp_4) &&
	    (offset != +8 || ia32_cg_config.use_sub_esp_8))
		return;

	if (offset < 0) {
		/* we need a free register for pop */
		reg = get_free_gp_reg();
		if (reg == NULL)
			return;

		dbgi  = get_irn_dbg_info(node);
		block = get_nodes_block(node);
		stack = be_get_IncSP_pred(node);

		stack = create_pop(dbgi, irg, block, stack, node, reg);

		if (offset == -8) {
			stack = create_pop(dbgi, irg, block, stack, node, reg);
		}
	} else {
		dbgi  = get_irn_dbg_info(node);
		block = get_nodes_block(node);
		stack = be_get_IncSP_pred(node);
		stack = create_push(dbgi, irg, block, stack, node);

		if (offset == +8) {
			stack = create_push(dbgi, irg, block, stack, node);
		}
	}

	be_peephole_exchange(node, stack);
}

/**
 * Peephole optimisation for ia32_Const's
 */
static void peephole_ia32_Const(ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);
	const arch_register_t       *reg;
	ir_graph                    *irg = current_ir_graph;
	ir_node                     *block;
	dbg_info                    *dbgi;
	ir_node                     *produceval;
	ir_node                     *xor;
	ir_node                     *noreg;

	/* try to transform a mov 0, reg to xor reg reg */
	if (attr->offset != 0 || attr->symconst != NULL)
		return;
	if (ia32_cg_config.use_mov_0)
		return;
	/* xor destroys the flags, so no-one must be using them */
	if (be_peephole_get_value(CLASS_ia32_flags, REG_EFLAGS) != NULL)
		return;

	reg = arch_get_irn_register(arch_env, node);
	assert(be_peephole_get_reg_value(reg) == NULL);

	/* create xor(produceval, produceval) */
	block      = get_nodes_block(node);
	dbgi       = get_irn_dbg_info(node);
	produceval = new_rd_ia32_ProduceVal(dbgi, irg, block);
	arch_set_irn_register(arch_env, produceval, reg);

	noreg = ia32_new_NoReg_gp(cg);
	xor   = new_rd_ia32_Xor(dbgi, irg, block, noreg, noreg, new_NoMem(),
	                        produceval, produceval);
	arch_set_irn_register(arch_env, xor, reg);

	sched_add_before(node, produceval);
	sched_add_before(node, xor);

	be_peephole_exchange(node, xor);
}

static INLINE int is_noreg(ia32_code_gen_t *cg, const ir_node *node)
{
	return node == cg->noreg_gp;
}

static ir_node *create_immediate_from_int(ia32_code_gen_t *cg, int val)
{
	ir_graph *irg         = current_ir_graph;
	ir_node  *start_block = get_irg_start_block(irg);
	ir_node  *immediate   = new_rd_ia32_Immediate(NULL, irg, start_block, NULL,
	                                              0, val);
	arch_set_irn_register(cg->arch_env, immediate, &ia32_gp_regs[REG_GP_NOREG]);

	return immediate;
}

static ir_node *create_immediate_from_am(ia32_code_gen_t *cg,
                                         const ir_node *node)
{
	ir_graph  *irg     = get_irn_irg(node);
	ir_node   *block   = get_nodes_block(node);
	int        offset  = get_ia32_am_offs_int(node);
	int        sc_sign = is_ia32_am_sc_sign(node);
	ir_entity *entity  = get_ia32_am_sc(node);
	ir_node   *res;

	res = new_rd_ia32_Immediate(NULL, irg, block, entity, sc_sign, offset);
	arch_set_irn_register(cg->arch_env, res, &ia32_gp_regs[REG_GP_NOREG]);
	return res;
}

static int is_am_one(const ir_node *node)
{
	int        offset  = get_ia32_am_offs_int(node);
	ir_entity *entity  = get_ia32_am_sc(node);

	return offset == 1 && entity == NULL;
}

static int is_am_minus_one(const ir_node *node)
{
	int        offset  = get_ia32_am_offs_int(node);
	ir_entity *entity  = get_ia32_am_sc(node);

	return offset == -1 && entity == NULL;
}

/**
 * Transforms a LEA into an Add or SHL if possible.
 */
static void peephole_ia32_Lea(ir_node *node)
{
	const arch_env_t      *arch_env = cg->arch_env;
	ir_graph              *irg      = current_ir_graph;
	ir_node               *base;
	ir_node               *index;
	const arch_register_t *base_reg;
	const arch_register_t *index_reg;
	const arch_register_t *out_reg;
	int                    scale;
	int                    has_immediates;
	ir_node               *op1;
	ir_node               *op2;
	dbg_info              *dbgi;
	ir_node               *block;
	ir_node               *res;
	ir_node               *noreg;
	ir_node               *nomem;

	assert(is_ia32_Lea(node));

	/* we can only do this if are allowed to globber the flags */
	if(be_peephole_get_value(CLASS_ia32_flags, REG_EFLAGS) != NULL)
		return;

	base  = get_irn_n(node, n_ia32_Lea_base);
	index = get_irn_n(node, n_ia32_Lea_index);

	if(is_noreg(cg, base)) {
		base     = NULL;
		base_reg = NULL;
	} else {
		base_reg = arch_get_irn_register(arch_env, base);
	}
	if(is_noreg(cg, index)) {
		index     = NULL;
		index_reg = NULL;
	} else {
		index_reg = arch_get_irn_register(arch_env, index);
	}

	if(base == NULL && index == NULL) {
		/* we shouldn't construct these in the first place... */
#ifdef DEBUG_libfirm
		ir_fprintf(stderr, "Optimisation warning: found immediate only lea\n");
#endif
		return;
	}

	out_reg = arch_get_irn_register(arch_env, node);
	scale   = get_ia32_am_scale(node);
	assert(!is_ia32_need_stackent(node) || get_ia32_frame_ent(node) != NULL);
	/* check if we have immediates values (frame entities should already be
	 * expressed in the offsets) */
	if(get_ia32_am_offs_int(node) != 0 || get_ia32_am_sc(node) != NULL) {
		has_immediates = 1;
	} else {
		has_immediates = 0;
	}

	/* we can transform leas where the out register is the same as either the
	 * base or index register back to an Add or Shl */
	if(out_reg == base_reg) {
		if(index == NULL) {
#ifdef DEBUG_libfirm
			if(!has_immediates) {
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
			}
#endif
			op1 = base;
			goto make_add_immediate;
		}
		if(scale == 0 && !has_immediates) {
			op1 = base;
			op2 = index;
			goto make_add;
		}
		/* can't create an add */
		return;
	} else if(out_reg == index_reg) {
		if(base == NULL) {
			if(has_immediates && scale == 0) {
				op1 = index;
				goto make_add_immediate;
			} else if(!has_immediates && scale > 0) {
				op1 = index;
				op2 = create_immediate_from_int(cg, scale);
				goto make_shl;
			} else if(!has_immediates) {
#ifdef DEBUG_libfirm
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
#endif
			}
		} else if(scale == 0 && !has_immediates) {
			op1 = index;
			op2 = base;
			goto make_add;
		}
		/* can't create an add */
		return;
	} else {
		/* can't create an add */
		return;
	}

make_add_immediate:
	if(ia32_cg_config.use_incdec) {
		if(is_am_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_rd_ia32_Inc(dbgi, irg, block, op1);
			arch_set_irn_register(arch_env, res, out_reg);
			goto exchange;
		}
		if(is_am_minus_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_rd_ia32_Dec(dbgi, irg, block, op1);
			arch_set_irn_register(arch_env, res, out_reg);
			goto exchange;
		}
	}
	op2 = create_immediate_from_am(cg, node);

make_add:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_NoMem();
	res   = new_rd_ia32_Add(dbgi, irg, block, noreg, noreg, nomem, op1, op2);
	arch_set_irn_register(arch_env, res, out_reg);
	set_ia32_commutative(res);
	goto exchange;

make_shl:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	noreg = ia32_new_NoReg_gp(cg);
	nomem = new_NoMem();
	res   = new_rd_ia32_Shl(dbgi, irg, block, op1, op2);
	arch_set_irn_register(arch_env, res, out_reg);
	goto exchange;

exchange:
	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(cg, node));

	/* add new ADD/SHL to schedule */
	DBG_OPT_LEA2ADD(node, res);

	/* exchange the Add and the LEA */
	sched_add_before(node, res);
	be_peephole_exchange(node, res);
}

/**
 * Split a Imul mem, imm into a Load mem and Imul reg, imm if possible.
 */
static void peephole_ia32_Imul_split(ir_node *imul) {
	const ir_node         *right = get_irn_n(imul, n_ia32_IMul_right);
	const arch_register_t *reg;
	ir_node               *load, *block, *base, *index, *mem, *res, *noreg;
	dbg_info              *dbgi;
	ir_graph              *irg;

	if (! is_ia32_Immediate(right) || get_ia32_op_type(imul) != ia32_AddrModeS) {
		/* no memory, imm form ignore */
		return;
	}
	/* we need a free register */
	reg = get_free_gp_reg();
	if (reg == NULL)
		return;

	/* fine, we can rebuild it */
	dbgi  = get_irn_dbg_info(imul);
	block = get_nodes_block(imul);
	irg   = current_ir_graph;
	base  = get_irn_n(imul, n_ia32_IMul_base);
	index = get_irn_n(imul, n_ia32_IMul_index);
	mem   = get_irn_n(imul, n_ia32_IMul_mem);
	load = new_rd_ia32_Load(dbgi, irg, block, base, index, mem);

	/* copy all attributes */
	set_irn_pinned(load, get_irn_pinned(imul));
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, get_ia32_ls_mode(imul));

	set_ia32_am_scale(load, get_ia32_am_scale(imul));
	set_ia32_am_sc(load, get_ia32_am_sc(imul));
	set_ia32_am_offs_int(load, get_ia32_am_offs_int(imul));
	if (is_ia32_am_sc_sign(imul))
		set_ia32_am_sc_sign(load);
	if (is_ia32_use_frame(imul))
		set_ia32_use_frame(load);
	set_ia32_frame_ent(load, get_ia32_frame_ent(imul));

	sched_add_before(imul, load);

	mem = new_rd_Proj(dbgi, irg, block, load, mode_M, pn_ia32_Load_M);
	res = new_rd_Proj(dbgi, irg, block, load, mode_Iu, pn_ia32_Load_res);

	arch_set_irn_register(arch_env, res, reg);
	be_peephole_new_node(res);

	set_irn_n(imul, n_ia32_IMul_mem, mem);
	noreg = get_irn_n(imul, n_ia32_IMul_left);
	set_irn_n(imul, n_ia32_IMul_left, res);
	set_ia32_op_type(imul, ia32_Normal);
}

/**
 * Replace xorps r,r and xorpd r,r by pxor r,r
 */
static void peephole_ia32_xZero(ir_node *xor) {
	set_irn_op(xor, op_ia32_xPzero);
}

/**
 * Register a peephole optimisation function.
 */
static void register_peephole_optimisation(ir_op *op, peephole_opt_func func) {
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func)func;
}

/* Perform peephole-optimizations. */
void ia32_peephole_optimization(ia32_code_gen_t *new_cg)
{
	cg       = new_cg;
	arch_env = cg->arch_env;

	/* register peephole optimisations */
	clear_irp_opcodes_generic_func();
	register_peephole_optimisation(op_ia32_Const,    peephole_ia32_Const);
	register_peephole_optimisation(op_be_IncSP,      peephole_be_IncSP);
	register_peephole_optimisation(op_ia32_Lea,      peephole_ia32_Lea);
	register_peephole_optimisation(op_ia32_Cmp,      peephole_ia32_Cmp);
	register_peephole_optimisation(op_ia32_Cmp8Bit,  peephole_ia32_Cmp);
	register_peephole_optimisation(op_ia32_Test,     peephole_ia32_Test);
	register_peephole_optimisation(op_ia32_Test8Bit, peephole_ia32_Test);
	register_peephole_optimisation(op_be_Return,     peephole_ia32_Return);
	if (! ia32_cg_config.use_imul_mem_imm32)
		register_peephole_optimisation(op_ia32_IMul, peephole_ia32_Imul_split);
	if (ia32_cg_config.use_pxor)
		register_peephole_optimisation(op_ia32_xZero, peephole_ia32_xZero);

	be_peephole_opt(cg->birg);
}

/**
 * Removes node from schedule if it is not used anymore. If irn is a mode_T node
 * all it's Projs are removed as well.
 * @param irn  The irn to be removed from schedule
 */
static INLINE void try_kill(ir_node *node)
{
	if(get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge, *next;
		foreach_out_edge_safe(node, edge, next) {
			ir_node *proj = get_edge_src_irn(edge);
			try_kill(proj);
		}
	}

	if(get_irn_n_edges(node) != 0)
		return;

	if (sched_is_scheduled(node)) {
		sched_remove(node);
	}

	kill_node(node);
}

static void optimize_conv_store(ir_node *node)
{
	ir_node *pred;
	ir_node *pred_proj;
	ir_mode *conv_mode;
	ir_mode *store_mode;

	if(!is_ia32_Store(node) && !is_ia32_Store8Bit(node))
		return;

	assert(n_ia32_Store_val == n_ia32_Store8Bit_val);
	pred_proj = get_irn_n(node, n_ia32_Store_val);
	if(is_Proj(pred_proj)) {
		pred = get_Proj_pred(pred_proj);
	} else {
		pred = pred_proj;
	}
	if(!is_ia32_Conv_I2I(pred) && !is_ia32_Conv_I2I8Bit(pred))
		return;
	if(get_ia32_op_type(pred) != ia32_Normal)
		return;

	/* the store only stores the lower bits, so we only need the conv
	 * it it shrinks the mode */
	conv_mode  = get_ia32_ls_mode(pred);
	store_mode = get_ia32_ls_mode(node);
	if(get_mode_size_bits(conv_mode) < get_mode_size_bits(store_mode))
		return;

	set_irn_n(node, n_ia32_Store_val, get_irn_n(pred, n_ia32_Conv_I2I_val));
	if(get_irn_n_edges(pred_proj) == 0) {
		kill_node(pred_proj);
		if(pred != pred_proj)
			kill_node(pred);
	}
}

static void optimize_load_conv(ir_node *node)
{
	ir_node *pred, *predpred;
	ir_mode *load_mode;
	ir_mode *conv_mode;

	if (!is_ia32_Conv_I2I(node) && !is_ia32_Conv_I2I8Bit(node))
		return;

	assert(n_ia32_Conv_I2I_val == n_ia32_Conv_I2I8Bit_val);
	pred = get_irn_n(node, n_ia32_Conv_I2I_val);
	if(!is_Proj(pred))
		return;

	predpred = get_Proj_pred(pred);
	if(!is_ia32_Load(predpred))
		return;

	/* the load is sign extending the upper bits, so we only need the conv
	 * if it shrinks the mode */
	load_mode = get_ia32_ls_mode(predpred);
	conv_mode = get_ia32_ls_mode(node);
	if(get_mode_size_bits(conv_mode) < get_mode_size_bits(load_mode))
		return;

	if(get_mode_sign(conv_mode) != get_mode_sign(load_mode)) {
		/* change the load if it has only 1 user */
		if(get_irn_n_edges(pred) == 1) {
			ir_mode *newmode;
			if(get_mode_sign(conv_mode)) {
				newmode = find_signed_mode(load_mode);
			} else {
				newmode = find_unsigned_mode(load_mode);
			}
			assert(newmode != NULL);
			set_ia32_ls_mode(predpred, newmode);
		} else {
			/* otherwise we have to keep the conv */
			return;
		}
	}

	/* kill the conv */
	exchange(node, pred);
}

static void optimize_conv_conv(ir_node *node)
{
	ir_node *pred_proj, *pred, *result_conv;
	ir_mode *pred_mode, *conv_mode;
	int      conv_mode_bits;
	int      pred_mode_bits;

	if (!is_ia32_Conv_I2I(node) && !is_ia32_Conv_I2I8Bit(node))
		return;

	assert(n_ia32_Conv_I2I_val == n_ia32_Conv_I2I8Bit_val);
	pred_proj = get_irn_n(node, n_ia32_Conv_I2I_val);
	if(is_Proj(pred_proj))
		pred = get_Proj_pred(pred_proj);
	else
		pred = pred_proj;

	if(!is_ia32_Conv_I2I(pred) && !is_ia32_Conv_I2I8Bit(pred))
		return;

	/* we know that after a conv, the upper bits are sign extended
	 * so we only need the 2nd conv if it shrinks the mode */
	conv_mode      = get_ia32_ls_mode(node);
	conv_mode_bits = get_mode_size_bits(conv_mode);
	pred_mode      = get_ia32_ls_mode(pred);
	pred_mode_bits = get_mode_size_bits(pred_mode);

	if(conv_mode_bits == pred_mode_bits
			&& get_mode_sign(conv_mode) == get_mode_sign(pred_mode)) {
		result_conv = pred_proj;
	} else if(conv_mode_bits <= pred_mode_bits) {
		/* if 2nd conv is smaller then first conv, then we can always take the
		 * 2nd conv */
		if(get_irn_n_edges(pred_proj) == 1) {
			result_conv = pred_proj;
			set_ia32_ls_mode(pred, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				set_irn_op(pred, op_ia32_Conv_I2I8Bit);
				set_ia32_in_req_all(pred, get_ia32_in_req_all(node));
			}
		} else {
			/* we don't want to end up with 2 loads, so we better do nothing */
			if(get_irn_mode(pred) == mode_T) {
				return;
			}

			result_conv = exact_copy(pred);
			set_ia32_ls_mode(result_conv, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				set_irn_op(result_conv, op_ia32_Conv_I2I8Bit);
				set_ia32_in_req_all(result_conv, get_ia32_in_req_all(node));
			}
		}
	} else {
		/* if both convs have the same sign, then we can take the smaller one */
		if(get_mode_sign(conv_mode) == get_mode_sign(pred_mode)) {
			result_conv = pred_proj;
		} else {
			/* no optimisation possible if smaller conv is sign-extend */
			if(mode_is_signed(pred_mode)) {
				return;
			}
			/* we can take the smaller conv if it is unsigned */
			result_conv = pred_proj;
		}
	}

	/* kill the conv */
	exchange(node, result_conv);

	if(get_irn_n_edges(pred_proj) == 0) {
		kill_node(pred_proj);
		if(pred != pred_proj)
			kill_node(pred);
	}
	optimize_conv_conv(result_conv);
}

static void optimize_node(ir_node *node, void *env)
{
	(void) env;

	optimize_load_conv(node);
	optimize_conv_store(node);
	optimize_conv_conv(node);
}

/**
 * Performs conv and address mode optimization.
 */
void ia32_optimize_graph(ia32_code_gen_t *cg)
{
	irg_walk_blkwise_graph(cg->irg, NULL, optimize_node, cg);

	if (cg->dump)
		be_dump(cg->irg, "-opt", dump_ir_block_graph_sched);
}

void ia32_init_optimize(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.optimize");
}
