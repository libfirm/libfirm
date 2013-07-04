/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements several optimizations for IA32.
 * @author      Matthias Braun, Christian Wuerdig
 */
#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "irtools.h"
#include "firm_types.h"
#include "iredges.h"
#include "tv.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "heights.h"
#include "irprintf.h"
#include "irdump.h"
#include "error.h"
#include "firmstat_t.h"

#include "be_t.h"
#include "beabi.h"
#include "benode.h"
#include "besched.h"
#include "bepeephole.h"

#include "ia32_new_nodes.h"
#include "ia32_optimize.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_common_transform.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_architecture.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static void copy_mark(const ir_node *old, ir_node *newn)
{
	if (is_ia32_is_reload(old))
		set_ia32_is_reload(newn);
	if (is_ia32_is_spill(old))
		set_ia32_is_spill(newn);
	if (is_ia32_is_remat(old))
		set_ia32_is_remat(newn);
}

typedef enum produces_flag_t {
	produces_no_flag,
	produces_zero_sign,
	produces_zero_in_carry
} produces_flag_t;

/**
 * Return which usable flag the given node produces about the result.
 * That is zero (ZF) and sign(SF).
 * We do not check for carry (CF) or overflow (OF).
 *
 * @param node  the node to check
 * @param pn    the projection number of the used result
 */
static produces_flag_t check_produces_zero_sign(ir_node *node, int pn)
{
	ir_node                     *count;
	const ia32_immediate_attr_t *imm_attr;

	if (!is_ia32_irn(node))
		return produces_no_flag;

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
			break;

		case iro_ia32_ShlD:
		case iro_ia32_ShrD:
			assert((int)n_ia32_ShlD_count == (int)n_ia32_ShrD_count);
			count = get_irn_n(node, n_ia32_ShlD_count);
			goto check_shift_amount;

		case iro_ia32_Shl:
		case iro_ia32_Shr:
		case iro_ia32_Sar:
			assert((int)n_ia32_Shl_count == (int)n_ia32_Shr_count
					&& (int)n_ia32_Shl_count == (int)n_ia32_Sar_count);
			count = get_irn_n(node, n_ia32_Shl_count);
check_shift_amount:
			/* when shift count is zero the flags are not affected, so we can only
			 * do this for constants != 0 */
			if (!is_ia32_Immediate(count))
				return produces_no_flag;

			imm_attr = get_ia32_immediate_attr_const(count);
			if (imm_attr->symconst != NULL)
				return produces_no_flag;
			if ((imm_attr->offset & 0x1f) == 0)
				return produces_no_flag;
			break;

		case iro_ia32_Mul:
			return pn == pn_ia32_Mul_res_high ?
				produces_zero_in_carry : produces_no_flag;

		default:
			return produces_no_flag;
	}

	return pn == pn_ia32_res ? produces_zero_sign : produces_no_flag;
}

/**
 * Replace Cmp(x, 0) by a Test(x, x)
 */
static void peephole_ia32_Cmp(ir_node *const node)
{
	if (get_ia32_op_type(node) != ia32_Normal)
		return;

	ir_node *const right = get_irn_n(node, n_ia32_Cmp_right);
	if (!is_ia32_Immediate(right))
		return;

	ia32_immediate_attr_t const *const imm = get_ia32_immediate_attr_const(right);
	if (imm->symconst != NULL || imm->offset != 0)
		return;

	dbg_info *const dbgi         = get_irn_dbg_info(node);
	ir_node  *const block        = get_nodes_block(node);
	ir_graph *const irg          = get_Block_irg(block);
	ir_node  *const noreg        = ia32_new_NoReg_gp(irg);
	ir_node  *const nomem        = get_irg_no_mem(irg);
	ir_node  *const op           = get_irn_n(node, n_ia32_Cmp_left);
	int       const ins_permuted = get_ia32_attr(node)->data.ins_permuted;

	ir_mode *const ls_mode = get_ia32_ls_mode(node);
	ir_node *const test    = get_mode_size_bits(ls_mode) == 8
		? new_bd_ia32_Test_8bit(dbgi, block, noreg, noreg, nomem, op, op, ins_permuted)
		: new_bd_ia32_Test     (dbgi, block, noreg, noreg, nomem, op, op, ins_permuted);
	set_ia32_ls_mode(test, ls_mode);

	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Cmp_eflags);
	arch_set_irn_register_out(test, pn_ia32_Test_eflags, reg);

	foreach_out_edge_safe(node, edge) {
		ir_node *const user = get_edge_src_irn(edge);

		if (is_Proj(user))
			exchange(user, test);
	}

	sched_add_before(node, test);
	copy_mark(node, test);
	be_peephole_exchange(node, test);
}

/**
 * Peephole optimization for Test instructions.
 * - Remove the Test, if an appropriate flag was produced which is still live
 * - Change a Test(x, c) to 8Bit, if 0 <= c < 256 (3 byte shorter opcode)
 */
static void peephole_ia32_Test(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_ia32_Test_left);
	ir_node *right = get_irn_n(node, n_ia32_Test_right);

	if (left == right) { /* we need a test for 0 */
		ir_node         *block = get_nodes_block(node);
		int              pn    = pn_ia32_res;
		ir_node         *op    = left;
		ir_node         *flags_proj;
		ir_mode         *flags_mode;
		ir_mode         *op_mode;
		ir_node         *schedpoint;
		produces_flag_t  produced;

		if (get_nodes_block(left) != block)
			return;

		if (is_Proj(op)) {
			pn = get_Proj_proj(op);
			op = get_Proj_pred(op);
		}

		/* walk schedule up and abort when we find left or some other node
		 * destroys the flags */
		schedpoint = node;
		for (;;) {
			schedpoint = sched_prev(schedpoint);
			if (schedpoint == op)
				break;
			if (arch_irn_is(schedpoint, modify_flags))
				return;
			if (schedpoint == block)
				panic("couldn't find left");
		}

		produced = check_produces_zero_sign(op, pn);
		if (produced == produces_no_flag)
			return;

		/* make sure users only look at the sign/zero flag */
		foreach_out_edge(node, edge) {
			ir_node              *user = get_edge_src_irn(edge);
			ia32_condition_code_t cc  = get_ia32_condcode(user);

			if (cc == ia32_cc_equal || cc == ia32_cc_not_equal)
				continue;
			if (produced == produces_zero_sign
				&& (cc == ia32_cc_sign || cc == ia32_cc_not_sign)) {
				continue;
			}
			return;
		}

		op_mode = get_ia32_ls_mode(op);
		if (op_mode == NULL)
			op_mode = get_irn_mode(op);

		/* Make sure we operate on the same bit size */
		if (get_mode_size_bits(op_mode) != get_mode_size_bits(get_ia32_ls_mode(node)))
			return;

		if (produced == produces_zero_in_carry) {
			/* patch users to look at the carry instead of the zero flag */
			foreach_out_edge(node, edge) {
				ir_node              *user = get_edge_src_irn(edge);
				ia32_condition_code_t cc   = get_ia32_condcode(user);

				switch (cc) {
				case ia32_cc_equal:     cc = ia32_cc_above_equal; break;
				case ia32_cc_not_equal: cc = ia32_cc_below;       break;
				default: panic("unexpected pn");
				}
				set_ia32_condcode(user, cc);
			}
		}

		if (get_irn_mode(op) != mode_T) {
			set_irn_mode(op, mode_T);

			/* If there are other users, reroute them to result proj */
			if (get_irn_n_edges(op) != 2) {
				ir_node *res = new_r_Proj(op, mode_Iu, pn_ia32_res);
				edges_reroute_except(op, res, res);
			}
		} else {
			if (get_irn_n_edges(left) == 2)
				kill_node(left);
		}

		flags_mode = ia32_reg_classes[CLASS_ia32_flags].mode;
		flags_proj = new_r_Proj(op, flags_mode, pn_ia32_flags);
		arch_set_irn_register(flags_proj, &ia32_registers[REG_EFLAGS]);

		assert(get_irn_mode(node) != mode_T);

		be_peephole_exchange(node, flags_proj);
	} else if (is_ia32_Immediate(right)) {
		ia32_immediate_attr_t const *const imm = get_ia32_immediate_attr_const(right);
		unsigned                           offset;

		/* A test with a symconst is rather strange, but better safe than sorry */
		if (imm->symconst != NULL)
			return;

		offset = imm->offset;
		if (get_ia32_op_type(node) == ia32_AddrModeS) {
			ia32_attr_t *const attr = get_ia32_attr(node);
			ir_graph    *const irg  = get_irn_irg(node);

			if ((offset & 0xFFFFFF00) == 0) {
				/* attr->am_offs += 0; */
			} else if ((offset & 0xFFFF00FF) == 0) {
				ir_node *imm_node = ia32_create_Immediate(irg, NULL, 0, offset >>  8);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_offs += 1;
			} else if ((offset & 0xFF00FFFF) == 0) {
				ir_node *imm_node = ia32_create_Immediate(irg, NULL, 0, offset >> 16);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_offs += 2;
			} else if ((offset & 0x00FFFFFF) == 0) {
				ir_node *imm_node = ia32_create_Immediate(irg, NULL, 0, offset >> 24);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_offs += 3;
			} else {
				return;
			}
		} else if (offset < 256) {
			arch_register_t const* const reg = arch_get_irn_register(left);

			if (reg != &ia32_registers[REG_EAX] &&
					reg != &ia32_registers[REG_EBX] &&
					reg != &ia32_registers[REG_ECX] &&
					reg != &ia32_registers[REG_EDX]) {
				return;
			}
		} else {
			return;
		}

		/* Technically we should build a Test8Bit because of the register
		 * constraints, but nobody changes registers at this point anymore. */
		set_ia32_ls_mode(node, mode_Bu);
	}
}

/**
 * AMD Athlon works faster when RET is not destination of
 * conditional jump or directly preceded by other jump instruction.
 * Can be avoided by placing a Rep prefix before the return.
 */
static void peephole_ia32_Return(ir_node *node)
{
	if (!ia32_cg_config.use_pad_return)
		return;

	/* check if this return is the first on the block */
	sched_foreach_reverse_before(node, irn) {
		if (is_Phi(irn) || be_is_Start(irn))
			continue;
		/* arg, IncSP 0 nodes might occur, ignore these */
		if (be_is_IncSP(irn) && be_get_IncSP_offset(irn) == 0)
			continue;
		return;
	}

	/* ensure, that the 3 byte return is generated */
	be_Return_set_emit_pop(node, 1);
}

/* only optimize up to 48 stores behind IncSPs */
#define MAXPUSH_OPTIMIZE    48

/**
 * Tries to create Push's from IncSP, Store combinations.
 * The Stores are replaced by Push's, the IncSP is modified
 * (possibly into IncSP 0, but not removed).
 */
static void peephole_IncSP_Store_to_push(ir_node *irn)
{
	int       i;
	int       maxslot;
	ir_node  *stores[MAXPUSH_OPTIMIZE];
	ir_node  *block;
	ir_graph *irg;
	ir_node  *curr_sp;
	ir_mode  *spmode;
	ir_node  *first_push = NULL;

	memset(stores, 0, sizeof(stores));

	int inc_ofs = be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	/*
	 * We first walk the schedule after the IncSP node as long as we find
	 * suitable Stores that could be transformed to a Push.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	maxslot = -1;
	sched_foreach_after(irn, node) {
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
		if (!is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
			break;

		offset = get_ia32_am_offs_int(node);
		/* we should NEVER access uninitialized stack BELOW the current SP */
		assert(offset >= 0);

		/* storing at half-slots is bad */
		if ((offset & 3) != 0)
			break;

		if (inc_ofs - 4 < offset || offset >= MAXPUSH_OPTIMIZE * 4)
			continue;
		storeslot = offset >> 2;

		/* storing into the same slot twice is bad (and shouldn't happen...) */
		if (stores[storeslot] != NULL)
			break;

		stores[storeslot] = node;
		if (storeslot > maxslot)
			maxslot = storeslot;
	}

	curr_sp = irn;

	for (i = -1; i < maxslot; ++i) {
		if (stores[i + 1] == NULL)
			break;
	}

	/* walk through the Stores and create Pushs for them */
	block  = get_nodes_block(irn);
	spmode = get_irn_mode(irn);
	irg    = get_irn_irg(irn);
	for (; i >= 0; --i) {
		const arch_register_t *spreg;
		ir_node *push;
		ir_node *val, *mem, *mem_proj;
		ir_node *store = stores[i];
		ir_node *noreg = ia32_new_NoReg_gp(irg);

		val = get_irn_n(store, n_ia32_unary_op);
		mem = get_irn_n(store, n_ia32_mem);
		spreg = arch_get_irn_register(curr_sp);

		push = new_bd_ia32_Push(get_irn_dbg_info(store), block, noreg, noreg,
		                        mem, val, curr_sp);
		copy_mark(store, push);

		if (first_push == NULL)
			first_push = push;

		sched_add_after(skip_Proj(curr_sp), push);

		/* create stackpointer Proj */
		curr_sp = new_r_Proj(push, spmode, pn_ia32_Push_stack);
		arch_set_irn_register(curr_sp, spreg);

		/* create memory Proj */
		mem_proj = new_r_Proj(push, mode_M, pn_ia32_Push_M);

		/* rewire Store Projs */
		foreach_out_edge_safe(store, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			if (!is_Proj(proj))
				continue;
			switch (get_Proj_proj(proj)) {
			case pn_ia32_Store_M:
				exchange(proj, mem_proj);
				break;
			default:
				panic("unexpected Proj on Store->IncSp");
			}
		}

		/* use the memproj now */
		be_peephole_exchange(store, push);

		inc_ofs -= 4;
	}

	foreach_out_edge_safe(irn, edge) {
		ir_node *const src = get_edge_src_irn(edge);
		int      const pos = get_edge_src_pos(edge);

		if (src == first_push)
			continue;

		set_irn_n(src, pos, curr_sp);
	}

	be_set_IncSP_offset(irn, inc_ofs);
}

/**
 * Tries to create Pops from Load, IncSP combinations.
 * The Loads are replaced by Pops, the IncSP is modified
 * (possibly into IncSP 0, but not removed).
 */
static void peephole_Load_IncSP_to_pop(ir_node *irn)
{
	const arch_register_t *esp = &ia32_registers[REG_ESP];
	int      i, maxslot, ofs;
	ir_node  *loads[MAXPUSH_OPTIMIZE];
	unsigned regmask = 0;
	unsigned copymask = ~0;

	memset(loads, 0, sizeof(loads));

	int inc_ofs = -be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	/*
	 * We first walk the schedule before the IncSP node as long as we find
	 * suitable Loads that could be transformed to a Pop.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	maxslot = -1;
	ir_node *pred_sp = be_get_IncSP_pred(irn);
	sched_foreach_reverse_before(irn, node) {
		int offset;
		int loadslot;
		const arch_register_t *sreg, *dreg;

		/* it has to be a Load */
		if (!is_ia32_Load(node)) {
			if (be_is_Copy(node)) {
				dreg = arch_get_irn_register(node);
				if (dreg->reg_class != &ia32_reg_classes[CLASS_ia32_gp]) {
					/* not a GP copy, ignore */
					continue;
				}
				sreg = arch_get_irn_register(be_get_Copy_op(node));
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

		/* it has to use our predecessor sp value */
		if (get_irn_n(node, n_ia32_base) != pred_sp) {
			/* it would be ok if this load does not use a Pop result,
			 * but we do not check this */
			break;
		}

		/* should have NO index */
		if (!is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
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

		dreg = arch_get_irn_register_out(node, pn_ia32_Load_res);
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
	ir_node *const block = get_nodes_block(irn);
	if (inc_ofs > 0) {
		pred_sp = be_new_IncSP(esp, block, pred_sp, -inc_ofs, be_get_IncSP_align(irn));
		sched_add_before(irn, pred_sp);
	}

	/* walk through the Loads and create Pops for them */
	for (++i; i <= maxslot; ++i) {
		ir_node *load = loads[i];
		ir_node *mem, *pop;
		const arch_register_t *reg;

		mem = get_irn_n(load, n_ia32_mem);
		reg = arch_get_irn_register_out(load, pn_ia32_Load_res);

		pop = new_bd_ia32_Pop(get_irn_dbg_info(load), block, mem, pred_sp);
		arch_set_irn_register_out(pop, pn_ia32_Load_res, reg);

		copy_mark(load, pop);

		/* create stackpointer Proj */
		pred_sp = new_r_Proj(pop, mode_Iu, pn_ia32_Pop_stack);
		arch_set_irn_register(pred_sp, esp);

		sched_add_before(irn, pop);

		/* rewire now */
		foreach_out_edge_safe(load, edge) {
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
static const arch_register_t *get_free_gp_reg(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	int i;

	for (i = 0; i < N_ia32_gp_REGS; ++i) {
		const arch_register_t *reg = &ia32_reg_classes[CLASS_ia32_gp].regs[i];
		if (!rbitset_is_set(birg->allocatable_regs, reg->global_index))
			continue;

		if (be_peephole_get_value(reg->global_index) == NULL)
			return reg;
	}

	return NULL;
}

/**
 * Creates a Pop instruction before the given schedule point.
 *
 * @param dbgi        debug info
 * @param block       the block
 * @param stack       the previous stack value
 * @param schedpoint  the new node is added before this node
 * @param reg         the register to pop
 *
 * @return the new stack value
 */
static ir_node *create_pop(dbg_info *dbgi, ir_node *block,
                           ir_node *stack, ir_node *schedpoint,
                           const arch_register_t *reg)
{
	const arch_register_t *esp = &ia32_registers[REG_ESP];
	ir_graph *irg = get_irn_irg(block);
	ir_node *pop;
	ir_node *keep;
	ir_node *val;
	ir_node *in[1];

	pop   = new_bd_ia32_Pop(dbgi, block, get_irg_no_mem(irg), stack);

	stack = new_r_Proj(pop, mode_Iu, pn_ia32_Pop_stack);
	arch_set_irn_register(stack, esp);
	val   = new_r_Proj(pop, mode_Iu, pn_ia32_Pop_res);
	arch_set_irn_register(val, reg);

	sched_add_before(schedpoint, pop);

	in[0] = val;
	keep  = be_new_Keep(block, 1, in);
	sched_add_before(schedpoint, keep);

	return stack;
}

/**
 * Optimize an IncSp by replacing it with Push/Pop.
 */
static void peephole_be_IncSP(ir_node *node)
{
	const arch_register_t *esp = &ia32_registers[REG_ESP];
	const arch_register_t *reg;
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

	if (arch_get_irn_register(node) != esp)
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
		reg = get_free_gp_reg(get_irn_irg(node));
		if (reg == NULL)
			return;

		dbgi  = get_irn_dbg_info(node);
		block = get_nodes_block(node);
		stack = be_get_IncSP_pred(node);

		stack = create_pop(dbgi, block, stack, node, reg);

		if (offset == -8) {
			stack = create_pop(dbgi, block, stack, node, reg);
		}
	} else {
		dbgi  = get_irn_dbg_info(node);
		block = get_nodes_block(node);
		stack = be_get_IncSP_pred(node);
		stack = new_bd_ia32_PushEax(dbgi, block, stack);
		arch_set_irn_register(stack, esp);
		sched_add_before(node, stack);

		if (offset == +8) {
			stack = new_bd_ia32_PushEax(dbgi, block, stack);
			arch_set_irn_register(stack, esp);
			sched_add_before(node, stack);
		}
	}

	be_peephole_exchange(node, stack);
}

/**
 * Peephole optimization for ia32_Const's
 */
static void peephole_ia32_Const(ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);
	const arch_register_t       *reg;
	ir_node                     *block;
	dbg_info                    *dbgi;
	ir_node                     *xorn;

	/* try to transform a mov 0, reg to xor reg reg */
	if (attr->offset != 0 || attr->symconst != NULL)
		return;
	if (ia32_cg_config.use_mov_0)
		return;
	/* xor destroys the flags, so no-one must be using them */
	if (be_peephole_get_value(REG_EFLAGS) != NULL)
		return;

	reg = arch_get_irn_register(node);
	assert(be_peephole_get_reg_value(reg) == NULL);

	/* create xor(produceval, produceval) */
	block = get_nodes_block(node);
	dbgi  = get_irn_dbg_info(node);
	xorn  = new_bd_ia32_Xor0(dbgi, block);
	arch_set_irn_register(xorn, reg);

	sched_add_before(node, xorn);

	copy_mark(node, xorn);
	be_peephole_exchange(node, xorn);
}

static inline int is_noreg(const ir_node *node)
{
	return is_ia32_NoReg_GP(node);
}

ir_node *ia32_immediate_from_long(long val)
{
	ir_graph *irg         = current_ir_graph;
	ir_node  *start_block = get_irg_start_block(irg);
	ir_node  *immediate
		= new_bd_ia32_Immediate(NULL, start_block, NULL, 0, 0, val);
	arch_set_irn_register(immediate, &ia32_registers[REG_GP_NOREG]);

	return immediate;
}

static ir_node *create_immediate_from_am(const ir_node *node)
{
	ir_node   *block   = get_nodes_block(node);
	int        offset  = get_ia32_am_offs_int(node);
	int        sc_sign = is_ia32_am_sc_sign(node);
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	int        sc_no_pic_adjust = attr->data.am_sc_no_pic_adjust;
	ir_entity *entity  = get_ia32_am_sc(node);
	ir_node   *res;

	res = new_bd_ia32_Immediate(NULL, block, entity, sc_sign, sc_no_pic_adjust,
	                            offset);
	arch_set_irn_register(res, &ia32_registers[REG_GP_NOREG]);
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

	assert(is_ia32_Lea(node));

	/* we can only do this if it is allowed to clobber the flags */
	if (be_peephole_get_value(REG_EFLAGS) != NULL)
		return;

	base  = get_irn_n(node, n_ia32_Lea_base);
	index = get_irn_n(node, n_ia32_Lea_index);

	if (is_noreg(base)) {
		base     = NULL;
		base_reg = NULL;
	} else {
		base_reg = arch_get_irn_register(base);
	}
	if (is_noreg(index)) {
		index     = NULL;
		index_reg = NULL;
	} else {
		index_reg = arch_get_irn_register(index);
	}

	if (base == NULL && index == NULL) {
		/* we shouldn't construct these in the first place... */
#ifdef DEBUG_libfirm
		ir_fprintf(stderr, "Optimisation warning: found immediate only lea\n");
#endif
		return;
	}

	out_reg = arch_get_irn_register(node);
	scale   = get_ia32_am_scale(node);
	assert(!is_ia32_need_stackent(node) || get_ia32_frame_ent(node) != NULL);
	/* check if we have immediates values (frame entities should already be
	 * expressed in the offsets) */
	if (get_ia32_am_offs_int(node) != 0 || get_ia32_am_sc(node) != NULL) {
		has_immediates = 1;
	} else {
		has_immediates = 0;
	}

	/* we can transform leas where the out register is the same as either the
	 * base or index register back to an Add or Shl */
	if (out_reg == base_reg) {
		if (index == NULL) {
#ifdef DEBUG_libfirm
			if (!has_immediates) {
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
			}
#endif
			op1 = base;
			goto make_add_immediate;
		}
		if (scale == 0 && !has_immediates) {
			op1 = base;
			op2 = index;
			goto make_add;
		}
		/* can't create an add */
		return;
	} else if (out_reg == index_reg) {
		if (base == NULL) {
			if (has_immediates && scale == 0) {
				op1 = index;
				goto make_add_immediate;
			} else if (!has_immediates && scale > 0) {
				op1 = index;
				op2 = ia32_immediate_from_long(scale);
				goto make_shl;
			} else if (!has_immediates) {
#ifdef DEBUG_libfirm
				ir_fprintf(stderr, "Optimisation warning: found lea which is "
				           "just a copy\n");
#endif
			}
		} else if (scale == 0 && !has_immediates) {
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
	if (ia32_cg_config.use_incdec) {
		if (is_am_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_bd_ia32_Inc(dbgi, block, op1);
			arch_set_irn_register(res, out_reg);
			goto exchange;
		}
		if (is_am_minus_one(node)) {
			dbgi  = get_irn_dbg_info(node);
			block = get_nodes_block(node);
			res   = new_bd_ia32_Dec(dbgi, block, op1);
			arch_set_irn_register(res, out_reg);
			goto exchange;
		}
	}
	op2 = create_immediate_from_am(node);

make_add:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *noreg = ia32_new_NoReg_gp(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	res   = new_bd_ia32_Add(dbgi, block, noreg, noreg, nomem, op1, op2);
	arch_set_irn_register(res, out_reg);
	set_ia32_commutative(res);
	goto exchange;

make_shl:
	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	res   = new_bd_ia32_Shl(dbgi, block, op1, op2);
	arch_set_irn_register(res, out_reg);
	goto exchange;

exchange:
	SET_IA32_ORIG_NODE(res, node);

	/* add new ADD/SHL to schedule */
	DBG_OPT_LEA2ADD(node, res);

	/* exchange the Add and the LEA */
	sched_add_before(node, res);
	copy_mark(node, res);
	be_peephole_exchange(node, res);
}

/**
 * Split a Imul mem, imm into a Load mem and Imul reg, imm if possible.
 */
static void peephole_ia32_Imul_split(ir_node *imul)
{
	const ir_node         *right = get_irn_n(imul, n_ia32_IMul_right);
	const arch_register_t *reg;
	ir_node               *res;

	if (!is_ia32_Immediate(right) || get_ia32_op_type(imul) != ia32_AddrModeS) {
		/* no memory, imm form ignore */
		return;
	}
	/* we need a free register */
	reg = get_free_gp_reg(get_irn_irg(imul));
	if (reg == NULL)
		return;

	/* fine, we can rebuild it */
	res = ia32_turn_back_am(imul);
	arch_set_irn_register(res, reg);
}

/**
 * Replace xorps r,r and xorpd r,r by pxor r,r
 */
static void peephole_ia32_xZero(ir_node *xorn)
{
	set_irn_op(xorn, op_ia32_xPzero);
}

/**
 * Replace 16bit sign extension from ax to eax by shorter cwtl
 */
static void peephole_ia32_Conv_I2I(ir_node *node)
{
	const arch_register_t *eax          = &ia32_registers[REG_EAX];
	ir_mode               *smaller_mode = get_ia32_ls_mode(node);
	ir_node               *val          = get_irn_n(node, n_ia32_Conv_I2I_val);
	dbg_info              *dbgi;
	ir_node               *block;
	ir_node               *cwtl;

	if (get_mode_size_bits(smaller_mode) != 16 ||
			!mode_is_signed(smaller_mode)          ||
			eax != arch_get_irn_register(val)      ||
			eax != arch_get_irn_register_out(node, pn_ia32_Conv_I2I_res))
		return;

	dbgi  = get_irn_dbg_info(node);
	block = get_nodes_block(node);
	cwtl  = new_bd_ia32_Cwtl(dbgi, block, val);
	arch_set_irn_register(cwtl, eax);
	sched_add_before(node, cwtl);
	be_peephole_exchange(node, cwtl);
}

/**
 * Register a peephole optimization function.
 */
static void register_peephole_optimization(ir_op *op, peephole_opt_func func)
{
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func)func;
}

/* Perform peephole-optimizations. */
void ia32_peephole_optimization(ir_graph *irg)
{
	/* we currently do it in 2 passes because:
	 *    Lea -> Add could be usefull as flag producer for Test later
	 */

	/* pass 1 */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_ia32_Cmp,      peephole_ia32_Cmp);
	register_peephole_optimization(op_ia32_Lea,      peephole_ia32_Lea);
	if (ia32_cg_config.use_short_sex_eax)
		register_peephole_optimization(op_ia32_Conv_I2I, peephole_ia32_Conv_I2I);
	if (ia32_cg_config.use_pxor)
		register_peephole_optimization(op_ia32_xZero, peephole_ia32_xZero);
	if (! ia32_cg_config.use_imul_mem_imm32)
		register_peephole_optimization(op_ia32_IMul, peephole_ia32_Imul_split);
	be_peephole_opt(irg);

	/* pass 2 */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_ia32_Const, peephole_ia32_Const);
	register_peephole_optimization(op_be_IncSP,   peephole_be_IncSP);
	register_peephole_optimization(op_ia32_Test,  peephole_ia32_Test);
	register_peephole_optimization(op_be_Return,  peephole_ia32_Return);
	be_peephole_opt(irg);
}

/**
 * Removes node from schedule if it is not used anymore. If irn is a mode_T node
 * all its Projs are removed as well.
 * @param irn  The irn to be removed from schedule
 */
static inline void try_kill(ir_node *node)
{
	if (get_irn_mode(node) == mode_T) {
		foreach_out_edge_safe(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			try_kill(proj);
		}
	}

	if (get_irn_n_edges(node) != 0)
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

	if (!is_ia32_Store(node))
		return;

	pred_proj = get_irn_n(node, n_ia32_Store_val);
	if (is_Proj(pred_proj)) {
		pred = get_Proj_pred(pred_proj);
	} else {
		pred = pred_proj;
	}
	if (!is_ia32_Conv_I2I(pred))
		return;
	if (get_ia32_op_type(pred) != ia32_Normal)
		return;

	/* the store only stores the lower bits, so we only need the conv
	 * it it shrinks the mode */
	conv_mode  = get_ia32_ls_mode(pred);
	store_mode = get_ia32_ls_mode(node);
	if (get_mode_size_bits(conv_mode) < get_mode_size_bits(store_mode))
		return;

	ir_fprintf(stderr, "Optimisation warning: unoptimized ia32 Store(Conv) (%+F, %+F)\n", node, pred);
	set_irn_n(node, n_ia32_Store_val, get_irn_n(pred, n_ia32_Conv_I2I_val));
	if (get_irn_n_edges(pred_proj) == 0) {
		kill_node(pred_proj);
		if (pred != pred_proj)
			kill_node(pred);
	}
}

static void optimize_load_conv(ir_node *node)
{
	ir_node *pred, *predpred;
	ir_mode *load_mode;
	ir_mode *conv_mode;

	if (!is_ia32_Conv_I2I(node))
		return;

	pred = get_irn_n(node, n_ia32_Conv_I2I_val);
	if (!is_Proj(pred))
		return;

	predpred = get_Proj_pred(pred);
	if (!is_ia32_Load(predpred))
		return;

	/* the load is sign extending the upper bits, so we only need the conv
	 * if it shrinks the mode */
	load_mode = get_ia32_ls_mode(predpred);
	conv_mode = get_ia32_ls_mode(node);
	if (get_mode_size_bits(conv_mode) < get_mode_size_bits(load_mode))
		return;

	if (get_mode_sign(conv_mode) != get_mode_sign(load_mode)) {
		/* change the load if it has only 1 user */
		if (get_irn_n_edges(pred) == 1) {
			ir_mode *newmode;
			if (get_mode_sign(conv_mode)) {
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
	ir_fprintf(stderr, "Optimisation warning: unoptimized ia32 Conv(Load) (%+F, %+F)\n", node, predpred);
	exchange(node, pred);
}

static void optimize_conv_conv(ir_node *node)
{
	ir_node *pred_proj, *pred, *result_conv;
	ir_mode *pred_mode, *conv_mode;
	int      conv_mode_bits;
	int      pred_mode_bits;

	if (!is_ia32_Conv_I2I(node))
		return;

	pred_proj = get_irn_n(node, n_ia32_Conv_I2I_val);
	if (is_Proj(pred_proj))
		pred = get_Proj_pred(pred_proj);
	else
		pred = pred_proj;

	if (!is_ia32_Conv_I2I(pred))
		return;

	/* we know that after a conv, the upper bits are sign extended
	 * so we only need the 2nd conv if it shrinks the mode */
	conv_mode      = get_ia32_ls_mode(node);
	conv_mode_bits = get_mode_size_bits(conv_mode);
	pred_mode      = get_ia32_ls_mode(pred);
	pred_mode_bits = get_mode_size_bits(pred_mode);

	if (conv_mode_bits == pred_mode_bits
			&& get_mode_sign(conv_mode) == get_mode_sign(pred_mode)) {
		result_conv = pred_proj;
	} else if (conv_mode_bits <= pred_mode_bits) {
		/* if 2nd conv is smaller then first conv, then we can always take the
		 * 2nd conv */
		if (get_irn_n_edges(pred_proj) == 1) {
			result_conv = pred_proj;
			set_ia32_ls_mode(pred, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				const arch_register_req_t **reqs = arch_get_irn_register_reqs_in(node);
				set_irn_op(pred, op_ia32_Conv_I2I);
				arch_set_irn_register_reqs_in(pred, reqs);
			}
		} else {
			/* we don't want to end up with 2 loads, so we better do nothing */
			if (get_irn_mode(pred) == mode_T) {
				return;
			}

			result_conv = exact_copy(pred);
			set_ia32_ls_mode(result_conv, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				const arch_register_req_t **reqs = arch_get_irn_register_reqs_in(node);
				set_irn_op(result_conv, op_ia32_Conv_I2I);
				arch_set_irn_register_reqs_in(result_conv, reqs);
			}
		}
	} else {
		/* if both convs have the same sign, then we can take the smaller one */
		if (get_mode_sign(conv_mode) == get_mode_sign(pred_mode)) {
			result_conv = pred_proj;
		} else {
			/* no optimization possible if smaller conv is sign-extend */
			if (mode_is_signed(pred_mode)) {
				return;
			}
			/* we can take the smaller conv if it is unsigned */
			result_conv = pred_proj;
		}
	}

	ir_fprintf(stderr, "Optimisation warning: unoptimized ia32 Conv(Conv) (%+F, %+F)\n", node, pred);
	/* Some user (like Phis) won't be happy if we change the mode. */
	set_irn_mode(result_conv, get_irn_mode(node));

	/* kill the conv */
	exchange(node, result_conv);

	if (get_irn_n_edges(pred_proj) == 0) {
		kill_node(pred_proj);
		if (pred != pred_proj)
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
void ia32_optimize_graph(ir_graph *irg)
{
	irg_walk_blkwise_graph(irg, NULL, optimize_node, NULL);
}

void ia32_init_optimize(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.optimize");
}
