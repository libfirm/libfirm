/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements several optimizations for IA32.
 * @author      Matthias Braun, Christian Wuerdig
 */
#include "debug.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "ircons.h"
#include "irtools.h"
#include "firm_types.h"
#include "iredges_t.h"
#include "tv.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "heights.h"
#include "irprintf.h"
#include "irdump.h"
#include "panic.h"

#include "be_t.h"
#include "bediagnostic.h"
#include "benode.h"
#include "besched.h"
#include "bepeephole.h"

#include "ia32_new_nodes.h"
#include "ia32_optimize.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_transform.h"
#include "ia32_architecture.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool is_hl_register(arch_register_t const *const reg)
{
	return
		reg == &ia32_registers[REG_EAX] ||
		reg == &ia32_registers[REG_EBX] ||
		reg == &ia32_registers[REG_ECX] ||
		reg == &ia32_registers[REG_EDX];
}

static void copy_mark(const ir_node *old, ir_node *newn)
{
	if (is_ia32_is_reload(old))
		set_ia32_is_reload(newn);
	if (is_ia32_is_spill(old))
		set_ia32_is_spill(newn);
	if (is_ia32_is_remat(old))
		set_ia32_is_remat(newn);
}

static void replace(ir_node *const old, ir_node *const newn)
{
	copy_mark(old, newn);
	be_peephole_replace(old, newn);
}

static ia32_immediate_attr_t const *get_op_imm_no_ent(ir_node *const node, unsigned const n)
{
	ir_node *const op = get_irn_n(node, n);
	if (!is_ia32_Immediate(op))
		return NULL;

	ia32_immediate_attr_t const *const imm = get_ia32_immediate_attr_const(op);
	if (imm->imm.entity)
		return NULL;

	return imm;
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
static produces_flag_t check_produces_zero_sign(ir_node *node, unsigned pn)
{
	if (!is_ia32_irn(node))
		return produces_no_flag;

	switch (get_ia32_irn_opcode(node)) {
		case iro_ia32_Adc:
		case iro_ia32_Add:
		case iro_ia32_And:
		case iro_ia32_Dec:
		case iro_ia32_Inc:
		case iro_ia32_Neg:
		case iro_ia32_Or:
		case iro_ia32_Popcnt:
		case iro_ia32_Sbb:
		case iro_ia32_Sub:
		case iro_ia32_Xor:
			break;

		{
			unsigned count_pos;
		case iro_ia32_ShlD: count_pos = n_ia32_ShlD_count; goto check_shift_amount;
		case iro_ia32_ShrD: count_pos = n_ia32_ShrD_count; goto check_shift_amount;
		case iro_ia32_Sar:  count_pos = n_ia32_Sar_count;  goto check_shift_amount;
		case iro_ia32_Shl:  count_pos = n_ia32_Shl_count;  goto check_shift_amount;
		case iro_ia32_Shr:  count_pos = n_ia32_Shr_count;  goto check_shift_amount;
check_shift_amount:;
			/* when shift count is zero the flags are not affected, so we can only
			 * do this for constants != 0 */
			ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, count_pos);
			if (!imm || (imm->imm.offset & 0x1f) == 0)
				return produces_no_flag;
			break;
		}

		case iro_ia32_Mul:
			return pn == pn_ia32_Mul_res_high ?
				produces_zero_in_carry : produces_no_flag;

		default:
			return produces_no_flag;
	}

	return pn == pn_ia32_res ? produces_zero_sign : produces_no_flag;
}

static bool is_op_32bit(ir_node *const node)
{
	ir_mode *const ls_mode = get_ia32_ls_mode(node);
	return !ls_mode || get_mode_size_bits(ls_mode) == 32;
}

static bool is_res_used(ir_node *const node, unsigned const num)
{
	return get_irn_mode(node) == mode_T && get_Proj_for_pn(node, num);
}

typedef ir_node *cons_binop(dbg_info*, ir_node*, ir_node*, ir_node*, ir_node*, ir_node*, ir_node*);

static void make_binop(ir_node *const node, cons_binop *const cons, ir_node *const left, ir_node *const right, ir_mode *const mode, arch_register_t const *const reg)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node  *const noreg = get_irn_n(node, n_ia32_base);
	ir_node  *const nomem = get_irn_n(node, n_ia32_mem);
	ir_node  *const binop = cons(dbgi, block, noreg, noreg, nomem, left, right);
	set_ia32_ls_mode(binop, mode);
	arch_set_irn_register_out(binop, pn_ia32_res, reg);
	replace(node, binop);
}

static void make_binop_imm(ir_node *const node, cons_binop *const cons, unsigned const val, ir_mode *const mode, arch_register_t const *const reg)
{
	ir_node  *const left  = get_irn_n(node, n_ia32_binary_left);
	ir_graph *const irg   = get_irn_irg(node);
	ir_node  *const right = ia32_create_Immediate(irg, val);
	make_binop(node, cons, left, right, mode, reg);
}

static void make_xor(ir_node *const and, cons_binop *const cons, ir_mode *const mode, arch_register_t const *const reg)
{
	ir_node *const left = get_irn_n(and, n_ia32_And_left);
	make_binop(and, cons, left, left, mode, reg);
}

/**
 * Use shorter instructions:
 * r & 0xFFFFFF00 -> xorb rl, rl (6 bytes -> 2 bytes)
 * r & 0xFFFF00FF -> xorb rh, rh (6 bytes -> 2 bytes)
 * r & 0xFFFF0000 -> xorw rx, rx (6 bytes -> 3 bytes)
 */
static void peephole_ia32_And(ir_node *const node)
{
	if (!is_op_32bit(node))
		return;

	ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, n_ia32_And_right);
	if (!imm)
		return;

	/* Perform the replacement only, if the flags are unused.
	 * Xor sets the flags differently than And. */
	if (is_res_used(node, pn_ia32_And_flags))
		return;

	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_And_res);
	uint32_t               const val = imm->imm.offset;
	if (val == 0xFFFF0000) {
		make_xor(node, &new_bd_ia32_Xor, mode_Hu, reg);
	} else if (is_hl_register(reg)) {
		if (val == 0xFFFFFF00) {
			make_xor(node, &new_bd_ia32_Xor_8bit, mode_Bu, reg);
		} else if (val == 0xFFFF00FF) {
			make_xor(node, &new_bd_ia32_Xor_8bit, ia32_mode_8h, reg);
		} else if ((val & 0xFFFFFF80) == 0xFFFFFF00) {
			make_binop_imm(node, &new_bd_ia32_And_8bit, val & 0xFF, mode_Bu, reg);
		} else if ((val & 0xFFFF00FF) == 0xFFFF00FF) {
			make_binop_imm(node, &new_bd_ia32_And_8bit, val >> 8 & 0xFF, ia32_mode_8h, reg);
		}
	}
}

/**
 * Use shorter instructions:
 * r | 0x000000XX -> orb $0xXX, rl (6[-1] bytes -> 3[-1] bytes)
 * r | 0x0000XX00 -> orb $0xXX, rh (6[-1] bytes -> 3[-1] bytes)
 */
static void peephole_ia32_Or(ir_node *const node)
{
	if (!is_op_32bit(node))
		return;

	ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, n_ia32_Or_right);
	if (!imm)
		return;

	if (is_res_used(node, pn_ia32_Or_flags))
		return;

	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Or_res);
	if (is_hl_register(reg)) {
		uint32_t const val = imm->imm.offset;
		if ((val & 0xFFFFFF80) == 0x00000080) {
			make_binop_imm(node, &new_bd_ia32_Or_8bit, val, mode_Bu, reg);
		} else if ((val & 0xFFFF00FF) == 0) {
			make_binop_imm(node, &new_bd_ia32_Or_8bit, val >> 8, ia32_mode_8h, reg);
		}
	}
}

static void make_not(ir_node *const xor, ir_node *(*const cons)(dbg_info*, ir_node*, ir_node*), ir_mode *const mode, arch_register_t const *const reg)
{
	dbg_info *const dbgi  = get_irn_dbg_info(xor);
	ir_node  *const block = get_nodes_block(xor);
	ir_node  *const left  = get_irn_n(xor, n_ia32_Xor_left);
	ir_node  *const not   = cons(dbgi, block, left);
	set_ia32_ls_mode(not, mode);
	arch_set_irn_register_out(not, pn_ia32_Not_res, reg);
	replace(xor, not);
}

/**
 * Use shorter instructions:
 * r ^ 0x000000FF -> notb rl, rl (6[-1] bytes -> 2 bytes)
 * r ^ 0x0000FF00 -> notb rh, rh (6[-1] bytes -> 2 bytes)
 * r ^ 0x0000FFFF -> notw rx, rx (6[-1] bytes -> 3 bytes)
 *
 * r ^ 0x000000XX -> xorb $0xXX, rl (6[-1] bytes -> 3[-1] bytes)
 * r ^ 0x0000XX00 -> xorb $0xXX, rh (6[-1] bytes -> 3[-1] bytes)
 */
static void peephole_ia32_Xor(ir_node *const node)
{
	if (!is_op_32bit(node))
		return;

	ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, n_ia32_Xor_right);
	if (!imm)
		return;

	/* Perform the replacement only, if the flags are unused.
	 * Not does not set the flags, while Xor does. */
	if (is_res_used(node, pn_ia32_Xor_flags))
		return;

	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Xor_res);
	uint32_t               const val = imm->imm.offset;
	if (val == 0x0000FFFF) {
		make_not(node, &new_bd_ia32_Not, mode_Hu, reg);
	} else if (is_hl_register(reg)) {
		if (val == 0x000000FF) {
			make_not(node, &new_bd_ia32_Not_8bit, mode_Bu, reg);
		} else if (val == 0x0000FF00) {
			make_not(node, &new_bd_ia32_Not_8bit, ia32_mode_8h, reg);
		} else if ((val & 0xFFFFFF80) == 0x00000080) {
			make_binop_imm(node, &new_bd_ia32_Xor_8bit, val, mode_Bu, reg);
		} else if ((val & 0xFFFF00FF) == 0) {
			make_binop_imm(node, &new_bd_ia32_Xor_8bit, val >> 8, ia32_mode_8h, reg);
		}
	}
}

/**
 * Replace Cmp(x, 0) by a Test(x, x)
 */
static void peephole_ia32_Cmp(ir_node *const node)
{
	if (get_ia32_op_type(node) != ia32_Normal)
		return;

	ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, n_ia32_Cmp_right);
	if (!imm || imm->imm.offset != 0)
		return;

	dbg_info *const dbgi         = get_irn_dbg_info(node);
	ir_node  *const block        = get_nodes_block(node);
	ir_graph *const irg          = get_irn_irg(node);
	ir_node  *const noreg        = ia32_new_NoReg_gp(irg);
	ir_node  *const nomem        = get_irg_no_mem(irg);
	ir_node  *const op           = get_irn_n(node, n_ia32_Cmp_left);
	int       const ins_permuted = get_ia32_attr(node)->ins_permuted;

	ir_mode *const ls_mode = get_ia32_ls_mode(node);
	ir_node *const test    = get_mode_size_bits(ls_mode) == 8
		? new_bd_ia32_Test_8bit(dbgi, block, noreg, noreg, nomem, op, op, ins_permuted)
		: new_bd_ia32_Test     (dbgi, block, noreg, noreg, nomem, op, op, ins_permuted);
	set_ia32_ls_mode(test, ls_mode);
	arch_set_irn_register_out(test, pn_ia32_Test_eflags, &ia32_registers[REG_EFLAGS]);

	foreach_out_edge_safe(node, edge) {
		ir_node *const user = get_edge_src_irn(edge);

		if (is_Proj(user))
			exchange(user, test);
	}

	replace(node, test);
}

/**
 * Peephole optimization for Test instructions.
 * - Remove the Test, if an appropriate flag was produced which is still live
 * - Change a Test(x, c) to 8Bit, if 0 <= c < 128 (3 byte shorter opcode)
 */
static void peephole_ia32_Test(ir_node *node)
{
	ir_node *left  = get_irn_n(node, n_ia32_Test_left);
	ir_node *right = get_irn_n(node, n_ia32_Test_right);

	if (left == right) { /* we need a test for 0 */
		ir_node *block = get_nodes_block(node);
		if (get_nodes_block(left) != block)
			return;

		unsigned pn = pn_ia32_res;
		ir_node *op = left;
		if (is_Proj(op)) {
			pn = get_Proj_num(op);
			op = get_Proj_pred(op);
		}

		/* walk schedule up and abort when we find left or some other node
		 * destroys the flags */
		ir_node *schedpoint = node;
		for (;;) {
			schedpoint = sched_prev(schedpoint);
			if (schedpoint == op)
				break;
			if (arch_irn_is(schedpoint, modify_flags))
				return;
			if (schedpoint == block)
				panic("couldn't find left");
		}

		produces_flag_t produced = check_produces_zero_sign(op, pn);
		if (produced == produces_no_flag)
			return;

		/* make sure users only look at the sign/zero flag */
		foreach_out_edge(node, edge) {
			ir_node *user = get_edge_src_irn(edge);
			if (is_ia32_CMovcc(user) || is_ia32_Jcc(user) ||
			    is_ia32_Setcc(user) || is_ia32_SetccMem(user)) {
				x86_condition_code_t  cc  = get_ia32_condcode(user);

				if (cc == x86_cc_equal || cc == x86_cc_not_equal)
					continue;
				if (produced == produces_zero_sign
					&& (cc == x86_cc_sign || cc == x86_cc_not_sign)) {
					continue;
				}
			}
			return;
		}

		ir_mode *op_mode = get_ia32_ls_mode(op);
		if (op_mode == NULL)
			op_mode = get_irn_mode(op);

		/* Make sure we operate on the same bit size */
		if (get_mode_size_bits(op_mode) != get_mode_size_bits(get_ia32_ls_mode(node)))
			return;

		if (produced == produces_zero_in_carry) {
			/* patch users to look at the carry instead of the zero flag */
			foreach_out_edge(node, edge) {
				ir_node              *user = get_edge_src_irn(edge);
				x86_condition_code_t  cc   = get_ia32_condcode(user);

				switch (cc) {
				case x86_cc_equal:     cc = x86_cc_above_equal; break;
				case x86_cc_not_equal: cc = x86_cc_below;       break;
				default: panic("unexpected pn");
				}
				set_ia32_condcode(user, cc);
			}
		}

		if (get_irn_mode(op) != mode_T) {
			/* If there are other users, reroute them to result proj */
			if (get_irn_n_edges(op) != 2) {
				be_peephole_to_tuple(op);
			} else {
				set_irn_mode(op, mode_T);
			}
		} else {
			if (get_irn_n_edges(left) == 2)
				kill_node(left);
		}

		ir_node *const flags_proj = be_new_Proj_reg(op, pn_ia32_flags, &ia32_registers[REG_EFLAGS]);
		be_peephole_exchange(node, flags_proj);
	} else {
		/* A test with an entity is rather strange, but better safe than sorry */
		ia32_immediate_attr_t const *const imm = get_op_imm_no_ent(node, n_ia32_Test_right);
		if (!imm)
			return;

		/*
		 * We have to take care that we end up with the same sign flag:
		 * testl(128, 128) -> SF=0
		 * testb(128, 128) -> SF=1
		 */
		unsigned offset = imm->imm.offset;
		if (get_ia32_op_type(node) == ia32_AddrModeS) {
			ia32_attr_t *const attr = get_ia32_attr(node);
			ir_graph    *const irg  = get_irn_irg(node);

			if ((offset & 0xFFFFFF80) == 0) {
				/* attr->am_imm.offset += 0; */
			} else if ((offset & 0xFFFF80FF) == 0) {
				ir_node *const imm_node = ia32_create_Immediate(irg, offset >>  8);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_imm.offset += 1;
			} else if ((offset & 0xFF80FFFF) == 0) {
				ir_node *const imm_node = ia32_create_Immediate(irg, offset >> 16);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_imm.offset += 2;
			} else if ((offset & 0x00FFFFFF) == 0) {
				ir_node *const imm_node = ia32_create_Immediate(irg, offset >> 24);
				set_irn_n(node, n_ia32_Test_right, imm_node);
				attr->am_imm.offset += 3;
			} else {
				return;
			}
		} else if ((offset & 0xFFFFFF80) == 0) {
			arch_register_t const* const reg = arch_get_irn_register(left);
			if (!is_hl_register(reg))
				return;
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
	sched_foreach_non_phi_reverse_before(node, irn) {
		if (be_is_Start(irn))
			continue;
		/* arg, IncSP 0 nodes might occur, ignore these */
		if (be_is_IncSP(irn) && be_get_IncSP_offset(irn) == 0)
			continue;
		return;
	}

	/* ensure, that the 3 byte return is generated */
	ia32_return_attr_t *attr = get_ia32_return_attr(node);
	attr->emit_pop = true;
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
	int inc_ofs = be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	int      maxslot                  = -1;
	ir_node *stores[MAXPUSH_OPTIMIZE];

	memset(stores, 0, sizeof(stores));

	/*
	 * We first walk the schedule after the IncSP node as long as we find
	 * suitable Stores that could be transformed to a Push.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	sched_foreach_after(irn, node) {
		/* it has to be a Store */
		if (!is_ia32_Store(node))
			break;

		/* it has to use our sp value */
		if (get_irn_n(node, n_ia32_base) != irn)
			continue;
		/* Store has to be attached to NoMem */
		ir_node *mem = get_irn_n(node, n_ia32_mem);
		if (!is_NoMem(mem))
			continue;

		/* unfortunately we can't support the full AMs possible for push at the
		 * moment. TODO: fix this */
		if (!is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
			break;

		int offset = get_ia32_am_offs_int(node);
		/* we should NEVER access uninitialized stack BELOW the current SP */
		assert(offset >= 0);

		/* storing at half-slots is bad */
		if ((offset & 3) != 0)
			break;

		if (inc_ofs - 4 < offset || offset >= MAXPUSH_OPTIMIZE * 4)
			continue;

		int storeslot = offset >> 2;

		/* storing into the same slot twice is bad (and shouldn't happen...) */
		if (stores[storeslot] != NULL)
			break;

		stores[storeslot] = node;
		if (storeslot > maxslot)
			maxslot = storeslot;
	}

	ir_node *curr_sp = irn;
	int      i;
	for (i = -1; i < maxslot; ++i) {
		if (stores[i + 1] == NULL)
			break;
	}

	/* walk through the Stores and create Pushs for them */
	ir_node  *const block      = get_nodes_block(irn);
	ir_graph *const irg        = get_irn_irg(irn);
	ir_node  *const noreg      = ia32_new_NoReg_gp(irg);
	ir_node        *first_push = NULL;
	for (; i >= 0; --i) {
		ir_node  *const store = stores[i];
		dbg_info *const dbgi  = get_irn_dbg_info(store);
		ir_node  *const mem   = get_irn_n(store, n_ia32_mem);
		ir_node  *const val   = get_irn_n(store, n_ia32_unary_op);
		ir_node  *const push  = new_bd_ia32_Push(dbgi, block, noreg, noreg, mem, val, curr_sp, ia32_mode_gp);
		copy_mark(store, push);

		if (first_push == NULL)
			first_push = push;

		sched_add_after(skip_Proj(curr_sp), push);

		/* create stackpointer Proj */
		curr_sp = be_new_Proj_reg(push, pn_ia32_Push_stack, &ia32_registers[REG_ESP]);

		/* use the memproj now */
		be_peephole_exchange(store, push);

		inc_ofs -= 4;
	}

	edges_reroute_except(irn, curr_sp, first_push);
	be_set_IncSP_offset(irn, inc_ofs);
}

/**
 * Tries to create Pops from Load, IncSP combinations.
 * The Loads are replaced by Pops, the IncSP is modified
 * (possibly into IncSP 0, but not removed).
 */
static void peephole_Load_IncSP_to_pop(ir_node *irn)
{

	int inc_ofs = -be_get_IncSP_offset(irn);
	if (inc_ofs < 4)
		return;

	ir_node  *loads[MAXPUSH_OPTIMIZE];
	unsigned  regmask                 = 0;
	unsigned  copymask                = ~0;

	memset(loads, 0, sizeof(loads));

	/*
	 * We first walk the schedule before the IncSP node as long as we find
	 * suitable Loads that could be transformed to a Pop.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	int      maxslot = -1;
	ir_node *pred_sp = be_get_IncSP_pred(irn);
	sched_foreach_reverse_before(irn, node) {
		/* it has to be a Load */
		if (!is_ia32_Load(node)) {
			if (be_is_Copy(node)) {
				const arch_register_t *dreg = arch_get_irn_register(node);
				if (dreg->cls != &ia32_reg_classes[CLASS_ia32_gp]) {
					/* not a GP copy, ignore */
					continue;
				}
				const arch_register_t *sreg = arch_get_irn_register(be_get_Copy_op(node));
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

		int offset = get_ia32_am_offs_int(node);
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
		int loadslot = offset >> 2;

		/* loading from the same slot twice is bad (and shouldn't happen...) */
		if (loads[loadslot] != NULL)
			break;

		const arch_register_t *dreg = arch_get_irn_register_out(node, pn_ia32_Load_res);
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
	int i;
	for (i = maxslot; i >= 0; --i) {
		ir_node *load = loads[i];

		if (load == NULL)
			break;
	}

	int ofs = inc_ofs - (maxslot + 1) * 4;
	inc_ofs = (i + 1) * 4;

	/* create a new IncSP if needed */
	ir_node *const block = get_nodes_block(irn);
	if (inc_ofs > 0) {
		pred_sp = ia32_new_IncSP(block, pred_sp, -inc_ofs, be_get_IncSP_align(irn));
		sched_add_before(irn, pred_sp);
	}

	/* walk through the Loads and create Pops for them */
	for (++i; i <= maxslot; ++i) {
		ir_node               *load = loads[i];
		ir_node               *mem  = get_irn_n(load, n_ia32_mem);
		const arch_register_t *reg  = arch_get_irn_register_out(load, pn_ia32_Load_res);

		ir_node *pop = new_bd_ia32_Pop(get_irn_dbg_info(load), block, mem, pred_sp);
		arch_set_irn_register_out(pop, pn_ia32_Load_res, reg);

		copy_mark(load, pop);

		/* create stackpointer Proj */
		pred_sp = be_new_Proj_reg(pop, pn_ia32_Pop_stack, &ia32_registers[REG_ESP]);

		sched_add_before(irn, pop);
		be_peephole_exchange(load, pop);
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

	for (int i = 0; i < N_ia32_gp_REGS; ++i) {
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
	ir_graph *const irg = get_irn_irg(block);
	ir_node  *const mem = get_irg_no_mem(irg);
	ir_node  *const pop = new_bd_ia32_Pop(dbgi, block, mem, stack);
	sched_add_before(schedpoint, pop);

	ir_node *const val  = be_new_Proj_reg(pop, pn_ia32_Pop_res, reg);
	ir_node *const keep = be_new_Keep_one(val);
	sched_add_before(schedpoint, keep);

	return be_new_Proj_reg(pop, pn_ia32_Pop_stack, &ia32_registers[REG_ESP]);
}

/**
 * Optimize an IncSp by replacing it with Push/Pop.
 */
static void peephole_be_IncSP(ir_node *node)
{
	/* first optimize incsp->incsp combinations */
	if (be_peephole_IncSP_IncSP(node))
		return;

	/* transform IncSP->Store combinations to Push where possible */
	peephole_IncSP_Store_to_push(node);

	/* transform Load->IncSP combinations to Pop where possible */
	peephole_Load_IncSP_to_pop(node);

	/* replace IncSP -4 by Pop freereg when possible */
	int offset = be_get_IncSP_offset(node);
	if ((offset != -8 || ia32_cg_config.use_add_esp_8) &&
	    (offset != -4 || ia32_cg_config.use_add_esp_4) &&
	    (offset != +4 || ia32_cg_config.use_sub_esp_4) &&
	    (offset != +8 || ia32_cg_config.use_sub_esp_8))
		return;

	ir_node  *stack = be_get_IncSP_pred(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	if (offset < 0) {
		/* we need a free register for pop */
		arch_register_t const *const reg = get_free_gp_reg(get_irn_irg(node));
		if (!reg)
			return;

		do {
			stack = create_pop(dbgi, block, stack, node, reg);
		} while ((offset += 4) != 0);
	} else {
		do {
			stack = new_bd_ia32_PushEax(dbgi, block, stack);
			arch_set_irn_register(stack, &ia32_registers[REG_ESP]);
			sched_add_before(node, stack);
		} while ((offset -= 4) != 0);
	}

	be_peephole_exchange(node, stack);
}

/**
 * Peephole optimization for ia32_Const's
 */
static void peephole_ia32_Const(ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);

	/* try to transform a mov 0, reg to xor reg reg */
	if (attr->imm.offset != 0 || attr->imm.entity != NULL)
		return;
	/* xor destroys the flags, so no-one must be using them */
	if (be_peephole_get_value(REG_EFLAGS) != NULL)
		return;

	const arch_register_t *reg = arch_get_irn_register(node);
	assert(be_peephole_get_reg_value(reg) == NULL);

	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *xorn  = new_bd_ia32_Xor0(dbgi, block);
	arch_set_irn_register(xorn, reg);
	replace(node, xorn);
}

static bool is_disp_const(ir_node const *const node, int32_t const val)
{
	return !get_ia32_am_ent(node) && get_ia32_am_offs_int(node) == val;
}

/**
 * Transforms a Lea into an Add or Shl if possible.
 */
static void peephole_ia32_Lea(ir_node *node)
{
	/* We can only do this if it is allowed to clobber the flags. */
	if (be_peephole_get_value(REG_EFLAGS))
		return;

	/* Frame entities should already be expressed in the offsets. */
	assert(get_ia32_attr_const(node)->am_imm.kind != X86_IMM_FRAMEOFFSET);

	/* We can transform Leas where the out register is the same as either the
	 * base or index register back to an Add or Shl. */
	ir_node                     *op1;
	ir_node                     *op2;
	ir_node               *const base    = get_irn_n(node, n_ia32_Lea_base);
	ir_node               *const index   = get_irn_n(node, n_ia32_Lea_index);
	arch_register_t const *const out_reg = arch_get_irn_register(node);
	if (out_reg == arch_get_irn_register(base)) {
		op1 = base;
		op2 = index;
	} else if (out_reg == arch_get_irn_register(index)) {
		op1 = index;
		op2 = base;
	} else {
#ifdef DEBUG_libfirm
		/* We shouldn't construct these in the first place. */
		if (is_ia32_NoReg_GP(base) && is_ia32_NoReg_GP(index))
			be_warningf(node, "found unoptimized Lea which is a Const");
#endif
		return; /* Neither base nor index use the same register as the Lea. */
	}

	ir_node       *res;
	bool     const has_2ops = !is_ia32_NoReg_GP(op2);
	bool     const has_disp = !is_disp_const(node, 0);
	unsigned const scale    = get_ia32_am_scale(node);
	if (scale == 0) {
		if (!has_2ops) {
			/* Lea base/index + disp -> Add+Imm. */
#ifdef DEBUG_libfirm
			if (!has_disp)
				be_warningf(node, "found unoptimized Lea which is a Copy");
#endif
			if (ia32_cg_config.use_incdec) {
				if (is_disp_const(node, 1)) {
					dbg_info *const dbgi  = get_irn_dbg_info(node);
					ir_node  *const block = get_nodes_block(node);
					res = new_bd_ia32_Inc(dbgi, block, op1);
					goto exchange;
				} else if (is_disp_const(node, -1)) {
					dbg_info *const dbgi  = get_irn_dbg_info(node);
					ir_node  *const block = get_nodes_block(node);
					res = new_bd_ia32_Dec(dbgi, block, op1);
					goto exchange;
				}
			}
			ir_graph          *const irg  = get_irn_irg(node);
			ia32_attr_t const *const attr = get_ia32_attr_const(node);
			op2 = ia32_create_Immediate_full(irg, &attr->am_imm);
		} else if (has_disp) {
			return; /* Lea has base, index and displacement. */
		}
		/* Lea base + index, base/index + disp -> Add. */
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = get_nodes_block(node);
		ir_graph *const irg   = get_irn_irg(node);
		ir_node  *const noreg = ia32_new_NoReg_gp(irg);
		ir_node  *const nomem = get_irg_no_mem(irg);
		res = new_bd_ia32_Add(dbgi, block, noreg, noreg, nomem, op1, op2);
		set_ia32_commutative(res);
	} else if (!has_2ops && !has_disp) {
		/* Lea index * scale -> Shl+Imm. */
		assert(op1 == index);
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = get_nodes_block(node);
		ir_graph *const irg   = get_irn_irg(node);
		ir_node  *const amt   = ia32_create_Immediate(irg, scale);
		res = new_bd_ia32_Shl(dbgi, block, op1, amt);
	} else {
		return; /* Lea has scaled index as well as base and/or displacement. */
	}

exchange:
	/* Replace the Lea by Add/Shl. */
	arch_set_irn_register(res, out_reg);
	SET_IA32_ORIG_NODE(res, node);
	replace(node, res);
}

/**
 * Split a Imul mem, imm into a Load mem and Imul reg, imm if possible.
 */
static void peephole_ia32_ImulImm_split(ir_node *imul)
{
	/* Ignore, if no memory, imm form. */
	if (get_ia32_op_type(imul) != ia32_AddrModeS)
		return;
	/* we need a free register */
	const arch_register_t *reg = get_free_gp_reg(get_irn_irg(imul));
	if (reg == NULL)
		return;

	/* fine, we can rebuild it */
	ir_node *res = ia32_turn_back_am(imul);
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

	if (get_mode_size_bits(smaller_mode) != 16 ||
			!mode_is_signed(smaller_mode)          ||
			eax != arch_get_irn_register(val)      ||
			eax != arch_get_irn_register_out(node, pn_ia32_Conv_I2I_res))
		return;

	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_node  *cwtl  = new_bd_ia32_Cwtl(dbgi, block, val);
	arch_set_irn_register(cwtl, eax);
	be_peephole_replace(node, cwtl);
}

/* Replace rolw $8, %[abcd]x by shorter xchgb %[abcd]l, %[abcd]h */
static void peephole_ia32_Rol(ir_node *node)
{
	if (get_mode_size_bits(get_ia32_ls_mode(node)) == 16) {
		arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Rol_res);
		if (is_hl_register(reg)) {
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = get_nodes_block(node);
			ir_node  *const val   = get_irn_n(node, n_ia32_Rol_val);
			ir_node  *const xchg  = new_bd_ia32_Bswap16(dbgi, block, val);
			arch_set_irn_register_out(xchg, pn_ia32_Bswap16_res, reg);
			be_peephole_replace(node, xchg);
		}
	}
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
	if (!ia32_cg_config.use_imul_mem_imm32)
		register_peephole_optimization(op_ia32_IMulImm, peephole_ia32_ImulImm_split);
	if (ia32_cg_config.optimize_size)
		register_peephole_optimization(op_ia32_Rol, peephole_ia32_Rol);
	be_peephole_opt(irg);

	/* pass 2 */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_ia32_And,     peephole_ia32_And);
	if (!ia32_cg_config.use_mov_0)
		register_peephole_optimization(op_ia32_Const, peephole_ia32_Const);
	register_peephole_optimization(op_be_IncSP,     peephole_be_IncSP);
	register_peephole_optimization(op_ia32_Or,      peephole_ia32_Or);
	register_peephole_optimization(op_ia32_Test,    peephole_ia32_Test);
	register_peephole_optimization(op_ia32_Return,  peephole_ia32_Return);
	register_peephole_optimization(op_ia32_Xor,     peephole_ia32_Xor);
	be_peephole_opt(irg);
}

static void optimize_conv_store(ir_node *node)
{
	if (!is_ia32_Store(node))
		return;

	ir_node *pred;
	ir_node *pred_proj = get_irn_n(node, n_ia32_Store_val);
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
	ir_mode *conv_mode  = get_ia32_ls_mode(pred);
	ir_mode *store_mode = get_ia32_ls_mode(node);
	if (get_mode_size_bits(conv_mode) < get_mode_size_bits(store_mode))
		return;

	be_warningf(node, "unoptimized ia32 Store(Conv)");
	set_irn_n(node, n_ia32_Store_val, get_irn_n(pred, n_ia32_Conv_I2I_val));
	if (get_irn_n_edges(pred_proj) == 0) {
		kill_node(pred_proj);
		if (pred != pred_proj)
			kill_node(pred);
	}
}

static void optimize_load_conv(ir_node *node)
{
	if (!is_ia32_Conv_I2I(node))
		return;

	ir_node *pred = get_irn_n(node, n_ia32_Conv_I2I_val);
	if (!is_Proj(pred))
		return;

	ir_node *predpred = get_Proj_pred(pred);
	if (!is_ia32_Load(predpred))
		return;

	/* the load is sign extending the upper bits, so we only need the conv
	 * if it shrinks the mode */
	ir_mode *load_mode = get_ia32_ls_mode(predpred);
	ir_mode *conv_mode = get_ia32_ls_mode(node);
	if (get_mode_size_bits(conv_mode) < get_mode_size_bits(load_mode))
		return;

	if (mode_is_signed(conv_mode) != mode_is_signed(load_mode)) {
		/* change the load if it has only 1 user */
		if (get_irn_n_edges(pred) == 1) {
			ir_mode *newmode;
			if (mode_is_signed(conv_mode)) {
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
	be_warningf(node, "unoptimized ia32 Conv(Load)");
	exchange(node, pred);
}

static void optimize_conv_conv(ir_node *node)
{
	if (!is_ia32_Conv_I2I(node))
		return;

	ir_node *pred;
	ir_node *pred_proj = get_irn_n(node, n_ia32_Conv_I2I_val);
	if (is_Proj(pred_proj))
		pred = get_Proj_pred(pred_proj);
	else
		pred = pred_proj;

	if (!is_ia32_Conv_I2I(pred))
		return;

	/* we know that after a conv, the upper bits are sign extended
	 * so we only need the 2nd conv if it shrinks the mode */
	ir_node *result_conv;
	ir_mode *conv_mode      = get_ia32_ls_mode(node);
	int      conv_mode_bits = get_mode_size_bits(conv_mode);
	ir_mode *pred_mode      = get_ia32_ls_mode(pred);
	int      pred_mode_bits = get_mode_size_bits(pred_mode);

	if (conv_mode_bits == pred_mode_bits
			&& mode_is_signed(conv_mode) == mode_is_signed(pred_mode)) {
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
				arch_set_irn_register_reqs_in(result_conv, reqs);
			}
		}
	} else {
		/* if both convs have the same sign, then we can take the smaller one */
		if (mode_is_signed(conv_mode) == mode_is_signed(pred_mode)) {
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

	be_warningf(node, "unoptimized ia32 Conv(Conv)");
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
	(void)env;

	optimize_load_conv(node);
	optimize_conv_store(node);
	optimize_conv_conv(node);
}

/**
 * Performs conv and address mode optimization.
 */
void ia32_optimize_graph(ir_graph *irg)
{
	assure_edges(irg);
	irg_walk_blkwise_graph(irg, NULL, optimize_node, NULL);
}

void ia32_init_optimize(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.optimize");
}
