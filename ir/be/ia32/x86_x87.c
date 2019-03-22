/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the x87 support and virtual to stack
 *              register translation for the ia32 backend.
 * @author      Michael Beck
 */
#include "x86_x87.h"

#include "array.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "bessaconstr.h"
#include "beutil.h"
#include "debug.h"
#include "gen_ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_bearch_t.h"
#include "ia32_new_nodes.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "irprog.h"
#include "irtools.h"
#include "obst.h"
#include "panic.h"
#include "pdeq.h"
#include "pmap.h"
#include <assert.h>

ir_mode *x86_mode_E;
ir_type *x86_type_E;

#define N_X87_REGS  8

static x87_simulator_config_t x87;

static bool is_x87_req(arch_register_req_t const *const req)
{
	return req->cls == x87.regclass;
}

/** the debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* Forward declaration. */
typedef struct x87_simulator x87_simulator;

/**
 * An entry on the simulated x87 stack.
 */
typedef struct st_entry {
	unsigned reg_idx; /**< the virtual register index of this stack value */
	ir_node *node;    /**< the node that produced this value */
} st_entry;

/**
 * The x87 state.
 */
struct x87_state {
	st_entry       st[N_X87_REGS]; /**< the register stack */
	unsigned       depth;          /**< the current stack depth */
	x87_simulator *sim;            /**< The simulator. */
};

/**
 * A block state: Every block has a x87 state at the beginning and at the end.
 */
typedef struct blk_state {
	x87_state const *begin; /**< state at the begin or NULL if not assigned */
	x87_state const *end;   /**< state at the end or NULL if not assigned */
} blk_state;

/** liveness bitset for fp registers. */
typedef unsigned char fp_liveness;

/**
 * The x87 simulator.
 */
struct x87_simulator {
	struct obstack obst;       /**< An obstack for fast allocating. */
	pmap          *blk_states; /**< Map blocks to states. */
	be_lv_t       *lv;         /**< intrablock liveness. */
	fp_liveness   *live;       /**< Liveness information. */
	deq_t          worklist;
	unsigned       n_idx;      /**< The cached get_irg_last_idx() result. */
};

/**
 * Returns the current stack depth.
 *
 * @param state  the x87 state
 * @return the x87 stack depth
 */
static unsigned x87_get_depth(const x87_state *state)
{
	return state->depth;
}

static st_entry *x87_get_entry(x87_state *const state, unsigned const pos)
{
	assert(pos < state->depth);
	return &state->st[N_X87_REGS - state->depth + pos];
}

static st_entry const *x87_get_entry_const(x87_state const *const state,
                                           unsigned const pos)
{
	assert(pos < state->depth);
	return &state->st[N_X87_REGS - state->depth + pos];
}

/**
 * Return the node at st(pos).
 *
 * @param state  the x87 state
 * @param pos    a stack position
 * @return the node that produced the value at st(pos)
 */
static ir_node *x87_get_st_node(x87_state const *const state,
                                unsigned const pos)
{
	return x87_get_entry_const(state, pos)->node;
}

/**
 * Return the virtual register index at st(pos).
 *
 * @param state  the x87 state
 * @param pos    a stack position
 * @return the fp register index that produced the value at st(pos)
 */
static unsigned x87_get_st_reg(const x87_state *state, unsigned pos)
{
	return x87_get_entry_const(state, pos)->reg_idx;
}

#ifdef DEBUG_libfirm
/**
 * Dump the stack for debugging.
 *
 * @param state  the x87 state
 */
static void x87_dump_stack(const x87_state *state)
{
	for (unsigned i = state->depth; i-- > 0;) {
		st_entry const *const entry = x87_get_entry_const(state, i);
		DB((dbg, LEVEL_2, "vf%d(%+F) ", entry->reg_idx, entry->node));
	}
	DB((dbg, LEVEL_2, "<-- TOS\n"));
}
#endif /* DEBUG_libfirm */

/**
 * Set a node to st(pos).
 *
 * @param state    the x87 state
 * @param node     the IR node that produces the value of the fp register
 * @param pos      the stack position where the new value should be entered
 */
static void x87_set_st(x87_state *const state, ir_node *const node,
                       unsigned const pos)
{
	st_entry *const entry = x87_get_entry(state, pos);
	entry->reg_idx = arch_get_irn_register(node)->index;
	entry->node    = node;

	DB((dbg, LEVEL_2, "After SET_REG: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Swap st(0) with st(pos).
 *
 * @param state    the x87 state
 * @param pos      the stack position to change the tos with
 */
static void x87_fxch(x87_state *state, unsigned pos)
{
	st_entry *const a = x87_get_entry(state, pos);
	st_entry *const b = x87_get_entry(state, 0);
	st_entry  const t = *a;
	*a = *b;
	*b = t;

	DB((dbg, LEVEL_2, "After FXCH: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Convert a node to the stack index.
 *
 * @param state    the x87 state
 * @param node     the node to find
 * @return the stack position where the node is stacked
 *         or X87_NOT_ON_STACK if the node was not found
 */
static unsigned x87_on_stack(x87_state const *const state,
                             ir_node const *const node)
{
	for (unsigned i = 0; i < state->depth; ++i) {
		if (x87_get_st_node(state, i) == node)
			return i;
	}
	return X87_NOT_ON_STACK;
}

static unsigned x87_reg_on_stack(x87_state const *const state,
                                 unsigned vreg_idx)
{
	for (unsigned i = 0; i < state->depth; ++i) {
		if (x87_get_st_reg(state, i) == vreg_idx)
			return i;
	}
	return X87_NOT_ON_STACK;
}

static unsigned is_at_pos(x87_state const *const state,
                          ir_node const *const val, unsigned const pos)
{
	arch_register_t const *const reg = arch_get_irn_register(val);
	return x87_get_st_reg(state, pos) == reg->index;
}

/**
 * Push a node onto the stack, double pushes are NOT allowed.
 *
 * @param state     the x87 state
 * @param node      the node that produces the value of the fp register
 */
void x86_x87_push(x87_state *const state, ir_node *const node)
{
	assert(x87_on_stack(state, node) == X87_NOT_ON_STACK && "double push");
	assert(state->depth < N_X87_REGS && "stack overrun");

	++state->depth;
	st_entry *const entry = x87_get_entry(state, 0);
	entry->reg_idx = arch_get_irn_register(node)->index;
	entry->node    = node;

	DB((dbg, LEVEL_2, "After PUSH: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Pop a virtual Register from the stack.
 *
 * @param state     the x87 state
 */
static void x87_pop(x87_state *state)
{
	assert(state->depth > 0 && "stack underrun");
	--state->depth;
	DB((dbg, LEVEL_2, "After POP: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Empty the fpu stack
 *
 * @param state     the x87 state
 */
static void x87_emms(x87_state *state)
{
	state->depth = 0;
}

/**
 * Returns the block state of a block.
 *
 * @param sim    the x87 simulator handle
 * @param block  the current block
 * @return the block state
 */
static blk_state *x87_get_bl_state(x87_simulator *sim, ir_node *block)
{
	blk_state *res = pmap_get(blk_state, sim->blk_states, block);
	if (res == NULL) {
		res = OALLOC(&sim->obst, blk_state);
		res->begin = NULL;
		res->end   = NULL;

		pmap_insert(sim->blk_states, block, res);
	}
	return res;
}

/**
 * Clone a x87 state.
 *
 * @param sim    the x87 simulator handle
 * @param src    the x87 state that will be cloned
 * @return a cloned copy of the src state
 */
static x87_state *x87_clone_state(x87_simulator *sim, const x87_state *src)
{
	x87_state *const res = OALLOC(&sim->obst, x87_state);
	*res = *src;
	return res;
}

static inline const arch_register_t *get_st_reg(unsigned index)
{
	return &x87.regclass->regs[index];
}

/**
 * Create a fxch node before another node.
 *
 * @param state   the x87 state
 * @param n       the node after the fxch
 * @param pos     exchange st(pos) with st(0)
 */
static void x87_create_fxch(x87_state *state, ir_node *n, unsigned pos)
{
	x87_fxch(state, pos);

	ir_node               *const block = get_nodes_block(n);
	arch_register_t const *const reg   = get_st_reg(pos);
	ir_node               *const fxch  = x87.new_bd_fxch(NULL, block, reg);

	keep_alive(fxch);

	sched_add_before(n, fxch);
	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fxch), reg->name));
}

static void move_to_pos(x87_state *const state, ir_node *const before,
                        ir_node *const val, unsigned const to)
{
	unsigned const from = x87_on_stack(state, val);
	assert(from != X87_NOT_ON_STACK);
	if (from != to) {
		if (from != 0)
			x87_create_fxch(state, before, from);
		if (to != 0)
			x87_create_fxch(state, before, to);
	}
}

static void move_to_tos(x87_state *const state, ir_node *const before,
                        ir_node *const val)
{
	move_to_pos(state, before, val, 0);
}

/* -------------- x87 perm --------------- */

/**
 * Calculate the necessary permutations to reach dst_state.
 *
 * These permutations are done with fxch instructions and placed
 * at the end of the block.
 *
 * Note that critical edges are removed here, so we need only
 * a shuffle if the current block has only one successor.
 *
 * @param block      the current block
 * @param state      the current x87 stack state, might be modified
 * @param dst_state  destination state
 * @return state
 */
static x87_state *x87_shuffle(ir_node *block, x87_state *state,
                              const x87_state *dst_state)
{
	assert(state->depth == dst_state->depth);

	/* Some mathematics here:
	 * If we have a cycle of length n that includes the tos,
	 * we need n-1 exchange operations.
	 * We can always add the tos and restore it, so we need
	 * n+1 exchange operations for a cycle not containing the tos.
	 * So, the maximum of needed operations is for a cycle of 7
	 * not including the tos == 8.
	 * This is the same number of ops we would need for using stores,
	 * so exchange is cheaper (we save the loads).
	 * On the other hand, we might need an additional exchange
	 * in the next block to bring one operand on top, so the
	 * number of ops in the first case is identical.
	 * Further, no more than 4 cycles can exists (4 x 2). */
	unsigned all_mask = (1 << (state->depth)) - 1;

	unsigned      cycles[4];
	unsigned char cycle_idx[4][8];
	unsigned      n_cycles;
	for (n_cycles = 0; all_mask != 0; ++n_cycles) {
		/* find the first free slot */
		unsigned i;
		for (i = 0; i < state->depth; ++i) {
			if (all_mask & (1 << i)) {
				all_mask &= ~(1 << i);

				/* check if there are differences here */
				if (x87_get_st_reg(state, i) != x87_get_st_reg(dst_state, i))
					break;
			}
		}

		if (all_mask == 0) {
			/* no more cycles found */
			break;
		}

		unsigned k = 0;
		cycles[n_cycles] = (1 << i);
		cycle_idx[n_cycles][k++] = i;
		for (unsigned src_idx = i, dst_idx; ; src_idx = dst_idx) {
			unsigned src_vreg = x87_get_st_reg(state, src_idx);
			dst_idx = x87_reg_on_stack(dst_state, src_vreg);
			assert(dst_idx != X87_NOT_ON_STACK);

			if ((all_mask & (1 << dst_idx)) == 0)
				break;

			cycle_idx[n_cycles][k++] = dst_idx;
			cycles[n_cycles] |=  (1 << dst_idx);
			all_mask       &= ~(1 << dst_idx);
		}
		cycle_idx[n_cycles][k] = (unsigned char)-1;
	}

	if (n_cycles == 0) {
		/* no permutation needed */
		return state;
	}

	/* Hmm: permutation needed */
	DB((dbg, LEVEL_2, "\n%+F needs permutation: from\n", block));
	DEBUG_ONLY(x87_dump_stack(state);)
	DB((dbg, LEVEL_2, "                  to\n"));
	DEBUG_ONLY(x87_dump_stack(dst_state);)

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_2, "Need %d cycles\n", n_cycles));
	for (unsigned ri = 0; ri < n_cycles; ++ri) {
		DB((dbg, LEVEL_2, " Ring %d:\n ", ri));
		for (unsigned k = 0; cycle_idx[ri][k] != (unsigned char)-1; ++k)
			DB((dbg, LEVEL_2, " st%d ->", cycle_idx[ri][k]));
		DB((dbg, LEVEL_2, "\n"));
	}
#endif

	/* Find the place node must be insert.
	 * We have only one successor block, so the last instruction should
	 * be a jump. */
	ir_node *const before = sched_last(block);
	assert(is_cfop(before));

	/* now do the permutations */
	for (unsigned ri = 0; ri < n_cycles; ++ri) {
		if ((cycles[ri] & 1) == 0)
			x87_create_fxch(state, before, cycle_idx[ri][0]);
		for (unsigned k = 1; cycle_idx[ri][k] != (unsigned char)-1; ++k) {
			x87_create_fxch(state, before, cycle_idx[ri][k]);
		}
		if ((cycles[ri] & 1) == 0)
			x87_create_fxch(state, before, cycle_idx[ri][0]);
	}
	return state;
}

static ir_node *x87_create_fdup(x87_state *const state, ir_node *const block,
                                ir_node *const val,
                                arch_register_t const *const out)
{
	unsigned               const pos  = x87_on_stack(state, val);
	arch_register_t const *const reg  = get_st_reg(pos);
	ir_node               *const fdup = x87.new_bd_fdup(NULL, block, val, reg);
	arch_set_irn_register(fdup, out);
	x86_x87_push(state, fdup);
	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fdup), reg->name));
	return fdup;
}

/**
 * Create an fdup before node @p n replacing operand @p op_n.
 *
 * @param state     the x87 state
 * @param n         the node after the fdup
 * @param val       the value to duplicate
 */
static ir_node *x87_dup_operand(x87_state *const state, ir_node *const n,
                                unsigned const op_n, ir_node *const val,
                                arch_register_t const *const out)
{
	ir_node *const block = get_nodes_block(n);
	ir_node *const fdup  = x87_create_fdup(state, block, val, out);
	sched_add_before(n, fdup);
	set_irn_n(n, op_n, fdup);
	return fdup;
}

ir_node *x86_x87_create_fpop(x87_state *const state, ir_node *const n,
                             unsigned const pos)
{
	if (pos != 0) {
		st_entry *const dst = x87_get_entry(state, pos);
		st_entry *const src = x87_get_entry(state, 0);
		*dst = *src;
	}
	x87_pop(state);
	ir_node               *const block = get_block(n);
	arch_register_t const *const reg = get_st_reg(pos);
	ir_node *const fpop = pos == 0 && x87.new_bd_ffreep != NULL
		? x87.new_bd_ffreep(NULL, block, reg)
		: x87.new_bd_fpop(  NULL, block, reg);

	keep_alive(fpop);
	sched_add_after(n, fpop);
	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fpop), reg->name));
	return fpop;
}

/* --------------------------------- liveness ------------------------------- */

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node
 * get inserted.
 *
 * @param irn      The node at which liveness should be computed.
 * @param live     The bitset of registers live before @p irn. This set gets
 *                 modified by updating it to the registers live after irn.
 * @return The live bitset.
 */
static fp_liveness fp_liveness_transfer(ir_node *irn, fp_liveness live)
{
	const arch_register_class_t *cls = x87.regclass;
	be_foreach_definition(irn, cls, def, req,
		const arch_register_t *reg = arch_get_irn_register(def);
		live &= ~(1 << reg->index);
	);
	be_foreach_use(irn, cls, in_req_, op, op_req_,
		const arch_register_t *reg = arch_get_irn_register(op);
		live |= 1 << reg->index;
	);
	return live;
}

/**
 * Put all live virtual registers at the end of a block into a bitset.
 *
 * @param sim      the simulator handle
 * @param bl       the block
 * @return The live bitset at the end of this block
 */
static fp_liveness fp_liveness_end_of_block(x87_simulator *sim,
                                            const ir_node *block)
{
	fp_liveness                  live = 0;
	const arch_register_class_t *cls  = x87.regclass;
	const be_lv_t               *lv   = sim->lv;

	be_lv_foreach_cls(lv, block, be_lv_state_end, cls, node) {
		const arch_register_t *reg = arch_get_irn_register(node);
		live |= 1 << reg->index;
	}

	return live;
}

/** get the register mask from an arch_register */
#define REGMASK(reg)    (1 << (reg->index))

/**
 * Return a bitset of argument registers which are live at the end of a node.
 *
 * @param sim    the simulator handle
 * @param pos    the node
 * @param kill   kill mask for the output registers
 * @return The live bitset.
 */
static fp_liveness fp_live_args_after(x87_simulator *sim, const ir_node *pos,
                                      fp_liveness kill)
{
	unsigned idx = get_irn_idx(pos);
	assert(idx < sim->n_idx);
	return sim->live[idx] & ~kill;
}

/**
 * Calculate the liveness for a whole block and cache it.
 *
 * @param sim   the simulator handle
 * @param block the block
 */
static void update_liveness(x87_simulator *sim, ir_node *block)
{
	fp_liveness live = fp_liveness_end_of_block(sim, block);
	/* Now iterate through the block backward and cache the results.
	 * Stop at the first Phi: this produces the live-in. */
	sched_foreach_non_phi_reverse(block, irn) {
		unsigned idx = get_irn_idx(irn);
		sim->live[idx] = live;

		live = fp_liveness_transfer(irn, live);
	}
	unsigned idx = get_irn_idx(block);
	sim->live[idx] = live;
}

/**
 * Returns true if node is live in the set.
 *
 * @param node  the node to check
 * @param live  a live bitset
 */
static inline bool is_fp_live(ir_node const *const node, fp_liveness const live)
{
	unsigned const reg_idx = arch_get_irn_register(node)->index;
	return live & (1 << reg_idx);
}

static const arch_register_t *find_unused_vreg(x87_state const *const state)
{
	unsigned depth = x87_get_depth(state);
	if (depth >= N_X87_REGS)
		return NULL;

	fp_liveness live = 0;
	for (unsigned i = 0; i < depth; ++i) {
		st_entry const *const entry = x87_get_entry_const(state, i);
		live |= 1u << entry->reg_idx;
	}

	for (unsigned i = 0; i < N_X87_REGS; ++i) {
		if ((live & (1u << i)) == 0)
			return &x87.regclass->regs[i];
	}
	/* We should not get here if depth is smaller than N_X87_REGS */
	panic("inconsistent state");
}

#ifdef DEBUG_libfirm
/**
 * Dump liveness info.
 *
 * @param live  the live bitset
 */
static void fp_dump_live(fp_liveness live)
{
	DB((dbg, LEVEL_2, "Live after: "));
	for (unsigned i = 0; i < N_X87_REGS; ++i) {
		if (live & (1 << i))
			DB((dbg, LEVEL_2, "vf%d ", i));
	}
	DB((dbg, LEVEL_2, "\n"));
}
#endif /* DEBUG_libfirm */

/* ------------------------------- simulators -------------------------------*/

static ir_node *get_result_node(ir_node *const n)
{
	return get_irn_mode(n) != mode_T ? n : get_Proj_for_pn(n, pn_ia32_res);
}

/**
 * Duplicate or shuffle a value to TOS so it can get used + popped by the next
 * instruction.
 */
static void value_to_tos(x87_state *const state, ir_node *const node,
                         int const in, arch_register_t const *const out)
{
	fp_liveness const live = fp_live_args_after(state->sim, node, REGMASK(out));
	ir_node    *const value = get_irn_n(node, in);
	if (is_fp_live(value, live)) {
		/* Operand still alive after instruction? Duplicate it. */
		x87_dup_operand(state, node, in, value, out);
	} else {
		/* Operand dies? Shuffle it to tos. */
		move_to_tos(state, node, value);
	}
}

void x86_sim_x87_binop(x87_state *const state, ir_node *const node,
                       int const n_op0, int const n_op1,
                       arch_register_t const *const out)
{
	ir_node    *      op0            = get_irn_n(node, n_op0);
	ir_node    *const op1            = get_irn_n(node, n_op1);
	fp_liveness const live           = fp_live_args_after(state->sim, node,
	                                                      REGMASK(out));
	bool        const op0_live_after = is_fp_live(op0, live);
	bool        const op1_live_after = is_fp_live(op1, live);

	DB((dbg, LEVEL_1, ">>> %+F %+F, %+F -> %s\n", node, op0, op1, out->name));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)

	assert(!is_ia32_NoReg_FP(op1));

	bool reverse;
	bool to_reg;
	bool pop = false;
	if (op0_live_after) {
		if (op1_live_after) {
			/* Both operands are live: push the first one.
			 * This works even for op0 == op1. */
			op0     = x87_dup_operand(state, node, n_op0, op0, out);
			reverse = false;
			to_reg  = false;
		} else {
			/* First live, second dead: Overwrite second. */
			if (is_at_pos(state, op0, 0)) {
				reverse = false;
				to_reg  = true;
			} else {
				move_to_tos(state, node, op1);
				reverse = true;
				to_reg  = false;
			}
		}
	} else {
		if (op1_live_after) {
			/* First dead, Second live: Overwrite first. */
			if (is_at_pos(state, op1, 0)) {
				reverse = true;
				to_reg  = true;
			} else {
				move_to_tos(state, node, op0);
				reverse = false;
				to_reg  = false;
			}
		} else {
			/* Both dead. */
			if (op0 == op1) {
				/* Operands are identical: No pop. */
				move_to_tos(state, node, op0);
				reverse = false;
				to_reg  = false;
			} else {
				/* Bring one operand to tos. Heuristically swap the operand not
				 * at st(1) to tos. This way, if any operand was at st(1), the
				 * result will end up in the new st(0) after the implicit pop.
				 * If the next operation uses the result, then no fxch will be
				 * necessary. */
				if (is_at_pos(state, op0, 0)) {
					reverse = false;
				} else if (is_at_pos(state, op1, 0)) {
					reverse = true;
				} else if (is_at_pos(state, op0, 1)) {
					move_to_tos(state, node, op1);
					reverse = true;
				} else {
					move_to_tos(state, node, op0);
					reverse = false;
				}
				to_reg = true;
				pop    = true;
			}
		}
	}

	unsigned const op_reg = x87_on_stack(state, reverse ? op0 : op1);
	/* Patch the operation. */
	x87_attr_t *const attr = x87.get_x87_attr(node);
	attr->reg        = get_st_reg(op_reg);
	attr->reverse    = reverse;
	attr->res_in_reg = to_reg;
	attr->pop        = pop;

	x87_set_st(state, get_result_node(node), to_reg ? op_reg : 0);
	if (pop)
		x87_pop(state);

	DEBUG_ONLY(
		char const *const st0  = get_st_reg(0)->name;
		char const *const reg  =  attr->reg ? attr->reg->name : "[AM]";
		char const *const l    =  attr->reverse    ? reg : st0;
		char const *const r    = !attr->reverse    ? reg : st0;
		char const *const o    =  attr->res_in_reg ? reg : st0;
		char const *const pops =  pop ? " [pop]" : "";
		DB((dbg, LEVEL_1, "<<< %s %s, %s -> %s%s\n", get_irn_opname(node), l, r,
		    o, pops));
	)
}

static void sim_ia32_binop_am(x87_state *const state, ir_node *const node)
{
	ir_node               *const right = get_irn_n(node, n_ia32_binary_right);
	arch_register_t const *const out
		= arch_get_irn_register_out(node, pn_ia32_res);

	if (is_ia32_NoReg_FP(right)) {
		/* if one operand is an address mode then this behaves like an unop */
		value_to_tos(state, node, n_ia32_binary_left, out);
		x87_set_st(state, get_result_node(node), 0);
	} else {
		x86_sim_x87_binop(state, node, n_ia32_binary_left, n_ia32_binary_right,
		                  out);
	}
	ia32_x87_attr_t *const attr = get_ia32_x87_attr(node);
	attr->x87.reverse ^= attr->attr.ins_permuted;
}


void x86_sim_x87_unop(x87_state *const state, ir_node *const node)
{
	arch_register_t const *const out  = arch_get_irn_register(node);
	DB((dbg, LEVEL_1, ">>> %+F -> %s\n", node, out->name));

	value_to_tos(state, node, 0, out);
	x87_set_st(state, node, 0);

	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(node),
	    get_st_reg(0)->name));
}

/**
 * Simulate a virtual Load instruction.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
void x86_sim_x87_load(x87_state *state, ir_node *n, ir_node *value)
{
	(void)n;
	DB((dbg, LEVEL_1, ">>> %+F\n", n));
	x86_x87_push(state, value);
	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(n),
	    get_st_reg(0)->name));
}

static void sim_ia32_x87_load(x87_state *state, ir_node *n)
{
	ir_node *value = get_result_node(n);
	x86_sim_x87_load(state, n, value);
}

/**
 * Simulate a virtual Store.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
void x86_sim_x87_store(x87_state *state, ir_node *n, int val_pos,
                       unsigned store_bits)
{
	ir_node *const val = get_irn_n(n, val_pos);
	DB((dbg, LEVEL_1, ">>> %+F %+F ->\n", n, val));

	fp_liveness const live = fp_live_args_after(state->sim, n, 0);
	if (!is_fp_live(val, live)) {
		/* we can only store the tos to memory */
		move_to_tos(state, n, val);
		goto do_pop;
	} else {
		if (store_bits > 64) {
			/* Problem: fst doesn't support 80bit modes. Code selection chooses
			 * an explicit fstp in this case which is fine, however if we create
			 * an 80bit fst because of a spill we may need some fixup here.
			 * Solution:
			 *   - stack not full: push value and fstp
			 *   - stack full: fstp value and load again */
			if (x87_get_depth(state) < N_X87_REGS) {
				/* ok, we have a free register: push + fstp */
				arch_register_t const *const out = find_unused_vreg(state);
				x87_dup_operand(state, n, val_pos, val, out);
do_pop:
				x87_pop(state);
			} else {
				/* This is still ia32 specific code */
				if (!is_ia32_fst(n))
					panic("TODO: out of stack spill case");

				/* we can only store the tos to memory */
				move_to_tos(state, n, val);

				/* stack full here: need fstp + load */
				ir_node *const block = get_nodes_block(n);
				ir_node *const mem   = get_Proj_for_pn(n, pn_ia32_st_M);
				ir_node *const base  = get_irn_n(n, n_ia32_base);
				ir_node *const idx   = get_irn_n(n, n_ia32_index);
				ir_node *const vfld  = new_bd_ia32_fld(NULL, block, base, idx,
				                                       mem, X86_SIZE_80);

				/* copy all attributes */
				ia32_copy_am_attrs(vfld, n);
				set_ia32_op_type(vfld, ia32_AddrModeS);

				arch_register_t const *const reg   = arch_get_irn_register(val);
				ir_node               *const rproj
					= be_new_Proj_reg(vfld, pn_ia32_fld_res, reg);
				/* Replace TOS by the reloaded value. */
				x87_set_st(state, rproj, 0);

				/* reroute all users of the store memory to the load memory */
				ir_node *const mproj = be_new_Proj(vfld, pn_ia32_fld_M);
				edges_reroute_except(mem, mproj, vfld);

				sched_add_after(n, vfld);

				/* rewire all users, scheduled after the store, to the loaded
				 * value */
				ir_graph *irg = get_irn_irg(val);
				be_ssa_construction_env_t env;
				be_ssa_construction_init(&env, irg);
				be_ssa_construction_add_copy(&env, rproj);
				be_ssa_construction_fix_users(&env, val);
				be_ssa_construction_destroy(&env);
			}

			x87.get_x87_attr(n)->pop = true;
		} else {
			/* we can only store the tos to memory */
			move_to_tos(state, n, val);
		}
	}

	DB((dbg, LEVEL_1, "<<< %s %s ->\n", get_irn_opname(n),
	    get_st_reg(0)->name));
}

static void sim_ia32_fst(x87_state *state, ir_node *n)
{
	x86_insn_size_t const size = get_ia32_attr_const(n)->size;
	x86_sim_x87_store(state, n, n_ia32_fst_val, x86_bytes_from_size(size) * 8);
}

static void sim_ia32_fist(x87_state *state, ir_node *n)
{
	x86_insn_size_t const size = get_ia32_attr_const(n)->size;
	x86_sim_x87_store(state, n, n_ia32_fist_val, x86_bytes_from_size(size) * 8);
}

void x86_sim_x87_store_pop(x87_state *const state, ir_node *const node,
                           int const val_pos)
{
	ir_node *const val = get_irn_n(node, val_pos);
	DB((dbg, LEVEL_1, ">>> %+F %s ->\n", node,
	    arch_get_irn_register(val)->name));

	/* input has a killed register req and cannot be alive after instruction */
	assert(!is_fp_live(val, fp_live_args_after(state->sim, node, 0)));

	/* we can only store the tos to memory */
	move_to_tos(state, node, val);

	x87_pop(state);

	DB((dbg, LEVEL_1, "<<< %s %s ->\n", get_irn_opname(node),
	    get_st_reg(0)->name));
}

static void sim_ia32_fisttp(x87_state *state, ir_node *n)
{
	x86_sim_x87_store_pop(state, n, n_ia32_fisttp_val);
}

static void sim_ia32_fistp(x87_state *state, ir_node *n)
{
	x86_sim_x87_store_pop(state, n, n_ia32_fistp_val);
}

static void sim_ia32_fstp(x87_state *state, ir_node *n)
{
	x86_sim_x87_store_pop(state, n, n_ia32_fstp_val);
}

/**
 * Simulate a virtual FtstFnstsw.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static void sim_FtstFnstsw(x87_state *state, ir_node *n)
{
	ir_node     *const val  = get_irn_n(n, n_ia32_FtstFnstsw_left);
	fp_liveness  const live = fp_live_args_after(state->sim, n, 0);

	DB((dbg, LEVEL_1, ">>> %+F %+F\n", n, val));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)

	/* bring the value to tos */
	move_to_tos(state, n, val);

	if (!is_fp_live(val, live))
		x86_x87_create_fpop(state, n, 0);
}

unsigned x86_sim_x87_fucom(x87_state *const state, ir_node *const node,
                           ir_node *const op0, ir_node *const op1)
{
	fp_liveness  const live           = fp_live_args_after(state->sim, node, 0);
	bool         const op0_live_after = is_fp_live(op0, live);

	DB((dbg, LEVEL_1, ">>> %+F %+F, %+F\n", node, op0, op1));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)

	bool       reverse;
	unsigned   pops           = 0;
	bool const op1_live_after = is_fp_live(op1, live);
	if (op0_live_after) {
		if (op1_live_after) {
			/* Both operands are live. */
			if (is_at_pos(state, op1, 0)) {
				reverse = true;
			} else {
				/* Move the first one to tos. */
				move_to_tos(state, node, op0);
				reverse = false;
			}
		} else {
			/* First live, second dead: Pop second. */
			move_to_tos(state, node, op1);
			reverse = true;
			pops    = 1;
		}
	} else {
		if (op1_live_after) {
			/* First dead, Second live: Pop first. */
			move_to_tos(state, node, op0);
			reverse = false;
			pops    = 1;
		} else {
			/* Both dead. */
			if (op0 == op1) {
				/* Operands are identical: One pop. */
				move_to_tos(state, node, op0);
				reverse = false;
				pops    = 1;
			} else {
				/* Both operands are dead.
				 * Move one operand to tos. Move the one not at
				 * pos 1, so we get a chance to use fucompp. */
				if (is_at_pos(state, op0, 0)) {
					reverse = false;
				} else if (is_at_pos(state, op1, 0)) {
					reverse = true;
				} else if (is_at_pos(state, op0, 1)) {
					move_to_tos(state, node, op1);
					reverse = true;
				} else {
					move_to_tos(state, node, op0);
					reverse = false;
				}
				pops = 2;
			}
		}
	}

	unsigned const op_reg = x87_on_stack(state, reverse ? op0 : op1);
	/* Patch the operation. */
	x87_attr_t *const attr = x87.get_x87_attr(node);
	attr->reg     = get_st_reg(op_reg);
	attr->pop     = pops != 0;
	attr->reverse = reverse;

	unsigned additional_pop = X87_NOT_ON_STACK;
	if (pops > 0) {
		x87_pop(state);
		assert(pops == 1 || pops == 2);
		if (pops == 2)
			additional_pop = op_reg - 1; /* -1 due to prior pop */
	}

	DEBUG_ONLY(
		char       const *const st0  = get_st_reg(0)->name;
		char       const *const reg  =  attr->reg ? attr->reg->name : "[AM]";
		char       const *const l    =  reverse ? reg : st0;
		char       const *const r    = !reverse ? reg : st0;
		char       const *const pop  =  attr->pop ? " [pop]" : "";
		DB((dbg, LEVEL_1, "<<< %s %s, %s%s\n", get_irn_opname(node), l, r,
		    pop));
	)
	return additional_pop;
}

static void sim_ia32_FucomFnstsw(x87_state *const state, ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_ia32_FucomFnstsw_left);
	ir_node *const op1 = get_irn_n(node, n_ia32_FucomFnstsw_right);
	unsigned const additional_pop = x86_sim_x87_fucom(state, node, op0, op1);

	ia32_x87_attr_t *const attr = get_ia32_x87_attr(node);
	attr->attr.ins_permuted ^= attr->x87.reverse;
	if (additional_pop != X87_NOT_ON_STACK) {
		if (additional_pop == 0) {
			set_irn_op(node, op_ia32_FucomppFnstsw);
			x87_pop(state);
		} else {
			x86_x87_create_fpop(state, node, additional_pop);
		}
	}
}

static void sim_ia32_Fucomi(x87_state *const state, ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_ia32_Fucomi_left);
	ir_node *const op1 = get_irn_n(node, n_ia32_Fucomi_right);
	unsigned const additional_pop = x86_sim_x87_fucom(state, node, op0, op1);

	ia32_x87_attr_t *const attr = get_ia32_x87_attr(node);
	attr->attr.ins_permuted ^= attr->x87.reverse;
	if (additional_pop != X87_NOT_ON_STACK)
		x86_x87_create_fpop(state, node, additional_pop);
}

/**
 * Simulate a be_Copy.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static void sim_be_Copy(x87_state *state, ir_node *n)
{
	if (!is_x87_req(arch_get_irn_register_req(n)))
		return;

	ir_node               *const pred = be_get_Copy_op(n);
	arch_register_t const *const out  = arch_get_irn_register(n);
	fp_liveness            const live
		= fp_live_args_after(state->sim, n, REGMASK(out));

	DB((dbg, LEVEL_1, ">>> %+F %+F -> %s\n", n, pred, out->name));
	DEBUG_ONLY(fp_dump_live(live);)

	if (is_fp_live(pred, live)) {
		/* Operand is still live, a real copy.
		 * Use an fdup or recreate constants. */
		ir_node       *copy;
		ir_node *const block = get_nodes_block(n);
		if (is_irn_constlike(pred)) {
			/* Copy a constant. */
			copy = exact_copy(pred);
			set_nodes_block(copy, block);
			arch_set_irn_register(copy, out);
			x86_x87_push(state, copy);
		} else {
			copy = x87_create_fdup(state, block, pred, out);
		}
		sched_replace(n, copy);
		exchange(n, copy);

		/* We have to make sure the old value doesn't go dead (which can happen
		 * when we recreate constants). As the simulator expected that value in
		 * the pred blocks. This is unfortunate as removing it would save us 1
		 * instruction, but we would have to rerun all the simulation to get
		 * this correct. */
		be_keep_if_unused(pred);

		DB((dbg, LEVEL_1, "<<< %+F %+F -> ?\n", copy, pred));
	} else {
		/* Just a virtual copy. */
		unsigned const op1_idx = x87_on_stack(state, pred);
		x87_set_st(state, n, op1_idx);
	}
}

/**
 * Simulate a ia32_Call.
 *
 * @param state      the x87 state
 * @param n          the node that should be simulated (and patched)
 */
static void sim_ia32_Call(x87_state *const state, ir_node *const n)
{
	DB((dbg, LEVEL_1, ">>> %+F\n", n));

	/* at the begin of a call the x87 state should be empty */
	assert(state->depth == 0 && "stack not empty before call");

	unsigned n_reg_results = get_ia32_call_attr_const(n)->n_reg_results;
	if (n_reg_results != 0) {
		/* If the called function returns a float, it is returned in st(0).
		 * This even happens if the return value is NOT used.
		 * Moreover, only one return result is supported. */
		const arch_register_req_t *req
			= arch_get_irn_register_req_out(n, pn_ia32_Call_first_result);
		if (is_x87_req(req)) {
			ir_node *const res = get_Proj_for_pn(n, pn_ia32_Call_first_result);
			x86_x87_push(state, res);
		}
	}
	DB((dbg, LEVEL_1, "Stack after: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Simulate a return node.
 */
void x86_sim_x87_ret(x87_state *const state, ir_node *const ret)
{
#ifdef DEBUG_libfirm
	/* only floating point return values must reside on stack */
	unsigned n_float_res = 0;
	for (unsigned i = 0, n = get_irn_arity(ret); i != n; ++i) {
		if (is_x87_req(arch_get_irn_register_req_in(ret, i)))
			++n_float_res;
	}
	assert(x87_get_depth(state) == n_float_res);
#else
	(void)ret;
#endif

	/* pop them virtually */
	x87_emms(state);
}

/**
 * Simulate a be_Perm.
 *
 * @param state  the x87 state
 * @param irn    the node that should be simulated (and patched)
 */
static void sim_be_Perm(x87_state *state, ir_node *irn)
{
	/* handle only floating point Perms */
	if (!is_x87_req(arch_get_irn_register_req_out(irn, 0)))
		return;

	DB((dbg, LEVEL_1, ">>> %+F\n", irn));

	/* Perm is a pure virtual instruction on x87.
	   All inputs must be on the FPU stack and are pairwise
	   different from each other.
	   So, all we need to do is to permutate the stack state. */
	unsigned const n         = (unsigned)get_irn_arity(irn);
	unsigned      *stack_pos = ALLOCAN(unsigned, n);

	/* collect old stack positions */
	foreach_irn_in(irn, i, pred) {
		unsigned const idx = x87_on_stack(state, pred);
		assert(idx != X87_NOT_ON_STACK);
		stack_pos[i] = idx;
	}
	/* now do the permutation */
	foreach_out_edge(irn, edge) {
		ir_node  *const proj = get_edge_src_irn(edge);
		unsigned  const num  = get_Proj_num(proj);

		assert(num < n);
		x87_set_st(state, proj, stack_pos[num]);
	}
	DB((dbg, LEVEL_1, "<<< %+F\n", irn));
}

static void sim_Phi(x87_state *state, ir_node *node)
{
	if (!is_x87_req(arch_get_irn_register_req(node)))
		return;

	/* just update the value we remember on stack */
	unsigned reg_idx   = arch_get_irn_register(node)->index;
	unsigned stack_idx = x87_reg_on_stack(state, reg_idx);
#ifndef NDEBUG
	/* The current node at stack_idx must be one of the phi inputs. */
	ir_node const *const existing = x87_get_st_node(state, stack_idx);
	bool                 found    = false;
	foreach_irn_in(node, i, pred) {
		if (pred == existing) {
			found = true;
			break;
		}
	}
	assert(found);
#endif
	x87_set_st(state, node, stack_idx);
}

/**
 * Kill any dead registers after @p after by popping them from the stack.
 *
 * @param sim    the simulator handle
 * @param after  the node after which values might be dead
 * @param state  the x87 state after @p after
 */
static void x87_kill_deads(x87_simulator *const sim, ir_node *const after,
                           x87_state *const state)
{
	unsigned depth = x87_get_depth(state);
	if (depth == 0)
		return;

	fp_liveness const live = fp_live_args_after(sim, after, 0);

	DB((dbg, LEVEL_1, "Killing deads:\n"));
	DEBUG_ONLY(fp_dump_live(live);)
	DEBUG_ONLY(x87_dump_stack(state);)

	if (live == 0) {
		/* special case: kill all registers */
		ir_node *(*cons)(dbg_info*, ir_node*);
		if (ia32_cg_config.use_femms) {
			/* use FEMMS on AMD processors to clear all */
			cons = &new_bd_ia32_femms;
			goto free_all;
		} else if (ia32_cg_config.use_emms) {
			/* use EMMS to clear all */
			cons = &new_bd_ia32_emms;
free_all:;
			ir_node *const block    = get_block(after);
			ir_node *const free_all = cons(NULL, block);
			sched_add_after(after, free_all);
			keep_alive(free_all);
			x87_emms(state);
			return;
		}
	}

	unsigned kill_mask = 0;
	for (unsigned i = depth; i-- > 0;) {
		ir_node const *const node = x87_get_st_node(state, i);
		if (!is_fp_live(node, live))
			kill_mask |= (1 << i);
	}

	/* now kill registers */
	ir_node *insert = after;
	while (kill_mask != 0) {
		unsigned i;
		if (kill_mask & 1) {
			/* Pop from TOS, if possible, looks nicer. */
			i = 0;
		} else {
			/* Remove deepest dead value from stack, so TOS gets moved there and
			 * later more pops from TOS might follow. */
			for (i = depth; i-- > 0;) {
				if (kill_mask & (1 << i)) {
					kill_mask &= ~(1 << i);
					break;
				}
			}
		}
		insert      = x86_x87_create_fpop(state, insert, i);
		depth      -= 1;
		kill_mask >>= 1;
	}
}

static bool reg_req_same_limited(arch_register_req_t const *const req,
                                 arch_register_req_t const *const ref_req)
{
	if (req == ref_req)
		return true;

	if (req->cls != ref_req->cls ||
	    !req->limited ||
	    !rbitsets_equal(req->limited, ref_req->limited, req->cls->n_regs))
		return false;

	return true;
}

static bool is_clobber(ir_node const *const asm_n, ir_node const *const value)
{
	arch_register_req_t const *const req = arch_get_irn_register_req(value);
	if (req->should_be_same != 0)
		return false;

	unsigned                 const num      = get_Proj_num(value);
	x86_asm_operand_t const *const operands
		= (x86_asm_operand_t const*)get_be_asm_attr_const(asm_n)->operands;
	for (size_t i = 0, n = ARR_LEN(operands); i != n; ++i) {
		x86_asm_operand_t const *const op = &operands[i];
		if (op->op.kind == BE_ASM_OPERAND_OUTPUT_VALUE && (unsigned)op->op.pos == num)
			return false;
	}

	return true;
}

static void sim_be_Asm(x87_state *const state, ir_node *const n)
{
	arch_register_req_t const *const req_t = x87.regclass->regs[0].single_req;
	arch_register_req_t const *const req_u = x87.regclass->regs[1].single_req;

	/* Collect in requirements. */
	ir_node *in_t = NULL;
	ir_node *in_u = NULL;
	be_foreach_use(n, x87.regclass, req, value, value_req,
		if (reg_req_same_limited(req, req_t)) {
			assert(!in_t);
			in_t = value;
		} else if (reg_req_same_limited(req, req_u)) {
			assert(!in_u);
			in_u = value;
		} else {
			panic("cannot handle %+F with x87 constraints", n);
		}
	);

	/* Move inputs to the correct place in the x87 stack and remove them from
	 * the simulator stack.  If they are not overwritten, they get pushed back
	 * later. */
	if (in_u) {
		if (!in_t)
			panic("\"u\" input constraint without \"t\" input constraint in %+F", n);
		move_to_pos(state, n, in_u, 1);
	}

	if (in_t) {
		move_to_tos(state, n, in_t);
		x87_pop(state);
	}

	if (in_u)
		x87_pop(state);

	/* Collect out requirements.
	 * If inputs are not overwritten, it is assumed that they remain on the
	 * stack.  Clobbering a register means that the value is popped from the
	 * stack by the inline assembler statement.
	 * Extension: GCC only allows an output "u" if there is an output "t".
	 * Here it is also ok, if it is just an input "t". */
	ir_node *out_t = in_t;
	ir_node *out_u = in_u;
	be_foreach_definition(n, x87.regclass, value, req,
		if (reg_req_same_limited(req, req_t)) {
			assert(out_t == in_t);
			out_t = is_clobber(n, value) ? NULL : value;
		} else if (reg_req_same_limited(req, req_u)) {
			assert(out_u == in_u);
			out_u = is_clobber(n, value) ? NULL : value;
		} else {
			panic("cannot handle %+F with x87 constraints", n);
		}
	);

	/* Put outputs (or non-overwritten inputs back) onto the simulator stack. */
	if (out_u) {
		if (!out_t && !in_t)
			panic("\"u\" output constraint without \"t\" constraint in %+F", n);
		x86_x87_push(state, out_u);
	}

	if (out_t)
		x86_x87_push(state, out_t);

	/* Remove dying values. */
	x87_kill_deads(state->sim, n, state);
}

/**
 * Simulate a Keep.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static void sim_be_Keep(x87_state *state, ir_node *node)
{
	DB((dbg, LEVEL_1, ">>> %+F\n", node));
	x87_kill_deads(state->sim, node, state);
	DB((dbg, LEVEL_1, "Stack after: "));
	DEBUG_ONLY(x87_dump_stack(state);)
}

/**
 * Run a simulation and fix all virtual instructions for a block.
 *
 * @param sim          the simulator handle
 * @param block        the current block
 */
static void x87_simulate_block(x87_simulator *sim, ir_node *block)
{
	blk_state       *const bl_state = x87_get_bl_state(sim, block);
	x87_state const *const begin    = bl_state->begin;
	assert(begin);
	/* already processed? */
	if (bl_state->end != NULL)
		return;

	DB((dbg, LEVEL_1, "Simulate %+F\n", block));
	DB((dbg, LEVEL_2, "State at Block begin:\n "));
	DEBUG_ONLY(x87_dump_stack(begin);)

	/* create a new state, will be changed */
	x87_state *const state = x87_clone_state(sim, begin);
	/* at block begin, kill all dead registers */
	x87_kill_deads(sim, block, state);

	sched_foreach_safe(block, n) {
		const ir_op *op = get_irn_op(n);
		if (op->ops.generic != NULL) {
			sim_func func = (sim_func)op->ops.generic;

			/* simulate it */
			func(state, n);
		}
	}

	DB((dbg, LEVEL_2, "State at Block end:\n "));
	DEBUG_ONLY(x87_dump_stack(state);)

	/* check if the state must be shuffled */
	foreach_block_succ(block, edge) {
		ir_node   *succ       = get_edge_src_irn(edge);
		blk_state *succ_state = x87_get_bl_state(sim, succ);

		if (succ_state->begin == NULL) {
			DB((dbg, LEVEL_2, "Set begin state for succ %+F:\n", succ));
			DEBUG_ONLY(x87_dump_stack(state);)
			succ_state->begin = state;

			deq_push_pointer_right(&sim->worklist, succ);
		} else {
			DB((dbg, LEVEL_2, "succ %+F already has a state, shuffling\n",
			    succ));
			/* There is already a begin state for the successor, bad.
			   Do the necessary permutations.
			   Note that critical edges are removed, so this is always possible:
			   If the successor has more than one possible input, then it must
			   be the only one.
			 */
			x87_shuffle(block, state, succ_state->begin);
		}
	}
	bl_state->end = state;
}

void x86_register_x87_sim(ir_op *op, sim_func func)
{
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func)func;
}

void x86_prepare_x87_callbacks(void)
{
	ir_clear_opcodes_generic_func();
	x86_register_x87_sim(op_Phi,     sim_Phi);
	x86_register_x87_sim(op_be_Asm,  sim_be_Asm);
	x86_register_x87_sim(op_be_Copy, sim_be_Copy);
	x86_register_x87_sim(op_be_Keep, sim_be_Keep);
	x86_register_x87_sim(op_be_Perm, sim_be_Perm);
}

void x86_prepare_x87_callbacks_ia32(void)
{
	x86_prepare_x87_callbacks();
	x86_register_x87_sim(op_ia32_Call,        sim_ia32_Call);
	x86_register_x87_sim(op_ia32_fabs,        x86_sim_x87_unop);
	x86_register_x87_sim(op_ia32_fadd,        sim_ia32_binop_am);
	x86_register_x87_sim(op_ia32_fchs,        x86_sim_x87_unop);
	x86_register_x87_sim(op_ia32_fdiv,        sim_ia32_binop_am);
	x86_register_x87_sim(op_ia32_fild,        sim_ia32_x87_load);
	x86_register_x87_sim(op_ia32_fist,        sim_ia32_fist);
	x86_register_x87_sim(op_ia32_fistp,       sim_ia32_fistp);
	x86_register_x87_sim(op_ia32_fisttp,      sim_ia32_fisttp);
	x86_register_x87_sim(op_ia32_fld1,        sim_ia32_x87_load);
	x86_register_x87_sim(op_ia32_fld,         sim_ia32_x87_load);
	x86_register_x87_sim(op_ia32_fldz,        sim_ia32_x87_load);
	x86_register_x87_sim(op_ia32_fmul,        sim_ia32_binop_am);
	x86_register_x87_sim(op_ia32_fst,         sim_ia32_fst);
	x86_register_x87_sim(op_ia32_fstp,        sim_ia32_fstp);
	x86_register_x87_sim(op_ia32_fsub,        sim_ia32_binop_am);
	x86_register_x87_sim(op_ia32_FtstFnstsw,  sim_FtstFnstsw);
	x86_register_x87_sim(op_ia32_FucomFnstsw, sim_ia32_FucomFnstsw);
	x86_register_x87_sim(op_ia32_Fucomi,      sim_ia32_Fucomi);
	x86_register_x87_sim(op_ia32_Ret,         x86_sim_x87_ret);
}

/**
 * Destroy a x87 simulator.
 *
 * @param sim  the simulator handle
 */
static void x87_destroy_simulator(x87_simulator *sim)
{
	pmap_destroy(sim->blk_states);
	obstack_free(&sim->obst, NULL);
	DB((dbg, LEVEL_1, "x87 Simulator stopped\n\n"));
}

/**
 * Pre-block walker: calculate the liveness information for the block
 * and store it into the sim->live cache.
 */
static void update_liveness_walker(ir_node *block, void *data)
{
	x87_simulator *sim = (x87_simulator*)data;
	update_liveness(sim, block);
}

/*
 * Run a simulation and fix all virtual instructions for a graph.
 * Replaces all virtual floating point instructions and registers
 * by real ones.
 */
void x86_x87_simulate_graph(ir_graph *irg, const x87_simulator_config_t *config)
{
	x87 = *config;

	/* Must have N_X87_REGS registers and optionally an additional "NoReg" */
	assert(x87.regclass->n_regs == N_X87_REGS ||
	       (x87.regclass->n_regs == N_X87_REGS+1 &&
	        x87.regclass->regs[N_X87_REGS].is_virtual));

	/* create the simulator */
	x87_simulator sim;
	obstack_init(&sim.obst);
	sim.blk_states = pmap_create();
	sim.n_idx      = get_irg_last_idx(irg);
	sim.live       = OALLOCN(&sim.obst, fp_liveness, sim.n_idx);

	DB((dbg, LEVEL_1, "--------------------------------\n"
		"x87 Simulator started for %+F\n", irg));

	ir_node   *start_block = get_irg_start_block(irg);
	blk_state *bl_state    = x87_get_bl_state(&sim, start_block);

	/* start with the empty state */
	x87_state const empty = { .sim = &sim };
	bl_state->begin = &empty;

	deq_init(&sim.worklist);
	deq_push_pointer_right(&sim.worklist, start_block);

	be_assure_live_sets(irg);
	sim.lv = be_get_irg_liveness(irg);

	/* Calculate the liveness for all nodes. We must precalculate this info,
	 * because the simulator adds new nodes (possible before Phi nodes) which
	 * would let a lazy calculation fail.
	 * On the other hand we reduce the computation amount due to
	 * precaching from O(n^2) to O(n) at the expense of O(n) cache memory.
	 */
	irg_block_walk_graph(irg, update_liveness_walker, NULL, &sim);

	/* iterate */
	do {
		ir_node *block = deq_pop_pointer_left(ir_node, &sim.worklist);
		x87_simulate_block(&sim, block);
	} while (!deq_empty(&sim.worklist));

	/* kill it */
	deq_free(&sim.worklist);
	x87_destroy_simulator(&sim);
}

/* Initializes the x87 simulator. */
void x86_init_x87(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.x86.x87");
}

ir_type *x86_init_x87_type(void)
{
	/* note mantissa is 64bit but with explicitely encoded 1 so the really
	 * usable part as counted by firm is only 63 bits */
	x86_mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                             ir_overflow_indefinite);
	ir_type *const type_f80 = get_type_for_mode(x86_mode_E);
	set_type_size(type_f80, 12);
	set_type_alignment(type_f80, 4);
	return type_f80;
}
