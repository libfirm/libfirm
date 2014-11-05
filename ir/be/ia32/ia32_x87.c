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
#include <assert.h>

#include "beirg.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "irprog.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "ircons.h"
#include "irgwalk.h"
#include "irtools.h"
#include "obst.h"
#include "pmap.h"
#include "array.h"
#include "pdeq.h"
#include "debug.h"
#include "panic.h"

#include "belive.h"
#include "besched.h"
#include "benode.h"
#include "bessaconstr.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_x87.h"
#include "ia32_architecture.h"

#define N_FLOAT_REGS  (N_ia32_fp_REGS-1)  // exclude NOREG

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
typedef struct x87_state {
	st_entry       st[N_FLOAT_REGS]; /**< the register stack */
	unsigned       depth;            /**< the current stack depth */
	x87_simulator *sim;              /**< The simulator. */
} x87_state;

/** An empty state, used for blocks without fp instructions. */
static x87_state empty = { { {0, NULL}, }, 0, NULL };

/**
 * The type of an instruction simulator function.
 *
 * @param state  the x87 state
 * @param n      the node to be simulated
 * @return true if a node was added AFTER n in schedule that MUST be simulated
 *         further
 */
typedef bool (*sim_func)(x87_state *state, ir_node *n);

/**
 * A block state: Every block has a x87 state at the beginning and at the end.
 */
typedef struct blk_state {
	x87_state *begin;   /**< state at the begin or NULL if not assigned */
	x87_state *end;     /**< state at the end or NULL if not assigned */
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
	unsigned       n_idx;      /**< The cached get_irg_last_idx() result. */
	waitq         *worklist;   /**< Worklist of blocks that must be processed. */
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
	return &state->st[N_FLOAT_REGS - state->depth + pos];
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
	return x87_get_entry((x87_state*)state, pos)->reg_idx;
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
		st_entry const *const entry = x87_get_entry((x87_state*)state, i);
		DB((dbg, LEVEL_2, "vf%d(%+F) ", entry->reg_idx, entry->node));
	}
	DB((dbg, LEVEL_2, "<-- TOS\n"));
}
#endif /* DEBUG_libfirm */

/**
 * Set a virtual register to st(pos).
 *
 * @param state    the x87 state
 * @param reg_idx  the fp register index that should be set
 * @param node     the IR node that produces the value of the fp register
 * @param pos      the stack position where the new value should be entered
 */
static void x87_set_st(x87_state *state, unsigned reg_idx, ir_node *node,
                       unsigned pos)
{
	st_entry *const entry = x87_get_entry(state, pos);
	entry->reg_idx = reg_idx;
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
 * Convert a virtual register to the stack index.
 *
 * @param state    the x87 state
 * @param reg_idx  the register fp index
 * @return the stack position where the register is stacked
 *         or -1 if the virtual register was not found
 */
static unsigned x87_on_stack(const x87_state *state, unsigned reg_idx)
{
	for (unsigned i = 0; i < state->depth; ++i) {
		if (x87_get_st_reg(state, i) == reg_idx)
			return i;
	}
	return (unsigned)-1;
}

/**
 * Push a virtual Register onto the stack, double pushes are NOT allowed.
 *
 * @param state     the x87 state
 * @param reg_idx   the register fp index
 * @param node      the node that produces the value of the fp register
 */
static void x87_push(x87_state *state, unsigned reg_idx, ir_node *node)
{
	assert(x87_on_stack(state, reg_idx) == (unsigned)-1 && "double push");
	assert(state->depth < N_FLOAT_REGS && "stack overrun");

	++state->depth;
	st_entry *const entry = x87_get_entry(state, 0);
	entry->reg_idx = reg_idx;
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

/**
 * Returns the first Proj of a mode_T node having a given mode.
 *
 * @param n  the mode_T node
 * @param m  the desired mode of the Proj
 * @return The first Proj of mode @p m found.
 */
static ir_node *get_irn_Proj_for_mode(ir_node *n, ir_mode *m)
{
	assert(get_irn_mode(n) == mode_T);
	foreach_out_edge(n, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (get_irn_mode(proj) == m)
			return proj;
	}
	panic("Proj not found");
}

static inline const arch_register_t *get_st_reg(unsigned index)
{
	return &ia32_registers[REG_ST0 + index];
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

	ir_node         *const block = get_nodes_block(n);
	ir_node         *const fxch  = new_bd_ia32_fxch(NULL, block);
	ia32_x87_attr_t *const attr  = get_ia32_x87_attr(fxch);
	attr->reg = get_st_reg(pos);

	keep_alive(fxch);

	sched_add_before(n, fxch);
	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fxch), attr->reg->name));
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
static x87_state *x87_shuffle(ir_node *block, x87_state *state, const x87_state *dst_state)
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
			dst_idx = x87_on_stack(dst_state, x87_get_st_reg(state, src_idx));

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

/**
 * Create a fpush before node n.
 *
 * @param state     the x87 state
 * @param n         the node after the fpush
 * @param pos       push st(pos) on stack
 * @param val       the value to push
 */
static void x87_create_fpush(x87_state *state, ir_node *n, unsigned pos,
                             unsigned const out_reg_idx, ir_node *const val)
{
	x87_push(state, out_reg_idx, val);

	ir_node         *const fpush = new_bd_ia32_fpush(NULL, get_nodes_block(n));
	ia32_x87_attr_t *const attr  = get_ia32_x87_attr(fpush);
	attr->reg = get_st_reg(pos);

	keep_alive(fpush);
	sched_add_before(n, fpush);

	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fpush), attr->reg->name));
}

/**
 * Create a fpop before node n.
 * This overwrites st(pos) with st(0) and pops st(0).
 *
 * @param state   the x87 state
 * @param n       the node after which to schedule the fpop
 * @param pos     the index of the entry to remove the register stack
 * @return the fpop node
 */
static ir_node *x87_create_fpop(x87_state *const state, ir_node *const n,
                                unsigned const pos)
{
	if (pos != 0) {
		st_entry *const dst = x87_get_entry(state, pos);
		st_entry *const src = x87_get_entry(state, 0);
		*dst = *src;
	}
	x87_pop(state);
	ir_node *const block = get_block(n);
	ir_node *const fpop  = pos == 0 && ia32_cg_config.use_ffreep ?
		new_bd_ia32_ffreep(NULL, block) :
		new_bd_ia32_fpop(  NULL, block);
	ia32_x87_attr_t *const attr = get_ia32_x87_attr(fpop);
	attr->reg = get_st_reg(pos);

	keep_alive(fpop);
	sched_add_after(n, fpop);
	DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fpop), attr->reg->name));
	return fpop;
}

/* --------------------------------- liveness ------------------------------------------ */

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node get inserted.
 *
 * @param irn      The node at which liveness should be computed.
 * @param live     The bitset of registers live before @p irn. This set gets modified by updating it to
 *                 the registers live after irn.
 * @return The live bitset.
 */
static fp_liveness fp_liveness_transfer(ir_node *irn, fp_liveness live)
{
	const arch_register_class_t *cls = &ia32_reg_classes[CLASS_ia32_fp];
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
static fp_liveness fp_liveness_end_of_block(x87_simulator *sim, const ir_node *block)
{
	fp_liveness                  live = 0;
	const arch_register_class_t *cls  = &ia32_reg_classes[CLASS_ia32_fp];
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
	/* now iterate through the block backward and cache the results */
	sched_foreach_reverse(block, irn) {
		/* stop at the first Phi: this produces the live-in */
		if (is_Phi(irn))
			break;

		unsigned idx = get_irn_idx(irn);
		sim->live[idx] = live;

		live = fp_liveness_transfer(irn, live);
	}
	unsigned idx = get_irn_idx(block);
	sim->live[idx] = live;
}

/**
 * Returns true if a register is live in a set.
 *
 * @param reg_idx  the fp register index
 * @param live     a live bitset
 */
static inline bool is_fp_live(unsigned reg_idx, fp_liveness live)
{
	return live & (1 << reg_idx);
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
	for (unsigned i = 0; i < N_FLOAT_REGS; ++i) {
		if (live & (1 << i)) {
			DB((dbg, LEVEL_2, "vf%d ", i));
		}
	}
	DB((dbg, LEVEL_2, "\n"));
}
#endif /* DEBUG_libfirm */

/* --------------------------------- simulators ---------------------------------------- */

/**
 * Simulate a virtual binop.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_binop(x87_state *const state, ir_node *const n)
{
	x87_simulator         *sim     = state->sim;
	ir_node               *op1     = get_irn_n(n, n_ia32_binary_left);
	ir_node               *op2     = get_irn_n(n, n_ia32_binary_right);
	const arch_register_t *op1_reg = arch_get_irn_register(op1);
	const arch_register_t *op2_reg = arch_get_irn_register(op2);
	const arch_register_t *out     = arch_get_irn_register_out(n, pn_ia32_res);
	unsigned reg_index_1           = op1_reg->index;
	unsigned reg_index_2           = op2_reg->index;
	fp_liveness            live    = fp_live_args_after(sim, n, REGMASK(out));

	DB((dbg, LEVEL_1, ">>> %+F %s, %s -> %s\n", n, op1_reg->name, op2_reg->name, out->name));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)

	unsigned op1_idx = x87_on_stack(state, reg_index_1);
	assert(op1_idx != (unsigned)-1);
	bool op1_live_after = is_fp_live(reg_index_1, live);

	unsigned               op2_idx;
	unsigned               out_idx;
	bool                   pop         = false;
	unsigned         const out_reg_idx = out->index;
	ia32_x87_attr_t *const attr        = get_ia32_x87_attr(n);
	if (reg_index_2 != REG_FP_FP_NOREG) {
		/* second operand is a fp register */
		op2_idx = x87_on_stack(state, reg_index_2);
		assert(op2_idx != (unsigned)-1);
		bool op2_live_after = is_fp_live(reg_index_2, live);

		if (op2_live_after) {
			/* Second operand is live. */

			if (op1_live_after) {
				/* Both operands are live: push the first one.
				 * This works even for op1 == op2. */
				x87_create_fpush(state, n, op1_idx, out_reg_idx, op1);
				/* now do fxxx (tos=tos X op) */
				op1_idx = 0;
				op2_idx += 1;
				out_idx = 0;
			} else {
				/* Second live, first operand is dead: Overwrite first. */
				if (op1_idx != 0 && op2_idx != 0) {
					/* Bring one operand to tos. */
					x87_create_fxch(state, n, op1_idx);
					op1_idx = 0;
				}
				out_idx = op1_idx;
			}
		} else {
			/* Second operand is dead. */
			if (op1_live_after) {
				/* First operand is live, second is dead: Overwrite second. */
				if (op1_idx != 0 && op2_idx != 0) {
					/* Bring one operand to tos. */
					x87_create_fxch(state, n, op2_idx);
					op2_idx = 0;
				}
				out_idx = op2_idx;
			} else {
				/* Both operands are dead. */
				if (op1_idx == op2_idx) {
					/* Operands are identical: no pop. */
					if (op1_idx != 0) {
						x87_create_fxch(state, n, op1_idx);
						op1_idx = 0;
						op2_idx = 0;
					}
				} else {
					if (op1_idx != 0 && op2_idx != 0) {
						/* Bring one operand to tos. Heuristically swap the operand not at
						 * st(1) to tos. This way, if any operand was at st(1), the result
						 * will end up in the new st(0) after the implicit pop. If the next
						 * operation uses the result, then no fxch will be necessary. */
						if (op1_idx != 1) {
							x87_create_fxch(state, n, op1_idx);
							op1_idx = 0;
						} else {
							x87_create_fxch(state, n, op2_idx);
							op2_idx = 0;
						}
					}
					pop = true;
				}
				out_idx = op1_idx != 0 ? op1_idx : op2_idx;
			}
		}
	} else {
		/* second operand is an address mode */
		if (op1_live_after) {
			/* first operand is live: push it here */
			x87_create_fpush(state, n, op1_idx, out_reg_idx, op1);
		} else {
			/* first operand is dead: bring it to tos */
			if (op1_idx != 0)
				x87_create_fxch(state, n, op1_idx);
		}

		op1_idx = attr->attr.data.ins_permuted ? -1 :  0;
		op2_idx = attr->attr.data.ins_permuted ?  0 : (unsigned)-1;
		out_idx = 0;
	}
	assert(op1_idx == 0       || op2_idx == 0);
	assert(out_idx == op1_idx || out_idx == op2_idx);

	x87_set_st(state, out_reg_idx, n, out_idx);
	if (pop)
		x87_pop(state);

	/* patch the operation */
	unsigned const reg_idx = op1_idx != 0 ? op1_idx : op2_idx;
	attr->reg                    = reg_idx != (unsigned)-1 ? get_st_reg(reg_idx) : NULL;
	attr->attr.data.ins_permuted = op1_idx != 0;
	attr->res_in_reg             = out_idx != 0;
	attr->pop                    = pop;

	DEBUG_ONLY(
		char const *const l = op1_idx != (unsigned)-1 ? get_st_reg(op1_idx)->name : "[AM]";
		char const *const r = op2_idx != (unsigned)-1 ? get_st_reg(op2_idx)->name : "[AM]";
		char const *const o = get_st_reg(out_idx)->name;
		DB((dbg, LEVEL_1, "<<< %s %s, %s -> %s\n", get_irn_opname(n), l, r, o));
	)
	return false;
}

/**
 * Simulate a virtual Unop.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_unop(x87_state *state, ir_node *n)
{
	arch_register_t const *const out  = arch_get_irn_register(n);
	fp_liveness            const live = fp_live_args_after(state->sim, n, REGMASK(out));
	DB((dbg, LEVEL_1, ">>> %+F -> %s\n", n, out->name));
	DEBUG_ONLY(fp_dump_live(live);)

	ir_node               *const op1         = get_irn_n(n, 0);
	arch_register_t const *const op1_reg     = arch_get_irn_register(op1);
	unsigned               const op1_reg_idx = op1_reg->index;
	unsigned               const op1_idx     = x87_on_stack(state, op1_reg_idx);
	unsigned               const out_reg_idx = out->index;
	if (is_fp_live(op1_reg_idx, live)) {
		/* push the operand here */
		x87_create_fpush(state, n, op1_idx, out_reg_idx, op1);
	} else {
		/* operand is dead, bring it to tos */
		if (op1_idx != 0)
			x87_create_fxch(state, n, op1_idx);
	}

	x87_set_st(state, out_reg_idx, n, 0);
	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(n), get_st_reg(0)->name));
	return false;
}

/**
 * Simulate a virtual Load instruction.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_load(x87_state *state, ir_node *n)
{
	assert((unsigned)pn_ia32_fld_res == (unsigned)pn_ia32_fild_res
	    && (unsigned)pn_ia32_fld_res == (unsigned)pn_ia32_fld1_res
	    && (unsigned)pn_ia32_fld_res == (unsigned)pn_ia32_fldz_res);
	const arch_register_t *out = arch_get_irn_register_out(n, pn_ia32_fld_res);

	DB((dbg, LEVEL_1, ">>> %+F -> %s\n", n, out->name));
	x87_push(state, out->index, n);
	assert(out == arch_get_irn_register_out(n, pn_ia32_fld_res));
	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(n), get_st_reg(0)->name));
	return false;
}

/**
 * Simulate a virtual Store.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_store(x87_state *state, ir_node *n)
{
	ir_node               *const val = get_irn_n(n, n_ia32_fst_val);
	arch_register_t const *const op2 = arch_get_irn_register(val);
	DB((dbg, LEVEL_1, ">>> %+F %s ->\n", n, op2->name));

	bool              do_pop          = false;
	bool              node_added      = false;
	unsigned    const op2_reg_idx     = op2->index;
	unsigned    const op2_idx         = x87_on_stack(state, op2_reg_idx);
	fp_liveness const live            = fp_live_args_after(state->sim, n, 0);
	bool        const live_after_node = is_fp_live(op2_reg_idx, live);
	assert(op2_idx != (unsigned)-1);
	if (live_after_node) {
		/* Problem: fst doesn't support 80bit modes (spills), only fstp does
		 *          fist doesn't support 64bit mode, only fistp
		 * Solution:
		 *   - stack not full: push value and fstp
		 *   - stack full: fstp value and load again
		 * Note that we cannot test on mode_E, because floats might be 80bit ... */
		ir_mode *const mode = get_ia32_ls_mode(n);
		if (get_mode_size_bits(mode) > (mode_is_int(mode) ? 32U : 64U)) {
			do_pop = true;
			if (x87_get_depth(state) < N_FLOAT_REGS) {
				/* ok, we have a free register: push + fstp */
				x87_create_fpush(state, n, op2_idx, REG_FP_FP_NOREG, val);
			} else {
				/* we can only store the tos to memory */
				if (op2_idx != 0)
					x87_create_fxch(state, n, op2_idx);

				/* stack full here: need fstp + load */
				ir_node *const block = get_nodes_block(n);
				ir_node *const mem   = get_irn_Proj_for_mode(n, mode_M);
				ir_node *const vfld  = new_bd_ia32_fld(NULL, block, get_irn_n(n, 0), get_irn_n(n, 1), mem, mode);

				/* copy all attributes */
				ia32_copy_am_attrs(vfld, n);
				set_ia32_op_type(vfld, ia32_AddrModeS);

				ir_node *const rproj = new_r_Proj(vfld, mode, pn_ia32_fld_res);
				ir_node *const mproj = new_r_Proj(vfld, mode_M, pn_ia32_fld_M);

				arch_set_irn_register(rproj, op2);

				/* reroute all former users of the store memory to the load memory */
				edges_reroute_except(mem, mproj, vfld);

				sched_add_after(n, vfld);

				/* rewire all users, scheduled after the store, to the loaded value */
				ir_graph *irg = get_irn_irg(val);
				be_ssa_construction_env_t env;
				be_ssa_construction_init(&env, irg);
				be_ssa_construction_add_copy(&env, rproj);
				be_ssa_construction_fix_users(&env, val);
				be_ssa_construction_destroy(&env);

				node_added = true;
			}
		} else {
			/* we can only store the tos to memory */
			if (op2_idx != 0)
				x87_create_fxch(state, n, op2_idx);
		}
	} else {
		/* we can only store the tos to memory */
		if (op2_idx != 0)
			x87_create_fxch(state, n, op2_idx);

		do_pop = true;
	}

	if (do_pop)
		x87_pop(state);

	ia32_x87_attr_t *const attr = get_ia32_x87_attr(n);
	attr->pop = do_pop;
	DB((dbg, LEVEL_1, "<<< %s %s ->\n", get_irn_opname(n), get_st_reg(0)->name));
	return node_added;
}

static bool sim_fprem(x87_state *const state, ir_node *const n)
{
	(void)state;
	(void)n;
	panic("TODO implement");
}

/**
 * Simulate a virtual fisttp.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_fisttp(x87_state *state, ir_node *n)
{
	ir_node               *val = get_irn_n(n, n_ia32_fisttp_val);
	const arch_register_t *op2 = arch_get_irn_register(val);

	unsigned const op2_idx = x87_on_stack(state, op2->index);
	DB((dbg, LEVEL_1, ">>> %+F %s ->\n", n, op2->name));
	assert(op2_idx != (unsigned)-1);

	/* Note: although the value is still live here, it is destroyed because
	 * of the pop. The register allocator is aware of that and introduced a copy
	 * if the value must be alive. */

	/* we can only store the tos to memory */
	if (op2_idx != 0)
		x87_create_fxch(state, n, op2_idx);

	x87_pop(state);

	DB((dbg, LEVEL_1, "<<< %s %s ->\n", get_irn_opname(n), get_st_reg(0)->name));
	return false;
}

/**
 * Simulate a virtual FtstFnstsw.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_FtstFnstsw(x87_state *state, ir_node *n)
{
	x87_simulator         *sim         = state->sim;
	ir_node               *op1_node    = get_irn_n(n, n_ia32_FtstFnstsw_left);
	const arch_register_t *reg1        = arch_get_irn_register(op1_node);
	unsigned               reg_index_1 = reg1->index;
	unsigned               op1_idx     = x87_on_stack(state, reg_index_1);
	fp_liveness            live        = fp_live_args_after(sim, n, 0);

	DB((dbg, LEVEL_1, ">>> %+F %s\n", n, reg1->name));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)
	assert(op1_idx != (unsigned)-1);

	/* bring the value to tos */
	if (op1_idx != 0)
		x87_create_fxch(state, n, op1_idx);

	if (!is_fp_live(reg_index_1, live))
		x87_create_fpop(state, n, 0);
	return false;
}

/**
 * Simulate a Fucom
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_Fucom(x87_state *state, ir_node *n)
{
	ia32_x87_attr_t       *attr        = get_ia32_x87_attr(n);
	x87_simulator         *sim         = state->sim;
	ir_node               *op1_node    = get_irn_n(n, n_ia32_FucomFnstsw_left);
	ir_node               *op2_node    = get_irn_n(n, n_ia32_FucomFnstsw_right);
	const arch_register_t *op1         = arch_get_irn_register(op1_node);
	const arch_register_t *op2         = arch_get_irn_register(op2_node);
	unsigned               reg_index_1 = op1->index;
	unsigned               reg_index_2 = op2->index;
	fp_liveness            live        = fp_live_args_after(sim, n, 0);

	DB((dbg, LEVEL_1, ">>> %+F %s, %s\n", n, op1->name, op2->name));
	DEBUG_ONLY(fp_dump_live(live);)
	DB((dbg, LEVEL_1, "Stack before: "));
	DEBUG_ONLY(x87_dump_stack(state);)

	unsigned op1_idx = x87_on_stack(state, reg_index_1);
	assert(op1_idx != (unsigned)-1);

	unsigned op2_idx;
	unsigned pops = 0;
	/* BEWARE: check for comp a,a cases, they might happen */
	if (reg_index_2 != REG_FP_FP_NOREG) {
		/* second operand is a fp register */
		op2_idx = x87_on_stack(state, reg_index_2);
		assert(op2_idx != (unsigned)-1);

		if (is_fp_live(reg_index_2, live)) {
			/* second operand is live */

			if (is_fp_live(reg_index_1, live)) {
				/* both operands are live */
				if (op1_idx != 0 && op2_idx != 0) {
					/* bring the first one to tos */
					x87_create_fxch(state, n, op1_idx);
					if (op1_idx == op2_idx)
						op2_idx = 0;
					op1_idx = 0;
					/* res = tos X op */
				}
			} else {
				/* second live, first operand is dead here, bring it to tos.
				   This means further, op1_idx != op2_idx. */
				assert(op1_idx != op2_idx);
				if (op1_idx != 0) {
					x87_create_fxch(state, n, op1_idx);
					if (op2_idx == 0)
						op2_idx = op1_idx;
					op1_idx = 0;
				}
				/* res = tos X op, pop */
				pops = 1;
			}
		} else {
			/* second operand is dead */
			if (is_fp_live(reg_index_1, live)) {
				/* first operand is live: bring second to tos.
				   This means further, op1_idx != op2_idx. */
				assert(op1_idx != op2_idx);
				if (op2_idx != 0) {
					x87_create_fxch(state, n, op2_idx);
					if (op1_idx == 0)
						op1_idx = op2_idx;
					op2_idx = 0;
				}
				/* res = op X tos, pop */
				pops = 1;
			} else {
				/* both operands are dead here, check first for identity. */
				if (op1_idx == op2_idx) {
					/* identically, one pop needed */
					if (op1_idx != 0) {
						x87_create_fxch(state, n, op1_idx);
						op1_idx = 0;
						op2_idx = 0;
					}
					/* res = tos X op, pop */
					pops    = 1;
				} else {
					if (op1_idx != 0 && op2_idx != 0) {
						/* Both not at tos: Move one operand to tos. Move the one not at
						 * pos 1, so we get a chance to use fucompp. */
						if (op1_idx != 1) {
							x87_create_fxch(state, n, op1_idx);
							op1_idx = 0;
						} else {
							x87_create_fxch(state, n, op2_idx);
							op2_idx = 0;
						}
					}
					pops = 2;
				}
			}
		}
	} else {
		/* second operand is an address mode */
		if (op1_idx != 0)
			x87_create_fxch(state, n, op1_idx);
		/* Pop first operand, if it is dead. */
		if (!is_fp_live(reg_index_1, live))
			pops = 1;

		op1_idx = attr->attr.data.ins_permuted ? (unsigned)-1 :            0;
		op2_idx = attr->attr.data.ins_permuted ?            0 : (unsigned)-1;
	}
	assert(op1_idx == 0 || op2_idx == 0);

	/* patch the operation */
	if (is_ia32_FucomFnstsw(n) && pops == 2 && (op1_idx == 1 || op2_idx == 1)) {
		set_irn_op(n, op_ia32_FucomppFnstsw);
		x87_pop(state);
		x87_pop(state);
	} else {
		if (pops != 0)
			x87_pop(state);
		if (pops == 2) {
			unsigned const idx = (op1_idx != 0 ? op1_idx : op2_idx) - 1 /* Due to prior pop. */;
			x87_create_fpop(state, n, idx);
		}
	}

	unsigned const reg_idx = op1_idx != 0 ? op1_idx : op2_idx;
	attr->reg                    = reg_idx != (unsigned)-1 ? get_st_reg(reg_idx) : NULL;
	attr->attr.data.ins_permuted = op1_idx != 0;
	attr->pop                    = pops != 0;

	DEBUG_ONLY(
		char const *const l = op1_idx != (unsigned)-1 ? get_st_reg(op1_idx)->name : "[AM]";
		char const *const r = op2_idx != (unsigned)-1 ? get_st_reg(op2_idx)->name : "[AM]";
		DB((dbg, LEVEL_1, "<<< %s %s, %s\n", get_irn_opname(n), l, r));
	)
	return false;
}

/**
 * Simulate a Keep.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_Keep(x87_state *state, ir_node *node)
{
	DB((dbg, LEVEL_1, ">>> %+F\n", node));

	foreach_irn_in(node, i, op) {
		const arch_register_t *op_reg = arch_get_irn_register(op);
		if (op_reg->reg_class != &ia32_reg_classes[CLASS_ia32_fp])
			continue;

		unsigned    reg_id       = op_reg->index;
		fp_liveness live         = fp_live_args_after(state->sim, node, 0);
		unsigned    op_stack_idx = x87_on_stack(state, reg_id);
		if (op_stack_idx != (unsigned)-1 && !is_fp_live(reg_id, live))
			x87_create_fpop(state, node, 0);
	}

	DB((dbg, LEVEL_1, "Stack after: "));
	DEBUG_ONLY(x87_dump_stack(state);)
	return false;
}

/**
 * Keep the given node alive by adding a be_Keep.
 *
 * @param node  the node to kept alive
 */
static void keep_float_node_alive(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	ir_node *keep  = be_new_Keep(block, 1, &node);
	sched_add_after(node, keep);
}

/**
 * Create a copy of a node. Recreate the node if it's a constant.
 *
 * @param state  the x87 state
 * @param n      the node to be copied
 * @return the copy of n
 */
static ir_node *create_Copy(x87_state *state, ir_node *n)
{
	/* Do not copy constants, recreate them. */
	ir_node       *res;
	ir_node *const block = get_nodes_block(n);
	ir_node *const pred  = be_get_Copy_op(n);
	if (is_irn_constlike(pred)) {
		/* Copy a constant. */
		res = exact_copy(pred);
		set_nodes_block(res, block);
	} else {
		dbg_info *const dbgi    = get_irn_dbg_info(n);
		ir_mode  *const mode    = get_irn_mode(n);
		res = new_bd_ia32_fpushCopy(dbgi, block, pred, mode);

		ia32_x87_attr_t       *const attr    = get_ia32_x87_attr(res);
		arch_register_t const *const op1     = arch_get_irn_register(pred);
		unsigned               const op1_idx = x87_on_stack(state, op1->index);
		attr->reg = get_st_reg(op1_idx);
	}
	arch_register_t const *const out = arch_get_irn_register(n);
	x87_push(state, out->index, res);
	arch_set_irn_register(res, out);
	return res;
}

/**
 * Simulate a be_Copy.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_Copy(x87_state *state, ir_node *n)
{
	arch_register_class_t const *const cls = arch_get_irn_register_req(n)->cls;
	if (cls != &ia32_reg_classes[CLASS_ia32_fp])
		return false;

	ir_node               *const pred = be_get_Copy_op(n);
	arch_register_t const *const op1  = arch_get_irn_register(pred);
	arch_register_t const *const out  = arch_get_irn_register(n);
	fp_liveness            const live = fp_live_args_after(state->sim, n, REGMASK(out));

	DB((dbg, LEVEL_1, ">>> %+F %s -> %s\n", n, op1->name, out->name));
	DEBUG_ONLY(fp_dump_live(live);)

	if (is_fp_live(op1->index, live)) {
		/* Operand is still live, a real copy. We need here an fpush that can
		   hold a a register, so use the fpushCopy or recreate constants */
		ir_node *const node = create_Copy(state, n);
		sched_replace(n, node);
		exchange(n, node);

		/* We have to make sure the old value doesn't go dead (which can happen
		 * when we recreate constants). As the simulator expected that value in
		 * the pred blocks. This is unfortunate as removing it would save us 1
		 * instruction, but we would have to rerun all the simulation to get
		 * this correct. */
		if (get_irn_n_edges(pred) == 0)
			keep_float_node_alive(pred);

		DB((dbg, LEVEL_1, "<<< %+F %s -> ?\n", node, op1->name));
	} else {
		/* Just a virtual copy. */
		unsigned const op1_idx = x87_on_stack(state, op1->index);
		x87_set_st(state, out->index, n, op1_idx);
	}
	return false;
}

static bool sim_Asm(x87_state *const state, ir_node *const n)
{
	(void)state;
	be_foreach_use(n, &ia32_reg_classes[CLASS_ia32_fp], in_req, value, value_req,
		panic("cannot handle %+F with x87 constraints", n);
	);

	be_foreach_out(n, i) {
		arch_register_req_t const *const req = arch_get_irn_register_req_out(n, i);
		if (req->cls == &ia32_reg_classes[CLASS_ia32_fp])
			panic("cannot handle %+F with x87 constraints", n);
	}
	return false;
}

/**
 * Simulate a ia32_Call.
 *
 * @param state      the x87 state
 * @param n          the node that should be simulated (and patched)
 */
static bool sim_Call(x87_state *state, ir_node *n)
{
	DB((dbg, LEVEL_1, ">>> %+F\n", n));

	/* at the begin of a call the x87 state should be empty */
	assert(state->depth == 0 && "stack not empty before call");

	ir_type *const call_tp = get_ia32_call_attr_const(n)->call_tp;
	if (get_method_n_ress(call_tp) != 0) {
		/* If the called function returns a float, it is returned in st(0).
		 * This even happens if the return value is NOT used.
		 * Moreover, only one return result is supported. */
		ir_type *const res_type = get_method_res_type(call_tp, 0);
		ir_mode *const mode     = get_type_mode(res_type);
		if (mode && mode_is_float(mode)) {
			ir_node               *const resproj = get_Proj_for_pn(n, pn_ia32_Call_first_result);
			arch_register_t const *const reg     = arch_get_irn_register(resproj);
			x87_push(state, reg->index, resproj);
		}
	}
	DB((dbg, LEVEL_1, "Stack after: "));
	DEBUG_ONLY(x87_dump_stack(state);)
	return false;
}

/**
 * Simulate a be_Return.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 */
static bool sim_Return(x87_state *state, ir_node *n)
{
#ifdef DEBUG_libfirm
	/* only floating point return values must reside on stack */
	unsigned n_float_res = 0;
	foreach_irn_in(n, i, res) {
		if (mode_is_float(get_irn_mode(res)))
			++n_float_res;
	}
	assert(x87_get_depth(state) == n_float_res);
#else
	(void)n;
#endif

	/* pop them virtually */
	x87_emms(state);
	return false;
}

/**
 * Simulate a be_Perm.
 *
 * @param state  the x87 state
 * @param irn    the node that should be simulated (and patched)
 */
static bool sim_Perm(x87_state *state, ir_node *irn)
{
	/* handle only floating point Perms */
	ir_node *pred = get_irn_n(irn, 0);
	if (! mode_is_float(get_irn_mode(pred)))
		return false;

	DB((dbg, LEVEL_1, ">>> %+F\n", irn));

	/* Perm is a pure virtual instruction on x87.
	   All inputs must be on the FPU stack and are pairwise
	   different from each other.
	   So, all we need to do is to permutate the stack state. */
	unsigned const n         = (unsigned)get_irn_arity(irn);
	unsigned      *stack_pos = ALLOCAN(unsigned, n);

	/* collect old stack positions */
	foreach_irn_in(irn, i, pred) {
		const arch_register_t *inreg = arch_get_irn_register(pred);
		unsigned idx = x87_on_stack(state, inreg->index);
		assert(idx != (unsigned)-1);
		stack_pos[i] = idx;
	}
	/* now do the permutation */
	foreach_out_edge(irn, edge) {
		ir_node               *proj = get_edge_src_irn(edge);
		const arch_register_t *out  = arch_get_irn_register(proj);
		unsigned               num  = get_Proj_num(proj);

		assert(num < n);
		x87_set_st(state, out->index, proj, stack_pos[num]);
	}
	DB((dbg, LEVEL_1, "<<< %+F\n", irn));
	return false;
}

/**
 * Kill any dead registers at block start by popping them from the stack.
 *
 * @param sim    the simulator handle
 * @param block  the current block
 * @param state  the x87 state at the begin of the block
 */
static void x87_kill_deads(x87_simulator *const sim, ir_node *const block, x87_state *const state)
{
	unsigned depth = x87_get_depth(state);
	if (depth == 0)
		return;

	fp_liveness const live = fp_live_args_after(sim, block, 0);

	DB((dbg, LEVEL_1, "Killing deads:\n"));
	DEBUG_ONLY(fp_dump_live(live);)
	DEBUG_ONLY(x87_dump_stack(state);)

	if (live == 0) {
		/* special case: kill all registers */
		ir_node *free_all;
		if (ia32_cg_config.use_femms) {
			/* use FEMMS on AMD processors to clear all */
			free_all = new_bd_ia32_femms(NULL, block);
			goto sched_free_all;
		} else if (ia32_cg_config.use_emms) {
			/* use EMMS to clear all */
			free_all = new_bd_ia32_emms(NULL, block);
sched_free_all:
			sched_add_after(block, free_all);
			keep_alive(free_all);
			x87_emms(state);
			return;
		}
	}

	unsigned kill_mask = 0;
	for (unsigned i = depth; i-- > 0;) {
		unsigned const reg = x87_get_st_reg(state, i);
		if (!is_fp_live(reg, live))
			kill_mask |= (1 << i);
	}

	/* now kill registers */
	ir_node *insert = block;
	while (kill_mask != 0) {
		unsigned i;
		if (kill_mask & 1) {
			/* Pop from TOS, if possible, looks nicer. */
			i = 0;
		} else {
			/* Remove deepest dead value from stack, so TOS gets moved there and later
			 * more pops from TOS might follow. */
			for (i = depth; i-- > 0;) {
				if (kill_mask & (1 << i)) {
					kill_mask &= ~(1 << i);
					break;
				}
			}
		}
		insert      = x87_create_fpop(state, insert, i);
		depth      -= 1;
		kill_mask >>= 1;
	}
}

/**
 * Run a simulation and fix all virtual instructions for a block.
 *
 * @param sim          the simulator handle
 * @param block        the current block
 */
static void x87_simulate_block(x87_simulator *sim, ir_node *block)
{
	blk_state *bl_state = x87_get_bl_state(sim, block);
	x87_state *state    = bl_state->begin;
	assert(state != NULL);
	/* already processed? */
	if (bl_state->end != NULL)
		return;

	DB((dbg, LEVEL_1, "Simulate %+F\n", block));
	DB((dbg, LEVEL_2, "State at Block begin:\n "));
	DEBUG_ONLY(x87_dump_stack(state);)

	/* create a new state, will be changed */
	state = x87_clone_state(sim, state);
	/* at block begin, kill all dead registers */
	x87_kill_deads(sim, block, state);

	/* beware, n might change */
	for (ir_node *n = sched_first(block), *next; !sched_is_end(n); n = next) {
		next = sched_next(n);

		/* get the next node to be simulated here.
		 * n might be completely removed from the schedule- */
		const ir_op *op = get_irn_op(n);
		if (op->ops.generic != NULL) {
			sim_func func = (sim_func)op->ops.generic;

			/* simulate it */
			bool node_added = (*func)(state, n);

			/* sim_func might have added an additional node after n,
			 * so update next node
			 * beware: n must not be changed by sim_func
			 * (i.e. removed from schedule) in this case */
			if (node_added)
				next = sched_next(n);
		}
	}

	DB((dbg, LEVEL_2, "State at Block end:\n ")); DEBUG_ONLY(x87_dump_stack(state);)

	/* check if the state must be shuffled */
	foreach_block_succ(block, edge) {
		ir_node   *succ       = get_edge_src_irn(edge);
		blk_state *succ_state = x87_get_bl_state(sim, succ);

		if (succ_state->begin == NULL) {
			DB((dbg, LEVEL_2, "Set begin state for succ %+F:\n", succ));
			DEBUG_ONLY(x87_dump_stack(state);)
			succ_state->begin = state;

			waitq_put(sim->worklist, succ);
		} else {
			DB((dbg, LEVEL_2, "succ %+F already has a state, shuffling\n", succ));
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

/**
 * Register a simulator function.
 *
 * @param op    the opcode to simulate
 * @param func  the simulator function for the opcode
 */
static void register_sim(ir_op *op, sim_func func)
{
	assert(op->ops.generic == NULL);
	op->ops.generic = (op_func)func;
}

/**
 * Create a new x87 simulator.
 *
 * @param sim       a simulator handle, will be initialized
 * @param irg       the current graph
 */
static void x87_init_simulator(x87_simulator *sim, ir_graph *irg)
{
	obstack_init(&sim->obst);
	sim->blk_states = pmap_create();
	sim->n_idx      = get_irg_last_idx(irg);
	sim->live       = OALLOCN(&sim->obst, fp_liveness, sim->n_idx);

	DB((dbg, LEVEL_1, "--------------------------------\n"
		"x87 Simulator started for %+F\n", irg));

	/* set the generic function pointer of instruction we must simulate */
	ir_clear_opcodes_generic_func();

	register_sim(op_be_Copy,           sim_Copy);
	register_sim(op_be_Keep,           sim_Keep);
	register_sim(op_be_Perm,           sim_Perm);
	register_sim(op_ia32_Asm,          sim_Asm);
	register_sim(op_ia32_Call,         sim_Call);
	register_sim(op_ia32_fabs,         sim_unop);
	register_sim(op_ia32_fadd,         sim_binop);
	register_sim(op_ia32_fchs,         sim_unop);
	register_sim(op_ia32_fdiv,         sim_binop);
	register_sim(op_ia32_fild,         sim_load);
	register_sim(op_ia32_fist,         sim_store);
	register_sim(op_ia32_fisttp,       sim_fisttp);
	register_sim(op_ia32_fld1,         sim_load);
	register_sim(op_ia32_fld,          sim_load);
	register_sim(op_ia32_fldz,         sim_load);
	register_sim(op_ia32_fmul,         sim_binop);
	register_sim(op_ia32_fprem,        sim_fprem);
	register_sim(op_ia32_fst,          sim_store);
	register_sim(op_ia32_fsub,         sim_binop);
	register_sim(op_ia32_FtstFnstsw,   sim_FtstFnstsw);
	register_sim(op_ia32_FucomFnstsw,  sim_Fucom);
	register_sim(op_ia32_Fucomi,       sim_Fucom);
	register_sim(op_ia32_Return,       sim_Return);
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
void ia32_x87_simulate_graph(ir_graph *irg)
{
	/* create the simulator */
	x87_simulator sim;
	x87_init_simulator(&sim, irg);

	ir_node   *start_block = get_irg_start_block(irg);
	blk_state *bl_state    = x87_get_bl_state(&sim, start_block);

	/* start with the empty state */
	empty.sim       = &sim;
	bl_state->begin = &empty;

	sim.worklist = new_waitq();
	waitq_put(sim.worklist, start_block);

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
		ir_node *block = (ir_node*)waitq_get(sim.worklist);
		x87_simulate_block(&sim, block);
	} while (! waitq_empty(sim.worklist));

	/* kill it */
	del_waitq(sim.worklist);
	x87_destroy_simulator(&sim);
}

/* Initializes the x87 simulator. */
void ia32_init_x87(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.x87");
}
