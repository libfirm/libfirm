/**
 * This file implements the x87 support and virtual to stack
 * register translation for the ia32 backend.
 *
 * @author: Michael Beck
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <assert.h>

#include "irnode_t.h"
#include "irop_t.h"
#include "irprog.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "ircons.h"
#include "obst.h"
#include "pmap.h"
#include "pdeq.h"
#include "irprintf.h"
#include "debug.h"

#include "../belive_t.h"
#include "../besched.h"
#include "../benode_t.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_x87.h"

#define N_x87_REGS 8

/* first and second binop index */
#define BINOP_IDX_1	2
#define BINOP_IDX_2 3

/* the unop index */
#define UNOP_IDX  0

/* the store val index */
#define STORE_VAL_IDX 2

#define MASK_TOS(x)		((x) & (N_x87_REGS - 1))

/** the debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An exchange template.
 * Note that our virtual functions have the same inputs
 * and attributes as the real ones, so we can simple exchange
 * their opcodes!
 * Further, x87 supports inverse instructions, so we can handle them.
 */
typedef struct _exchange_tmpl {
	ir_op *normal_op;       /**< the normal one */
	ir_op *reverse_op;      /**< the reverse one if exists */
	ir_op *normal_pop_op;   /**< the normal one with tos pop */
	ir_op *reverse_pop_op;  /**< the reverse one with tos pop */
} exchange_tmpl;

/**
 * An entry on the simulated x87 stack.
 */
typedef struct _st_entry {
	int     reg_idx;        /**< the virtual register index of this stack value */
	ir_node *node;          /**< the node that produced this value */
} st_entry;

/**
 * The x87 state.
 */
typedef struct _x87_state {
	st_entry st[N_x87_REGS];  /**< the register stack */
	int depth;                /**< the current stack depth */
	int tos;                  /**< position of the tos */
} x87_state;

/** An empty state, used for blocks without fp instructions. */
static const x87_state _empty = { { {0, NULL}, }, 0, 0 };
static x87_state *empty = (x87_state *)&_empty;

/** The type of an instruction simulator */
typedef void (*sim_func)(x87_state *state, ir_node *n, const arch_env_t *env);

/**
 * A block state: Every block has a x87 state at the beginning and at the end.
 */
typedef struct _blk_state {
	x87_state *begin;   /**< state at the begin or NULL if not assigned */
	x87_state *end;     /**< state at the end or NULL if not assigned */
} blk_state;

#define PTR_TO_BLKSTATE(p)	((blk_state *)(p))

/**
 * The x87 simulator.
 */
typedef struct _x87_simulator {
	struct obstack obst;      /**< an obstack for fast allocating */
	pmap *blk_states;         /**< map blocks to states */
	const arch_env_t *env;		/**< architecture environment */
} x87_simulator;

/**
 * Returns the stack depth.
 *
 * @param state  the x87 state
 *
 * @return the x87 stack depth
 */
static int x87_get_depth(const x87_state *state) {
	return state->depth;
}

/**
 * Check if the state is empty.
 *
 * @param state  the x87 state
 *
 * returns non-zero if the x87 stack is empty
 */
static int x87_state_is_empty(const x87_state *state) {
	return state->depth == 0;
}

/**
 * Return the virtual register index at st(pos).
 *
 * @param state  the x87 state
 * @param pos    a stack position
 *
 * @return the vfp register index that produced the value at st(pos)
 */
static int x87_get_st_reg(const x87_state *state, int pos) {
	assert(pos < state->depth);
	return state->st[MASK_TOS(state->tos + pos)].reg_idx;
}

/**
 * Return the node at st(pos).
 *
 * @param state  the x87 state
 * @param pos    a stack position
 *
 * @return the IR node that produced the value at st(pos)
 */
static ir_node *x87_get_st_node(const x87_state *state, int pos) {
	assert(pos < state->depth);
	return state->st[MASK_TOS(state->tos + pos)].node;
}

#ifdef DEBUG_libfirm
/**
 * Dump the stack for debugging.
 *
 * @param state  the x87 state
 */
static void x87_dump_stack(const x87_state *state) {
	int i;

	for (i = state->depth - 1; i >= 0; --i) {
		DB((dbg, LEVEL_2, "vf%d ", x87_get_st_reg(state, i)));
	}
	DB((dbg, LEVEL_2, "<-- TOS\n"));
}
#endif /* DEBUG_libfirm */

/**
 * Set a virtual register to st(pos).
 *
 * @param state    the x87 state
 * @param reg_idx  the vfp register index that should be set
 * @param node     the IR node that produces the value of the vfp register
 * @param pos      the stack position where the new value should be entered
 */
static void x87_set_st(x87_state *state, int reg_idx, ir_node *node, int pos) {
	assert(0 < state->depth);
	state->st[MASK_TOS(state->tos + pos)].reg_idx = reg_idx;
	state->st[MASK_TOS(state->tos + pos)].node    = node;

	DB((dbg, LEVEL_2, "After SET_REG:\n ")); DEBUG_ONLY(x87_dump_stack(state));
}

/**
 * Set the tos virtual register.
 *
 * @param state    the x87 state
 * @param reg_idx  the vfp register index that should be set
 * @param node     the IR node that produces the value of the vfp register
 */
static void x87_set_tos(x87_state *state, int reg_idx, ir_node *node) {
	x87_set_st(state, reg_idx, node, 0);
}

/**
 * Flush the x87 stack.
 *
 * @param state    the x87 state
 */
static void x87_flush(x87_state *state) {
	state->depth = 0;
	state->tos   = 0;
}

/**
 * Swap st(0) with st(pos).
 *
 * @param state    the x87 state
 * @param pos      the stack position to change the tos with
 */
static void x87_fxch(x87_state *state, int pos) {
	st_entry entry;
	assert(pos < state->depth);

	entry = state->st[MASK_TOS(state->tos + pos)];
	state->st[MASK_TOS(state->tos + pos)] = state->st[MASK_TOS(state->tos)];
	state->st[MASK_TOS(state->tos)] = entry;

	DB((dbg, LEVEL_2, "After FXCH:\n ")); DEBUG_ONLY(x87_dump_stack(state));
}

/**
 * Convert a virtual register to the stack index.
 *
 * @param state    the x87 state
 * @param reg_idx  the register vfp index
 *
 * @return the stack position where the register is stacked
 *         or -1 if the virtual register was not found
 */
static int x87_on_stack(const x87_state *state, int reg_idx) {
	int i, tos = state->tos;

	for (i = 0; i < state->depth; ++i)
		if (state->st[MASK_TOS(tos + i)].reg_idx == reg_idx)
			return i;
	return -1;
}

/**
 * Push a virtual Register onto the stack.
 *
 * @param state    the x87 state
 * @param reg_idx  the register vfp index
 * @param node     the node that produces the value of the vfp register
 */
static void x87_push(x87_state *state, int reg_idx, ir_node *node) {
	assert(x87_on_stack(state, reg_idx) == -1 && "double push");
	assert(state->depth < N_x87_REGS && "stack overrun");

	++state->depth;
	state->tos = MASK_TOS(state->tos - 1);
	state->st[state->tos].reg_idx = reg_idx;
	state->st[state->tos].node    = node;

	DB((dbg, LEVEL_2, "After PUSH:\n ")); DEBUG_ONLY(x87_dump_stack(state));
}

/**
 * Pop a virtual Register from the stack.
 */
static void x87_pop(x87_state *state) {
	assert(state->depth > 0 && "stack underrun");

	--state->depth;
	state->tos = MASK_TOS(state->tos + 1);

	DB((dbg, LEVEL_2, "After POP:\n ")); DEBUG_ONLY(x87_dump_stack(state));
}

/**
 * Returns the block state of a block.
 *
 * @param sim    the x87 simulator handle
 * @param block  the current block
 *
 * @return the block state
 */
static blk_state *x87_get_bl_state(x87_simulator *sim, ir_node *block) {
	pmap_entry *entry = pmap_find(sim->blk_states, block);

	if (! entry) {
		blk_state *bl_state = obstack_alloc(&sim->obst, sizeof(*bl_state));
		bl_state->begin = NULL;
		bl_state->end   = NULL;

		pmap_insert(sim->blk_states, block, bl_state);
		return bl_state;
	}

	return entry ? PTR_TO_BLKSTATE(entry->value) : NULL;
}

/**
 * Creates a new x87 state.
 *
 * @param sim    the x87 simulator handle
 * @return a new x87 state
 */
static x87_state *x87_alloc_state(x87_simulator *sim) {
	x87_state *res = obstack_alloc(&sim->obst, sizeof(*res));
	return res;
}

/**
 * Create a new empty x87 state.
 *
 * @param sim    the x87 simulator handle
 * @return a new empty x87 state
 */
static x87_state *x87_alloc_empty_state(x87_simulator *sim) {
	x87_state *res = x87_alloc_state(sim);

	x87_flush(res);
	return res;
}

/**
 * Clone a x87 state.
 *
 * @param sim    the x87 simulator handle
 * @param src    the x87 state that will be cloned
 *
 * @return a cloned copy of the src state
 */
static x87_state *x87_clone_state(x87_simulator *sim, const x87_state *src) {
	x87_state *res = x87_alloc_state(sim);

	memcpy(res, src, sizeof(*res));
	return res;
}

/**
 * Patch a virtual instruction into a x87 one and return
 * the value node.
 *
 * @param n   the IR node to patch
 * @param op  the x87 opcode to patch in
 */
static ir_node *x87_patch_insn(ir_node *n, ir_op *op) {
	ir_mode *mode = get_irn_mode(n);
	ir_node *res = n;

	set_irn_op(n, op);

	if (mode == mode_T) {
		/* patch all Proj's */
		const ir_edge_t *edge;

		foreach_out_edge(n, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			if (is_Proj(proj)) {
				mode = get_irn_mode(proj);
				if (mode_is_float(mode)) {
					res = proj;
					set_irn_mode(proj, mode_E);
				}
			}
		}
	}
	else if (mode_is_float(mode))
		set_irn_mode(n, mode_E);
	return res;
}

/* -------------- x87 perm --------------- */

/**
 * Creates a fxch for shuffle.
 *
 * @param state     the x87 state
 * @param pos       parameter for fxch
 * @param dst_block the block of the user
 *
 * Creates a new fxch node and reroute the user of the old node
 * to the fxch.
 *
 * @return the fxch node
 */
static ir_node *x87_fxch_shuffle(x87_state *state, int pos, ir_node *block, ir_node *dst_block)
{
	const ir_edge_t *edge;
	ir_node *n = x87_get_st_node(state, pos);
	ir_node *user = NULL;
	ir_node *fxch;
	int node_idx;
	ia32_attr_t *attr;

	if (block == get_nodes_block(n)) {
		/* this is a node from out block: change it's user */
		foreach_out_edge(n, edge) {
			ir_node *succ = get_edge_src_irn(edge);

			if (is_Phi(succ) && get_nodes_block(succ) == dst_block) {
				user = succ;
				node_idx = get_edge_src_pos(edge);
				break;
			}
		}
		assert(user);
	}

	fxch = new_rd_ia32_fxch(NULL, get_irn_irg(block), block, n, get_irn_mode(n));
	attr = get_ia32_attr(fxch);
	attr->x87[0] = &ia32_st_regs[pos];
	attr->x87[2] = &ia32_st_regs[0];

	if (user) {
		DB((dbg, LEVEL_2, "%+F replaced input %d of %+F\n", fxch, node_idx, user));
		set_irn_n(user, node_idx, fxch);
	}
	else {
		/*
		 * This is a node from a dominator block. Changing it's user might be wrong,
		 * so just keep it alive.
		 * The "right" solution would require a new Phi, but we don't care here.
		 */
		keep_alive(fxch);
	}

	x87_fxch(state, pos);
	return fxch;
}

/**
 * Calculate the necessary permutations to reach dst_state.
 *
 * These permutations are done with fxch instructions and placed
 * at the end of the block.
 *
 * Note that critical edges are removed here, so we need only
 * a shuffle if the current block has only one successor.
 *
 * @param sim        the simulator handle
 * @param block      the current block
 * @param state      the current x87 stack state, might be modified
 * @param dst_block  the destination block
 * @param dst_state  destination state
 *
 * @return state
 */
static x87_state *x87_shuffle(x87_simulator *sim, ir_node *block, x87_state *state, ir_node *dst_block, const x87_state *dst_state) {
	int i, n_cycles, k, ri;
	unsigned cycles[4], all_mask;
	char cycle_idx[4][8];
	ir_node *fxch;
	ir_node *before, *after;

	assert(state->depth == dst_state->depth);

	/* Some mathematics here:
	   If we have a cycle of lenght n that includes the tos,
	   we need n-1 exchange operations.
	   We can always add the tos and restore it, so we need
	   n+1 exchange operations for a cycle not containing the tos.
	   So, the maximum of needed operations is for a cycle of 7
	   not including the tos == 8.
	   This is so same number of ops we would need for store,
	   so exchange is cheaper (we save the loads).
	   On the other hand, we might need an additional exchange
	   in the next block to bring one operand on top, so the
	   number of ops in the first case is identical.
		 Further, no more than 4 cycles can exists.
	*/
	all_mask = (1 << (state->depth)) - 1;

	for (n_cycles = 0; all_mask; ++n_cycles) {
		int src_idx, dst_idx;

		/* find the first free slot */
		for (i = 0; i < state->depth; ++i) {
			if (all_mask & (1 << i)) {
				all_mask &= ~(1 << i);

				/* check if there are differences here */
				if (x87_get_st_reg(state, i) != x87_get_st_reg(dst_state, i))
					break;
			}
		}

		if (! all_mask) {
			/* no more cycles found */
			break;
		}

		k = 0;
		cycles[n_cycles] = (1 << i);
		cycle_idx[n_cycles][k++] = i;
		for (src_idx = i; ; src_idx = dst_idx) {
			dst_idx = x87_on_stack(dst_state, x87_get_st_reg(state, src_idx));

			if ((all_mask & (1 << dst_idx)) == 0)
				break;

			cycle_idx[n_cycles][k++] = dst_idx;
			cycles[n_cycles] |=  (1 << dst_idx);
			all_mask       &= ~(1 << dst_idx);
		}
		cycle_idx[n_cycles][k] = -1;
	}

	if (n_cycles <= 0) {
		/* no permutation needed */
		return state;
	}

	/* Hmm: permutation needed */
	DB((dbg, LEVEL_2, "\n%+F needs permutation: from\n", block));
	DEBUG_ONLY(x87_dump_stack(state));
	DB((dbg, LEVEL_2, "                  to\n"));
	DEBUG_ONLY(x87_dump_stack(dst_state));


#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_2, "Need %d cycles\n", n_cycles));
	for (ri = 0; ri < n_cycles; ++ri) {
		DB((dbg, LEVEL_2, " Ring %d:\n ", ri));
		for (k = 0; cycle_idx[ri][k] != -1; ++k)
			DB((dbg, LEVEL_2, " st%d ->", cycle_idx[ri][k]));
		DB((dbg, LEVEL_2, "\n"));
	}
#endif

	after = NULL;

	/*
	 * Find the place node must be insert.
	 * We have only one successor block, so the last instruction should
	 * be a jump.
	 */
	before = sched_last(block);
	assert(is_cfop(before));

	/* now do the permutations */
	for (ri = 0; ri < n_cycles; ++ri) {
		if ((cycles[ri] & 1) == 0) {
			/* this cycle does not include the tos */
			fxch = x87_fxch_shuffle(state, cycle_idx[ri][0], block, dst_block);
			if (after)
				sched_add_after(after, fxch);
			else
				sched_add_before(before, fxch);
			after = fxch;
		}
		for (k = 1; cycle_idx[ri][k] != -1; ++k) {
			fxch = x87_fxch_shuffle(state, cycle_idx[ri][k], block, dst_block);
			if (after)
				sched_add_after(after, fxch);
			else
				sched_add_before(before, fxch);
			after = fxch;
		}
		if ((cycles[ri] & 1) == 0) {
			/* this cycle does not include the tos */
			fxch = x87_fxch_shuffle(state, cycle_idx[ri][0], block, dst_block);
			sched_add_after(after, fxch);
		}
	}
	return state;
}

/**
 * Create a fxch node before another node.
 *
 * @param state   the x87 state
 * @param n       the node before the fxch
 * @param pos     exchange st(pos) with st(0)
 * @param op_idx  if >= 0, replace input op_idx of n with the fxch result
 *
 * @return the fxch
 */
static ir_node *x87_create_fxch(x87_state *state, ir_node *n, int pos, int op_idx) {
	ir_node *fxch, *pred;
	ia32_attr_t *attr;

	x87_fxch(state, pos);

	if (op_idx >= 0)
		pred = get_irn_n(n, op_idx);
	else
		pred = x87_get_st_node(state, pos);

	fxch = new_rd_ia32_fxch(NULL, get_irn_irg(n), get_nodes_block(n), pred, get_irn_mode(pred));
	attr = get_ia32_attr(fxch);
	attr->x87[0] = &ia32_st_regs[pos];
	attr->x87[2] = &ia32_st_regs[0];

	if (op_idx >= 0)
		set_irn_n(n, op_idx, fxch);

	sched_add_before(n, fxch);
	DB((dbg, LEVEL_1, "<<< %s %s, %s\n", get_irn_opname(fxch), attr->x87[0]->name, attr->x87[2]->name));
	return fxch;
}

/**
 * Create a fpush before node n.
 *
 * @param state   the x87 state
 * @param n       the node before the fpush
 * @param pos     push st(pos) on stack
 * @param op_idx  if >= 0, replace input op_idx of n with the fpush result
 */
static void x87_create_fpush(const arch_env_t *env, x87_state *state, ir_node *n, int pos, int op_idx) {
	ir_node *fpush, *pred;
	ia32_attr_t *attr;
	const arch_register_t *out = arch_get_irn_register(env, n);

	x87_push(state, arch_register_get_index(out), n);

	pred = get_irn_n(n, op_idx);
	fpush = new_rd_ia32_fpush(NULL, get_irn_irg(n), get_nodes_block(n), pred, get_irn_mode(pred));
	attr = get_ia32_attr(fpush);
	attr->x87[0] = &ia32_st_regs[pos];
	attr->x87[2] = &ia32_st_regs[0];
	if (op_idx >= 0)
		set_irn_n(n, op_idx, fpush);

	sched_add_before(n, fpush);
	DB((dbg, LEVEL_1, "<<< %s %s, %s\n", get_irn_opname(fpush), attr->x87[0]->name, attr->x87[2]->name));
}

/**
 * Create a fpop before node n.
 *
 * @param state   the x87 state
 * @param n       the node before the fpop
 * @param num     pop 1 or 2 values
 * @param pred    node to use as predecessor of the fpop
 *
 * @return the fpop node
 */
static ir_node *x87_create_fpop(const arch_env_t *env, x87_state *state, ir_node *n, int num, ir_node *pred) {
	ir_node *fpop;
	ia32_attr_t *attr;

	while (num > 0) {
		x87_pop(state);
		fpop = new_rd_ia32_fpop(NULL, get_irn_irg(n), get_nodes_block(n), pred, mode_E);
		attr = get_ia32_attr(fpop);
		attr->x87[0] = &ia32_st_regs[0];
		attr->x87[1] = &ia32_st_regs[0];
		attr->x87[2] = &ia32_st_regs[0];

		sched_add_before(n, fpop);
		DB((dbg, LEVEL_1, "<<< %s %s\n", get_irn_opname(fpop), attr->x87[0]->name));

		pred = fpop;
		--num;
	}
	return fpop;
}

/* --------------------------------- liveness ------------------------------------------ */

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node get inserted.
 * @param arch_env The architecture environment.
 * @param irn      The node at which liveness should be computed.
 * @param live     The bitset of registers live before @p irn. This set gets modified by updating it to
 *                 the registers live after irn.
 * @return live.
 */
static unsigned vfp_liveness_transfer(const arch_env_t *arch_env, ir_node *irn, unsigned live)
{
	int i, n;
	const arch_register_class_t *cls = &ia32_reg_classes[CLASS_ia32_vfp];

	if (arch_irn_consider_in_reg_alloc(arch_env, cls, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			live &= ~(1 << reg->index);
	}

	for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if (mode_is_float(get_irn_mode(op)) && arch_irn_consider_in_reg_alloc(arch_env, cls, op)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, op);
			live |= 1 << reg->index;
		}
	}

	return live;
}

/**
 * Put all live virtual registers at the end of a block into a bitset.
 * @param arch_env The architecture environment.
 * @param bl       The block.
 * @return The live bitset.
 */
static unsigned vfp_liveness_end_of_block(const arch_env_t *arch_env, const ir_node *bl)
{
	irn_live_t *li;
	unsigned live = 0;
	const arch_register_class_t *cls = &ia32_reg_classes[CLASS_ia32_vfp];

	live_foreach(bl, li) {
		ir_node *irn = (ir_node *) li->irn;
		if (live_is_end(li) && arch_irn_consider_in_reg_alloc(arch_env, cls, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			live |= 1 << reg->index;
		}
	}

	return live;
}

/**
 * Compute a bitset of registers which are live at another node.
 * @param arch_env The architecture environment.
 * @param pos      The node.
 * @return The live bitset.
 */
static unsigned vfp_liveness_nodes_live_at(const arch_env_t *arch_env, const ir_node *pos)
{
	const ir_node *bl = is_Block(pos) ? pos : get_nodes_block(pos);
	const arch_register_class_t *cls = &ia32_reg_classes[CLASS_ia32_vfp];
	ir_node *irn;
	unsigned live;

	live = vfp_liveness_end_of_block(arch_env, bl);

	sched_foreach_reverse(bl, irn) {
		/*
		 * If we encounter the node we want to insert the Perm after,
		 * exit immediately, so that this node is still live
		 */
		if (irn == pos)
			return live;

		live = vfp_liveness_transfer(arch_env, irn, live);
	}

	return live;
}

/**
 * Returns true if a register is live in a set.
 *
 * @param reg_idx  the vfp register index
 * @param live     a live bitset
 */
static unsigned is_vfp_live(int reg_idx, unsigned live) {
	return live & (1 << reg_idx);
}

#ifdef DEBUG_libfirm
/**
 * Dump liveness info.
 *
 * @param live  the live bitset
 */
static void vfp_dump_live(unsigned live) {
	int i;

	DB((dbg, LEVEL_2, "Live registers here: \n"));
	for (i = 0; i < 8; ++i) {
		if (live & (1 << i)) {
			DB((dbg, LEVEL_2, " vf%d", i));
		}
	}
	DB((dbg, LEVEL_2, "\n"));
}
#endif /* DEBUG_libfirm */

/* --------------------------------- simulators ---------------------------------------- */

#define XCHG(a, b) do { int t = (a); (a) = (b); (b) = t; } while (0)

/**
 * Simulate a virtual binop.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 * @param tmpl   the template containing the 4 possible x87 opcodes
 */
static void sim_binop(x87_state *state, ir_node *n, const arch_env_t *env, const exchange_tmpl *tmpl) {
	int op2_idx, op1_idx = -1;
	int out_idx, do_pop =0;
	ia32_attr_t *attr;
	ir_op *dst;
	const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_1));
	const arch_register_t *op2 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_2));
	const arch_register_t *out = arch_get_irn_register(env, n);
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	DB((dbg, LEVEL_1, ">>> %s %s, %s -> %s\n", get_irn_opname(n),
		arch_register_get_name(op1), arch_register_get_name(op2),
		arch_register_get_name(out)));
	DEBUG_ONLY(vfp_dump_live(live));

	op1_idx = x87_on_stack(state, arch_register_get_index(op1));
	op2_idx = x87_on_stack(state, arch_register_get_index(op2));

	if (op2->index != REG_VFP_NOREG) {
		/* second operand is a vfp register */

		if (is_vfp_live(op2->index, live)) {
			/* Second operand is live. */

			if (is_vfp_live(op1->index, live)) {
				/* Both operands are live: push the first one.
				   This works even for op1 == op2. */
				x87_create_fpush(env, state, n, op2_idx, BINOP_IDX_2);
				out_idx = op2_idx = 0;
				++op1_idx;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
			else {
				/* Second live, first operand is dead here, bring it to tos. */
				if (op1_idx != 0) {
					x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
					if (op2_idx == 0)
						op2_idx = op1_idx;
				}
				op1_idx = out_idx = 0;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
		}
		else {
			/* Second operand is dead. */
			if (is_vfp_live(op1->index, live)) {
				/* First operand is live: bring second to tos. */
				if (op2_idx != 0) {
					x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
					if (op1_idx == 0)
						op1_idx = op2_idx;
				}
				op2_idx = out_idx = 0;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
			else {
				/* Both operands are dead here, pop them from the stack. */
				if (op2_idx == 0) {
					out_idx = op1_idx;
					XCHG(op2_idx, op1_idx);
					if (op1_idx == op2_idx) {
						/* Both are identically, no pop needed. */
						dst = tmpl->reverse_op;
						do_pop = 0;
					}
					else {
						dst = tmpl->reverse_pop_op;
						do_pop = 1;
					}
				}
				else if (op1_idx == 0) {
					out_idx = op2_idx;
					if (op1_idx == op2_idx) {
						/* Both are identically, no pop needed. */
						dst = tmpl->normal_op;
						do_pop = 0;
					}
					else {
						dst = tmpl->normal_pop_op;
						do_pop = 1;
					}
				}
				else {
					/* Bring the first on top. */
					x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
					if (op1_idx == op2_idx) {
						/* Both are identically, no pop needed. */
						out_idx = op1_idx = op2_idx = 0;
						dst = tmpl->normal_op;
						do_pop = 0;
					}
					else {
						op1_idx = 0;
						out_idx = op2_idx;
						dst = tmpl->normal_pop_op;
						do_pop = 1;
					}
				}
			}
		}
	}
	else {
		/* second operand is an address mode */
		if (is_vfp_live(op1->index, live)) {
			/* first operand is live: push it here */
			x87_create_fpush(env, state, n, op1_idx, BINOP_IDX_1);
		}
		else {
			/* first operand is dead: bring it to tos */
			if (op1_idx != 0)
				x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
		}
		op1_idx = out_idx = 0;
		dst = tmpl->normal_op;
		do_pop = 0;
	}

	x87_set_st(state, arch_register_get_index(out), x87_patch_insn(n, dst), out_idx);
	if (do_pop)
		x87_pop(state);

	/* patch the operation */
	attr = get_ia32_attr(n);
	attr->x87[0] = op1 = &ia32_st_regs[op1_idx];
	if (op2_idx >= 0)
		attr->x87[1] = op2 = &ia32_st_regs[op2_idx];
	attr->x87[2] = out = &ia32_st_regs[out_idx];

	if (op2_idx > 0)
		DB((dbg, LEVEL_1, "<<< %s %s, %s -> %s\n", get_irn_opname(n),
			arch_register_get_name(op1), arch_register_get_name(op2),
			arch_register_get_name(out)));
	else
		DB((dbg, LEVEL_1, "<<< %s %s, [AM] -> %s\n", get_irn_opname(n),
			arch_register_get_name(op1),
			arch_register_get_name(out)));
}

/**
 * Simulate a virtual Unop.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 * @param op     the x87 opcode that will replace n's opcode
 */
static void sim_unop(x87_state *state, ir_node *n, const arch_env_t *env, ir_op *op) {
	int op1_idx, out_idx;
	const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, UNOP_IDX));
	const arch_register_t *out = arch_get_irn_register(env, n);
	ia32_attr_t *attr;
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	DB((dbg, LEVEL_1, ">>> %s -> %s\n", get_irn_opname(n), out->name));
	DEBUG_ONLY(vfp_dump_live(live));

	op1_idx = x87_on_stack(state, arch_register_get_index(op1));

	if (is_vfp_live(op1->index, live)) {
		/* push the operand here */
		x87_create_fpush(env, state, n, op1_idx, UNOP_IDX);
	}
	else {
		/* operand is dead, bring it to tos */
		if (op1_idx != 0)
			x87_create_fxch(state, n, op1_idx, UNOP_IDX);
	}

	x87_set_tos(state, arch_register_get_index(out), x87_patch_insn(n, op));
	op1_idx = out_idx = 0;
	attr = get_ia32_attr(n);
	attr->x87[0] = op1 = &ia32_st_regs[0];
	attr->x87[2] = out = &ia32_st_regs[0];
	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(n), out->name));
}

/**
 * Simulate a virtual Load instruction.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 * @param op     the x87 opcode that will replace n's opcode
 */
static void sim_load(x87_state *state, ir_node *n, const arch_env_t *env, ir_op *op) {
	const arch_register_t *out = arch_get_irn_register(env, n);
	ia32_attr_t *attr;

	DB((dbg, LEVEL_1, ">>> %s -> %s\n", get_irn_opname(n), arch_register_get_name(out)));
	x87_push(state, arch_register_get_index(out), x87_patch_insn(n, op));
	attr = get_ia32_attr(n);
	attr->x87[2] = out = &ia32_st_regs[0];
	DB((dbg, LEVEL_1, "<<< %s -> %s\n", get_irn_opname(n), arch_register_get_name(out)));
}

/**
 * Simulate a virtual Store.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 * @param op     the x87 store opcode
 * @param op_p   the x87 store and pop opcode
 */
static void sim_store(x87_state *state, ir_node *n, const arch_env_t *env, ir_op *op, ir_op *op_p) {
	int op2_idx;
	const arch_register_t *op2 = arch_get_irn_register(env, get_irn_n(n, STORE_VAL_IDX));
	ia32_attr_t *attr;
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	op2_idx = x87_on_stack(state, arch_register_get_index(op2));
	assert(op2_idx >= 0);

	DB((dbg, LEVEL_1, ">>> %s %s ->\n", get_irn_opname(n), arch_register_get_name(op2)));

	/* we can only store the tos to memory */
	if (op2_idx != 0)
		x87_create_fxch(state, n, op2_idx, STORE_VAL_IDX);

	if (is_vfp_live(op2->index, live))
		x87_patch_insn(n, op);
	else {
		x87_pop(state);
		x87_patch_insn(n, op_p);
	}

	attr = get_ia32_attr(n);
	attr->x87[1] = op2 = &ia32_st_regs[0];
	DB((dbg, LEVEL_1, "<<< %s %s ->\n", get_irn_opname(n), arch_register_get_name(op2)));
}

/**
 * Simulate a virtual Phi.
 * Just for cosmetic reasons change the mode of Phi nodes to mode_E.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 */
static void sim_Phi(x87_state *state, ir_node *n, const arch_env_t *env) {
	ir_mode *mode = get_irn_mode(n);

	if (mode_is_float(mode))
		set_irn_mode(n, mode_E);
}


#define _GEN_BINOP(op, rev) \
static void sim_##op(x87_state *state, ir_node *n, const arch_env_t *env) { \
	exchange_tmpl tmpl = { op_ia32_##op, op_ia32_##rev, op_ia32_##op##p, op_ia32_##rev##p }; \
	sim_binop(state, n, env, &tmpl); \
}

#define GEN_BINOP(op)	  _GEN_BINOP(op, op)
#define GEN_BINOPR(op)	_GEN_BINOP(op, op##r)

#define GEN_LOAD2(op, nop) \
static void sim_##op(x87_state *state, ir_node *n, const arch_env_t *env) { \
	sim_load(state, n, env, op_ia32_##nop); \
}

#define GEN_LOAD(op)	GEN_LOAD2(op, op)

#define GEN_UNOP(op) \
static void sim_##op(x87_state *state, ir_node *n, const arch_env_t *env) { \
	sim_unop(state, n, env, op_ia32_##op); \
}

#define GEN_STORE(op) \
static void sim_##op(x87_state *state, ir_node *n, const arch_env_t *env) { \
	sim_store(state, n, env, op_ia32_##op, op_ia32_##op##p); \
}

/* all stubs */
GEN_BINOP(fadd)
GEN_BINOPR(fsub)
GEN_BINOP(fmul)
GEN_BINOPR(fdiv)

GEN_UNOP(fabs)
GEN_UNOP(fchs)
GEN_UNOP(fsin)
GEN_UNOP(fcos)
GEN_UNOP(fsqrt)

GEN_LOAD(fld)
GEN_LOAD(fild)
GEN_LOAD(fldz)
GEN_LOAD(fld1)
GEN_LOAD2(fConst, fldConst)

GEN_STORE(fst)
GEN_STORE(fist)

/**
 * Simulate a fCondJmp.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 */
static void sim_fCondJmp(x87_state *state, ir_node *n, const arch_env_t *env) {
	int op2_idx, op1_idx = -1, pop_cnt = 0;
	ia32_attr_t *attr;
	ir_op *dst;
	const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_1));
	const arch_register_t *op2 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_2));
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	DB((dbg, LEVEL_1, ">>> %s %s, %s\n", get_irn_opname(n),
		arch_register_get_name(op1), arch_register_get_name(op2)));
	DEBUG_ONLY(vfp_dump_live(live));

	op1_idx = x87_on_stack(state, arch_register_get_index(op1));
	op2_idx = x87_on_stack(state, arch_register_get_index(op2));

	/* BEWARE: check for comp a,a cases, they might happen */
	if (op2->index != REG_VFP_NOREG) {
		/* second operand is a vfp register */

		if (is_vfp_live(op2->index, live)) {
			/* second operand is live */

			if (is_vfp_live(op1->index, live)) {
				/* both operands are live: move one of them to tos */
				if (op2_idx == 0) {
					XCHG(op2_idx, op1_idx);
					dst = op_ia32_fcomrJmp;
				}
				else if (op1_idx == 0) {
					dst = op_ia32_fcomJmp;
				}
				else {
					/* bring the first on top */
					x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
					if (op1_idx == op2_idx)
						op2_idx = 0;
					op1_idx = 0;
					dst     = op_ia32_fcomJmp;
				}
			}
			else {
				/* second live, first operand is dead here, bring it to tos.
				   This means further, op1_idx != op2_idx. */
				if (op1_idx != 0) {
					x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
					if (op2_idx == 0)
						op2_idx = op1_idx;
				}
				op1_idx = 0;
				dst     = op_ia32_fcompJmp;
				pop_cnt = 1;
			}
		}
		else {
			/* second operand is dead */
			if (is_vfp_live(op1->index, live)) {
				/* first operand is live: bring second to tos.
				   This means further, op1_idx != op2_idx. */
				if (op2_idx != 0) {
					x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
					if (op1_idx == 0)
						op1_idx = op2_idx;
				}
				op2_idx = 0;
				dst     = op_ia32_fcomrpJmp;
				pop_cnt = 1;
			}
			else {
				/* both operands are dead here, check first for identity. */
				if (op1_idx == op2_idx) {
					/* identically, one one needed */
					if (op1_idx != 0) {
						x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
						op1_idx = op2_idx = 0;
					}
					dst     = op_ia32_fcompJmp;
					pop_cnt = 1;
				}
				/* different, move them to st and st(1) and pop both.
				   The tricky part is to get one into st(1).*/
				else if (op2_idx == 1) {
					/* good, second operand is already in the right place, move the first */
					if (op1_idx != 0) {
						/* bring the first on top */
						x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
						op1_idx = 0;
					}
					dst     = op_ia32_fcomppJmp;
					pop_cnt = 2;
				}
				else if (op1_idx == 1) {
					/* good, first operand is already in the right place, move the second */
					if (op2_idx != 0) {
						/* bring the first on top */
						x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
						op2_idx = 0;
					}
					dst     = op_ia32_fcomrppJmp;
					pop_cnt = 2;
				}
				else {
					/* if one is already the TOS, we need two fxch */
					if (op1_idx == 0) {
						/* first one is TOS, move to st(1) */
						x87_create_fxch(state, n, 1, BINOP_IDX_1);
						op1_idx = 1;
						x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
						op2_idx = 0;
						dst     = op_ia32_fcomrppJmp;
						pop_cnt = 2;
					}
					else if (op2_idx == 0) {
						/* second one is TOS, move to st(1) */
						x87_create_fxch(state, n, 1, BINOP_IDX_2);
						op2_idx = 1;
						x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
						op1_idx = 0;
						dst     = op_ia32_fcomrppJmp;
						pop_cnt = 2;
					}
					else {
						/* none of them is either TOS or st(1), 3 fxch needed */
						x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
						x87_create_fxch(state, n, 1, BINOP_IDX_2);
						x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
						op1_idx = 0;
						op2_idx = 1;
						dst     = op_ia32_fcomppJmp;
						pop_cnt = 2;
					}
				}
			}
		}
	}
	else {
		/* second operand is an address mode */
		if (is_vfp_live(op1->index, live)) {
			/* first operand is live: bring it to TOS */
			if (op1_idx != 0) {
				x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
				op1_idx = 0;
			}
			dst = op_ia32_fcomJmp;
		}
		else {
			/* first operand is dead: bring it to tos */
			if (op1_idx != 0) {
				x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
				op1_idx = 0;
			}
		}
		dst     = op_ia32_fcompJmp;
		pop_cnt = 1;
	}

	x87_patch_insn(n, dst);
	if (pop_cnt > 1)
		x87_pop(state);
	if (pop_cnt > 0)
		x87_pop(state);

	/* patch the operation */
	attr = get_ia32_attr(n);
	attr->x87[0] = op1 = &ia32_st_regs[op1_idx];
	if (op2_idx >= 0)
		attr->x87[1] = op2 = &ia32_st_regs[op2_idx];

	if (op2_idx >= 0)
		DB((dbg, LEVEL_1, "<<< %s %s, %s\n", get_irn_opname(n),
			arch_register_get_name(op1), arch_register_get_name(op2)));
	else
		DB((dbg, LEVEL_1, "<<< %s %s, [AM]\n", get_irn_opname(n),
			arch_register_get_name(op1)));
}

/**
 * Simulate a be_Copy.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 */
static void sim_Copy(x87_state *state, ir_node *n, const arch_env_t *env) {
	ir_mode *mode = get_irn_mode(n);

	if (mode_is_float(mode)) {
		const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, 0));
		const arch_register_t *out = arch_get_irn_register(env, n);
		ir_node *node, *next;
		ia32_attr_t *attr;
		int op1_idx, out_idx;
		unsigned live = vfp_liveness_nodes_live_at(env, n);

		op1_idx = x87_on_stack(state, arch_register_get_index(op1));

		DB((dbg, LEVEL_1, ">>> %s %s -> %s\n", get_irn_opname(n),
			arch_register_get_name(op1), arch_register_get_name(out)));
	  DEBUG_ONLY(vfp_dump_live(live));

		if (is_vfp_live(op1->index, live)) {
			/* operand is still live,a real copy */
			node = new_rd_ia32_fpush(get_irn_dbg_info(n), get_irn_irg(n), get_nodes_block(n), get_irn_n(n, 0), mode);
			arch_set_irn_register(env, node, out);

			x87_push(state, arch_register_get_index(out), node);

			attr = get_ia32_attr(node);
			attr->x87[0] = op1 = &ia32_st_regs[op1_idx];
			attr->x87[2] = out = &ia32_st_regs[0];

			next = sched_next(n);
			sched_remove(n);
			exchange(n, node);
			sched_add_before(next, node);
			DB((dbg, LEVEL_1, ">>> %s %s -> %s\n", get_irn_opname(node), op1->name, out->name));
		}
		else {
			out_idx = x87_on_stack(state, arch_register_get_index(out));

			if (out_idx >= 0 && out_idx != op1_idx) {
				/* op1 must be killed and placed where out is */
				if (out_idx == 0) {
					/* best case, simple remove and rename */
					x87_patch_insn(n, op_ia32_Pop);
					attr = get_ia32_attr(n);
					attr->x87[0] = op1 = &ia32_st_regs[0];

					x87_pop(state);
					x87_set_st(state, arch_register_get_index(out), n, op1_idx - 1);
				}
				else {
					/* move op1 to tos, store and pop it */
					if (op1_idx != 0) {
						x87_create_fxch(state, n, op1_idx, 0);
						op1_idx = 0;
					}
					x87_patch_insn(n, op_ia32_Pop);
					attr = get_ia32_attr(n);
					attr->x87[0] = op1 = &ia32_st_regs[out_idx];

					x87_pop(state);
					x87_set_st(state, arch_register_get_index(out), n, out_idx - 1);
				}
				DB((dbg, LEVEL_1, ">>> %s %s\n", get_irn_opname(n), op1->name));
			}
			else {
				/* just a virtual copy */
				x87_set_st(state, arch_register_get_index(out), get_unop_op(n), op1_idx);
				sched_remove(n);
				DB((dbg, LEVEL_1, ">>> KILLED %s\n", get_irn_opname(n)));
				exchange(n, get_unop_op(n));
			}
		}
	}
}

/**
 * Simulate a be_Call.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated
 * @param env    the architecture environment
 */
static void sim_Call(x87_state *state, ir_node *n, const arch_env_t *env) {
	ir_type *call_tp = be_Call_get_type(n);

	/* at the begin of a call the x87 state should be empty */
	assert(state->depth == 0 && "stack not empty before call");

	/*
	 * If the called function returns a float, it is returned in st(0).
	 * This even happens if the return value is NOT used.
	 * Moreover, only one return result is supported.
	 */
	if (get_method_n_ress(call_tp) > 0) {
		ir_type *res_type = get_method_res_type(call_tp, 0);
		ir_mode *mode     = get_type_mode(res_type);

		if (mode && mode_is_float(mode)) {
			/*
			 * TODO: what to push here? The result might be unused and currently
			 * we have no possibility to detect this :-(
			 */
			x87_push(state, 0, n);
		}
	}
}

/**
 * Simulate a be_Spill.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 *
 * Should not happen, spills are lowered before x87 simulator see them.
 */
static void sim_Spill(x87_state *state, ir_node *n, const arch_env_t *env) {
	assert(0 && "Spill not lowered");
	sim_fst(state, n, env);
}

/**
 * Simulate a be_Reload.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 *
 * Should not happen, reloads are lowered before x87 simulator see them.
 */
static void sim_Reload(x87_state *state, ir_node *n, const arch_env_t *env) {
	assert(0 && "Reload not lowered");
	sim_fld(state, n, env);
}

/**
 * Simulate a be_Return.
 *
 * @param state  the x87 state
 * @param n      the node that should be simulated (and patched)
 * @param env    the architecture environment
 */
static void sim_Return(x87_state *state, ir_node *n, const arch_env_t *env) {
	int n_res = be_Return_get_n_rets(n);
	int i, n_float_res = 0;

	/* only floating point return values must resist on stack */
	for (i = 0; i < n_res; ++i) {
		ir_node *res = get_irn_n(n, be_pos_Return_val + i);

		if (mode_is_float(get_irn_mode(res)))
			++n_float_res;
	}
	assert(x87_get_depth(state) == n_float_res);

	/* pop them virtually */
	for (i = n_float_res - 1; i >= 0; --i)
		x87_pop(state);
}

/**
 * Kill any dead registers at block start by popping them from the stack.
 *
 * @param sim          the simulator handle
 * @param block        the current block
 * @param start_state  the x87 state at the begin of the block
 */
static x87_state *x87_kill_deads(x87_simulator *sim, ir_node *block, x87_state *start_state) {
	x87_state *state = start_state;
	ir_node *first_insn = sched_first(block);
	ir_node *keep = NULL;
	unsigned live = vfp_liveness_nodes_live_at(sim->env, block);
	unsigned kill_mask;
	int i, depth, num_pop;

	kill_mask = 0;
	depth = x87_get_depth(state);
	for (i = depth - 1; i >= 0; --i) {
		int reg = x87_get_st_reg(state, i);

		if (! is_vfp_live(reg, live))
			kill_mask |= (1 << i);
	}

	if (kill_mask) {
		/* create a new state, will be changed */
		state = x87_clone_state(sim, state);

		DB((dbg, LEVEL_1, "Killing deads:\n"));
		DEBUG_ONLY(vfp_dump_live(live));
		DEBUG_ONLY(x87_dump_stack(state));

		/* now kill registers */
		while (kill_mask) {
			/* we can only kill from TOS, so bring them up */
			if (! (kill_mask & 1)) {
				/* search from behind, because we can to a double-pop */
				for (i = depth - 1; i >= 0; --i) {
					if (kill_mask & (1 << i)) {
						kill_mask &= ~(1 << i);
						kill_mask |= 1;
						break;
					}
				}

				if (keep)
					x87_set_st(state, -1, keep, i);
				keep = x87_create_fxch(state, first_insn, i, -1);
			}
			else if (! keep)
				keep = x87_get_st_node(state, 0);

			if ((kill_mask & 3) == 3) {
				/* we can do a double-pop */
				num_pop = 2;
			}
			else {
				/* only a single pop */
				num_pop = 1;
			}

			depth -= num_pop;
			kill_mask >>= num_pop;
			keep = x87_create_fpop(sim->env, state, first_insn, num_pop, keep);
		}
		add_End_keepalive(get_irg_end(get_irn_irg(block)), keep);
	}
	return state;
}

/**
 * Run a simulation and fix all virtual instructions for a block.
 *
 * @param sim          the simulator handle
 * @param block        the current block
 *
 * @return non-zero if simulation is complete,
 *         zero if the simulation must be rerun
 */
static int x87_simulate_block(x87_simulator *sim, ir_node *block) {
	ir_node *n, *next;
	blk_state *bl_state = x87_get_bl_state(sim, block);
	x87_state *state = bl_state->begin;
	const ir_edge_t *edge;
	ir_node *start_block;

	/* if we have no assigned start state, we must wait ... */
	if (! state)
		return 0;

	assert(bl_state->end == NULL);

	DB((dbg, LEVEL_1, "Simulate %+F\n", block));

	/* at block begin, kill all dead registers */
	state = x87_kill_deads(sim, block, state);

	/* beware, n might changed */
	for (n = sched_first(block); !sched_is_end(n); n = next) {
		ir_op *op = get_irn_op(n);

		next = sched_next(n);
		if (op->ops.generic) {
			sim_func func = (sim_func)op->ops.generic;

			/* have work to do */
			if (state == bl_state->begin) {
				/* create a new state, will be changed */
				state = x87_clone_state(sim, state);
			}

			/* simulate it */
			(*func)(state, n, sim->env);
		}
	}

	start_block = get_irg_start_block(get_irn_irg(block));

	/* check if the state must be shuffled */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		blk_state *succ_state = x87_get_bl_state(sim, succ);

		if (succ_state->begin && succ != start_block) {
			/* There is already a begin state for this block, bad.
			   Do the necessary permutations.
			   Note that critical edges are removed, so this is always possible. */
			x87_shuffle(sim, block, state, succ, succ_state->begin);

			/* Note further, that there can be only one such situation,
			   so we can break here. */
			break;
		}
	}
	bl_state->end = state;

	/* now propagate the state to all successor blocks */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		blk_state *succ_state = x87_get_bl_state(sim, succ);

		if (! succ_state->begin)
			succ_state->begin = state;
	}

	return 1;
}

/**
 * Create a new x87 simulator.
 *
 * @param sim   a simulator handle, will be initialized
 * @param irg   the current graph
 * @param env   the architecture environment
 */
static void x87_init_simulator(x87_simulator *sim, ir_graph *irg, const arch_env_t *env) {
	obstack_init(&sim->obst);
	sim->blk_states = pmap_create();
	sim->env        = env;

	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.x87");

	DB((dbg, LEVEL_1, "--------------------------------\n"
		"x87 Simulator started for %+F\n", irg));

  /* set the generic function pointer of instruction we must simulate */
	clear_irp_opcodes_generic_func();

#define ASSOC(op)       (op_ ## op)->ops.generic = (op_func)(sim_##op)
#define ASSOC_IA32(op)  (op_ia32_v ## op)->ops.generic = (op_func)(sim_##op)
#define ASSOC_BE(op)    (op_be_ ## op)->ops.generic = (op_func)(sim_##op)
	ASSOC_IA32(fConst);
	ASSOC_IA32(fld);
	ASSOC_IA32(fild);
	ASSOC_IA32(fld1);
	ASSOC_IA32(fldz);
	ASSOC_IA32(fadd);
	ASSOC_IA32(fsub);
	ASSOC_IA32(fmul);
	ASSOC_IA32(fdiv);
	ASSOC_IA32(fldz);
	ASSOC_IA32(fabs);
	ASSOC_IA32(fchs);
	ASSOC_IA32(fsin);
	ASSOC_IA32(fcos);
	ASSOC_IA32(fsqrt);
	ASSOC_IA32(fist);
	ASSOC_IA32(fst);
	ASSOC_IA32(fCondJmp);
	ASSOC_BE(Copy);
	ASSOC_BE(Call);
	ASSOC_BE(Spill);
	ASSOC_BE(Reload);
	ASSOC_BE(Return);
	ASSOC(Phi);
#undef ASSOC_BE
#undef ASSOC_IA32
#undef ASSOC
}

/**
 * Destroy a x87 simulator.
 *
 * @param sim  the simulator handle
 */
static void x87_destroy_simulator(x87_simulator *sim) {
	pmap_destroy(sim->blk_states);
	obstack_free(&sim->obst, NULL);
	DB((dbg, LEVEL_1, "x87 Simulator stopped\n\n"));
}

/**
 * Run a simulation and fix all virtual instructions for a graph.
 *
 * @param env       the architecture environment
 * @param irg       the current graph
 * @param blk_list  the block schedule list
 *
 * Needs a block-schedule.
 */
void x87_simulate_graph(const arch_env_t *env, ir_graph *irg, ir_node **blk_list) {
	ir_node *block, *start_block;
	pdeq *worklist;
	blk_state *bl_state;
	x87_simulator sim;
	int i;

	/* we need liveness info for the current graph */
	be_liveness(irg);

	/* create the simulator */
	x87_init_simulator(&sim, irg, env);

	start_block = get_irg_start_block(irg);
	bl_state = x87_get_bl_state(&sim, start_block);

	/* start with the empty state */
	bl_state->begin = empty;

	worklist = new_pdeq();

	/* create the worklist for the schedule. */
	for (i = 0; i < ARR_LEN(blk_list); ++i)
		pdeq_putr(worklist, blk_list[i]);

	/* iterate */
	do {
		block = pdeq_getl(worklist);
		if (! x87_simulate_block(&sim, block)) {
			pdeq_putr(worklist, block);
			continue;
		}
	} while (! pdeq_empty(worklist));

	/* kill it */
	x87_destroy_simulator(&sim);
}
