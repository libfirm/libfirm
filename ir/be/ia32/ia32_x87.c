/**
 * This file implements the x87 support and virtual to stack
 * register translation for the ia32 backend.
 *
 * @author: Michael Beck
 *
 * $Id$
 */
#include <assert.h>

#include "irnode_t.h"
#include "irop_t.h"
#include "irprog.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "obst.h"
#include "pmap.h"
#include "pdeq.h"
#include "irprintf.h"

#include "..\belive_t.h"
#include "..\besched.h"
#include "..\benode_t.h"
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

/** the virtual floating point flag */
#define irop_flag_vp   (irop_flag_machine << 1)

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
 * The x87 state.
 */
typedef struct _x87_state {
	const arch_register_t *st[N_x87_REGS];  /**< the register stack */
	int depth;                              /**< the current stack depth */
	int tos;                                /**< position of the tos */
} x87_state;

/** An empty state, used for blocks without fp instructions. */
static const x87_state _empty = { {0}, 0, 0 };
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
 * Check if the state is empty.
 */
static int x87_state_is_empty(const x87_state *state) {
	return state->depth == 0;
}

/**
 * Return the virtual register at st(pos).
 */
static const arch_register_t *x87_get_reg(const x87_state *state, int pos) {
	assert(pos < state->depth);
	return state->st[MASK_TOS(state->tos + pos)];
}

/**
 * Dump the stack for debugging.
 */
static void x87_dump_stack(const x87_state *state) {
	int i;

	for (i = state->depth - 1; i >= 0; --i) {
		const arch_register_t *vreg = x87_get_reg(state, i);
		ir_printf("%s ", vreg->name);
	}
	ir_printf("<-- TOS\n");
}

/**
 * Set a virtual register to st(pos)
 */
static void x87_set_reg(x87_state *state, const arch_register_t *vreg, int pos) {
	assert(0 < state->depth);
	state->st[MASK_TOS(state->tos + pos)] = vreg;

	printf("After SET_REG:\n "); x87_dump_stack(state);
}

/**
 * Set the tos virtual register
 */
static void x87_set_tos_reg(x87_state *state, const arch_register_t *vreg) {
	x87_set_reg(state, vreg, 0);
}

/**
 * Flush the x87 stack.
 */
static void x87_flush(x87_state *state) {
	state->depth = 0;
	state->tos   = 0;
}

/**
 * Swap st(0) with st(pos).
 */
static void x87_fxch(x87_state *state, int pos) {
	const arch_register_t *vreg;
	assert(pos < state->depth);

	vreg = state->st[MASK_TOS(state->tos + pos)];
	state->st[MASK_TOS(state->tos + pos)] = state->st[MASK_TOS(state->tos)];
	state->st[MASK_TOS(state->tos)] = vreg;

	printf("After FXCH:\n "); x87_dump_stack(state);
}

/**
 * Convert a virtual register to the stack index.
 * Return -1 if the virtual register was not found.
 */
static int x87_on_stack(const x87_state *state, const arch_register_t *vreg) {
	int i, tos = state->tos;

	for (i = 0; i < state->depth; ++i)
		if (state->st[MASK_TOS(tos + i)] == vreg)
			return i;
	return -1;
}

/**
 * Push a virtual Register onto the stack.
 */
static void x87_push(x87_state *state, const arch_register_t *vreg) {
//	assert(x87_on_stack(state, vreg) == -1 && "double push");
	assert(state->depth < N_x87_REGS && "stack overrun");

	++state->depth;
	state->tos = MASK_TOS(state->tos - 1);
	state->st[state->tos] = vreg;

	printf("After PUSH:\n "); x87_dump_stack(state);
}

/**
 * Pop a virtual Register from the stack.
 */
static void x87_pop(x87_state *state) {
	assert(state->depth > 0 && "stack underrun");

	--state->depth;
	state->tos = MASK_TOS(state->tos + 1);

	printf("After POP:\n "); x87_dump_stack(state);
}

/**
 * Returns the block state of a block.
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
 * Create a new x87 state.
 */
static x87_state *x87_alloc_state(x87_simulator *sim) {
	x87_state *res = obstack_alloc(&sim->obst, sizeof(*res));
	return res;
}

/**
 * Create a new empty x87 state.
 */
static x87_state *x87_alloc_empty_state(x87_simulator *sim) {
	x87_state *res = x87_alloc_state(sim);

	x87_flush(res);
	return res;
}

/**
 * Clone a x87 state.
 */
static x87_state *x87_clone_state(x87_simulator *sim, const x87_state *src) {
	x87_state *res = x87_alloc_state(sim);

	memcpy(res, src, sizeof(*res));
	return res;
}

/**
 * Calculate the necessary permutations to reach dst_state.
 */
static x87_state *x87_shuffle(x87_simulator *sim, ir_node *block, x87_state *state, const x87_state *dst_state) {
	assert(state->depth == dst_state->depth);

	if (state == empty)
		state = x87_clone_state(sim, state);
	return state;
}

/**
 * Create a fxch before node n.
 */
static void x87_create_fxch(x87_state *state, ir_node *n, int pos, int op_idx) {
	ir_node *fxch, *pred;
	ia32_attr_t *attr;

	x87_fxch(state, pos);

	pred = get_irn_n(n, op_idx);
	fxch = new_rd_ia32_fxch(NULL, get_irn_irg(n), get_nodes_block(n), pred, get_irn_mode(pred));
	attr = get_ia32_attr(fxch);
	attr->x87[0] = &ia32_st_regs[pos];
	attr->x87[2] = &ia32_st_regs[0];
	set_irn_n(n, op_idx, fxch);

	sched_add_before(n, fxch);
	printf("<<< %s %s, %s\n", get_irn_opname(fxch), attr->x87[0]->name, attr->x87[2]->name);
}

/**
 * Create a fpush before node n.
 */
static void x87_create_fpush(const arch_env_t *env, x87_state *state, ir_node *n, int pos, int op_idx) {
	ir_node *fpush, *pred;
	ia32_attr_t *attr;

	x87_push(state, arch_get_irn_register(env, n));

	pred = get_irn_n(n, op_idx);
	fpush = new_rd_ia32_fpush(NULL, get_irn_irg(n), get_nodes_block(n), pred, get_irn_mode(pred));
	attr = get_ia32_attr(fpush);
	attr->x87[0] = &ia32_st_regs[pos];
	attr->x87[2] = &ia32_st_regs[0];
	set_irn_n(n, op_idx, fpush);

	sched_add_before(n, fpush);
	printf("<<< %s %s, %s\n", get_irn_opname(fpush), attr->x87[0]->name, attr->x87[2]->name);
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

	if(arch_irn_consider_in_reg_alloc(arch_env, cls, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			live &= ~(1 << reg->index);
	}

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if (arch_irn_consider_in_reg_alloc(arch_env, cls, op)) {
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
 */
static unsigned is_vfp_live(const arch_register_t *reg, unsigned live) {
	return live & (1 << reg->index);
}

static vfp_dump_live(unsigned live) {
	int i;

	printf("Live registers here: \n");
	for (i = 0; i < 8; ++i) {
		if (live & (1 << i)) {
			printf(" vf%d", i);
		}
	}
	printf("\n");
}

/* --------------------------------- simulators ---------------------------------------- */

#define XCHG(a, b) do { int t =(a); (a) = (b); (b) = t; } while (0)

/**
 * Simulate a virtual binop
 */
static void sim_binop(x87_state *state, ir_node *n, const arch_env_t *env, const exchange_tmpl *tmpl) {
	int op1_idx, op2_idx = -1;
	int out_idx, do_pop =0;
	ia32_attr_t *attr;
	ir_op *dst;
	const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_1));
	const arch_register_t *op2 = arch_get_irn_register(env, get_irn_n(n, BINOP_IDX_2));
	const arch_register_t *out = arch_get_irn_register(env, n);
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	printf(">>> %s %s, %s -> %s\n", get_irn_opname(n), op1->name, op2->name, out->name);
  vfp_dump_live(live);

	op1_idx = x87_on_stack(state, op1);

	if (op2->reg_class == &ia32_reg_classes[CLASS_ia32_vfp]) {
		/* second operand is a vfp register */
		op2_idx = x87_on_stack(state, op2);

		if (is_vfp_live(op1, live)) {
			/* first operand is live */

			if (is_vfp_live(op2, live)) {
				/* both operands are live: push the first one */
				x87_create_fpush(env, state, n, op1_idx, BINOP_IDX_1);
				out_idx = op1_idx = 0;
				++op2_idx;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
			else {
				/* first live, second operand is dead here, bring it to tos */
				if (op2_idx != 0) {
					x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
					if (op1_idx == 0)
						op1_idx = op2_idx;
				}
				op2_idx = out_idx = 0;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
		}
		else {
			/* first operand is dead */
			if (is_vfp_live(op2, live)) {
				/* second operand is live: bring first to tos */
				if (op1_idx != 0) {
					x87_create_fxch(state, n, op1_idx, BINOP_IDX_1);
					if (op2_idx == 0)
						op2_idx = op1_idx;
				}
				op1_idx = out_idx = 0;
				dst = tmpl->normal_op;
				do_pop = 0;
			}
			else {
				/* both operands are dead here, pop them from the stack */
				if (op1_idx == 0) {
					out_idx = op2_idx;
					XCHG(op1_idx, op2_idx);
					dst = tmpl->reverse_pop_op;
					do_pop = 1;
				}
				else if (op2_idx == 0) {
					out_idx = op1_idx;
					dst = tmpl->normal_pop_op;
					do_pop = 1;
				}
				else {
					/* bring the second on top */
					x87_create_fxch(state, n, op2_idx, BINOP_IDX_2);
					op2_idx = 0;
					out_idx = op1_idx;
					dst = tmpl->normal_pop_op;
					do_pop = 1;
				}
			}
		}
	}
	else {
		/* second operand is an address mode */
		if (is_vfp_live(op1, live)) {
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

	x87_set_reg(state, out, out_idx);
	if (do_pop)
		x87_pop(state);

	/* patch the operation */
	n->op = dst;
	attr = get_ia32_attr(n);
	attr->x87[0] = op1 = &ia32_st_regs[op1_idx];
	attr->x87[1] = op2 = &ia32_st_regs[op2_idx];
	attr->x87[2] = out = &ia32_st_regs[out_idx];

	printf("<<< %s %s, %s -> %s\n", get_irn_opname(n), op1->name, op2->name, out->name);
}

/**
 * Simulate a virtual Unop
 */
static void sim_unop(x87_state *state, ir_node *n, const arch_env_t *env, ir_op *op) {
	int op1_idx, out_idx;
	const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, UNOP_IDX));
	const arch_register_t *out = arch_get_irn_register(env, n);
	ia32_attr_t *attr;
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	printf(">>> %s -> %s\n", get_irn_opname(n), out->name);
  vfp_dump_live(live);

	op1_idx = x87_on_stack(state, op1);

	if (is_vfp_live(op1, live)) {
		/* push the operand here */
		x87_create_fpush(env, state, n, op1_idx, UNOP_IDX);
	}
	else {
		/* operand is dead, bring it to tos */
		if (op1_idx != 0)
			x87_create_fxch(state, n, op1_idx, UNOP_IDX);
	}

	x87_set_tos_reg(state, out);
	op1_idx = out_idx = 0;
	n->op = op;
	attr = get_ia32_attr(n);
	attr->x87[0] = op1 = &ia32_st_regs[0];
	attr->x87[2] = out = &ia32_st_regs[0];
	printf("<<< %s -> %s\n", get_irn_opname(n), out->name);
}

/**
 * Simulate a virtual Load instructions
 */
static void sim_load(x87_state *state, ir_node *n, const arch_env_t *env, ir_op *op) {
	const arch_register_t *out = arch_get_irn_register(env, n);
	ia32_attr_t *attr;

	printf(">>> %s -> %s\n", get_irn_opname(n), out->name);
	x87_push(state, out);
	n->op = op;
	attr = get_ia32_attr(n);
	attr->x87[2] = out = &ia32_st_regs[0];
	printf("<<< %s -> %s\n", get_irn_opname(n), out->name);
}

/**
 * Simulate a virtual Store
 */
static void sim_fst(x87_state *state, ir_node *n, const arch_env_t *env) {
	int op2_idx;
	const arch_register_t *op2 = arch_get_irn_register(env, get_irn_n(n, STORE_VAL_IDX));
	ia32_attr_t *attr;
	unsigned live = vfp_liveness_nodes_live_at(env, n);

	op2_idx = x87_on_stack(state, op2);

	printf(">>> %s %s ->\n", get_irn_opname(n), op2->name);

	/* we can only store the tos to memory */
	if (op2_idx != 0)
		x87_create_fxch(state, n, op2_idx, STORE_VAL_IDX);

	if (is_vfp_live(op2, live))
		n->op = op_ia32_fst;
	else {
		x87_pop(state);
		n->op = op_ia32_fstp;
	}

	attr = get_ia32_attr(n);
	attr->x87[1] = op2 = &ia32_st_regs[0];
	printf("<<< %s %s ->\n", get_irn_opname(n), op2->name);
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

/* all stubs */
GEN_BINOP(fadd)
GEN_BINOPR(fsub)
GEN_BINOP(fmul)
GEN_BINOPR(fdiv)

GEN_LOAD(fld)
GEN_LOAD(fldz)
GEN_LOAD(fld1)
GEN_LOAD2(fConst, fldConst)

GEN_UNOP(fabs)
GEN_UNOP(fchs)
GEN_UNOP(fsin)
GEN_UNOP(fcos)
GEN_UNOP(fsqrt)

/**
 * Simulate a virtual Copy
 */
static void sim_Copy(x87_state *state, ir_node *n, const arch_env_t *env) {
	ir_mode *mode = get_irn_mode(n);

	if (mode_is_float(mode)) {
		const arch_register_t *op1 = arch_get_irn_register(env, get_irn_n(n, 0));
		const arch_register_t *out = arch_get_irn_register(env, n);
		ir_node *node, *next;
		ia32_attr_t *attr;
		int op1_idx;
		unsigned live = vfp_liveness_nodes_live_at(env, n);

		op1_idx = x87_on_stack(state, op1);

		printf(">>> %s %s -> %s\n", get_irn_opname(n), op1->name, out->name);
	  vfp_dump_live(live);

		if (is_vfp_live(op1, live)) {
			/* operand is still live,a real copy */
			x87_push(state, out);

			node = new_rd_ia32_fpush(get_irn_dbg_info(n), get_irn_irg(n), get_nodes_block(n), get_irn_n(n, 0), mode);
			arch_set_irn_register(env, node, out);

			attr = get_ia32_attr(node);
			attr->x87[0] = op1 = &ia32_st_regs[op1_idx];
			attr->x87[2] = out = &ia32_st_regs[0];

			next = sched_next(n);
			sched_remove(n);
			exchange(n, node);
			sched_add_before(next, node);
			printf(">>> %s %s -> %s\n", get_irn_opname(node), op1->name, out->name);
		}
		else {
			/* just a virtual copy */
			x87_set_reg(state, out, op1_idx);
			sched_remove(n);
			printf(">>> KILLED %s\n", get_irn_opname(n));
		}
	}
}

/**
 * Run a simulation and fix all virtual instructions for a block.
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
			x87_shuffle(sim, block, state, succ_state->begin);

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
 */
static void x87_init_simulator(x87_simulator *sim, const arch_env_t *env) {
	obstack_init(&sim->obst);
	sim->blk_states = pmap_create();
	sim->env        = env;

	clear_irp_opcodes_generic_func();

#define ASSOC(op)	    (op_ia32_v ## op)->ops.generic = (op_func)(sim_##op)
#define ASSOC_BE(op)	(op_be_ ## op)->ops.generic = (op_func)(sim_##op)
	ASSOC(fConst);
	ASSOC(fld);
	ASSOC(fld1);
	ASSOC(fldz);
	ASSOC(fadd);
	ASSOC(fsub);
	ASSOC(fmul);
	ASSOC(fdiv);
	ASSOC(fldz);
	ASSOC(fabs);
	ASSOC(fchs);
	ASSOC(fsin);
	ASSOC(fcos);
	ASSOC(fsqrt);
	ASSOC(fst);
	ASSOC_BE(Copy);
#undef ASSOC_BE
#undef ASSOC
}

/**
 * Destroy a x87 simulator.
 */
static void x87_destroy_simulator(x87_simulator *sim) {
	pmap_destroy(sim->blk_states);
	obstack_free(&sim->obst, NULL);
}

/**
 * Run a simulation and fix all virtual instructions for a graph.
 *
 * Needs a block-schedule.
 */
void x87_simulate_graph(const arch_env_t *env, ir_graph *irg, ir_node **blk_list) {
	ir_node *block, *start_block;
	pdeq *worklist;
	blk_state *bl_state;
	x87_simulator sim;
	int i;

	be_liveness(irg);
	be_liveness_dumpto(irg, "-x87-live");

	x87_init_simulator(&sim, env);

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

	x87_destroy_simulator(&sim);
}
