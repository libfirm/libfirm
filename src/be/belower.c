/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Performs lowering of perm nodes. Inserts copies to assure
 *              register constraints.
 * @author      Christian Wuerdig
 * @date        14.12.2005
 */
#include "belower.h"

#include "adt/obstack.h"
#include "array.h"
#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "bessaconstr.h"
#include "bestat.h"
#include "debug.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irnodeset.h"
#include "raw_bitset.h"
#include "target_t.h"
#include "util.h"
#include "xmalloc.h"
#include <stdlib.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_permmove;)

/** Lowering walker environment. */
typedef struct lower_env_t {
	bool             use_copies;
	struct obstack   obst;
	ir_nodehashmap_t live_regs; /**< maps Perm nodes to a raw bitset that
	                                 maps register indices to register state
	                                 at end of Perm's block (used=0, free=1) */
} lower_env_t;

/** Holds a Perm register pair. */
typedef struct reg_pair_t {
	const arch_register_t *in_reg;    /**< a perm IN register */
	ir_node               *in_node;   /**< the in node to which the register belongs */

	const arch_register_t *out_reg;   /**< a perm OUT register */
	ir_node               *out_node;  /**< the out node to which the register belongs */
} reg_pair_t;

static void set_reg_free(unsigned *free_regs, ir_node const *irn, bool const reg_is_free)
{
	if (!mode_is_data(get_irn_mode(irn)))
		return;
	arch_register_t const *reg = arch_get_irn_register(irn);
	if (reg_is_free) {
		rbitset_set(free_regs, reg->global_index);
	} else {
		rbitset_clear(free_regs, reg->global_index);
	}
}

/* Save the register situation at the end of the Perm's block, i.e. mark all
 * registers holding live values as used.
 * As we do this before we modify the graph, we might mark more registers
 * as used than really necessary.
 */
static void mark_live_nodes_registers(const ir_node *irn, lower_env_t *env)
{
	ir_node                     *block     = get_nodes_block(irn);
	ir_graph                    *irg       = get_irn_irg(irn);
	arch_register_class_t const *cls       = arch_get_irn_register(get_irn_n(irn, 0))->cls;
	be_irg_t                    *birg      = be_birg_from_irg(irg);
	unsigned                     n_regs    = ir_target.isa->n_registers;
	unsigned                    *free_regs = rbitset_duplicate_obstack_alloc(&env->obst, birg->allocatable_regs, n_regs);

	be_lv_t *lv = be_get_irg_liveness(irg);
	be_lv_foreach_cls(lv, block, be_lv_state_end, cls, live) {
		set_reg_free(free_regs, live, false);
	}

	ir_nodehashmap_insert(&env->live_regs, (ir_node*)irn, free_regs);
}

static void live_nodes_registers_walker(ir_node *irn, void *env)
{
	if (!be_is_Perm(irn))
		return;

	mark_live_nodes_registers(irn, (lower_env_t*)env);
}

static arch_register_t const *get_free_register(ir_node *const perm, lower_env_t *env)
{
	if (!env->use_copies)
		return NULL;

	ir_node                     *block     = get_nodes_block(perm);
	arch_register_class_t const *cls       = arch_get_irn_register(get_irn_n(perm, 0))->cls;
	unsigned                    *free_regs = (unsigned*)ir_nodehashmap_get(arch_register_t const, &env->live_regs, perm);

	sched_foreach_non_phi_reverse(block, node) {
		/* If we later implement first the chains and then the cycles
		   of the Perm, we *cannot* regard the Perm's own outputs as
		   free registers. */
		bool const reg_is_free = perm != node;
		be_foreach_value(node, value,
			set_reg_free(free_regs, value, reg_is_free);
		);

		foreach_irn_in(node, i, in) {
			set_reg_free(free_regs, in, false);
		}

		if (perm == node)
			break;
	}

	arch_register_t const *const regs   = ir_target.isa->registers;
	unsigned               const n_regs = ir_target.isa->n_registers;
	rbitset_foreach(free_regs, n_regs, free_idx) {
		arch_register_t const *free_reg = &regs[free_idx];
		if (free_reg->cls != cls)
			continue;

		return free_reg;
	}

	return NULL;
}

/**
 * Lowers a perm node.  Resolves cycles and creates a bunch of
 * copy and swap operations to permute registers.
 *
 * @param perm        The perm node
 * @param env         The lowering environment, containing information on,
 *                    i.e., whether to use copies if free register available
 */
static void lower_perm_node(ir_node *const perm, arch_register_class_t const *const cls, unsigned const arity, ir_node **const projs, lower_env_t *const env)
{
	DBG((dbg, LEVEL_1, "lowering %+F\n", perm));

	bool             const use_copies = env->use_copies;
	unsigned         const n_regs     = cls->n_regs;
	/* Registers used as inputs of the Perm. */
	unsigned        *const inregs     = rbitset_alloca(n_regs);
	/* Map from register index to pair with this register as output. */
	reg_pair_t     **const oregmap    = ALLOCANZ(reg_pair_t*, n_regs);
	reg_pair_t      *const pairs      = ALLOCAN(reg_pair_t, arity);
	reg_pair_t            *pair       = pairs;
	arch_register_t const *free_reg   = NULL;

	/* Collect all input-output pairs of the Perm. */
	for (unsigned pos = 0; pos != arity; ++pos) {
		ir_node *const out = projs[pos];
		if (!out)
			continue;

		ir_node               *const in   = get_irn_n(perm, pos);
		arch_register_t const *const ireg = arch_get_irn_register(in);
		arch_register_t const *const oreg = arch_get_irn_register_out(perm, pos);

		if (ireg == oreg) {
			DBG((dbg, LEVEL_2, "%+F: removing equal perm register pair (%+F, %+F, %s)\n", perm, in, out, oreg->name));
			exchange(out, in);
			continue;
		}

		pair->in_reg   = ireg;
		pair->in_node  = in;
		pair->out_reg  = oreg;
		pair->out_node = out;

		oregmap[oreg->index] = pair++;
		rbitset_set(inregs, ireg->index);
	}

	if (pair == pairs) {
		DBG((dbg, LEVEL_1, "%+F is identity\n", perm));
		goto done;
	}

	DBG((dbg, LEVEL_1, "%+F has %d unresolved constraints\n", perm, (int)(pair - pairs)));

	/* Build Copy chains. */
	for (unsigned i = 0; i != n_regs; ++i) {
		if (rbitset_is_set(inregs, i))
			continue;
		/* Found end of chain, i.e. register which is written to, but not read.
		 * Generate copies by following the chain backwards. */
		unsigned k = i;
		for (reg_pair_t const *p; (p = oregmap[k]);) {
			oregmap[k] = NULL;
			ir_node *const copy = be_new_Copy_before_reg(p->in_node, perm, p->out_reg);
			DBG((dbg, LEVEL_2, "%+F: inserting %+F for %+F from %s to %s\n", perm, copy, p->in_node, p->in_reg, p->out_reg));
			exchange(p->out_node, copy);

			const unsigned new_k = p->in_reg->index;
			if (!oregmap[new_k] && !free_reg) {
				/* The invariant of Perm nodes allows us to overwrite
				 * the first register in a chain with an arbitrary value.
				 * Only consider this source register if it is allocatable, otherwise it
				 * might be a special register, e.g. a null register. */
				be_irg_t const *const birg = be_birg_from_irg(get_irn_irg(perm));
				if (rbitset_is_set(birg->allocatable_regs, p->in_reg->global_index))
					free_reg = p->in_reg;
			}
			k = new_k;

			rbitset_clear(inregs, k);
		}
	}

	if (rbitset_is_empty(inregs, n_regs)) {
		goto done;
	}

	if (use_copies && free_reg == NULL) {
		free_reg = get_free_register(perm, env);
	}

	if (use_copies && free_reg != NULL) {
		/* Implement cycles using copies and the free register. */
		for (unsigned i = 0; i != n_regs; /* empty */) {
			if (!rbitset_is_set(inregs, i)) {
				++i;
				continue;
			}
			reg_pair_t *start = oregmap[i];

			ir_node *const save_copy = be_new_Copy_before_reg(start->in_node, perm, free_reg);

			reg_pair_t *p = oregmap[start->in_reg->index];
			do {
				ir_node *const copy = be_new_Copy_before_reg(p->in_node, perm, p->out_reg);
				exchange(p->out_node, copy);
				unsigned const in_idx = p->in_reg->index;
				rbitset_clear(inregs, in_idx);
				p = oregmap[in_idx];
			} while (p != start);

			rbitset_clear(inregs, start->in_reg->index);
			ir_node *const restore_copy = be_new_Copy_before_reg(save_copy, perm, start->out_reg);
			exchange(start->out_node, restore_copy);
		}
	} else {
		if (arity == 2) {
			DBG((dbg, LEVEL_1, "%+F is transposition\n", perm));
			return;
		}

		/* Decompose cycles into transpositions.
		 *
		 * Use as many independent transpositions as possible and do not thread
		 * one value through all transpositions.
		 * I.e., for the first level of decomposition of a n-Perm do floor(n/2)
		 * transpositions. This puts floor(n/2) values into the right registers.
		 * Repeat this for all remaining values until all have the right
		 * register.
		 * This way no value is threaded through more than ceil(ld(n/2))
		 * transpositions (compared to one value being threaded through all
		 * transpositions using a naive decomposition).
		 *
		 * good:            bad:
		 * r1 r2 r3 r4 r5   r1 r2 r3 r4 r5
		 * +---+ +---+      +---+
		 *    +------+         +---+
		 *          +---+         +---+
		 * r2 r3 r4 r5 r1            +---+
		 *                  r2 r3 r4 r5 r1
		 */
		ir_node *const block = get_nodes_block(perm);
		for (unsigned i = 0; i != n_regs;) {
			if (!rbitset_is_set(inregs, i)) {
				++i;
				continue;
			}
			reg_pair_t             *p     = oregmap[i];
			reg_pair_t const *const start = p;
			for (;;) {
				reg_pair_t const *const q = oregmap[p->in_reg->index];
				if (q == start)
					break;

				rbitset_clear(inregs, q->out_reg->index);
				p->in_reg = q->in_reg;

				ir_node *const in[] = { p->in_node, q->in_node };
				ir_node *const xchg = be_new_Perm(block, ARRAY_SIZE(in), in);
				DBG((dbg, LEVEL_2, "%+F: inserting %+F for %+F (%s) and %+F (%s)\n", perm, xchg, in[0], arch_get_irn_register(in[0]), in[1], arch_get_irn_register(in[1])));
				p->in_node           = be_new_Proj_reg(xchg, 0, q->in_reg);
				ir_node *const new_q = be_new_Proj_reg(xchg, 1, q->out_reg);
				exchange(q->out_node, new_q);
				sched_add_before(perm, xchg);
				/* Prevent that the broken down Perm is visited by the walker. */
				mark_irn_visited(xchg);

				p = oregmap[q->in_reg->index];
				if (p == start) {
					if (start->in_reg == start->out_reg) {
						rbitset_clear(inregs, q->in_reg->index);
						exchange(start->out_node, start->in_node);
					}
					break;
				}
			}
		}
	}

done:
	sched_remove(perm);
	kill_node(perm);
}

static bool is_node_operand(ir_node *const node, ir_node const *const operand)
{
	foreach_irn_in(node, i, in) {
		if (in == operand)
			return true;
	}
	return false;
}

/**
 * Push nodes that do not need to be permed through the Perm.
 * This is commonly a reload cascade at block ends.
 *
 * @param perm The perm
 *
 * @return     true, if there is something left to perm over.
 *             false, if removed the complete perm.
 */
static bool push_through_perm(ir_node *const perm, arch_register_class_t const *const cls, unsigned const arity, ir_node **const projs)
{
	DB((dbg_permmove, LEVEL_1, "perm move %+F irg %+F\n", perm, get_irn_irg(perm)));

	unsigned new_size = arity;
	ir_node *node     = sched_prev(perm);
	for (ir_node *prev; !sched_is_begin(node); node = prev) {
		if (arch_irn_is(node, schedule_first)) {
			DB((dbg_permmove, LEVEL_2, "\tcannot move past schedule_first %+F\n", node));
			break;
		}

		prev = sched_prev(node);

		/* Remove Copy with src-reg = dst-reg, which would otherwise block moving
		 * the Perm. */
		if (be_is_Copy(node)) {
			ir_node               *const op       = get_irn_n(node, n_be_Copy_op);
			arch_register_t const *const reg_op   = arch_get_irn_register(op);
			arch_register_t const *const reg_copy = arch_get_irn_register_out(node, 0);
			if (reg_copy == reg_op) {
				DB((dbg_permmove, LEVEL_2, "\tremoving nop %+F\n", node));
				sched_remove(node);
				exchange(node, op);
				continue;
			}

			/* Swap, if at least one side will not change the register anymore.
			 * This avoids copying the value forth and back.
			 *
			 * a           a           a           a
			 * |\          |\          |\          |\
			 * | \         | \         | \         | \
			 * | Copy    Copy |        | Perm      |  |
			 * |  |b       |b |        |  |a       |  |
			 * Perm   ->   Perm   -> Copy |   -> Copy |
			 * |b |a swap  |b |a push  |b |a lower |b |a
			 *
			 * a           a           a           a
			 * |\          |\          |\          |\
			 * | \         | \         | \         | \
			 * | Copy    Copy |        | Perm      |  |
			 * |  |b       |b |        |  |a       |  |
			 * Perm   ->   Perm   -> Copy |   -> Copy |
			 * |c |a swap  |c |a push  |c |a lower |c |a
			 *
			 * a           a           a           a
			 * |\          |\          |\          |\
			 * | \         | \         | \         | \
			 * | Copy    Copy |        | Perm      | Copy
			 * |  |b       |b |        |  |c       |  |c
			 * Perm   ->   Perm   -> Copy |   -> Copy |
			 * |b |c swap  |b |c push  |b |c lower |b |c
			 */
			int pos_copy = -1;
			int pos_op   = -1;
			foreach_irn_in(perm, i, in) {
				if (in == node) {
					pos_copy = i;
					if (pos_op >= 0)
						goto check_swap;
				} else if (in == op) {
					pos_op = i;
					if (pos_copy >= 0) {
check_swap:
						if (arch_get_irn_register_out(perm, pos_copy) == reg_op ||
						    arch_get_irn_register_out(perm, pos_op)   == reg_copy) {
							set_irn_n(perm, pos_copy, op);
							set_irn_n(perm, pos_op, node);
						}
						break;
					}
				}
			}
		}

		be_foreach_use(node, cls, in_req, op, op_req,
			/* A Perm will only be pushed up to the first instruction
			 * which lets an operand of itself die.
			 * If we would allow to move the Perm above this instruction,
			 * the former dead operand would be live now at the point of
			 * the Perm, increasing the register pressure by one. */
			if (reg_req_has_constraint(in_req) || !is_node_operand(perm, op)) {
				DB((dbg_permmove, LEVEL_2, "\tcannot move past %+F due to operand %+F\n", node, op));
				goto done;
			}
		);

		be_foreach_definition(node, cls, value, req,
			if (reg_req_has_constraint(req))
				goto done;
		);

		DBG((dbg_permmove, LEVEL_2, "\tmoving %+F after %+F\n", node, perm));

		/* Rewire Perm results to pushed through instruction. */
		for (unsigned pn = 0; pn != arity; ++pn) {
			ir_node *const proj = projs[pn];
			ir_node *const in   = get_irn_n(perm, pn);
			if (in == node || (is_Proj(in) && get_Proj_pred(in) == node)) {
				/* Give it the proj's register. */
				arch_set_irn_register(in, arch_get_irn_register_out(perm, pn));
				/* Reroute all users of the proj to the moved node. */
				exchange(proj, in);
				projs[pn] = NULL;
				--new_size;
			} else {
				/* Translate the node's inputs through the Perm. */
				foreach_irn_in(node, i, node_in) {
					if (node_in == in)
						set_irn_n(node, i, proj);
				}
			}
		}
	}
done:

	if (new_size == 0) {
		sched_remove(perm);
		kill_node(perm);
		return false;
	} else {
		/* Move the Perm before all pushed through nodes. This may happen even if
		 * the Perm did not get smaller. */
		sched_remove(perm);
		sched_add_after(node, perm);
		return true;
	}
}

/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env)
{
	if (!be_is_Perm(irn))
		return;
	ir_node *const perm = irn;

	arch_register_class_t const *const cls   = arch_get_irn_register_req_out(perm, 0)->cls;
	unsigned                     const arity = get_irn_arity(perm);

	/* Collect all Projs of the Perm in an array sorted by Proj number. */
	ir_node **const projs = ALLOCAN(ir_node*, arity);
	DEBUG_ONLY(memset(projs, 0, sizeof(*projs) * arity);)
	foreach_out_edge(perm, edge) {
		ir_node *const proj = get_edge_src_irn(edge);
		unsigned const pn   = get_Proj_num(proj);
		assert(pn < arity);
		projs[pn] = proj;
	}

	bool const perm_stayed = push_through_perm(perm, cls, arity, projs);
	if (perm_stayed) {
		lower_env_t *env = (lower_env_t*)walk_env;
		lower_perm_node(perm, cls, arity, projs, env);
	}
}

void lower_nodes_after_ra(ir_graph *irg, bool use_copies)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.lower");
	FIRM_DBG_REGISTER(dbg_permmove, "firm.be.lower.permmove");

	/* we will need interference */
	be_assure_live_chk(irg);

	lower_env_t env;
	env.use_copies = use_copies;

	if (use_copies) {
		ir_nodehashmap_init(&env.live_regs);
		obstack_init(&env.obst);
		be_assure_live_sets(irg);
		irg_walk_graph(irg, NULL, live_nodes_registers_walker, &env);
	}

	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, &env);

	if (use_copies) {
		ir_nodehashmap_destroy(&env.live_regs);
		obstack_free(&env.obst, NULL);
		be_invalidate_live_sets(irg);
	}
}
