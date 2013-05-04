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
#include <stdlib.h>

#include "ircons.h"
#include "debug.h"
#include "xmalloc.h"
#include "irnodeset.h"
#include "irnodehashmap.h"
#include "irgmod.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "array_t.h"
#include "util.h"

#include "bearch.h"
#include "beirg.h"
#include "belower.h"
#include "benode.h"
#include "besched.h"
#include "bestat.h"
#include "bessaconstr.h"
#include "beintlive_t.h"

#undef KEEP_ALIVE_COPYKEEP_HACK

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_constr;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_permmove;)

/** Associates an ir_node with its copy and CopyKeep. */
typedef struct {
	ir_nodeset_t copies; /**< all non-spillable copies of this irn */
	const arch_register_class_t *cls;
} op_copy_assoc_t;

/** Environment for constraints. */
typedef struct {
	ir_graph        *irg;
	ir_nodehashmap_t op_set;
	struct obstack   obst;
} constraint_env_t;

/** Lowering walker environment. */
typedef struct lower_env_t {
	bool do_copy;
} lower_env_t;

/** Holds a Perm register pair. */
typedef struct reg_pair_t {
	const arch_register_t *in_reg;    /**< a perm IN register */
	ir_node               *in_node;   /**< the in node to which the register belongs */

	const arch_register_t *out_reg;   /**< a perm OUT register */
	ir_node               *out_node;  /**< the out node to which the register belongs */
} reg_pair_t;

/**
 * Lowers a perm node.  Resolves cycles and creates a bunch of
 * copy and swap operations to permute registers.
 *
 * @param perm     The perm node
 * @param env      The lowerer environment
 */
static void lower_perm_node(ir_node *const perm, lower_env_t *const env)
{
	(void)env;

	DBG((dbg, LEVEL_1, "lowering %+F\n", perm));

	arch_register_class_t const *const cls     = arch_get_irn_register_req_out(perm, 0)->cls;
	unsigned                     const n_regs  = cls->n_regs;
	/* Registers used as inputs of the Perm. */
	unsigned                    *const inregs  = rbitset_alloca(n_regs);
	/* Map from register index to pair with this register as output. */
	reg_pair_t                 **const oregmap = ALLOCANZ(reg_pair_t*, n_regs);
	size_t                       const arity   = get_irn_arity(perm);
	reg_pair_t                  *const pairs   = ALLOCAN(reg_pair_t, arity);
	reg_pair_t                        *pair    = pairs;
	/* Collect all input-output pairs of the Perm. */
	foreach_out_edge_safe(perm, edge) {
		ir_node               *const out  = get_edge_src_irn(edge);
		unsigned               const pos  = get_Proj_proj(out);
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

	ir_node *const block = get_nodes_block(perm);
	/* Build Copy chains. */
	for (unsigned i = 0; i != n_regs; ++i) {
		if (rbitset_is_set(inregs, i))
			continue;
		/* Found end of chain, i.e. register which is written to, but not read.
		 * Generate copies by following the chain backwards. */
		unsigned k = i;
		for (reg_pair_t const *p; (p = oregmap[k]);) {
			oregmap[k] = NULL;
			ir_node *const copy = be_new_Copy(block, p->in_node);
			DBG((dbg, LEVEL_2, "%+F: inserting %+F for %+F from %s to %s\n", perm, copy, p->in_node, p->in_reg, p->out_reg));
			arch_set_irn_register(copy, p->out_reg);
			exchange(p->out_node, copy);
			sched_add_before(perm, copy);
			k = p->in_reg->index;
			rbitset_clear(inregs, k);
		}
	}

	if (arity == 2 && !rbitset_is_empty(inregs, n_regs)) {
		DBG((dbg, LEVEL_1, "%+F is transposition\n", perm));
		return;
	}

	/* Decompose cycles into transpositions.
	 *
	 * Use as many independent transpositions as possible and do not thread one
	 * value through all transpositions.
	 * I.e., for the first level of decomposition of a n-Perm do floor(n/2)
	 * transpositions. This puts floor(n/2) values into the right registers.
	 * Repeat this for all remaining values until all have the right register.
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
	for (unsigned i = 0; i != n_regs;) {
		if (!rbitset_is_set(inregs, i)) {
			++i;
			continue;
		}
		reg_pair_t             *p     = oregmap[i];
		reg_pair_t const *const start = p;
		ir_mode          *const mode  = get_irn_mode(p->out_node);
		for (;;) {
			reg_pair_t const *const q = oregmap[p->in_reg->index];
			if (q == start)
				break;

			rbitset_clear(inregs, q->out_reg->index);
			p->in_reg = q->in_reg;

			ir_node *const in[]  = { p->in_node, q->in_node };
			ir_node *const xchg  = be_new_Perm(cls, block, ARRAY_SIZE(in), in);
			DBG((dbg, LEVEL_2, "%+F: inserting %+F for %+F (%s) and %+F (%s)\n", perm, xchg, in[0], arch_get_irn_register(in[0]), in[1], arch_get_irn_register(in[1])));
			p->in_node = new_r_Proj(xchg, mode, 0);
			ir_node *const projq = new_r_Proj(xchg, mode, 1);
			arch_set_irn_register_out(xchg, 0, q->in_reg);
			arch_set_irn_register_out(xchg, 1, q->out_reg);
			exchange(q->out_node, projq);
			sched_add_before(perm, xchg);

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

done:
	sched_remove(perm);
	kill_node(perm);
}

static int has_irn_users(const ir_node *irn)
{
	return get_irn_out_edge_first_kind(irn, EDGE_KIND_NORMAL) != 0;
}

static ir_node *find_copy(ir_node *irn, ir_node *op)
{
	ir_node *cur_node;

	for (cur_node = irn;;) {
		cur_node = sched_prev(cur_node);
		if (! be_is_Copy(cur_node))
			return NULL;
		if (be_get_Copy_op(cur_node) == op && arch_irn_is(cur_node, dont_spill))
			return cur_node;
	}
}

static void gen_assure_different_pattern(ir_node *irn, ir_node *other_different, constraint_env_t *env)
{
	ir_nodehashmap_t            *op_set;
	ir_node                     *block;
	const arch_register_class_t *cls;
	ir_node                     *keep, *cpy;
	op_copy_assoc_t             *entry;

	arch_register_req_t const *const req = arch_get_irn_register_req(other_different);
	if (arch_register_req_is(req, ignore) ||
			!mode_is_datab(get_irn_mode(other_different))) {
		DB((dbg_constr, LEVEL_1, "ignore constraint for %+F because other_irn is ignore or not a datab node\n", irn));
		return;
	}

	op_set = &env->op_set;
	block  = get_nodes_block(irn);
	cls    = req->cls;

	/* Make a not spillable copy of the different node   */
	/* this is needed because the different irn could be */
	/* in block far far away                             */
	/* The copy is optimized later if not needed         */

	/* check if already exists such a copy in the schedule immediately before */
	cpy = find_copy(skip_Proj(irn), other_different);
	if (! cpy) {
		cpy = be_new_Copy(block, other_different);
		arch_set_irn_flags(cpy, arch_irn_flags_dont_spill);
		DB((dbg_constr, LEVEL_1, "created non-spillable %+F for value %+F\n", cpy, other_different));
	} else {
		DB((dbg_constr, LEVEL_1, "using already existing %+F for value %+F\n", cpy, other_different));
	}

	/* Add the Keep resp. CopyKeep and reroute the users */
	/* of the other_different irn in case of CopyKeep.   */
	if (has_irn_users(other_different)) {
		keep = be_new_CopyKeep_single(block, cpy, irn);
		be_node_set_reg_class_in(keep, 1, cls);
	} else {
		ir_node *in[2];

		in[0] = irn;
		in[1] = cpy;
		keep = be_new_Keep(block, 2, in);
	}

	DB((dbg_constr, LEVEL_1, "created %+F(%+F, %+F)\n\n", keep, irn, cpy));

	/* insert copy and keep into schedule */
	assert(sched_is_scheduled(irn) && "need schedule to assure constraints");
	if (! sched_is_scheduled(cpy))
		sched_add_before(skip_Proj(irn), cpy);
	sched_add_after(skip_Proj(irn), keep);

	/* insert the other different and its copies into the map */
	entry = ir_nodehashmap_get(op_copy_assoc_t, op_set, other_different);
	if (! entry) {
		entry      = OALLOC(&env->obst, op_copy_assoc_t);
		entry->cls = cls;
		ir_nodeset_init(&entry->copies);

		ir_nodehashmap_insert(op_set, other_different, entry);
	}

	/* insert copy */
	ir_nodeset_insert(&entry->copies, cpy);

	/* insert keep in case of CopyKeep */
	if (be_is_CopyKeep(keep))
		ir_nodeset_insert(&entry->copies, keep);
}

/**
 * Checks if node has a must_be_different constraint in output and adds a Keep
 * then to assure the constraint.
 *
 * @param irn          the node to check
 * @param skipped_irn  if irn is a Proj node, its predecessor, else irn
 * @param env          the constraint environment
 */
static void assure_different_constraints(ir_node *irn, ir_node *skipped_irn, constraint_env_t *env)
{
	const arch_register_req_t *req = arch_get_irn_register_req(irn);

	if (arch_register_req_is(req, must_be_different)) {
		const unsigned other = req->other_different;
		int i;

		if (arch_register_req_is(req, should_be_same)) {
			const unsigned same = req->other_same;

			if (is_po2(other) && is_po2(same)) {
				int idx_other = ntz(other);
				int idx_same  = ntz(same);

				/*
				 * We can safely ignore a should_be_same x must_be_different y
				 * IFF both inputs are equal!
				 */
				if (get_irn_n(skipped_irn, idx_other) == get_irn_n(skipped_irn, idx_same)) {
					return;
				}
			}
		}
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_node *different_from = get_irn_n(skipped_irn, i);
				gen_assure_different_pattern(irn, different_from, env);
			}
		}
	}
}

/**
 * Calls the functions to assure register constraints.
 *
 * @param block    The block to be checked
 * @param walk_env The walker environment
 */
static void assure_constraints_walker(ir_node *block, void *walk_env)
{
	constraint_env_t *env = (constraint_env_t*)walk_env;

	sched_foreach_reverse(block, irn) {
		be_foreach_value(irn, value,
			if (mode_is_datab(get_irn_mode(value)))
				assure_different_constraints(value, irn, env);
		);
	}
}

/**
 * Melt all copykeeps pointing to the same node
 * (or Projs of the same node), copying the same operand.
 */
static void melt_copykeeps(constraint_env_t *cenv)
{
	ir_nodehashmap_iterator_t map_iter;
	ir_nodehashmap_entry_t    map_entry;

	/* for all */
	foreach_ir_nodehashmap(&cenv->op_set, map_entry, map_iter) {
		op_copy_assoc_t *entry = (op_copy_assoc_t*)map_entry.data;
		int     idx, num_ck;
		struct obstack obst;
		ir_node **ck_arr, **melt_arr;

		obstack_init(&obst);

		/* collect all copykeeps */
		num_ck = idx = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp)) {
				obstack_grow(&obst, &cp, sizeof(cp));
				++num_ck;
			}
#ifdef KEEP_ALIVE_COPYKEEP_HACK
			else {
				set_irn_mode(cp, mode_ANY);
				keep_alive(cp);
			}
#endif /* KEEP_ALIVE_COPYKEEP_HACK */
		}

		/* compare each copykeep with all other copykeeps */
		ck_arr = (ir_node **)obstack_finish(&obst);
		for (idx = 0; idx < num_ck; ++idx) {
			ir_node *ref, *ref_mode_T;

			if (ck_arr[idx]) {
				int j, n_melt;
				ir_node **new_ck_in;
				ir_node *sched_pt = NULL;

				n_melt     = 1;
				ref        = ck_arr[idx];
				ref_mode_T = skip_Proj(get_irn_n(ref, 1));
				obstack_grow(&obst, &ref, sizeof(ref));

				DB((dbg_constr, LEVEL_1, "Trying to melt %+F:\n", ref));

				/* check for copykeeps pointing to the same mode_T node as the reference copykeep */
				for (j = 0; j < num_ck; ++j) {
					ir_node *cur_ck = ck_arr[j];

					if (j != idx && cur_ck && skip_Proj(get_irn_n(cur_ck, 1)) == ref_mode_T) {
						obstack_grow(&obst, &cur_ck, sizeof(cur_ck));
						ir_nodeset_remove(&entry->copies, cur_ck);
						DB((dbg_constr, LEVEL_1, "\t%+F\n", cur_ck));
						ck_arr[j] = NULL;
						++n_melt;
						sched_remove(cur_ck);
					}
				}
				ck_arr[idx] = NULL;

				/* check, if we found some candidates for melting */
				if (n_melt == 1) {
					DB((dbg_constr, LEVEL_1, "\tno candidate found\n"));
					continue;
				}

				ir_nodeset_remove(&entry->copies, ref);
				sched_remove(ref);

				melt_arr = (ir_node **)obstack_finish(&obst);
				/* melt all found copykeeps */
				NEW_ARR_A(ir_node *, new_ck_in, n_melt);
				for (j = 0; j < n_melt; ++j) {
					new_ck_in[j] = get_irn_n(melt_arr[j], 1);

					/* now, we can kill the melted keep, except the */
					/* ref one, we still need some information      */
					if (melt_arr[j] != ref)
						kill_node(melt_arr[j]);
				}

				ir_node *const new_ck = be_new_CopyKeep(get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in);
#ifdef KEEP_ALIVE_COPYKEEP_HACK
				keep_alive(new_ck);
#endif /* KEEP_ALIVE_COPYKEEP_HACK */

				/* set register class for all kept inputs */
				for (j = 1; j <= n_melt; ++j)
					be_node_set_reg_class_in(new_ck, j, entry->cls);

				ir_nodeset_insert(&entry->copies, new_ck);

				/* find scheduling point */
				sched_pt = ref_mode_T;
				do {
					/* just walk along the schedule until a non-Keep/CopyKeep node is found */
					sched_pt = sched_next(sched_pt);
				} while (be_is_Keep(sched_pt) || be_is_CopyKeep(sched_pt));

				sched_add_before(sched_pt, new_ck);
				DB((dbg_constr, LEVEL_1, "created %+F, scheduled before %+F\n", new_ck, sched_pt));

				/* finally: kill the reference copykeep */
				kill_node(ref);
			}
		}

		obstack_free(&obst, NULL);
	}
}

void assure_constraints(ir_graph *irg)
{
	constraint_env_t          cenv;
	ir_nodehashmap_iterator_t map_iter;
	ir_nodehashmap_entry_t    map_entry;

	FIRM_DBG_REGISTER(dbg_constr, "firm.be.lower.constr");

	cenv.irg = irg;
	ir_nodehashmap_init(&cenv.op_set);
	obstack_init(&cenv.obst);

	irg_block_walk_graph(irg, NULL, assure_constraints_walker, &cenv);

	/* melt copykeeps, pointing to projs of */
	/* the same mode_T node and keeping the */
	/* same operand                         */
	melt_copykeeps(&cenv);

	/* for all */
	foreach_ir_nodehashmap(&cenv.op_set, map_entry, map_iter) {
		op_copy_assoc_t          *entry = (op_copy_assoc_t*)map_entry.data;
		size_t                    n     = ir_nodeset_size(&entry->copies);
		ir_node                 **nodes = ALLOCAN(ir_node*, n);
		be_ssa_construction_env_t senv;

		/* put the node in an array */
		DBG((dbg_constr, LEVEL_1, "introduce copies for %+F ", map_entry.node));

		/* collect all copies */
		n = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			nodes[n++] = cp;
			DB((dbg_constr, LEVEL_1, ", %+F ", cp));
		}

		DB((dbg_constr, LEVEL_1, "\n"));

		/* introduce the copies for the operand and its copies */
		be_ssa_construction_init(&senv, irg);
		be_ssa_construction_add_copy(&senv, map_entry.node);
		be_ssa_construction_add_copies(&senv, nodes, n);
		be_ssa_construction_fix_users(&senv, map_entry.node);
		be_ssa_construction_destroy(&senv);

		/* Could be that not all CopyKeeps are really needed, */
		/* so we transform unnecessary ones into Keeps.       */
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp) && get_irn_n_edges(cp) < 1) {
				int      n   = get_irn_arity(cp);
				ir_node *keep;

				keep = be_new_Keep(get_nodes_block(cp), n, get_irn_in(cp) + 1);
				sched_replace(cp, keep);

				/* Set all ins (including the block) of the CopyKeep BAD to keep the verifier happy. */
				kill_node(cp);
			}
		}

		ir_nodeset_destroy(&entry->copies);
	}

	ir_nodehashmap_destroy(&cenv.op_set);
	obstack_free(&cenv.obst, NULL);
	be_invalidate_live_sets(irg);
}

/**
 * Push nodes that do not need to be permed through the Perm.
 * This is commonly a reload cascade at block ends.
 * @note This routine needs interference.
 * @note Probably, we can implement it a little more efficient.
 *       Especially searching the frontier lazily might be better.
 *
 * @param perm The perm
 * @param env  The lowerer environment
 *
 * @return     1, if there is something left to perm over.
 *             0, if removed the complete perm.
 */
static int push_through_perm(ir_node *perm)
{
	ir_graph *irg     = get_irn_irg(perm);
	ir_node *bl       = get_nodes_block(perm);
	int  arity        = get_irn_arity(perm);
	int *map;
	int *proj_map;
	bitset_t *moved   = bitset_alloca(arity);
	int n_moved;
	int new_size;
	ir_node *frontier = bl;
	int i, n;
	be_lv_t *lv = be_get_irg_liveness(irg);

	/* get some Proj and find out the register class of that Proj. */
	ir_node                     *one_proj = get_edge_src_irn(get_irn_out_edge_first_kind(perm, EDGE_KIND_NORMAL));
	const arch_register_class_t *cls      = arch_get_irn_reg_class(one_proj);
	assert(is_Proj(one_proj));

	DB((dbg_permmove, LEVEL_1, "perm move %+F irg %+F\n", perm, irg));

	/* Find the point in the schedule after which the
	 * potentially movable nodes must be defined.
	 * A Perm will only be pushed up to first instruction
	 * which lets an operand of itself die.
	 * If we would allow to move the Perm above this instruction,
	 * the former dead operand would be live now at the point of
	 * the Perm, increasing the register pressure by one.
	 */
	sched_foreach_reverse_before(perm, irn) {
		if (is_Phi(irn)) {
			frontier = irn;
			goto found_front;
		}
		be_foreach_use(irn, cls, in_req_, op, op_req_,
			if (!be_values_interfere(lv, op, one_proj)) {
				frontier = irn;
				goto found_front;
			}
		);
	}
found_front:

	DB((dbg_permmove, LEVEL_2, "\tfrontier: %+F\n", frontier));

	n_moved = 0;
	for (;;) {
		ir_node *const node = sched_prev(perm);
		if (node == frontier)
			break;

		be_foreach_use(node, cls, in_req, value, value_req,
			goto done;
		);

		be_foreach_definition(node, cls, value, req,
			if (req->type != arch_register_req_type_normal &&
			    req->type != arch_register_req_type_should_be_same)
				goto done;
		);

		DBG((dbg_permmove, LEVEL_2, "\tmoving %+F after %+F\n", node, perm));

		/* move the movable node in front of the Perm */
		sched_remove(node);
		sched_add_after(perm, node);

		/* Rewire Perm results to pushed through instruction. */
		foreach_out_edge_safe(perm, edge) {
			ir_node *const proj = get_edge_src_irn(edge);
			int      const pn   = get_Proj_proj(proj);
			ir_node *const in   = get_irn_n(perm, pn);
			if (in == node || (is_Proj(in) && get_Proj_pred(in) == node)) {
				/* Give it the proj's register. */
				arch_set_irn_register(in, arch_get_irn_register_out(perm, pn));
				/* Reroute all users of the proj to the moved node. */
				exchange(proj, in);
				bitset_set(moved, pn);
				++n_moved;
			}
		}
	}
done:

	/* well, we could not push anything through the perm */
	if (n_moved == 0)
		return 1;

	new_size = arity - n_moved;
	if (new_size == 0) {
		sched_remove(perm);
		kill_node(perm);
		return 0;
	}

	map      = ALLOCAN(int, new_size);
	proj_map = ALLOCAN(int, arity);
	memset(proj_map, -1, sizeof(proj_map[0]));
	n   = 0;
	for (i = 0; i < arity; ++i) {
		if (bitset_is_set(moved, i))
			continue;
		map[n]      = i;
		proj_map[i] = n;
		n++;
	}
	assert(n == new_size);
	foreach_out_edge(perm, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		int      pn   = get_Proj_proj(proj);
		pn = proj_map[pn];
		assert(pn >= 0);
		set_Proj_proj(proj, pn);
	}

	be_Perm_reduce(perm, new_size, map);
	return 1;
}

/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env)
{
	int perm_stayed;

	if (!be_is_Perm(irn))
		return;

	perm_stayed = push_through_perm(irn);
	if (perm_stayed)
		lower_perm_node(irn, (lower_env_t*)walk_env);
}

void lower_nodes_after_ra(ir_graph *irg, int do_copy)
{
	lower_env_t env;

	FIRM_DBG_REGISTER(dbg, "firm.be.lower");
	FIRM_DBG_REGISTER(dbg_permmove, "firm.be.lower.permmove");

	env.do_copy = do_copy;

	/* we will need interference */
	be_assure_live_chk(irg);

	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, &env);
}
