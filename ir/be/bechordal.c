/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 */
#include "bechordal_common.h"
#include "bechordal_draw.h"
#include "bechordal_t.h"
#include "beinsn_t.h"
#include "belive_t.h"
#include "besched.h"
#include "beirg.h"
#include "bemodule.h"
#include "debug.h"
#include "irdump.h"

#define USE_HUNGARIAN 0

#if USE_HUNGARIAN
#include "hungarian.h"
#else
#include "bipartite.h"
#endif

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static int get_next_free_reg(bitset_t *const available)
{
	return bitset_next_set(available, 0);
}

static unsigned const *get_decisive_partner_regs(be_operand_t const *const o1, size_t const n_regs)
{
	be_operand_t const *const o2 = o1->partner;
	if (!o2 || rbitset_contains(o1->regs, o2->regs, n_regs)) {
		return o1->regs;
	} else if (rbitset_contains(o2->regs, o1->regs, n_regs)) {
		return o2->regs;
	} else {
		return NULL;
	}
}

static void pair_up_operands(be_chordal_env_t const *const env, be_insn_t *const insn)
{
	/* For each out operand, try to find an in operand which can be assigned the
	 * same register as the out operand. */
	int       const n_regs = env->cls->n_regs;
	unsigned *const bs     = rbitset_alloca(n_regs);
	be_lv_t  *const lv     = be_get_irg_liveness(env->irg);
	for (int j = 0; j < insn->use_start; ++j) {
		/* Try to find an in operand which has ... */
		be_operand_t       *smallest        = NULL;
		int                 smallest_n_regs = n_regs + 1;
		be_operand_t *const out_op          = &insn->ops[j];
		for (int i = insn->use_start; i < insn->n_ops; ++i) {
			be_operand_t *const op = &insn->ops[i];
			if (op->partner || be_values_interfere(lv, insn->irn, op->carrier))
				continue;

			rbitset_copy(bs, op->regs, n_regs);
			rbitset_and(bs, out_op->regs, n_regs);
			int const n_total = rbitset_popcount(op->regs, n_regs);
			if (!rbitset_is_empty(bs, n_regs) && n_total < smallest_n_regs) {
				smallest        = op;
				smallest_n_regs = n_total;
			}
		}

		if (smallest != NULL) {
			for (int i = insn->use_start; i < insn->n_ops; ++i) {
				if (insn->ops[i].carrier == smallest->carrier)
					insn->ops[i].partner = out_op;
			}

			out_op->partner   = smallest;
			smallest->partner = out_op;
		}
	}
}

static bool list_contains_irn(ir_node *const *const list, size_t const n, ir_node *const irn)
{
	for (ir_node *const *i = list; i != list + n; ++i) {
		if (*i == irn)
			return true;
	}
	return false;
}

static void handle_constraints(be_chordal_env_t *const env, ir_node *const irn)
{
	void *const base = obstack_base(&env->obst);
	be_insn_t  *insn = be_scan_insn(env, irn);

	/* Perms inserted before the constraint handling phase are considered to be
	 * correctly precolored. These Perms arise during the ABI handling phase. */
	if (!insn || is_Phi(irn))
		goto end;

	/* Prepare the constraint handling of this node.
	 * Perms are constructed and Copies are created for constrained values
	 * interfering with the instruction. */
	ir_node *const perm = pre_process_constraints(env, &insn);

	/* find suitable in operands to the out operands of the node. */
	pair_up_operands(env, insn);

	/* Look at the in/out operands and add each operand (and its possible partner)
	 * to a bipartite graph (left: nodes with partners, right: admissible colors). */
	int                        n_alloc     = 0;
	int                  const n_regs      = env->cls->n_regs;
	ir_node            **const alloc_nodes = ALLOCAN(ir_node*, n_regs);
	pmap                *const partners    = pmap_create();
#if USE_HUNGARIAN
	hungarian_problem_t *const bp          = hungarian_new(n_regs, n_regs, HUNGARIAN_MATCH_PERFECT);
#else
	bipartite_t         *const bp          = bipartite_new(n_regs, n_regs);
#endif
	for (int i = 0; i < insn->n_ops; ++i) {
		/* If the operand has no partner or the partner has not been marked
		 * for allocation, determine the admissible registers and mark it
		 * for allocation by associating the node and its partner with the
		 * set of admissible registers via a bipartite graph. */
		be_operand_t *const op = &insn->ops[i];
		if (op->partner && pmap_contains(partners, op->partner->carrier))
			continue;

		ir_node *const partner = op->partner ? op->partner->carrier : NULL;
		pmap_insert(partners, op->carrier, partner);
		if (partner != NULL)
			pmap_insert(partners, partner, op->carrier);

		/* Don't insert a node twice. */
		if (list_contains_irn(alloc_nodes, n_alloc, op->carrier))
			continue;

		alloc_nodes[n_alloc] = op->carrier;

		DBG((dbg, LEVEL_2, "\tassociating %+F and %+F\n", op->carrier, partner));

		unsigned const *const bs = get_decisive_partner_regs(op, n_regs);
		if (bs) {
			DBG((dbg, LEVEL_2, "\tallowed registers for %+F: %B\n", op->carrier, bs));

			rbitset_foreach(bs, n_regs, col) {
#if USE_HUNGARIAN
				hungarian_add(bp, n_alloc, col, 1);
#else
				bipartite_add(bp, n_alloc, col);
#endif
			}
		} else {
			DBG((dbg, LEVEL_2, "\tallowed registers for %+F: none\n", op->carrier));
		}

		n_alloc++;
	}

	/* Put all nodes which live through the constrained instruction also to the
	 * allocation bipartite graph. They are considered unconstrained. */
	if (perm != NULL) {
		be_lv_t *const lv = be_get_irg_liveness(env->irg);
		foreach_out_edge(perm, edge) {
			ir_node *const proj = get_edge_src_irn(edge);
			assert(is_Proj(proj));

			if (!be_values_interfere(lv, proj, irn) || pmap_contains(partners, proj))
				continue;

			/* Don't insert a node twice. */
			if (list_contains_irn(alloc_nodes, n_alloc, proj))
				continue;

			assert(n_alloc < n_regs);

			alloc_nodes[n_alloc] = proj;
			pmap_insert(partners, proj, NULL);

			bitset_foreach(env->allocatable_regs, col) {
#if USE_HUNGARIAN
				hungarian_add(bp, n_alloc, col, 1);
#else
				bipartite_add(bp, n_alloc, col);
#endif
			}

			n_alloc++;
		}
	}

	/* Compute a valid register allocation. */
	int *const assignment = ALLOCAN(int, n_regs);
#if USE_HUNGARIAN
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);
	int const match_res = hungarian_solve(bp, assignment, NULL, 1);
	assert(match_res == 0 && "matching failed");
#else
	bipartite_matching(bp, assignment);
#endif

	/* Assign colors obtained from the matching. */
	for (int i = 0; i < n_alloc; ++i) {
		assert(assignment[i] >= 0 && "there must have been a register assigned (node not register pressure faithful?)");
		arch_register_t const *const reg = arch_register_for_index(env->cls, assignment[i]);

		ir_node *const irn = alloc_nodes[i];
		if (irn != NULL) {
			arch_set_irn_register(irn, reg);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));
		}

		ir_node *const partner = pmap_get(ir_node, partners, alloc_nodes[i]);
		if (partner != NULL) {
			arch_set_irn_register(partner, reg);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", partner, reg->name));
		}
	}

	/* Allocate the non-constrained Projs of the Perm. */
	if (perm != NULL) {
		bitset_t *const available = bitset_alloca(n_regs);
		bitset_copy(available, env->allocatable_regs);

		/* Put the colors of all Projs in a bitset. */
		foreach_out_edge(perm, edge) {
			ir_node               *const proj = get_edge_src_irn(edge);
			arch_register_t const *const reg  = arch_get_irn_register(proj);
			if (reg != NULL)
				bitset_clear(available, reg->index);
		}

		/* Assign the not yet assigned Projs of the Perm a suitable color. */
		foreach_out_edge(perm, edge) {
			ir_node               *const proj = get_edge_src_irn(edge);
			arch_register_t const *const reg  = arch_get_irn_register(proj);

			DBG((dbg, LEVEL_2, "\tchecking reg of %+F: %s\n", proj, reg ? reg->name : "<none>"));

			if (reg == NULL) {
				size_t const col = get_next_free_reg(available);
				arch_register_t const *const new_reg = arch_register_for_index(env->cls, col);
				bitset_clear(available, new_reg->index);
				arch_set_irn_register(proj, new_reg);
				DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", proj, new_reg->name));
			}
		}
	}

#if USE_HUNGARIAN
	hungarian_free(bp);
#else
	bipartite_free(bp);
#endif
	pmap_destroy(partners);

end:
	obstack_free(&env->obst, base);
}

/**
 * Handle constraint nodes in each basic block.
 * handle_constraints() inserts Perm nodes which perm
 * over all values live at the constrained node right in front
 * of the constrained node. These Perms signal a constrained node.
 * For further comments, refer to handle_constraints().
 */
static void constraints(ir_node *const bl, void *const data)
{
	be_chordal_env_t *const env = (be_chordal_env_t*)data;
	sched_foreach_safe(bl, irn) {
		handle_constraints(env, irn);
	}
}

static void assign(ir_node *const block, void *const env_ptr)
{
	be_chordal_env_t *const env  = (be_chordal_env_t*)env_ptr;
	struct list_head *const head = get_block_border_head(env, block);

	DBG((dbg, LEVEL_4, "Assigning colors for block %+F\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	foreach_border_head(head, b) {
		DBG((dbg, LEVEL_4, "\t%s %+F/%d\n", b->is_def ? "def" : "use",
					b->irn, get_irn_idx(b->irn)));
	}

	bitset_t *const available = bitset_alloca(env->allocatable_regs->size);
	bitset_copy(available, env->allocatable_regs);

	/* Mind that the sequence of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work. */
	foreach_border_head(head, b) {
		ir_node *const irn = b->irn;

		/* Assign a color, if it is a local def. Global defs already have a
		 * color. */
		if (!b->is_def) {
			/* Make the color available upon a use. */
			arch_register_t const *const reg = arch_get_irn_register(irn);
			assert(reg && "Register must have been assigned");
			bitset_set(available, reg->index);
		} else {
			int                    col;
			arch_register_t const *reg = arch_get_irn_register(irn);
			/* All live-ins must have a register assigned. (The dominators were
			 * allocated before.) */
			assert(b->is_real || reg);
			if (reg) {
				DBG((dbg, LEVEL_4, "%+F has reg %s\n", irn, reg->name));
				col = reg->index;
				assert(bitset_is_set(available, col) && "pre-colored register must be free");
			} else {
				assert(!arch_irn_is_ignore(irn));
				col = get_next_free_reg(available);
				reg = arch_register_for_index(env->cls, col);
				arch_set_irn_register(irn, reg);
			}
			bitset_clear(available, col);

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n", reg->name, col, irn));
		}
	}
}

static void be_ra_chordal_color(be_chordal_env_t *const chordal_env)
{
	char            buf[256];
	ir_graph *const irg = chordal_env->irg;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	be_assure_live_sets(irg);

	be_timer_push(T_CONSTR);

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, chordal_env);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		snprintf(buf, sizeof(buf), "%s-constr", chordal_env->cls->name);
		dump_ir_graph(irg, buf);
	}

	be_timer_pop(T_CONSTR);

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, create_borders, NULL, chordal_env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, chordal_env);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_TREE_INTV) {
		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", chordal_env->cls->name, irg);
		plotter_t *const plotter = new_plotter_ps(buf);
		draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter);
		plotter_free(plotter);
	}
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal)
void be_init_chordal(void)
{
	static be_ra_chordal_coloring_t coloring = {
		be_ra_chordal_color
	};
	FIRM_DBG_REGISTER(dbg, "firm.be.chordal");

	be_register_chordal_coloring("default", &coloring);
}
