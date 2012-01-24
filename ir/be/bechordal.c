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
 * @brief       Chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 */
#include "config.h"

#include <ctype.h>

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "raw_bitset.h"
#include "iterator.h"
#include "bipartite.h"
#include "hungarian.h"

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "irtools.h"
#include "debug.h"
#include "iredges.h"

#include "beutil.h"
#include "besched.h"
#include "besched.h"
#include "belive_t.h"
#include "benode.h"
#include "bearch.h"
#include "beirgmod.h"
#include "beifg.h"
#include "beinsn_t.h"
#include "bestatevent.h"
#include "beirg.h"
#include "beintlive_t.h"
#include "bera.h"
#include "bechordal_t.h"
#include "bechordal_draw.h"
#include "bemodule.h"
#include "bearch.h"
#include "bechordal_common.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define DUMP_INTERVALS

typedef struct be_chordal_alloc_env_t {
	be_chordal_env_t *chordal_env;

	pset *pre_colored;     /**< Set of precolored nodes. */
	bitset_t *live;        /**< A liveness bitset. */
	bitset_t *tmp_colors;  /**< An auxiliary bitset which is as long as the number of colors in the class. */
	bitset_t *colors;      /**< The color mask. */
	bitset_t *in_colors;   /**< Colors used by live in values. */
	int colors_n;          /**< The number of colors. */
} be_chordal_alloc_env_t;

static int get_next_free_reg(const be_chordal_alloc_env_t *alloc_env, bitset_t *colors)
{
	bitset_t *tmp = alloc_env->tmp_colors;
	bitset_copy(tmp, colors);
	bitset_flip_all(tmp);
	bitset_and(tmp, alloc_env->chordal_env->allocatable_regs);
	return bitset_next_set(tmp, 0);
}

static bitset_t *get_decisive_partner_regs(bitset_t *bs, const be_operand_t *o1, const be_operand_t *o2)
{
	bitset_t *res = bs;

	if (!o1) {
		bitset_copy(bs, o2->regs);
		return bs;
	}

	if (!o2) {
		bitset_copy(bs, o1->regs);
		return bs;
	}

	assert(o1->req->cls == o2->req->cls || ! o1->req->cls || ! o2->req->cls);

	if (bitset_contains(o1->regs, o2->regs)) {
		bitset_copy(bs, o1->regs);
	} else if (bitset_contains(o2->regs, o1->regs)) {
		bitset_copy(bs, o2->regs);
	} else {
		res = NULL;
	}

	return res;
}

static void pair_up_operands(const be_chordal_alloc_env_t *alloc_env, be_insn_t *insn)
{
	const be_chordal_env_t *env = alloc_env->chordal_env;
	bitset_t               *bs  = bitset_alloca(env->cls->n_regs);
	int                     i;
	int                     j;

	/*
	 * For each out operand, try to find an in operand which can be assigned the
	 * same register as the out operand.
	 */
	for (j = 0; j < insn->use_start; ++j) {
		be_operand_t *smallest        = NULL;
		int           smallest_n_regs = env->cls->n_regs + 1;
		be_operand_t *out_op          = &insn->ops[j];

		/* Try to find an in operand which has ... */
		for (i = insn->use_start; i < insn->n_ops; ++i) {
			int           n_total;
			be_operand_t *op = &insn->ops[i];
			be_lv_t      *lv;

			if (op->partner != NULL)
				continue;
			lv = be_get_irg_liveness(env->irg);
			if (be_values_interfere(lv, op->irn, op->carrier))
				continue;

			bitset_copy(bs, op->regs);
			bitset_and(bs, out_op->regs);
			n_total = bitset_popcount(op->regs);

			if (!bitset_is_empty(bs) && n_total < smallest_n_regs) {
				smallest        = op;
				smallest_n_regs = n_total;
			}
		}

		if (smallest != NULL) {
			for (i = insn->use_start; i < insn->n_ops; ++i) {
				if (insn->ops[i].carrier == smallest->carrier)
					insn->ops[i].partner = out_op;
			}

			out_op->partner   = smallest;
			smallest->partner = out_op;
		}
	}
}

static ir_node *handle_constraints(be_chordal_alloc_env_t *alloc_env,
                                   ir_node *irn)
{
	int n_regs;
	bitset_t *bs;
	ir_node **alloc_nodes;
	//hungarian_problem_t *bp;
	int *assignment;
	pmap *partners;
	int i, n_alloc;
	size_t col;
	const ir_edge_t *edge;
	ir_node *perm = NULL;
	//int match_res, cost;
	be_chordal_env_t *env  = alloc_env->chordal_env;
	void *base             = obstack_base(env->obst);
	be_insn_t *insn        = chordal_scan_insn(env, irn);
	ir_node *res           = insn->next_insn;
	bipartite_t *bp;

	if (insn->pre_colored) {
		int i;
		for (i = 0; i < insn->use_start; ++i)
			pset_insert_ptr(alloc_env->pre_colored, insn->ops[i].carrier);
	}

	/*
	 * Perms inserted before the constraint handling phase are considered to be
	 * correctly precolored. These Perms arise during the ABI handling phase.
	 */
	if (!insn->has_constraints || is_Phi(irn))
		goto end;

	n_regs      = env->cls->n_regs;
	bs          = bitset_alloca(n_regs);
	alloc_nodes = ALLOCAN(ir_node*, n_regs);
	//bp          = hungarian_new(n_regs, n_regs, 2, HUNGARIAN_MATCH_PERFECT);
	bp          = bipartite_new(n_regs, n_regs);
	assignment  = ALLOCAN(int, n_regs);
	partners    = pmap_create();

	/*
	 * prepare the constraint handling of this node.
	 * Perms are constructed and Copies are created for constrained values
	 * interfering with the instruction.
	 */
	perm = pre_process_constraints(alloc_env->chordal_env, &insn);

	/* find suitable in operands to the out operands of the node. */
	pair_up_operands(alloc_env, insn);

	/*
	 * look at the in/out operands and add each operand (and its possible partner)
	 * to a bipartite graph (left: nodes with partners, right: admissible colors).
	 */
	for (i = 0, n_alloc = 0; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];

		/*
		 * If the operand has no partner or the partner has not been marked
		 * for allocation, determine the admissible registers and mark it
		 * for allocation by associating the node and its partner with the
		 * set of admissible registers via a bipartite graph.
		 */
		if (!op->partner || !pmap_contains(partners, op->partner->carrier)) {
			ir_node *partner = op->partner ? op->partner->carrier : NULL;
			int i;

			pmap_insert(partners, op->carrier, partner);
			if (partner != NULL)
				pmap_insert(partners, partner, op->carrier);

			/* don't insert a node twice */
			for (i = 0; i < n_alloc; ++i) {
				if (alloc_nodes[i] == op->carrier) {
					break;
				}
			}
			if (i < n_alloc)
				continue;

			alloc_nodes[n_alloc] = op->carrier;

			DBG((dbg, LEVEL_2, "\tassociating %+F and %+F\n", op->carrier,
			     partner));

			bitset_clear_all(bs);
			get_decisive_partner_regs(bs, op, op->partner);

			DBG((dbg, LEVEL_2, "\tallowed registers for %+F: %B\n", op->carrier,
			     bs));

			bitset_foreach(bs, col) {
				//hungarian_add(bp, n_alloc, col, 1);
				bipartite_add(bp, n_alloc, col);
			}

			n_alloc++;
		}
	}

	/*
	 * Put all nodes which live through the constrained instruction also to the
	 * allocation bipartite graph. They are considered unconstrained.
	 */
	if (perm != NULL) {
		foreach_out_edge(perm, edge) {
			int i;
			ir_node *proj = get_edge_src_irn(edge);
			be_lv_t *lv   = be_get_irg_liveness(env->irg);

			assert(is_Proj(proj));

			if (!be_values_interfere(lv, proj, irn)
			    || pmap_contains(partners, proj))
				continue;

			/* don't insert a node twice */
			for (i = 0; i < n_alloc; ++i) {
				if (alloc_nodes[i] == proj) {
					break;
				}
			}
			if (i < n_alloc)
				continue;


			assert(n_alloc < n_regs);

			alloc_nodes[n_alloc] = proj;
			pmap_insert(partners, proj, NULL);

			bitset_foreach(env->allocatable_regs, col) {
				//hungarian_add(bp, n_alloc, col, 1);
				bipartite_add(bp, n_alloc, col);
			}

			n_alloc++;
		}
	}

	/* Compute a valid register allocation. */
#if 0
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);
	match_res = hungarian_solve(bp, assignment, &cost, 1);
	assert(match_res == 0 && "matching failed");
#else
	/*bipartite_dump_f(stderr, bp);*/
	bipartite_matching(bp, assignment);
#endif

	/* Assign colors obtained from the matching. */
	for (i = 0; i < n_alloc; ++i) {
		const arch_register_t *reg;
		ir_node *irn;

		assert(assignment[i] >= 0 && "there must have been a register assigned (node not register pressure faithful?)");
		reg = arch_register_for_index(env->cls, assignment[i]);

		irn = alloc_nodes[i];
		if (irn != NULL) {
			arch_set_irn_register(irn, reg);
			(void) pset_hinsert_ptr(alloc_env->pre_colored, irn);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));
		}

		irn = (ir_node*)pmap_get(partners, alloc_nodes[i]);
		if (irn != NULL) {
			arch_set_irn_register(irn, reg);
			(void) pset_hinsert_ptr(alloc_env->pre_colored, irn);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));
		}
	}

	/* Allocate the non-constrained Projs of the Perm. */
	if (perm != NULL) {
		bitset_clear_all(bs);

		/* Put the colors of all Projs in a bitset. */
		foreach_out_edge(perm, edge) {
			ir_node *proj              = get_edge_src_irn(edge);
			const arch_register_t *reg = arch_get_irn_register(proj);

			if (reg != NULL)
				bitset_set(bs, reg->index);
		}

		/* Assign the not yet assigned Projs of the Perm a suitable color. */
		foreach_out_edge(perm, edge) {
			ir_node *proj              = get_edge_src_irn(edge);
			const arch_register_t *reg = arch_get_irn_register(proj);

			DBG((dbg, LEVEL_2, "\tchecking reg of %+F: %s\n", proj, reg ? reg->name : "<none>"));

			if (reg == NULL) {
				col = get_next_free_reg(alloc_env, bs);
				reg = arch_register_for_index(env->cls, col);
				bitset_set(bs, reg->index);
				arch_set_irn_register(proj, reg);
				pset_insert_ptr(alloc_env->pre_colored, proj);
				DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", proj, reg->name));
			}
		}
	}

	bipartite_free(bp);
	//hungarian_free(bp);
	pmap_destroy(partners);

end:
	obstack_free(env->obst, base);
	return res;
}

/**
 * Handle constraint nodes in each basic block.
 * handle_constraints() inserts Perm nodes which perm
 * over all values live at the constrained node right in front
 * of the constrained node. These Perms signal a constrained node.
 * For further comments, refer to handle_constraints().
 */
static void constraints(ir_node *bl, void *data)
{
	be_chordal_alloc_env_t *env    = (be_chordal_alloc_env_t*)data;
	ir_node                *irn;

	for (irn = sched_first(bl); !sched_is_end(irn);) {
		irn = handle_constraints(env, irn);
	}
}

static void assign(ir_node *block, void *env_ptr)
{
	be_chordal_alloc_env_t *alloc_env = (be_chordal_alloc_env_t*)env_ptr;
	be_chordal_env_t *env       = alloc_env->chordal_env;
	bitset_t *live              = alloc_env->live;
	bitset_t *colors            = alloc_env->colors;
	bitset_t *in_colors         = alloc_env->in_colors;
	struct list_head *head      = get_block_border_head(env, block);
	be_lv_t *lv                 = be_get_irg_liveness(env->irg);

	const ir_node *irn;
	border_t *b;
	int idx;

	bitset_clear_all(colors);
	bitset_clear_all(live);
	bitset_clear_all(in_colors);

	DBG((dbg, LEVEL_4, "Assigning colors for block %+F\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	list_for_each_entry(border_t, b, head, list) {
		DBG((dbg, LEVEL_4, "\t%s %+F/%d\n", b->is_def ? "def" : "use",
					b->irn, get_irn_idx(b->irn)));
	}

	/*
	 * Add initial defs for all values live in.
	 * Since their colors have already been assigned (The dominators were
	 * allocated before), we have to mark their colors as used also.
	 */
	be_lv_foreach(lv, block, be_lv_state_in, idx) {
		irn = be_lv_get_irn(lv, block, idx);
		if (has_reg_class(env, irn)) {
			const arch_register_t *reg = arch_get_irn_register(irn);
			int col;

			assert(reg && "Node must have been assigned a register");
			col = arch_register_get_index(reg);

			DBG((dbg, LEVEL_4, "%+F has reg %s\n", irn, reg->name));

			/* Mark the color of the live in value as used. */
			bitset_set(colors, col);
			bitset_set(in_colors, col);

			/* Mark the value live in. */
			bitset_set(live, get_irn_idx(irn));
		}
	}

	/*
	 * Mind that the sequence of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work.
	 */
	list_for_each_entry_reverse(border_t, b, head, list) {
		ir_node *irn = b->irn;
		int nr       = get_irn_idx(irn);
		int ignore   = arch_irn_is_ignore(irn);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if (b->is_def && !be_is_live_in(lv, block, irn)) {
			const arch_register_t *reg;
			int col;

			if (ignore || pset_find_ptr(alloc_env->pre_colored, irn)) {
				reg = arch_get_irn_register(irn);
				col = reg->index;
				assert(!bitset_is_set(colors, col) && "pre-colored register must be free");
			} else {
				col = get_next_free_reg(alloc_env, colors);
				reg = arch_register_for_index(env->cls, col);
				assert(arch_get_irn_register(irn) == NULL && "This node must not have been assigned a register yet");
			}

			bitset_set(colors, col);
			arch_set_irn_register(irn, reg);

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n", arch_register_get_name(reg), col, irn));

			assert(!bitset_is_set(live, nr) && "Value's definition must not have been encountered");
			bitset_set(live, nr);
		} else if (!b->is_def) {
			/* Clear the color upon a use. */
			const arch_register_t *reg = arch_get_irn_register(irn);
			int col;

			assert(reg && "Register must have been assigned");

			col = arch_register_get_index(reg);

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}
}

void be_ra_chordal_color(be_chordal_env_t *chordal_env)
{
	be_chordal_alloc_env_t env;
	char buf[256];
	const arch_register_class_t *cls = chordal_env->cls;

	int       colors_n = arch_register_class_n_regs(cls);
	ir_graph *irg      = chordal_env->irg;

	be_assure_live_sets(irg);
	assure_doms(irg);

	env.chordal_env   = chordal_env;
	env.colors_n      = colors_n;
	env.colors        = bitset_alloca(colors_n);
	env.tmp_colors    = bitset_alloca(colors_n);
	env.in_colors     = bitset_alloca(colors_n);
	env.pre_colored   = pset_new_ptr_default();

	be_timer_push(T_SPLIT);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_SPLIT) {
		snprintf(buf, sizeof(buf), "%s-split", chordal_env->cls->name);
		dump_ir_graph(chordal_env->irg, buf);
	}

	be_timer_pop(T_SPLIT);

	be_timer_push(T_CONSTR);

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, &env);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		snprintf(buf, sizeof(buf), "%s-constr", chordal_env->cls->name);
		dump_ir_graph(chordal_env->irg, buf);
	}

	be_timer_pop(T_CONSTR);

	env.live = bitset_malloc(get_irg_last_idx(chordal_env->irg));

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, create_borders, NULL, env.chordal_env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, &env);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_TREE_INTV) {
		plotter_t *plotter;
		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", chordal_env->cls->name, irg);
		plotter = new_plotter_ps(buf);
		draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter);
		plotter_free(plotter);
	}

	bitset_free(env.live);
	del_pset(env.pre_colored);
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
