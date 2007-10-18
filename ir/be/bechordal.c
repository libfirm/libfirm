/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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
#include "irbitset.h"
#include "debug.h"
#include "xmalloc.h"
#include "iredges.h"

#include "beutil.h"
#include "besched.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bearch_t.h"
#include "beirgmod.h"
#include "beifg.h"
#include "beinsn_t.h"
#include "bestatevent.h"
#include "beirg_t.h"
#include "beintlive_t.h"
#include "bera.h"
#include "bechordal_t.h"
#include "bechordal_draw.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define NO_COLOR (-1)

#define DUMP_INTERVALS

typedef struct _be_chordal_alloc_env_t {
	be_chordal_env_t *chordal_env;

	pset *pre_colored;              /**< Set of precolored nodes. */
	bitset_t *live;				    /**< A liveness bitset. */
	bitset_t *tmp_colors;           /**< An auxiliary bitset which is as long as the number of colors in the class. */
	bitset_t *colors;			    /**< The color mask. */
	bitset_t *in_colors;            /**< Colors used by live in values. */
	int colors_n;                   /**< The number of colors. */
} be_chordal_alloc_env_t;

#include "fourcc.h"

/* Make a fourcc for border checking. */
#define BORDER_FOURCC				FOURCC('B', 'O', 'R', 'D')

#if 0
static void check_border_list(struct list_head *head)
{
  border_t *x;
  list_for_each_entry(border_t, x, head, list) {
    assert(x->magic == BORDER_FOURCC);
  }
}

static void check_heads(be_chordal_env_t *env)
{
  pmap_entry *ent;
  for(ent = pmap_first(env->border_heads); ent; ent = pmap_next(env->border_heads)) {
    /* ir_printf("checking border list of block %+F\n", ent->key); */
    check_border_list(ent->value);
  }
}
#endif

/**
 * Add an interval border to the list of a block's list
 * of interval border.
 * @note You always have to create the use before the def.
 * @param env The environment.
 * @param head The list head to enqueue the borders.
 * @param irn The node (value) the border belongs to.
 * @param pressure The pressure at this point in time.
 * @param step A time step for the border.
 * @param is_def Is the border a use or a def.
 * @return The created border.
 */
static INLINE border_t *border_add(be_chordal_env_t *env, struct list_head *head,
			ir_node *irn, unsigned step, unsigned pressure,
			unsigned is_def, unsigned is_real)
{
	border_t *b;

	if(!is_def) {
		border_t *def;

		b = obstack_alloc(env->obst, sizeof(*b));

		/* also allocate the def and tie it to the use. */
		def = obstack_alloc(env->obst, sizeof(*def));
		memset(def, 0, sizeof(*def));
		b->other_end = def;
		def->other_end = b;

		/*
		 * Set the link field of the irn to the def.
		 * This strongly relies on the fact, that the use is always
		 * made before the def.
		 */
		set_irn_link(irn, def);

		DEBUG_ONLY(b->magic = BORDER_FOURCC);
		DEBUG_ONLY(def->magic = BORDER_FOURCC);
	}

	/*
	 * If the def is encountered, the use was made and so was the
	 * the def node (see the code above). It was placed into the
	 * link field of the irn, so we can get it there.
	 */
	else {
		b = get_irn_link(irn);

		DEBUG_ONLY(assert(b && b->magic == BORDER_FOURCC && "Illegal border encountered"));
	}

	b->pressure = pressure;
	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((dbg, LEVEL_5, "\t\t%s adding %+F, step: %d\n", is_def ? "def" : "use", irn, step));


	return b;
}

/**
 * Check, if an irn is of the register class currently under processing.
 * @param env The chordal environment.
 * @param irn The node.
 * @return 1, if the node is of that register class, 0 if not.
 */
static INLINE int has_reg_class(const be_chordal_env_t *env, const ir_node *irn)
{
	return arch_irn_consider_in_reg_alloc(env->birg->main_env->arch_env, env->cls, irn);
}

#define has_limited_constr(req, irn) \
	(arch_get_register_req(arch_env, (req), irn, -1) && (req)->type == arch_register_req_type_limited)

static int get_next_free_reg(const be_chordal_alloc_env_t *alloc_env, bitset_t *colors)
{
	bitset_t *tmp = alloc_env->tmp_colors;
	bitset_copy(tmp, colors);
	bitset_or(tmp, alloc_env->chordal_env->ignore_colors);
	return bitset_next_clear(tmp, 0);
}

static bitset_t *get_decisive_partner_regs(bitset_t *bs, const be_operand_t *o1, const be_operand_t *o2)
{
	bitset_t *res = bs;

	if(!o1) {
		bitset_copy(bs, o2->regs);
		return bs;
	}

	if(!o2) {
		bitset_copy(bs, o1->regs);
		return bs;
	}

	assert(o1->req->cls == o2->req->cls || ! o1->req->cls || ! o2->req->cls);

	if(bitset_contains(o1->regs, o2->regs))
		bitset_copy(bs, o1->regs);
	else if(bitset_contains(o2->regs, o1->regs))
		bitset_copy(bs, o2->regs);
	else
		res = NULL;

	return res;
}

static be_insn_t *chordal_scan_insn(be_chordal_env_t *env, ir_node *irn)
{
	be_insn_env_t ie;

	ie.ignore_colors = env->ignore_colors;
	ie.aenv          = env->birg->main_env->arch_env;
	ie.obst          = env->obst;
	ie.cls           = env->cls;
	return be_scan_insn(&ie, irn);
}

static ir_node *prepare_constr_insn(be_chordal_env_t *env, ir_node *irn)
{
	const be_irg_t *birg   = env->birg;
	const arch_env_t *aenv = birg->main_env->arch_env;
	bitset_t *tmp          = bitset_alloca(env->cls->n_regs);
	bitset_t *def_constr   = bitset_alloca(env->cls->n_regs);
	ir_node *bl            = get_nodes_block(irn);
	be_lv_t *lv            = env->birg->lv;

	be_insn_t *insn;
	int i, j;

	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		ir_node *op = get_irn_n(irn, i);
		ir_node *copy;
		const arch_register_t *reg;
		const arch_register_req_t *req;

		if (arch_get_irn_reg_class(aenv, irn, i) != env->cls)
			continue;

		reg = arch_get_irn_register(aenv, op);

		if (reg == NULL || !arch_register_type_is(reg, ignore))
			continue;
		if(arch_register_type_is(reg, joker))
			continue;

		req = arch_get_register_req(aenv, irn, i);
		if (!arch_register_req_is(req, limited))
			continue;

		if (rbitset_is_set(req->limited, reg->index))
			continue;

		copy = be_new_Copy(env->cls, env->irg, bl, op);
		be_stat_ev("constr_copy", 1);

		sched_add_before(irn, copy);
		set_irn_n(irn, i, copy);
		DBG((dbg, LEVEL_3, "inserting ignore arg copy %+F for %+F pos %d\n", copy, irn, i));
	}

    insn = chordal_scan_insn(env, irn);

	if(!insn->has_constraints)
		goto end;

	/* insert copies for nodes that occur constrained more than once. */
	for(i = insn->use_start; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];

		if(!op->has_constraints)
			continue;

		for(j = i + 1; j < insn->n_ops; ++j) {
			ir_node *copy;
			be_operand_t *a_op = &insn->ops[j];

			if(a_op->carrier != op->carrier || !a_op->has_constraints)
				continue;

			/* if the constraint is the same, no copy is necessary
			 * TODO generalise unequal but overlapping constraints */
			if (a_op->req == op->req)
				continue;

			if (be_is_Copy(get_irn_n(insn->irn, a_op->pos)))
				continue;

			copy = be_new_Copy(env->cls, env->irg, bl, op->carrier);
			be_stat_ev("constr_copy", 1);

			sched_add_before(insn->irn, copy);
			set_irn_n(insn->irn, a_op->pos, copy);
			DBG((dbg, LEVEL_3, "inserting multiple constr copy %+F for %+F pos %d\n", copy, insn->irn, a_op->pos));
		}
	}

	/* collect all registers occuring in out constraints. */
	for(i = 0; i < insn->use_start; ++i) {
		be_operand_t *op = &insn->ops[i];
		if(op->has_constraints)
			bitset_or(def_constr, op->regs);
	}

	/*
		insert copies for all constrained arguments living through the node
		and being constrained to a register which also occurs in out constraints.
	*/
	for(i = insn->use_start; i < insn->n_ops; ++i) {
		ir_node *copy;
		be_operand_t *op = &insn->ops[i];

		bitset_copy(tmp, op->regs);
		bitset_and(tmp, def_constr);

		/*
			Check, if
			1) the operand is constrained.
			2) lives through the node.
			3) is constrained to a register occuring in out constraints.
		*/
		if(!op->has_constraints ||
		   !values_interfere(birg, insn->irn, op->carrier) ||
		   bitset_popcnt(tmp) == 0)
			continue;

		/*
		   only create the copy if the operand is no copy.
		   this is necessary since the assure constraints phase inserts
		   Copies and Keeps for operands which must be different from the
		   results. Additional copies here would destroy this.
		 */
		if (be_is_Copy(get_irn_n(insn->irn, op->pos)))
			continue;

		copy = be_new_Copy(env->cls, env->irg, bl, op->carrier);

		sched_add_before(insn->irn, copy);
		set_irn_n(insn->irn, op->pos, copy);
		DBG((dbg, LEVEL_3, "inserting constr copy %+F for %+F pos %d\n", copy, insn->irn, op->pos));
		be_liveness_update(lv, op->carrier);
	}

end:
	obstack_free(env->obst, insn);
	return insn->next_insn;
}

static void pre_spill_prepare_constr_walker(ir_node *bl, void *data)
{
	be_chordal_env_t *env = data;
	ir_node *irn;
	for(irn = sched_first(bl); !sched_is_end(irn);) {
		irn = prepare_constr_insn(env, irn);
	}
}

void be_pre_spill_prepare_constr(be_chordal_env_t *cenv) {
	irg_block_walk_graph(cenv->irg, pre_spill_prepare_constr_walker, NULL, (void *) cenv);
}

static void pair_up_operands(const be_chordal_alloc_env_t *alloc_env, be_insn_t *insn)
{
	const be_chordal_env_t *env = alloc_env->chordal_env;

	int n_uses   = be_insn_n_uses(insn);
	int n_defs   = be_insn_n_defs(insn);
	bitset_t *bs = bitset_alloca(env->cls->n_regs);
	int *pairing = alloca(MAX(n_defs, n_uses) * sizeof(pairing[0]));

	int i, j;

	/*
		For each out operand, try to find an in operand which can be assigned the
		same register as the out operand.
	*/
	for (j = 0; j < insn->use_start; ++j) {
		int smallest         = -1;
		int smallest_n_regs  = 2 * env->cls->n_regs + 1;
		be_operand_t *out_op = &insn->ops[j];

		/* Try to find an in operand which has ... */
		for(i = insn->use_start; i < insn->n_ops; ++i) {
			int n_total;
			const be_operand_t *op = &insn->ops[i];

			if (op->partner != NULL)
				continue;
			if (values_interfere(env->birg, op->irn, op->carrier))
				continue;

			bitset_clear_all(bs);
			bitset_copy(bs, op->regs);
			bitset_and(bs, out_op->regs);
			n_total = bitset_popcnt(op->regs) + bitset_popcnt(out_op->regs);

			if (bitset_popcnt(bs) > 0 && n_total < smallest_n_regs) {
				smallest = i;
				smallest_n_regs = n_total;
			}
		}

		if (smallest >= 0) {
			be_operand_t *partner = &insn->ops[smallest];
			for(i = insn->use_start; i < insn->n_ops; ++i) {
				if(insn->ops[i].carrier == partner->carrier)
					insn->ops[i].partner = out_op;
			}

			out_op->partner  = partner;
			partner->partner = out_op;
		}
	}
}


static ir_node *pre_process_constraints(be_chordal_alloc_env_t *alloc_env,
                                        be_insn_t **the_insn)
{
	be_chordal_env_t *env       = alloc_env->chordal_env;
	const arch_env_t *aenv      = env->birg->main_env->arch_env;
	be_insn_t *insn             = *the_insn;
	ir_node *perm               = NULL;
	bitset_t *out_constr        = bitset_alloca(env->cls->n_regs);
	const ir_edge_t *edge;
	int i;

	assert(insn->has_constraints && "only do this for constrained nodes");

	/*
		Collect all registers that occur in output constraints.
		This is necessary, since if the insn has one of these as an input constraint
		and the corresponding operand interferes with the insn, the operand must
		be copied.
	*/
	for(i = 0; i < insn->use_start; ++i) {
		be_operand_t *op = &insn->ops[i];
		if(op->has_constraints)
			bitset_or(out_constr, op->regs);
	}

	/*
		Make the Perm, recompute liveness and re-scan the insn since the
		in operands are now the Projs of the Perm.
	*/
	perm = insert_Perm_after(env->birg, env->cls, sched_prev(insn->irn));

	/* Registers are propagated by insert_Perm_after(). Clean them here! */
	if(perm == NULL)
		return NULL;

	be_stat_ev("constr_perm", get_irn_arity(perm));
	foreach_out_edge(perm, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		arch_set_irn_register(aenv, proj, NULL);
	}

	/*
		We also have to re-build the insn since the input operands are now the Projs of
		the Perm. Recomputing liveness is also a good idea if a Perm is inserted, since
		the live sets may change.
	*/
	obstack_free(env->obst, insn);
	*the_insn = insn = chordal_scan_insn(env, insn->irn);

	/*
		Copy the input constraints of the insn to the Perm as output
		constraints. Succeeding phases (coalescing) will need that.
	*/
	for(i = insn->use_start; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];
		ir_node *proj = op->carrier;
		/*
			Note that the predecessor must not be a Proj of the Perm,
			since ignore-nodes are not Perm'ed.
		*/
		if(op->has_constraints &&  is_Proj(proj) && get_Proj_pred(proj) == perm) {
			be_set_constr_limited(perm, BE_OUT_POS(get_Proj_proj(proj)), op->req);
		}
	}

	return perm;
}

static ir_node *handle_constraints(be_chordal_alloc_env_t *alloc_env,
                                   ir_node *irn, int *silent)
{
	const arch_env_t *aenv;
	int n_regs;
	bitset_t *bs;
	ir_node **alloc_nodes;
	//hungarian_problem_t *bp;
	int *assignment;
	pmap *partners;
	int i, n_alloc;
	bitset_pos_t col;
	const ir_edge_t *edge;
	ir_node *perm = NULL;
	//int match_res, cost;
	be_chordal_env_t *env  = alloc_env->chordal_env;
	void *base             = obstack_base(env->obst);
	be_insn_t *insn        = chordal_scan_insn(env, irn);
	ir_node *res           = insn->next_insn;
	int be_silent          = *silent;
	be_irg_t *birg         = env->birg;
	bipartite_t *bp;

	if(insn->pre_colored) {
		int i;
		for(i = 0; i < insn->use_start; ++i)
			pset_insert_ptr(alloc_env->pre_colored, insn->ops[i].carrier);
	}

	/*
		If the current node is a barrier toggle the silent flag.
		If we are in the start block, we are ought to be silent at the beginning,
		so the toggling activates the constraint handling but skips the barrier.
		If we are in the end block we handle the in requirements of the barrier
		and set the rest to silent.
	*/
	if(be_is_Barrier(irn))
		*silent = !*silent;

	if(be_silent)
		goto end;

	/*
		Perms inserted before the constraint handling phase are considered to be
		correctly precolored. These Perms arise during the ABI handling phase.
	*/
	if(!insn->has_constraints)
		goto end;

	aenv        = env->birg->main_env->arch_env;
	n_regs      = env->cls->n_regs;
	bs          = bitset_alloca(n_regs);
	alloc_nodes = alloca(n_regs * sizeof(alloc_nodes[0]));
	//bp          = hungarian_new(n_regs, n_regs, 2, HUNGARIAN_MATCH_PERFECT);
	bp          = bipartite_new(n_regs, n_regs);
	assignment  = alloca(n_regs * sizeof(assignment[0]));
	partners    = pmap_create();

	/*
		prepare the constraint handling of this node.
		Perms are constructed and Copies are created for constrained values
		interfering with the instruction.
	*/
	perm = pre_process_constraints(alloc_env, &insn);

	/* find suitable in operands to the out operands of the node. */
	pair_up_operands(alloc_env, insn);

	/*
		look at the in/out operands and add each operand (and its possible partner)
		to a bipartite graph (left: nodes with partners, right: admissible colors).
	*/
	for(i = 0, n_alloc = 0; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];

		/*
			If the operand has no partner or the partner has not been marked
			for allocation, determine the admissible registers and mark it
			for allocation by associating the node and its partner with the
			set of admissible registers via a bipartite graph.
		*/
		if(!op->partner || !pmap_contains(partners, op->partner->carrier)) {
			ir_node *partner = op->partner ? op->partner->carrier : NULL;
			int i;

			pmap_insert(partners, op->carrier, partner);
			if(partner != NULL)
				pmap_insert(partners, partner, op->carrier);

			/* don't insert a node twice */
			for(i = 0; i < n_alloc; ++i) {
				if(alloc_nodes[i] == op->carrier) {
					break;
				}
			}
			if(i < n_alloc)
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
		Put all nodes which live through the constrained instruction also to the
		allocation bipartite graph. They are considered unconstrained.
	*/
	if(perm != NULL) {
		foreach_out_edge(perm, edge) {
			int i;
			ir_node *proj = get_edge_src_irn(edge);

			assert(is_Proj(proj));

			if(!values_interfere(birg, proj, irn) || pmap_contains(partners, proj))
				continue;

			/* don't insert a node twice */
			for(i = 0; i < n_alloc; ++i) {
				if(alloc_nodes[i] == proj) {
					break;
				}
			}
			if(i < n_alloc)
				continue;


			assert(n_alloc < n_regs);

			alloc_nodes[n_alloc] = proj;
			pmap_insert(partners, proj, NULL);

			bitset_clear_all(bs);
			arch_put_non_ignore_regs(aenv, env->cls, bs);
			bitset_andnot(bs, env->ignore_colors);
			bitset_foreach(bs, col) {
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
	bipartite_matching(bp, assignment);
#endif

	/* Assign colors obtained from the matching. */
	for(i = 0; i < n_alloc; ++i) {
		const arch_register_t *reg;
		ir_node *irn;

		assert(assignment[i] >= 0 && "there must have been a register assigned");
		reg = arch_register_for_index(env->cls, assignment[i]);
		assert(! (reg->type & arch_register_type_ignore));

		irn = alloc_nodes[i];
		if (irn != NULL) {
			arch_set_irn_register(aenv, irn, reg);
			(void) pset_hinsert_ptr(alloc_env->pre_colored, irn);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));
		}

		irn = pmap_get(partners, alloc_nodes[i]);
		if (irn != NULL) {
			arch_set_irn_register(aenv, irn, reg);
			(void) pset_hinsert_ptr(alloc_env->pre_colored, irn);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));
		}
	}

	/* Allocate the non-constrained Projs of the Perm. */
	if(perm != NULL) {
		bitset_clear_all(bs);

		/* Put the colors of all Projs in a bitset. */
		foreach_out_edge(perm, edge) {
			ir_node *proj              = get_edge_src_irn(edge);
			const arch_register_t *reg = arch_get_irn_register(aenv, proj);

			if(reg != NULL)
				bitset_set(bs, reg->index);
		}

		/* Assign the not yet assigned Projs of the Perm a suitable color. */
		foreach_out_edge(perm, edge) {
			ir_node *proj              = get_edge_src_irn(edge);
			const arch_register_t *reg = arch_get_irn_register(aenv, proj);

			DBG((dbg, LEVEL_2, "\tchecking reg of %+F: %s\n", proj, reg ? reg->name : "<none>"));

			if(reg == NULL) {
				col = get_next_free_reg(alloc_env, bs);
				reg = arch_register_for_index(env->cls, col);
				bitset_set(bs, reg->index);
				arch_set_irn_register(aenv, proj, reg);
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
	be_chordal_alloc_env_t *env = data;

	/*
		Start silent in the start block.
		The silence remains until the first barrier is seen.
		Each other block is begun loud.
	*/
	int silent                  = bl == get_irg_start_block(get_irn_irg(bl));
	ir_node *irn;

	/*
		If the block is the start block search the barrier and
		start handling constraints from there.
	*/

	for(irn = sched_first(bl); !sched_is_end(irn);) {
		irn = handle_constraints(env, irn, &silent);
	}
}

/**
 * Annotate the register pressure to the nodes and compute
 * the liveness intervals.
 * @param block The block to do it for.
 * @param env_ptr The environment.
 */
static void pressure(ir_node *block, void *env_ptr)
{
/* Convenience macro for a def */
#define border_def(irn, step, real) \
	border_add(env, head, irn, step, pressure--, 1, real)

/* Convenience macro for a use */
#define border_use(irn, step, real) \
	border_add(env, head, irn, step, ++pressure, 0, real)

	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env             = alloc_env->chordal_env;
	bitset_t *live                    = alloc_env->live;
	ir_node *irn;
	be_lv_t *lv                       = env->birg->lv;

	int i, n;
	bitset_pos_t elm;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;

	DBG((dbg, LEVEL_1, "Computing pressure in block %+F\n", block));
	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = obstack_alloc(env->obst, sizeof(*head));
	INIT_LIST_HEAD(head);
	assert(pmap_get(env->border_heads, block) == NULL);
	pmap_insert(env->border_heads, block, head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	be_lv_foreach(lv, block, be_lv_state_end, i) {
		ir_node *irn = be_lv_get_irn(lv, block, i);
		if(has_reg_class(env, irn)) {
			DBG((dbg, LEVEL_3, "\tMaking live: %+F/%d\n", irn, get_irn_idx(irn)));
			bitset_set(live, get_irn_idx(irn));
			border_use(irn, step, 0);
		}
	}
	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "\tinsn: %+F, pressure: %d\n", irn, pressure));
		DBG((dbg, LEVEL_2, "\tlive: %B\n", live));

		if (get_irn_mode(irn) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				/*
				 * If the node defines some value, which can put into a
				 * register of the current class, make a border for it.
				 */
				if(has_reg_class(env, proj)) {
					int nr = get_irn_idx(proj);

					bitset_clear(live, nr);
					border_def(proj, step, 1);
				}
			}
		}

		/*
		 * If the node defines some value, which can put into a
		 * register of the current class, make a border for it.
		 */
		if(has_reg_class(env, irn)) {
			int nr = get_irn_idx(irn);

			bitset_clear(live, nr);
			border_def(irn, step, 1);
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(has_reg_class(env, op)) {
					int nr = get_irn_idx(op);
					const char *msg = "-";

					if(!bitset_is_set(live, nr)) {
						border_use(op, step, 1);
						bitset_set(live, nr);
						msg = "X";
					}

					DBG((dbg, LEVEL_4, "\t\t%s pos: %d, use: %+F\n", msg, i, op));
				}
			}
		}
		++step;
	}

	bitset_foreach(live, elm) {
		ir_node *irn = get_idx_irn(env->irg, elm);
		if (be_is_live_in(lv, block, irn))
			border_def(irn, step, 0);
	}
}

static void assign(ir_node *block, void *env_ptr)
{
	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env       = alloc_env->chordal_env;
	bitset_t *live              = alloc_env->live;
	bitset_t *colors            = alloc_env->colors;
	bitset_t *in_colors         = alloc_env->in_colors;
	const arch_env_t *arch_env  = env->birg->main_env->arch_env;
	struct list_head *head      = get_block_border_head(env, block);
	be_lv_t *lv                 = env->birg->lv;

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
		if(has_reg_class(env, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
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
		int ignore   = arch_irn_is(arch_env, irn, ignore);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if(b->is_def && !be_is_live_in(lv, block, irn)) {
			const arch_register_t *reg;
			int col = NO_COLOR;

			if(ignore || pset_find_ptr(alloc_env->pre_colored, irn)) {
				reg = arch_get_irn_register(arch_env, irn);
				col = reg->index;
				assert(!bitset_is_set(colors, col) && "pre-colored register must be free");
			} else {
				col = get_next_free_reg(alloc_env, colors);
				reg = arch_register_for_index(env->cls, col);
				assert(arch_get_irn_register(arch_env, irn) == NULL && "This node must not have been assigned a register yet");
				assert(!arch_register_type_is(reg, ignore) && "Must not assign ignore register");
			}

			bitset_set(colors, col);
			arch_set_irn_register(arch_env, irn, reg);

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n", arch_register_get_name(reg), col, irn));

			assert(!bitset_is_set(live, nr) && "Value's definition must not have been encountered");
			bitset_set(live, nr);
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			int col;

			assert(reg && "Register must have been assigned");

			col = arch_register_get_index(reg);
#ifndef NDEBUG
			if(!arch_register_type_is(reg, ignore)) {
				assert(bitset_is_set(live, nr) && "Cannot have a non live use");
			}
#endif

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}
}

void be_ra_chordal_color(be_chordal_env_t *chordal_env)
{
	be_chordal_alloc_env_t env;
	char buf[256];
	be_lv_t *lv;
	be_irg_t *birg = chordal_env->birg;
	const arch_register_class_t *cls = chordal_env->cls;

	int colors_n          = arch_register_class_n_regs(cls);
	ir_graph *irg         = chordal_env->irg;

	be_assure_dom_front(birg);
	lv = be_assure_liveness(birg);
	be_liveness_assure_sets(lv);
	be_liveness_assure_chk(lv);

	assure_doms(irg);

	env.chordal_env   = chordal_env;
	env.colors_n      = colors_n;
	env.colors        = bitset_alloca(colors_n);
	env.tmp_colors    = bitset_alloca(colors_n);
	env.in_colors     = bitset_alloca(colors_n);
	env.pre_colored   = pset_new_ptr_default();

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, &env);

	if(chordal_env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		snprintf(buf, sizeof(buf), "-%s-constr", chordal_env->cls->name);
		be_dump(chordal_env->irg, buf, dump_ir_block_graph_sched);
	}

	env.live = bitset_malloc(get_irg_last_idx(chordal_env->irg));

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, &env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, &env);

	if(chordal_env->opts->dump_flags & BE_CH_DUMP_TREE_INTV) {
		plotter_t *plotter;
		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", chordal_env->cls->name, irg);
		plotter = new_plotter_ps(buf);
		draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter);
		plotter_free(plotter);
	}

	bitset_free(env.live);
	del_pset(env.pre_colored);
}

void be_init_chordal(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.chordal");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal);
