/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "irprog.h"
#include "irloop_t.h"

#include "xmalloc.h"
#include "bechordal_t.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define is_curr_reg_class(irn) (arch_get_irn_reg_class(co->chordal_env->arch_env, irn, arch_pos_make_out(0)) == co->chordal_env->cls)

#define MIN(a,b) ((a<b)?(a):(b))
#define MAX(a,b) ((a<b)?(b):(a))

/**
 * Computes the weight of a 'max independent set' wrt. ifg-edges only
 * (no coloring conflicts, no register constraints)
 * @return The costs of such a mis
 * NOTE: Code adapted from becopyheur
 * BETTER: Here we can be sure having a chordal graph to work on,
 * 		   so, for 'larger' opt-units we could use a special algorithm.
 */
static int ou_max_ind_set_costs(unit_t *ou) {
	ir_node **irns;
	int max, pos, curr_weight, best_weight = 0;
	bitset_t *curr;

	irns = alloca((ou->node_count-1) * sizeof(*irns));
	curr = bitset_alloca(ou->node_count-1);

	/* brute force the best set */
	bitset_set_all(curr);
	while ((max = bitset_popcnt(curr)) != 0) {
		/* check if curr is a stable set */
		int i, o, is_stable_set = 1;

		/* copy the irns */
		i = 0;
		bitset_foreach(curr, pos)
			irns[i++] = ou->nodes[1+pos];
		assert(i==max);

		for(i=0; i<max; ++i)
			for(o=i+1; o<max; ++o) /* !!!!! difference to qnode_max_ind_set(): NOT o=i */
				if (nodes_interfere(ou->co->chordal_env, irns[i], irns[o])) {
					is_stable_set = 0;
					break;
				}

		if (is_stable_set) {
			/* calc current weigth */
			curr_weight = 0;
			bitset_foreach(curr, pos)
				curr_weight += ou->costs[1+pos];

			/* any better ? */
			if (curr_weight > best_weight)
				best_weight = curr_weight;
		}

		bitset_minus1(curr);
	}
	return best_weight;
}

/**
 * Builds an optimization unit for a given optimizable irn (root).
 * This opt-unit is inserted in the main structure co.
 * If an arg of root itself is optimizable process this arg before with a
 * recursive call. For handling this situation and loops co->root is used
 * to remember all roots.
 */
static void co_append_unit(copy_opt_t *co, ir_node *root) {
	int i, arity;
	unit_t *unit;
	struct list_head *tmp;

	DBG((dbg, LEVEL_1, "\t  Root: %n %N\n", root, root));
	/* check if we encountered this root earlier */
	if (pset_find_ptr(co->roots, root))
		return;
	pset_insert_ptr(co->roots, root);

	assert(is_curr_reg_class(root) && "node is in wrong register class!");

	/* init unit */
	arity = get_irn_arity(root);
	unit = xcalloc(1, sizeof(*unit));
	unit->co = co;
	unit->node_count = 1;
	unit->nodes = xmalloc((arity+1) * sizeof(*unit->nodes));
	unit->costs = xmalloc((arity+1) * sizeof(*unit->costs));
	unit->nodes[0] = root;
	unit->complete_costs = 0;
	unit->sort_key = 0;
	INIT_LIST_HEAD(&unit->queue);

	/* check all args */
	if (is_Phi(root)) {
		for (i=0; i<arity; ++i) {
			ir_node *arg = get_irn_n(root, i);
			assert(is_curr_reg_class(arg) && "Argument not in same register class.");
			if (arg != root) {
				int o, arg_pos = 0;
				if (nodes_interfere(co->chordal_env, root, arg))
					assert(0 && "root and arg interfere");
				DBG((dbg, LEVEL_1, "\t   Member: %n %N\n", arg, arg));

				/* check if arg has occurred at a prior position in the arg/list */
				for (o=0; o<unit->node_count; ++o)
					if (unit->nodes[o] == arg) {
						arg_pos = o;
						break;
					}

				if (!arg_pos) { /* a new argument */
					/* insert node, set costs */
					unit->nodes[unit->node_count] = arg;
					unit->costs[unit->node_count] = co->get_costs(root, arg, i);
					unit->node_count++;
				} else { /* arg has occured before in same phi */
					/* increase costs for existing arg */
					unit->costs[arg_pos] = co->get_costs(root, arg, i);
				}
			}
		}
		unit->nodes = xrealloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
		unit->costs = xrealloc(unit->costs, unit->node_count * sizeof(*unit->costs));
	} else if (is_Copy(co->chordal_env->arch_env, root)) {
		assert(!nodes_interfere(co->chordal_env, root, get_Copy_src(root)));
		unit->nodes[1] = get_Copy_src(root);
		unit->costs[1] = co->get_costs(root, unit->nodes[1], -1);
		unit->node_count = 2;
		unit->nodes = xrealloc(unit->nodes, 2 * sizeof(*unit->nodes));
		unit->costs = xrealloc(unit->costs, 2 * sizeof(*unit->costs));
	} else
		assert(0 && "This is not an optimizable node!");
	/* TODO add ou's for 2-addr-code instructions */


	for(i=1; i<unit->node_count; ++i) {
		unit->sort_key = MAX(unit->sort_key, unit->costs[i]);
		unit->complete_costs += unit->costs[i];
	}

	/* insert according to average costs */
	tmp = &co->units;
	while (tmp->next != &co->units && list_entry_units(tmp->next)->sort_key > unit->sort_key)
		tmp = tmp->next;
	list_add(&unit->units, tmp);

	/* Init ifg_mis_size to node_count. So get_lower_bound returns correct results. */
	unit->minimal_costs = unit->complete_costs - ou_max_ind_set_costs(unit);
}

static void co_collect_in_block(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = get_block_border_head(co->chordal_env, block);
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && is_optimizable(co->chordal_env->arch_env, curr->irn))
			co_append_unit(co, curr->irn);
}

static void co_collect_units(copy_opt_t *co) {
	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	co->roots = pset_new_ptr(64);
	dom_tree_walk_irg(co->chordal_env->irg, co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, int (*get_costs)(ir_node*, ir_node*, int)) {
	const char *s1, *s2, *s3;
	int len;
	copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	co = xcalloc(1, sizeof(*co));
	co->chordal_env = chordal_env;
	co->get_costs = get_costs;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(co->chordal_env->irg));
	s3 = chordal_env->cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, DEBUG_LVL_CO);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL);

	INIT_LIST_HEAD(&co->units);
	co_collect_units(co);
	return co;
}

void free_copy_opt(copy_opt_t *co) {
	unit_t *curr, *tmp;
	xfree(co->name);
	list_for_each_entry_safe(unit_t, curr, tmp, &co->units, units) {
		xfree(curr->nodes);
		xfree(curr);
	}
}

int is_optimizable_arg(const copy_opt_t *co, ir_node *irn) {
	int i, max;
	for(i=0, max=get_irn_n_outs(irn); i<max; ++i) {
		ir_node *n = get_irn_out(irn, i);
		if ((is_Phi(n) || is_Perm(co->chordal_env->arch_env, n)) && (irn == n || !nodes_interfere(co->chordal_env, irn, n)))
			return 1;
	}
	return 0;
}

int get_costs_loop_depth(ir_node *root, ir_node* arg, int pos) {
	int cost = 0;
	ir_loop *loop;
	ir_node *root_block = get_nodes_block(root);

	assert(pos==-1 || is_Phi(root));
	if (pos == -1) {
		/* a perm places the copy in the same block as it resides */
		loop = get_irn_loop(root_block);
	} else {
		/* for phis the copies are placed in the corresponding pred-block */
		loop = get_irn_loop(get_Block_cfgpred_block(root_block, pos));
	}
	if (loop)
		cost = 2*get_loop_depth(loop);
	return cost+1;
}

int get_costs_all_one(ir_node *root, ir_node* arg, int pos) {
	return 1;
}

int co_get_copy_costs(const copy_opt_t *co) {
	int i, res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units) {
		int root_col = get_irn_col(co, curr->nodes[0]);
		for (i=1; i<curr->node_count; ++i)
			if (root_col != get_irn_col(co, curr->nodes[i])) {
				DBG((dbg, LEVEL_1, "  %n %N\n", curr->nodes[i], curr->nodes[i]));
				res += curr->costs[i];
			}
	}
	return res;
}

int co_get_lower_bound(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->minimal_costs;
	return res;
}
