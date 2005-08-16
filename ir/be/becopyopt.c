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

static firm_dbg_module_t *dbg = NULL;

#define is_curr_reg_class(irn) \
  (arch_get_irn_reg_class(get_arch_env(co), \
                          irn, arch_pos_make_out(0)) == co->chordal_env->cls)

#define MIN(a,b) ((a<b)?(a):(b))
#define MAX(a,b) ((a<b)?(b):(a))


/**
 * Determines a maximum weighted independent set with respect to
 * the interference and conflict edges of all nodes in a qnode.
 */
static int ou_max_ind_set_costs(unit_t *ou) {
	be_chordal_env_t *chordal_env = ou->co->chordal_env;
	ir_node **safe, **unsafe;
	int i, o, safe_count, safe_costs, unsafe_count, *unsafe_costs;
	bitset_t *curr;
	int max, pos, curr_weight, best_weight = 0;

	/* assign the nodes into two groups.
	 * safe: node has no interference, hence it is in every max stable set.
	 * unsafe: node has an interference
	 */
	safe = alloca((ou->node_count-1) * sizeof(*safe));
	safe_costs = 0;
	safe_count = 0;
	unsafe = alloca((ou->node_count-1) * sizeof(*unsafe));
	unsafe_costs = alloca((ou->node_count-1) * sizeof(*unsafe_costs));
	unsafe_count = 0;
	for(i=1; i<ou->node_count; ++i) {
		int is_safe = 1;
		for(o=1; o<ou->node_count; ++o) {
			if (i==o)
				continue;
			if (nodes_interfere(chordal_env, ou->nodes[i], ou->nodes[o])) {
				unsafe_costs[unsafe_count] = ou->costs[i];
				unsafe[unsafe_count] = ou->nodes[i];
				++unsafe_count;
				is_safe = 0;
				break;
			}
		}
		if (is_safe) {
			safe_costs += ou->costs[i];
			safe[safe_count++] = ou->nodes[i];
		}
	}



	/* now brute force the best set out of the unsafe nodes*/
	curr = bitset_alloca(unsafe_count);

	bitset_set_all(curr);
	while ((max = bitset_popcnt(curr)) != 0) {
		/* check if curr is a stable set */
		for (i=bitset_next_set(curr, 0); i!=-1; i=bitset_next_set(curr, i+1))
			for (o=bitset_next_set(curr, i+1); o!=-1; o=bitset_next_set(curr, o+1)) /* !!!!! difference to qnode_max_ind_set(): NOT (curr, i) */
					if (nodes_interfere(chordal_env, unsafe[i], unsafe[o]))
						goto no_stable_set;

		/* if we arrive here, we have a stable set */
		/* compute the weigth of the stable set*/
		curr_weight = 0;
		bitset_foreach(curr, pos)
			curr_weight += unsafe_costs[pos];

		/* any better ? */
		if (curr_weight > best_weight) {
			best_weight = curr_weight;
		}

no_stable_set:
		bitset_minus1(curr);
	}

	return safe_costs+best_weight;
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
	unit->nodes = xmalloc((arity+1) * sizeof(*unit->nodes));
	unit->costs = xmalloc((arity+1) * sizeof(*unit->costs));
	unit->node_count = 1;
	unit->nodes[0] = root;
	INIT_LIST_HEAD(&unit->queue);

	/* check all args */
	if (is_Phi(root) && mode_is_datab(get_irn_mode(root))) {
		for (i=0; i<arity; ++i) {
			int o, arg_pos = 0;
			ir_node *arg = get_irn_n(root, i);

			assert(is_curr_reg_class(arg) && "Argument not in same register class.");
			if (arg == root)
				continue;
			if (nodes_interfere(co->chordal_env, root, arg)) {
				unit->inevitable_costs += co->get_costs(root, arg, i);
				continue;
			}

			/* Else insert the argument of the phi to the members of this ou */
			DBG((dbg, LEVEL_1, "\t   Member: %n %N\n", arg, arg));

			/* Check if arg has occurred at a prior position in the arg/list */
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
				unit->costs[arg_pos] += co->get_costs(root, arg, i);
			}
		}
		unit->nodes = xrealloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
		unit->costs = xrealloc(unit->costs, unit->node_count * sizeof(*unit->costs));
	} else if (is_Copy(get_arch_env(co), root)) {
		assert(!nodes_interfere(co->chordal_env, root, get_Copy_src(root)));
		unit->nodes[1] = get_Copy_src(root);
		unit->costs[1] = co->get_costs(root, unit->nodes[1], -1);
		unit->node_count = 2;
		unit->nodes = xrealloc(unit->nodes, 2 * sizeof(*unit->nodes));
		unit->costs = xrealloc(unit->costs, 2 * sizeof(*unit->costs));
	} else
		assert(0 && "This is not an optimizable node!");
	/* TODO add ou's for 2-addr-code instructions */


	/* Determine the maximum costs this unit can cause: all_nodes_cost */
	for(i=1; i<unit->node_count; ++i) {
		unit->sort_key = MAX(unit->sort_key, unit->costs[i]);
		unit->all_nodes_costs += unit->costs[i];
	}

	/* Determine the minimal costs this unit will cause: min_nodes_costs */
	unit->min_nodes_costs += unit->all_nodes_costs - ou_max_ind_set_costs(unit);

	/* Insert the new ou according to its sort_key */
	tmp = &co->units;
	while (tmp->next != &co->units && list_entry_units(tmp->next)->sort_key > unit->sort_key)
		tmp = tmp->next;
	list_add(&unit->units, tmp);
}

static void co_collect_in_block(ir_node *block, void *env) {
	copy_opt_t *co = env;
	struct list_head *head = get_block_border_head(co->chordal_env, block);
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real && is_optimizable(get_arch_env(co), curr->irn))
			co_append_unit(co, curr->irn);
}

static void co_collect_units(copy_opt_t *co) {
	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	co->roots = pset_new_ptr(64);
	dom_tree_walk_irg(get_irg(co), co_collect_in_block, NULL, co);
	del_pset(co->roots);
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, int (*get_costs)(ir_node*, ir_node*, int)) {
	const char *s1, *s2, *s3;
	int len;
	copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL_CO);

	co = xcalloc(1, sizeof(*co));
	co->chordal_env = chordal_env;
	co->get_costs = get_costs;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(get_irg(co)));
	s3 = chordal_env->cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, DEBUG_IRG_LVL_CO);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL_CO);

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
		if (((is_Phi(n) && mode_is_datab(get_irn_mode(n))) ||
			 is_Perm(get_arch_env(co), n)) && (irn == n || !nodes_interfere(co->chordal_env, irn, n)))
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

int co_get_max_copy_costs(const copy_opt_t *co) {
	int i, res = 0;
	unit_t *curr;

	list_for_each_entry(unit_t, curr, &co->units, units) {
		res += curr->inevitable_costs;
		for (i=1; i<curr->node_count; ++i)
			res += curr->costs[i];
	}
	return res;
}

int co_get_inevit_copy_costs(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;

	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->inevitable_costs;
	return res;
}

int co_get_copy_costs(const copy_opt_t *co) {
	int i, res = 0;
	unit_t *curr;

	list_for_each_entry(unit_t, curr, &co->units, units) {
		int root_col = get_irn_col(co, curr->nodes[0]);
		DBG((dbg, LEVEL_1, "  %3d costs for root %+F color %d\n", curr->inevitable_costs, curr->nodes[0], root_col));
		res += curr->inevitable_costs;
		for (i=1; i<curr->node_count; ++i) {
			int arg_col = get_irn_col(co, curr->nodes[i]);
			if (root_col != arg_col) {
				DBG((dbg, LEVEL_1, "  %3d for arg %+F color %d\n", curr->costs[i], curr->nodes[i], arg_col));
				res += curr->costs[i];
			}
		}
	}
	return res;
}

int co_get_lower_bound(const copy_opt_t *co) {
	int res = 0;
	unit_t *curr;
	list_for_each_entry(unit_t, curr, &co->units, units)
		res += curr->inevitable_costs + curr->min_nodes_costs;
	return res;
}
