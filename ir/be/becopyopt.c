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

#include "xmalloc.h"
#include "debug.h"
#include "pmap.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irloop_t.h"
#include "iredges_t.h"
#include "phiclass.h"

#include "bearch.h"
#include "beutil.h"
#include "beifg_t.h"
#include "becopyopt_t.h"
#include "becopystat.h"

/******************************************************************************
    _____                           _
   / ____|                         | |
  | |  __  ___ _ __   ___ _ __ __ _| |
  | | |_ |/ _ \ '_ \ / _ \ '__/ _` | |
  | |__| |  __/ | | |  __/ | | (_| | |
   \_____|\___|_| |_|\___|_|  \__,_|_|

 ******************************************************************************/

static firm_dbg_module_t *dbg = NULL;

void be_copy_opt_init(void) {
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, int (*get_costs)(ir_node*, ir_node*, int)) {
	const char *s1, *s2, *s3;
	int len;
	copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");

	co = xcalloc(1, sizeof(*co));
	co->cenv      = chordal_env;
	co->aenv      = chordal_env->birg->main_env->arch_env;
	co->irg       = chordal_env->irg;
	co->cls       = chordal_env->cls;
	co->get_costs = get_costs;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(co->irg));
	s3 = chordal_env->cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);

	return co;
}

void free_copy_opt(copy_opt_t *co) {
	xfree(co->name);
}

int co_is_optimizable_root(const copy_opt_t *co, ir_node *irn) {
	arch_register_req_t req;

	if (arch_irn_is(co->aenv, irn, ignore))
		return 0;

	if (is_Reg_Phi(irn) || is_Perm_Proj(co->aenv, irn) || is_2addr_code(co->aenv, irn, &req))
		return 1;

	return 0;
}

int co_is_optimizable_arg(const copy_opt_t *co, ir_node *irn) {
	const ir_edge_t *edge;

	assert(0 && "Is buggy and obsolete. Do not use");

	if (arch_irn_is(co->aenv, irn, ignore))
		return 0;

	foreach_out_edge(irn, edge) {
		ir_node *n = edge->src;

		if (!nodes_interfere(co->cenv, irn, n) || irn == n) {
			arch_register_req_t req;
			arch_get_register_req(co->aenv, &req, n, -1);

			if(is_Reg_Phi(n) ||
			   is_Perm(co->aenv, n) ||
			   (arch_register_req_is(&req, should_be_same) && req.other_same == irn)
			  )
				return 1;
		}
	}

	return 0;
}

int co_get_costs_loop_depth(ir_node *root, ir_node* arg, int pos) {
	int cost = 0;
	ir_loop *loop;
	ir_node *root_block = get_nodes_block(root);

	if (is_Phi(root)) {
		/* for phis the copies are placed in the corresponding pred-block */
		loop = get_irn_loop(get_Block_cfgpred_block(root_block, pos));
	} else {
		/* a perm places the copy in the same block as it resides */
		loop = get_irn_loop(root_block);
	}
	if (loop) {
		int d = get_loop_depth(loop);
		cost = d*d;
	}
	return cost+1;
}

int co_get_costs_all_one(ir_node *root, ir_node* arg, int pos) {
	return 1;
}

/******************************************************************************
   ____        _   _    _       _ _          _____ _
  / __ \      | | | |  | |     (_) |        / ____| |
 | |  | |_ __ | |_| |  | |_ __  _| |_ ___  | (___ | |_ ___  _ __ __ _  __ _  ___
 | |  | | '_ \| __| |  | | '_ \| | __/ __|  \___ \| __/ _ \| '__/ _` |/ _` |/ _ \
 | |__| | |_) | |_| |__| | | | | | |_\__ \  ____) | || (_) | | | (_| | (_| |  __/
  \____/| .__/ \__|\____/|_| |_|_|\__|___/ |_____/ \__\___/|_|  \__,_|\__, |\___|
        | |                                                            __/ |
        |_|                                                           |___/
 ******************************************************************************/

/**
 * Determines a maximum weighted independent set with respect to
 * the interference and conflict edges of all nodes in a qnode.
 */
static int ou_max_ind_set_costs(unit_t *ou) {
	be_chordal_env_t *chordal_env = ou->co->cenv;
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


	/* now compute the best set out of the unsafe nodes*/
	if (unsafe_count > MIS_HEUR_TRIGGER) {
		bitset_t *best = bitset_alloca(unsafe_count);
		/* Heuristik: Greedy trial and error form index 0 to unsafe_count-1 */
		for (i=0; i<unsafe_count; ++i) {
			bitset_set(best, i);
			/* check if it is a stable set */
			for (o=bitset_next_set(best, 0); o!=-1 && o<i; o=bitset_next_set(best, o+1))
				if (nodes_interfere(chordal_env, unsafe[i], unsafe[o])) {
					bitset_clear(best, i); /* clear the bit and try next one */
					break;
				}
		}
		/* compute the weight */
		bitset_foreach(best, pos)
			best_weight += unsafe_costs[pos];
	} else {
		/* Exact Algorithm: Brute force */
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
	}

	return safe_costs+best_weight;
}

static void co_collect_units(ir_node *irn, void *env) {
	copy_opt_t *co = env;
	unit_t *unit;
	arch_register_req_t req;

	if (!is_curr_reg_class(co, irn))
		return;
	if (!co_is_optimizable_root(co, irn))
		return;

	/* Init a new unit */
	unit = xcalloc(1, sizeof(*unit));
	unit->co = co;
	unit->node_count = 1;
	INIT_LIST_HEAD(&unit->queue);

	/* Phi with some/all of its arguments */
	if (is_Reg_Phi(irn)) {
		int i, arity;

		/* init */
		arity = get_irn_arity(irn);
		unit->nodes = xmalloc((arity+1) * sizeof(*unit->nodes));
		unit->costs = xmalloc((arity+1) * sizeof(*unit->costs));
		unit->nodes[0] = irn;

		/* fill */
		for (i=0; i<arity; ++i) {
			int o, arg_pos;
			ir_node *arg = get_irn_n(irn, i);

			assert(is_curr_reg_class(co, arg) && "Argument not in same register class.");
			if (arg == irn)
				continue;
			if (nodes_interfere(co->cenv, irn, arg)) {
				unit->inevitable_costs += co->get_costs(irn, arg, i);
				continue;
			}

			/* Else insert the argument of the phi to the members of this ou */
			DBG((dbg, LEVEL_1, "\t   Member: %+F\n", arg));

			/* Check if arg has occurred at a prior position in the arg/list */
			arg_pos = 0;
			for (o=0; o<unit->node_count; ++o)
				if (unit->nodes[o] == arg) {
					arg_pos = o;
					break;
				}

			if (!arg_pos) { /* a new argument */
				/* insert node, set costs */
				unit->nodes[unit->node_count] = arg;
				unit->costs[unit->node_count] = co->get_costs(irn, arg, i);
				unit->node_count++;
			} else { /* arg has occured before in same phi */
				/* increase costs for existing arg */
				unit->costs[arg_pos] += co->get_costs(irn, arg, i);
			}
		}
		unit->nodes = xrealloc(unit->nodes, unit->node_count * sizeof(*unit->nodes));
		unit->costs = xrealloc(unit->costs, unit->node_count * sizeof(*unit->costs));
	} else

	/* Proj of a perm with corresponding arg */
	if (is_Perm_Proj(co->aenv, irn)) {
		assert(!nodes_interfere(co->cenv, irn, get_Perm_src(irn)));
		unit->nodes = xmalloc(2 * sizeof(*unit->nodes));
		unit->costs = xmalloc(2 * sizeof(*unit->costs));
		unit->node_count = 2;
		unit->nodes[0] = irn;
		unit->nodes[1] = get_Perm_src(irn);
		unit->costs[1] = co->get_costs(irn, unit->nodes[1], -1);
	} else

	/* Src == Tgt of a 2-addr-code instruction */
	if (is_2addr_code(co->aenv, irn, &req)) {
		ir_node *other = req.other_same;
		if (!nodes_interfere(co->cenv, irn, other)) {
			unit->nodes = xmalloc(2 * sizeof(*unit->nodes));
			unit->costs = xmalloc(2 * sizeof(*unit->costs));
			unit->node_count = 2;
			unit->nodes[0] = irn;
			unit->nodes[1] = other;
			unit->costs[1] = co->get_costs(irn, other, -120480);
		}
	} else
		assert(0 && "This is not an optimizable node!");

	/* Insert the new unit at a position according to its costs */
	if (unit->node_count > 1) {
		int i;
		struct list_head *tmp;

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
	} else {
		free(unit);
	}
}

void co_build_ou_structure(copy_opt_t *co) {
	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	INIT_LIST_HEAD(&co->units);
	irg_walk_graph(co->irg, co_collect_units, NULL, co);
}

void co_free_ou_structure(copy_opt_t *co) {
	unit_t *curr, *tmp;
	list_for_each_entry_safe(unit_t, curr, tmp, &co->units, units) {
		xfree(curr->nodes);
		xfree(curr->costs);
		xfree(curr);
	}
}

/* co_solve_heuristic() is implemented in becopyheur.c */

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

/******************************************************************************
   _____                 _        _____ _
  / ____|               | |      / ____| |
 | |  __ _ __ __ _ _ __ | |__   | (___ | |_ ___  _ __ __ _  __ _  ___
 | | |_ | '__/ _` | '_ \| '_ \   \___ \| __/ _ \| '__/ _` |/ _` |/ _ \
 | |__| | | | (_| | |_) | | | |  ____) | || (_) | | | (_| | (_| |  __/
  \_____|_|  \__,_| .__/|_| |_| |_____/ \__\___/|_|  \__,_|\__, |\___|
                  | |                                       __/ |
                  |_|                                      |___/
 ******************************************************************************/

static int compare_affinity_t(const void *k1, const void *k2, size_t size) {
	const affinity_t *n1 = k1;
	const affinity_t *n2 = k2;

	return (n1->irn != n2->irn);
}

static void add_edge(copy_opt_t *co, ir_node *n1, ir_node *n2, int costs) {
	affinity_t new_node, *node;
	neighb_t new_nbr, *nbr;
	int allocnew;

	new_node.irn        = n1;
	new_node.count      = 0;
	new_node.neighbours = NULL;
	node = set_insert(co->nodes, &new_node, sizeof(new_node), HASH_PTR(new_node.irn));

	allocnew = 1;
	for (nbr = node->neighbours; nbr; nbr = nbr->next)
		if (nbr->irn == n2) {
			allocnew = 0;
			break;
		}

	/* if we did not find n2 in n1's neighbourhood insert it */
	if (allocnew) {
		obstack_grow(&co->obst, &new_nbr, sizeof(new_nbr));
		nbr = obstack_finish(&co->obst);
		nbr->irn   = n2;
		nbr->costs = 0;
		nbr->next  = node->neighbours;
		node->neighbours = nbr;
		node->count++;
	}

	/* now nbr points to n1's neighbour-entry of n2 */
	nbr->costs += costs;
}

static INLINE void add_edges(copy_opt_t *co, ir_node *n1, ir_node *n2, int costs) {
	if (! be_ifg_connected(co->cenv->ifg, n1, n2)) {
		add_edge(co, n1, n2, costs);
		add_edge(co, n2, n1, costs);
	}
}

static void build_graph_walker(ir_node *irn, void *env) {
	copy_opt_t *co = env;
	int pos, max;
	arch_register_req_t req;

	if (!is_curr_reg_class(co, irn) || arch_irn_is(co->aenv, irn, ignore))
		return;

	/* Phis */
	if (is_Reg_Phi(irn))
		for (pos=0, max=get_irn_arity(irn); pos<max; ++pos) {
			ir_node *arg = get_irn_n(irn, pos);
			add_edges(co, irn, arg, co->get_costs(irn, arg, pos));
		}

	/* Perms */
	else if (is_Perm_Proj(co->aenv, irn)) {
		ir_node *arg = get_Perm_src(irn);
		add_edges(co, irn, arg, co->get_costs(irn, arg, 0));
	}

	/* 2-address code */
	else if (is_2addr_code(co->aenv, irn, &req))
		add_edges(co, irn, req.other_same, co->get_costs(irn, req.other_same, 0));
}

void co_build_graph_structure(copy_opt_t *co) {
	obstack_init(&co->obst);
	co->nodes = new_set(compare_affinity_t, 32);

	irg_walk_graph(co->irg, build_graph_walker, NULL, co);
}

void co_free_graph_structure(copy_opt_t *co) {
	del_set(co->nodes);
	obstack_free(&co->obst, NULL);
}

/* co_solve_ilp1() co_solve_ilp2() are implemented in becopyilpX.c */

int co_gs_is_optimizable(copy_opt_t *co, ir_node *irn) {
	affinity_t new_node, *n;

	new_node.irn = irn;
	n = set_find(co->nodes, &new_node, sizeof(new_node), HASH_PTR(new_node.irn));
	if (n) {
		return (n->count > 0);
	} else
		return 0;
}
