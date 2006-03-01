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

#include <libcore/lc_timing.h>

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
#include "becopyopt_t.h"
#include "becopystat.h"



static firm_dbg_module_t *dbg = NULL;

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
	if (!co_is_optimizable(co->aenv, irn, &req))
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
		assert(!nodes_interfere(co->cenv, irn, get_Copy_src(irn)));
		unit->nodes = xmalloc(2 * sizeof(*unit->nodes));
		unit->costs = xmalloc(2 * sizeof(*unit->costs));
		unit->node_count = 2;
		unit->nodes[0] = irn;
		unit->nodes[1] = get_Copy_src(irn);
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

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyoptmain");
}

copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, int (*get_costs)(ir_node*, ir_node*, int)) {
	const char *s1, *s2, *s3;
	int len;
	copy_opt_t *co;

	dbg = firm_dbg_register("ir.be.copyopt");

	co = xcalloc(1, sizeof(*co));
	co->cenv = chordal_env;
	co->aenv = chordal_env->main_env->arch_env;
	co->irg = chordal_env->irg;
	co->cls = chordal_env->cls;
	co->get_costs = get_costs;

	s1 = get_irp_prog_name();
	s2 = get_entity_name(get_irg_entity(co->irg));
	s3 = chordal_env->cls->name;
	len = strlen(s1) + strlen(s2) + strlen(s3) + 5;
	co->name = xmalloc(len);
	snprintf(co->name, len, "%s__%s__%s", s1, s2, s3);

	DBG((dbg, LEVEL_1, "\tCollecting optimization units\n"));
	INIT_LIST_HEAD(&co->units);
	irg_walk_graph(co->irg, co_collect_units, NULL, co);
	return co;
}

void free_copy_opt(copy_opt_t *co) {
	unit_t *curr, *tmp;
	xfree(co->name);
	list_for_each_entry_safe(unit_t, curr, tmp, &co->units, units) {
		xfree(curr->nodes);
		xfree(curr->costs);
		xfree(curr);
	}
}

int is_optimizable_arg(const copy_opt_t *co, ir_node *irn) {
	const ir_edge_t *edge;

	if (arch_irn_is_ignore(co->aenv, irn))
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

int get_costs_loop_depth(ir_node *root, ir_node* arg, int pos) {
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



#define DO_HEUR
#undef DO_CLASSES
#undef DO_ILP


/**
 * Helpers for saving and restoring colors of nodes.
 * Used to get dependable and comparable benchmark results.
 */
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))

typedef struct color_saver {
	arch_env_t *arch_env;
	be_chordal_env_t *chordal_env;
	pmap *saved_colors;
	int flag; /* 0 save, 1 load */
} color_save_t;

static void save_load(ir_node *irn, void *env) {
	color_save_t *saver = env;
	if (saver->chordal_env->cls == arch_get_irn_reg_class(saver->arch_env, irn, -1)) {
		if (saver->flag == 0) { /* save */
			const arch_register_t *reg = arch_get_irn_register(saver->arch_env, irn);
			pmap_insert(saver->saved_colors, irn, (void *) reg);
		} else { /*load */
			arch_register_t *reg = pmap_get(saver->saved_colors, irn);
			arch_set_irn_register(saver->arch_env, irn, reg);
		}
	}
}

static void save_colors(color_save_t *color_saver) {
	color_saver->flag = 0;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}

static void load_colors(color_save_t *color_saver) {
	color_saver->flag = 1;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}

#endif /* Need save/load stuff */



void co_compare_solvers(be_chordal_env_t *chordal_env) {
	copy_opt_t *co;

#ifdef DO_STAT
	lc_timer_t *timer;
	color_save_t saver;
	int costs, costs_inevit, costs_init, costs_heur, costs_classes, costs_ilp, lower_bound;
#endif

	co = new_copy_opt(chordal_env, get_costs_loop_depth);
	DBG((dbg, LEVEL_1, "----> CO: %s\n", co->name));
	phi_class_compute(chordal_env->irg);


#ifdef DO_STAT
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))
		saver.arch_env = chordal_env->main_env->arch_env;
		saver.chordal_env = chordal_env;
		saver.saved_colors = pmap_create();
		save_colors(&saver);
#endif

		costs_inevit = co_get_inevit_copy_costs(co);
		lower_bound  = co_get_lower_bound(co);
		costs_init   = co_get_copy_costs(co);

		DBG((dbg, LEVEL_1, "Inevit Costs: %3d\n", costs_inevit));
		DBG((dbg, LEVEL_1, "Lower Bound: %3d\n", lower_bound));
		DBG((dbg, LEVEL_1, "Init costs: %3d\n", costs_init));

		copystat_add_inevit_costs(costs_inevit);
		copystat_add_init_costs(costs_init);
		copystat_add_max_costs(co_get_max_copy_costs(co));
#endif


#ifdef DO_HEUR
#ifdef DO_STAT
	timer = lc_timer_register("heur", NULL);
	lc_timer_reset_and_start(timer);
#endif

	co_solve_heuristic(co);

#ifdef DO_STAT
	lc_timer_stop(timer);
	costs_heur = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Heur costs: %3d\n", costs_heur));
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
	copystat_add_heur_costs(costs_heur);
	assert(lower_bound <= costs_heur);
#endif
#endif /* DO_HEUR */



#ifdef DO_CLASSES
#ifdef DO_STAT
#ifdef DO_HEUR
	load_colors(&saver);
#endif
	timer = lc_timer_register("classes", NULL);
	lc_timer_reset_and_start(timer);
#endif

	co_classes_opt(co);

#ifdef DO_STAT
	lc_timer_stop(timer);
	costs_classes = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Classes costs: %3d\n", costs_classes));
	copystat_add_classes_time(lc_timer_elapsed_msec(timer));
	copystat_add_classes_costs(costs_heur);
	assert(lower_bound <= costs_classes);
#endif
#endif /* DO_CLASSES */



#ifdef DO_ILP
#ifdef DO_STAT
#if defined(DO_HEUR) || defined(DO_CLASSES)
	load_colors(&saver);
#endif
#endif

	co_solve_ilp1(co, 60.0);

#ifdef DO_STAT
	costs_ilp = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Opt  costs: %3d\n", costs_ilp));
	copystat_add_opt_costs(costs_ilp);
	assert(lower_bound <= costs_ilp);
#endif
#endif /* DO_ILP */


#ifdef DO_STAT
#if (defined(DO_HEUR) && defined(DO_BETTER)) || (defined(DO_HEUR) && defined(DO_ILP)) || (defined(DO_BETTER) && defined(DO_ILP))
	pmap_destroy(saver.saved_colors);
#endif
#endif
	free_copy_opt(co);
}
