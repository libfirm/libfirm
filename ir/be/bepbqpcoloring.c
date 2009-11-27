/*
 * bepbqpalloc.c
 *
 *  Created on: Nov 11, 2009
 *      Author: bersch
 */

/* 	miscellaneous includes */
#include "config.h"
#include "debug.h"
#include "error.h"

#include "irdom.h"
#include "iredges_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "time.h"

/* libfirm/ir/be includes */
#include "bearch.h"
#include "beirg.h"
#include "besched.h"
#include "bemodule.h"
#include "bechordal_constraints.h"
#include "bechordal.h"
#include "bechordal_t.h"
#include "beinsn_t.h"
#include "benode.h"
#include "belive.h"
#include "belive_t.h"
#include "beutil.h"
#include "plist.h"
#include "pqueue.h"

/* pbqp includes */
#include "kaps.h"
#include "matrix.h"
#include "vector.h"
#include "vector_t.h"
#include "heuristical.h"
#include "pbqp_t.h"
#include "html_dumper.h"
#include "pbqp_node_t.h"
#include "pbqp_node.h"


typedef struct _be_pbqp_alloc_env_t {
	pbqp 						*pbqp_inst;		/**< PBQP instance for register allocation */
	be_irg_t             		*birg;         	/**< Back-end IRG session. */
	ir_graph             		*irg;          	/**< The graph under examination. */
	const arch_register_class_t *cls;			/**< Current processed register class */
	be_lv_t                     *lv;
	bitset_t                    *ignored_regs;
	pbqp_matrix					*ife_matrix_dummy;
	pbqp_matrix					*aff_matrix_dummy;
	plist_t						*rpeo;
	unsigned					*restr_nodes;
	be_chordal_env_t			*env;
} be_pbqp_alloc_env_t;


#define is_Reg_Phi(irn)		(is_Phi(irn) && mode_is_data(get_irn_mode(irn)))
#define get_Perm_src(irn) 	(get_irn_n(get_Proj_pred(irn), get_Proj_proj(irn)))
#define is_Perm_Proj(irn) 	(is_Proj(irn) && be_is_Perm(get_Proj_pred(irn)))

static inline int is_2addr_code(const arch_register_req_t *req)
{
	return (req->type & arch_register_req_type_should_be_same) != 0;
}


#if KAPS_DUMP
static FILE *my_open(const be_chordal_env_t *env, const char *prefix, const char *suffix)
{
	FILE *result;
	char buf[1024];
	size_t i, n;
	char *tu_name;

	n = strlen(env->birg->main_env->cup_name);
	tu_name = XMALLOCN(char, n + 1);
	strcpy(tu_name, env->birg->main_env->cup_name);
	for (i = 0; i < n; ++i)
		if (tu_name[i] == '.')
			tu_name[i] = '_';

	ir_snprintf(buf, sizeof(buf), "%s%s_%F_%s%s", prefix, tu_name, env->irg, env->cls->name, suffix);
	xfree(tu_name);
	result = fopen(buf, "wt");
	if(result == NULL) {
		panic("Couldn't open '%s' for writing.", buf);
	}

	return result;
}
#endif


static unsigned create_pbqp_node(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *irn) {
	const arch_register_class_t *cls = pbqp_alloc_env->cls;
	pbqp     *pbqp_inst              = pbqp_alloc_env->pbqp_inst;
	bitset_t *ignored_regs           = pbqp_alloc_env->ignored_regs;
	unsigned  colors_n               = arch_register_class_n_regs(cls);
	unsigned  cntConstrains          = 0;

	/* create costs vector depending on register constrains */
	struct vector *costs_vector = vector_alloc(pbqp_inst, colors_n);

	/* set costs depending on register constrains */
	unsigned idx;
	for(idx = 0; idx < colors_n; idx++) {
		if(bitset_is_set(ignored_regs, idx) || !arch_reg_out_is_allocatable(irn, arch_register_for_index(cls, idx))) {
			vector_set(costs_vector, idx, INF_COSTS);
			cntConstrains++;
		}
	}

	/* add vector to pbqp node */
	add_node_costs(pbqp_inst, get_irn_idx(irn), costs_vector);

	/* return number of free selectable registers */
	return (colors_n - cntConstrains);
}

static void build_graph_walker(ir_node *irn, void *env) {
	be_pbqp_alloc_env_t         *pbqp_alloc_env = env;
	pbqp						*pbqp_inst		= pbqp_alloc_env->pbqp_inst;
	const arch_register_class_t *cls            = pbqp_alloc_env->cls;
	const arch_register_req_t   *req            = arch_get_register_req_out(irn);
	unsigned pos, max;

	if (arch_irn_consider_in_reg_alloc(cls, irn))
		return;

	if (is_Reg_Phi(irn)) { /* Phis */
		for (pos=0, max=get_irn_arity(irn); pos<max; ++pos) {
			ir_node *arg = get_irn_n(irn, pos);
			//add_edges(co, irn, arg, co->get_costs(co, irn, arg, pos));

			if (!arch_irn_consider_in_reg_alloc(cls, arg))
				continue;

			/* no edges to itself */
			if(irn == arg) {
				continue;
			}

			if(get_edge(pbqp_inst, get_irn_idx(irn), get_irn_idx(arg)) == NULL) {
				/* copy matrix */
				struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_inst, pbqp_alloc_env->aff_matrix_dummy);
				/* add costs matrix to affinity edge */
				add_edge_costs(pbqp_inst, get_irn_idx(irn), get_irn_idx(arg) , matrix);
			}
		}
	}
	else if (is_Perm_Proj(irn)) { /* Perms */
		ir_node *arg = get_Perm_src(irn);
		//add_edges(co, irn, arg, co->get_costs(co, irn, arg, 0));

		if (!arch_irn_consider_in_reg_alloc(cls, arg))
			return;

		if(get_edge(pbqp_inst, get_irn_idx(irn), get_irn_idx(arg)) == NULL) {
			/* copy matrix */
			struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_inst, pbqp_alloc_env->aff_matrix_dummy);
			/* add costs matrix to affinity edge */
			add_edge_costs(pbqp_inst, get_irn_idx(irn), get_irn_idx(arg) , matrix);
		}
	}
	else { /* 2-address code */
		if (is_2addr_code(req)) {
			const unsigned other = req->other_same;
			int i;

			for (i = 0; 1U << i <= other; ++i) {
				if (other & (1U << i)) {
					ir_node *other = get_irn_n(skip_Proj(irn), i);
//					if (!arch_irn_is_ignore(other)) {
						//add_edges(co, irn, other, co->get_costs(co, irn, other, 0));
						if (!arch_irn_consider_in_reg_alloc(cls, other))
							continue;

						/* no edges to itself */
						if(irn == other) {
							continue;
						}

						if(get_edge(pbqp_inst, get_irn_idx(irn), get_irn_idx(other)) == NULL) {
							/* copy matrix */
							struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_inst, pbqp_alloc_env->aff_matrix_dummy);
							/* add costs matrix to affinity edge */
							add_edge_costs(pbqp_inst, get_irn_idx(irn), get_irn_idx(other) , matrix);
						}
//					}
				}
			}
		}
	}
}

static void create_pbqp_coloring_inst(ir_node *block, void *data) {
	be_pbqp_alloc_env_t         *pbqp_alloc_env    	= data;
	be_lv_t                     *lv                	= pbqp_alloc_env->lv;
	const arch_register_class_t *cls               	= pbqp_alloc_env->cls;
	plist_t						*rpeo			   	= pbqp_alloc_env->rpeo;
	pbqp						*pbqp_inst		   	= pbqp_alloc_env->pbqp_inst;
	unsigned					*restr_nodes		= pbqp_alloc_env->restr_nodes;
	pbqp_matrix 				*ife_matrix_dummy	= pbqp_alloc_env->ife_matrix_dummy;
	pqueue_t  					*queue             	= new_pqueue();
	pqueue_t  					*restr_nodes_queue 	= new_pqueue();
	plist_t						*temp_list         	= plist_new();
	ir_node                     *irn;
	ir_nodeset_t                 live_nodes;

	/* first, determine the pressure */
	/* (this is only for compatibility with copymin optimization, it's not needed for pbqp coloring) */
	pressure(block, pbqp_alloc_env->env);

	/* calculate living nodes for the first step */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	/* create pbqp nodes, interference edges and reverse perfect elimination order */
	sched_foreach_reverse(block, irn) {
		ir_node *live, *if_live;
		ir_nodeset_iterator_t  iter, iter2;

		/* create nodes and interference edges */
		foreach_ir_nodeset(&live_nodes, live, iter) {
			/* create pbqp source node if it dosn't exist */
			if(get_node(pbqp_inst, get_irn_idx(live)) == NULL) {
				restr_nodes[get_irn_idx(live)] = create_pbqp_node(pbqp_alloc_env, live);
			}

			iter2 = iter;
			for(if_live = ir_nodeset_iterator_next(&iter2); if_live != NULL; if_live = ir_nodeset_iterator_next(&iter2)) {
				/* create pbqp target node if it dosn't exist */
				if(get_node(pbqp_inst, get_irn_idx(if_live)) == NULL) {
					restr_nodes[get_irn_idx(if_live)] = create_pbqp_node(pbqp_alloc_env, if_live);
				}
				else {
					/* no edges to itself */
					if(live == if_live)
						continue;
					/* only one interference edge between two nodes */
					if(get_edge(pbqp_inst, get_irn_idx(live), get_irn_idx(if_live)))
						continue;
				}

				/* do useful optimization to improve pbqp solving (we can do this because we know our matrix) */
				if(restr_nodes[get_irn_idx(live)] == 1 && restr_nodes[get_irn_idx(if_live)] == 1) {
					unsigned src_idx = vector_get_min_index(get_node(pbqp_inst, get_irn_idx(live))->costs);
					unsigned trg_idx = vector_get_min_index(get_node(pbqp_inst, get_irn_idx(if_live))->costs);
					assert(src_idx != trg_idx && "Interfering nodes could not have the same register!");
					continue;
				}
				if(restr_nodes[get_irn_idx(live)] == 1 || restr_nodes[get_irn_idx(if_live)] == 1) {
					if(restr_nodes[get_irn_idx(live)] == 1) {
						unsigned idx = vector_get_min_index(get_node(pbqp_inst, get_irn_idx(live))->costs);
						vector_set(get_node(pbqp_inst, get_irn_idx(if_live))->costs, idx, INF_COSTS);
					}
					else {
						unsigned idx = vector_get_min_index(get_node(pbqp_inst, get_irn_idx(if_live))->costs);
						vector_set(get_node(pbqp_inst, get_irn_idx(live))->costs, idx, INF_COSTS);
					}
					continue;
				}

				/* copy matrix */
				struct pbqp_matrix *matrix = pbqp_matrix_copy(pbqp_inst, ife_matrix_dummy);
				/* add costs matrix to interference edge */
				add_edge_costs(pbqp_inst, get_irn_idx(live), get_irn_idx(if_live) , matrix);
			}
		}

		/* order nodes for perfect elimination order */
		if (get_irn_mode(irn) == mode_T) {
			plist_element_t *first = plist_first(temp_list);
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;

				// insert proj node into priority queue (descending by the number of interference edges)
				if(restr_nodes[get_irn_idx(proj)] <= 4/*bitset_is_set(restr_nodes, get_irn_idx(proj))*/) {
					pqueue_put(restr_nodes_queue, proj, pbqp_node_get_degree(get_node(pbqp_inst, get_irn_idx(proj))));
				}
				else {
					pqueue_put(queue,proj, pbqp_node_get_degree(get_node(pbqp_inst, get_irn_idx(proj))));
				}

			}

			/* first insert all restricted nodes */
			while(!pqueue_empty(restr_nodes_queue)) {
				if(first == NULL) {
					plist_insert_back(temp_list, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(restr_nodes_queue))));
					first = plist_first(temp_list);
				} else {
					plist_insert_before(temp_list, first, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(restr_nodes_queue))));
				}
			}

			/* insert proj nodes descending by their number of interference edges */
			while(!pqueue_empty(queue)) {
				if(first == NULL) {
					plist_insert_back(temp_list, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(queue))));
					first = plist_first(temp_list);
				} else {
					plist_insert_before(temp_list, first, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(queue))));
				}
			}
		}
		else {
			if (arch_irn_consider_in_reg_alloc(cls, irn)) {
				plist_insert_front(temp_list, get_node(pbqp_inst, get_irn_idx(irn)));
			}
		}

		/* get living nodes for next step */
		if (!is_Phi(irn)) {
			be_liveness_transfer(cls, irn, &live_nodes);
		}
	}

	/* insert nodes into reverse perfect elimination order */
	plist_element_t *el;
	foreach_plist(temp_list, el) {
		plist_insert_back(rpeo, el->data);
	}

	/* free reserved memory */
	ir_nodeset_destroy(&live_nodes);
	plist_free(temp_list);
	del_pqueue(queue);
	del_pqueue(restr_nodes_queue);
}

static void insert_perms(ir_node *block, void *data) {
	/*
	 * Start silent in the start block.
	 * The silence remains until the first barrier is seen.
	 * Each other block is begun loud.
	 */
	be_chordal_env_t *env    = data;
	ir_node          *irn;
	int               silent = block == get_irg_start_block(get_irn_irg(block));

	/*
	 * If the block is the start block search the barrier and
	 * start handling constraints from there.
	 */
	for (irn = sched_first(block); !sched_is_end(irn);) {
		int silent_old = silent;	/* store old silent value */
		if (be_is_Barrier(irn))
			silent = !silent;		/* toggle silent flag */

		be_insn_t *insn        	= chordal_scan_insn(env, irn);
		irn 					= insn->next_insn;

		if (silent_old)
			continue;

		if (!insn->has_constraints)
			continue;

		pre_process_constraints(env, &insn);
	}
}


void be_pbqp_coloring(be_chordal_env_t *env) {
	ir_graph                      *irg  = env->irg;
	be_irg_t                      *birg = env->birg;
	const arch_register_class_t   *cls  = env->cls;
	unsigned colors_n				    = arch_register_class_n_regs(cls);
	be_pbqp_alloc_env_t pbqp_alloc_env;
	unsigned idx, row, col;
	be_lv_t *lv;

//	ir_timer_t *t_ra_pbqp_alloc_create    = ir_timer_register("be_pbqp_alloc_create", "pbqp alloc create");
//	ir_timer_t *t_ra_pbqp_alloc_solve     = ir_timer_register("be_pbqp_alloc_solve", "pbqp alloc solve");
//	ir_timer_t *t_ra_pbqp_alloc_create_aff  = ir_timer_register("be_pbqp_alloc_create_aff", "pbqp alloc create aff");

	lv = be_assure_liveness(birg);
	be_liveness_assure_sets(lv);
	be_liveness_assure_chk(lv);

//	printf("#### ----- === Allocating registers of %s (%s) ===\n", cls->name, get_entity_name(get_irg_entity(irg)));

	/* insert perms */
	assure_doms(irg);
	dom_tree_walk_irg(irg, insert_perms, NULL, env);

	/* dump graph after inserting perms */
	if (env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		char buf[256];
		snprintf(buf, sizeof(buf), "-%s-constr", cls->name);
		be_dump(irg, buf, dump_ir_block_graph_sched);
	}

	/* initialize pbqp allocation data structure */
	pbqp_alloc_env.pbqp_inst    = alloc_pbqp(get_irg_last_idx(irg));		/* initialize pbqp instance */
	pbqp_alloc_env.birg         = birg;
	pbqp_alloc_env.cls          = cls;
	pbqp_alloc_env.irg          = irg;
	pbqp_alloc_env.lv           = lv;
	pbqp_alloc_env.ignored_regs = bitset_malloc(colors_n);
	pbqp_alloc_env.rpeo			= plist_new();
	pbqp_alloc_env.restr_nodes  = XMALLOCNZ(unsigned, get_irg_last_idx(irg));
	pbqp_alloc_env.env			= env;
	be_put_ignore_regs(birg, cls, pbqp_alloc_env.ignored_regs);				/* get ignored registers */

	/* create costs matrix for interference edges */
	struct pbqp_matrix *ife_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
	/* set costs */
	for(row = 0, col=0; row < colors_n; row++, col++)
		pbqp_matrix_set(ife_matrix, row, col, INF_COSTS);

	pbqp_alloc_env.ife_matrix_dummy = ife_matrix;

	/* create costs matrix for affinity edges */
	struct pbqp_matrix *afe_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
	/* set costs */
	for(row = 0; row < colors_n; row++) {
		for(col = 0; col < colors_n; col++) {
			if(row != col)
				pbqp_matrix_set(afe_matrix, row, col, 2);
		}
	}
	pbqp_alloc_env.aff_matrix_dummy = afe_matrix;


	/* create pbqp instance */
//	ir_timer_reset_and_start(t_ra_pbqp_alloc_create);
	assure_doms(irg);
	dom_tree_walk_irg(irg, create_pbqp_coloring_inst , NULL, &pbqp_alloc_env);
//	ir_timer_stop(t_ra_pbqp_alloc_create);

	/* set up affinity edges */
//	ir_timer_reset_and_start(t_ra_pbqp_alloc_create_aff);
	irg_walk_graph(irg, build_graph_walker, NULL, &pbqp_alloc_env);
//	ir_timer_stop(t_ra_pbqp_alloc_create_aff);

#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE *file_before = my_open(env, "", "-pbqp_coloring.html");
	set_dumpfile(pbqp_alloc_env.pbqp_inst, file_before);
#endif

	/* solve pbqp instance */
//	ir_timer_reset_and_start(t_ra_pbqp_alloc_solve);
	solve_pbqp_heuristical_co(pbqp_alloc_env.pbqp_inst,pbqp_alloc_env.rpeo);
//	ir_timer_stop(t_ra_pbqp_alloc_solve);
	num solution = get_solution(pbqp_alloc_env.pbqp_inst);
	assert(solution != INF_COSTS && "No PBQP solution found");

	plist_element_t *element;
	foreach_plist(pbqp_alloc_env.rpeo, element) {
		pbqp_node *node			   = element->data;
		idx			               = node->index;
		ir_node *irn               = get_idx_irn(irg, idx);
		num color                  = get_node_solution(pbqp_alloc_env.pbqp_inst, idx);
		const arch_register_t *reg = arch_register_for_index(cls, color);

		arch_set_irn_register(irn, reg);
	}

//	printf("%-20s: %8.3lf msec\n" , ir_timer_get_description(t_ra_pbqp_alloc_create), (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create) / 1000.0);
//	printf("%-20s: %8.3lf msec\n" , ir_timer_get_description(t_ra_pbqp_alloc_solve), (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_solve) / 1000.0);
//	printf("%-20s: %8.3lf msec\n" , ir_timer_get_description(t_ra_pbqp_alloc_create_aff), (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create_aff) / 1000.0);


	/* free reserved memory */
#if KAPS_DUMP
	fclose(file_before);
#endif
	bitset_free(pbqp_alloc_env.ignored_regs);
	free_pbqp(pbqp_alloc_env.pbqp_inst);
	plist_free(pbqp_alloc_env.rpeo);
	xfree(pbqp_alloc_env.restr_nodes);
}


/**
 * Initializes this module.
 */
void be_init_pbqp_coloring(void) {

	static be_ra_chordal_coloring_t coloring = {
		be_pbqp_coloring
	};

	be_register_chordal_coloring("pbqp", &coloring);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_pbqp_alloc);
