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
 * @brief       PBQP based register allocation.
 * @author      Thomas Bersch
 * @date        27.11.2009
 * @version     $Id: bechordal.c 26750 2009-11-27 09:37:43Z bersch $
 */

/* 	miscellaneous includes */
#include "config.h"

#ifdef FIRM_KAPS

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
#include "bechordal_common.h"
#include "bechordal.h"
#include "bechordal_t.h"
#include "beinsn_t.h"
#include "benode.h"
#include "belive.h"
#include "belive_t.h"
#include "beutil.h"
#include "plist.h"
#include "pqueue.h"
#include "becopyopt.h"

/* pbqp includes */
#include "kaps.h"
#include "matrix.h"
#include "vector.h"
#include "vector_t.h"
#include "heuristical_co.h"
#include "heuristical_co_ld.h"
#include "pbqp_t.h"
#include "html_dumper.h"
#include "pbqp_node_t.h"
#include "pbqp_node.h"

#define TIMER 		1
#define PRINT_RPEO 	1


static int use_exec_freq 		= true;
static int use_late_decision 	= true;

typedef struct _be_pbqp_alloc_env_t {
	pbqp 						*pbqp_inst;			/**< PBQP instance for register allocation */
	be_irg_t             		*birg;         		/**< Back-end IRG session. */
	ir_graph             		*irg;          		/**< The graph under examination. */
	const arch_register_class_t *cls;				/**< Current processed register class */
	be_lv_t                     *lv;
	bitset_t                    *ignored_regs;
	pbqp_matrix					*ife_matrix_template;
	pbqp_matrix					*aff_matrix_template;
	plist_t						*rpeo;
	unsigned					*restr_nodes;
	unsigned					*ife_edge_num;
	be_chordal_env_t			*env;
} be_pbqp_alloc_env_t;


#define is_Reg_Phi(irn)											(is_Phi(irn) && mode_is_data(get_irn_mode(irn)))
#define get_Perm_src(irn) 										(get_irn_n(get_Proj_pred(irn), get_Proj_proj(irn)))
#define is_Perm_Proj(irn) 										(is_Proj(irn) && be_is_Perm(get_Proj_pred(irn)))
#define insert_edge(pbqp, src_node, trg_node, template_matrix) 	(add_edge_costs(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node), pbqp_matrix_copy(pbqp, template_matrix)))
#define get_free_regs(restr_nodes, cls, irn) 					(arch_register_class_n_regs(cls) - restr_nodes[get_irn_idx(irn)])

static inline int is_2addr_code(const arch_register_req_t *req)
{
	return (req->type & arch_register_req_type_should_be_same) != 0;
}

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_BOOL      ("exec_freq", "use exec_freq",  &use_exec_freq),
	LC_OPT_ENT_BOOL      ("late_decision", "use late decision for register allocation",  &use_late_decision),
	LC_OPT_LAST
};

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
	if (result == NULL) {
		panic("Couldn't open '%s' for writing.", buf);
	}

	return result;
}
#endif


static void create_pbqp_node(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *irn)
{
	const arch_register_class_t *cls = pbqp_alloc_env->cls;
	pbqp     *pbqp_inst              = pbqp_alloc_env->pbqp_inst;
	bitset_t *ignored_regs           = pbqp_alloc_env->ignored_regs;
	unsigned  colors_n               = arch_register_class_n_regs(cls);
	unsigned  cntConstrains          = 0;

	/* create costs vector depending on register constrains */
	struct vector *costs_vector = vector_alloc(pbqp_inst, colors_n);

	/* set costs depending on register constrains */
	unsigned idx;
	for (idx = 0; idx < colors_n; idx++) {
		if (bitset_is_set(ignored_regs, idx) || !arch_reg_out_is_allocatable(irn, arch_register_for_index(cls, idx))) {
			vector_set(costs_vector, idx, INF_COSTS);
			cntConstrains++;
		}
	}

	/* add vector to pbqp node */
	add_node_costs(pbqp_inst, get_irn_idx(irn), costs_vector);
	pbqp_alloc_env->restr_nodes[get_irn_idx(irn)] = cntConstrains;
}

static void insert_ife_edge(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *src_node, ir_node *trg_node)
{
	pbqp 						*pbqp                = pbqp_alloc_env->pbqp_inst;
	const arch_register_class_t *cls                 = pbqp_alloc_env->cls;
	pbqp_matrix 				*ife_matrix_template = pbqp_alloc_env->ife_matrix_template;
	unsigned 					*restr_nodes         = pbqp_alloc_env->restr_nodes;

	if (get_edge(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node)) == NULL) {

		/* increase ife edge counter */
		pbqp_alloc_env->ife_edge_num[get_irn_idx(src_node)]++;
		pbqp_alloc_env->ife_edge_num[get_irn_idx(trg_node)]++;

		/* do useful optimization to speed up pbqp solving (we can do this because we know our matrix) */
		if (get_free_regs(restr_nodes, cls, src_node) == 1 && get_free_regs(restr_nodes, cls, trg_node) == 1) {
			unsigned src_idx = vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs);
			unsigned trg_idx = vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs);
			assert(src_idx != trg_idx && "Interfering nodes could not have the same register!");
			return;
		}
		if (get_free_regs(restr_nodes, cls, src_node) == 1 || get_free_regs(restr_nodes, cls, trg_node) == 1) {
			if (get_free_regs(restr_nodes, cls, src_node) == 1) {
				unsigned idx = vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs);
				vector_set(get_node(pbqp, get_irn_idx(trg_node))->costs, idx, INF_COSTS);
			}
			else {
				unsigned idx = vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs);
				vector_set(get_node(pbqp, get_irn_idx(src_node))->costs, idx, INF_COSTS);
			}
			return;
		}

		/* insert interference edge */
		insert_edge(pbqp, src_node, trg_node, ife_matrix_template);
	}
}

static void inser_afe_edge(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *src_node, ir_node *trg_node, int pos)
{
	pbqp 						*pbqp             = pbqp_alloc_env->pbqp_inst;
	const arch_register_class_t *cls              = pbqp_alloc_env->cls;
	unsigned 					*restr_nodes      = pbqp_alloc_env->restr_nodes;
	pbqp_matrix					*afe_matrix       = pbqp_matrix_alloc(pbqp, arch_register_class_n_regs(cls), arch_register_class_n_regs(cls));
	unsigned 					 colors_n		  = arch_register_class_n_regs(cls);

	if (get_edge(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node)) == NULL) {
		if (use_exec_freq) {
			/* get exec_freq for copy_block */
			ir_node *root_bl = get_nodes_block(src_node);
			ir_node *copy_bl = is_Phi(src_node) ? get_Block_cfgpred_block(root_bl, pos) : root_bl;
			unsigned long res = get_block_execfreq_ulong(pbqp_alloc_env->birg->exec_freq, copy_bl);

			/* create afe-matrix */
			unsigned row, col;
			for (row = 0; row < colors_n; row++) {
				for (col = 0; col < colors_n; col++) {
					if (row != col)
						pbqp_matrix_set(afe_matrix, row, col, (num)res);
				}
			}
		}
		else {
			afe_matrix = pbqp_alloc_env->aff_matrix_template;
		}

		/* do useful optimization to speed up pbqp solving */
		if (get_free_regs(restr_nodes, cls, src_node) == 1 && get_free_regs(restr_nodes, cls, trg_node) == 1) {
			return;
		}
		if (get_free_regs(restr_nodes, cls, src_node) == 1 || get_free_regs(restr_nodes, cls, trg_node) == 1) {
			if (get_free_regs(restr_nodes, cls, src_node) == 1) {
				unsigned regIdx = vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs);
				vector_add_matrix_col(get_node(pbqp, get_irn_idx(trg_node))->costs, afe_matrix, regIdx);
			}
			else {
				unsigned regIdx = vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs);
				vector_add_matrix_col(get_node(pbqp, get_irn_idx(src_node))->costs, afe_matrix, regIdx);
			}
			return;
		}

		/* insert interference edge */
		insert_edge(pbqp, src_node, trg_node, afe_matrix);
	}
}

static void create_affinity_edges(ir_node *irn, void *env)
{
	be_pbqp_alloc_env_t         *pbqp_alloc_env   = env;
	const arch_register_class_t *cls              = pbqp_alloc_env->cls;
	const arch_register_req_t   *req              = arch_get_register_req_out(irn);
	unsigned pos, max;

	if (is_Reg_Phi(irn)) { /* Phis */
		for (pos=0, max=get_irn_arity(irn); pos<max; ++pos) {
			ir_node *arg = get_irn_n(irn, pos);

			if (!arch_irn_consider_in_reg_alloc(cls, arg))
				continue;

			/* no edges to itself */
			if (irn == arg) {
				continue;
			}

			inser_afe_edge(pbqp_alloc_env, irn, arg, pos);
		}
	}
	else if (is_Perm_Proj(irn)) { /* Perms */
		ir_node *arg = get_Perm_src(irn);
		if (!arch_irn_consider_in_reg_alloc(cls, arg))
			return;

		inser_afe_edge(pbqp_alloc_env, irn, arg, -1);
	}
	else { /* 2-address code */
		if (is_2addr_code(req)) {
			const unsigned other = req->other_same;
			int i;

			for (i = 0; 1U << i <= other; ++i) {
				if (other & (1U << i)) {
					ir_node *other = get_irn_n(skip_Proj(irn), i);
					if (!arch_irn_consider_in_reg_alloc(cls, other))
						continue;

					/* no edges to itself */
					if (irn == other) {
						continue;
					}

					inser_afe_edge(pbqp_alloc_env, irn, other, i);
				}
			}
		}
	}
}

static void create_pbqp_coloring_instance(ir_node *block, void *data)
{
	be_pbqp_alloc_env_t         *pbqp_alloc_env    	= data;
	be_lv_t                     *lv                	= pbqp_alloc_env->lv;
	const arch_register_class_t *cls               	= pbqp_alloc_env->cls;
	plist_t						*rpeo			   	= pbqp_alloc_env->rpeo;
	pbqp						*pbqp_inst		   	= pbqp_alloc_env->pbqp_inst;
	unsigned					*restr_nodes		= pbqp_alloc_env->restr_nodes;
	pqueue_t  					*queue             	= new_pqueue();
	pqueue_t  					*restr_nodes_queue 	= new_pqueue();
	plist_t						*temp_list         	= plist_new();
	plist_t						*sorted_list       	= plist_new();
	ir_node                     *irn;
	ir_nodeset_t                 live_nodes;
	plist_element_t *el;
	ir_node *last_element = NULL;

	/* first, determine the pressure */
	/* (this is only for compatibility with copymin optimization, it's not needed for pbqp coloring) */
	create_borders(block, pbqp_alloc_env->env);

	/* calculate living nodes for the first step */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	/* create pbqp nodes, interference edges and reverse perfect elimination order */
	sched_foreach_reverse(block, irn) {
		ir_node *live;
		ir_nodeset_iterator_t iter;

		if (get_irn_mode(irn) == mode_T) {
			const ir_edge_t *edge;
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;

				/* create pbqp source node if it dosn't exist */
				if (get_node(pbqp_inst, get_irn_idx(proj)) == NULL) {
					create_pbqp_node(pbqp_alloc_env, proj);
				}

				/* create nodes and interference edges */
				foreach_ir_nodeset(&live_nodes, live, iter) {
					/* create pbqp source node if it dosn't exist */
					if (get_node(pbqp_inst, get_irn_idx(live)) == NULL) {
						create_pbqp_node(pbqp_alloc_env, live);
					}

					/* no edges to itself */
					if (proj == live) {
						continue;
					}

					insert_ife_edge(pbqp_alloc_env, proj, live);
				}
			}
		}
		else {
			if (arch_irn_consider_in_reg_alloc(cls, irn)) {
				/* create pbqp source node if it dosn't exist */
				if (get_node(pbqp_inst, get_irn_idx(irn)) == NULL) {
					create_pbqp_node(pbqp_alloc_env, irn);
				}

				/* create nodes and interference edges */
				foreach_ir_nodeset(&live_nodes, live, iter) {
					/* create pbqp source node if it dosn't exist */
					if (get_node(pbqp_inst, get_irn_idx(live)) == NULL) {
						create_pbqp_node(pbqp_alloc_env, live);
					}

					/* no edges to itself */
					if (irn == live) {
						continue;
					}

					/* insert interference edge */
					insert_ife_edge(pbqp_alloc_env, irn, live);
				}
			}
		}

		/* get living nodes for next step */
		if (!is_Phi(irn)) {
			be_liveness_transfer(cls, irn, &live_nodes);
		}

		/* order nodes for perfect elimination order */
		if (get_irn_mode(irn) == mode_T) {
			bool allHaveIFEdges = true;

			const ir_edge_t *edge;
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;

				/* insert proj node into priority queue (descending by the number of interference edges) */
				if (get_free_regs(restr_nodes, cls, proj) <= 4) {
					pqueue_put(restr_nodes_queue, proj, pbqp_alloc_env->ife_edge_num[get_irn_idx(proj)]);
				}
				else {
					pqueue_put(queue, proj, pbqp_alloc_env->ife_edge_num[get_irn_idx(proj)]);
				}

				/* skip last step if there is no last_element */
				if(last_element == NULL)
					continue;

				/* check if proj has an if edge to last_element (at this time pbqp contains only if edges) */
				if(get_edge(pbqp_inst, proj->node_idx, last_element->node_idx) == NULL && get_edge(pbqp_inst, last_element->node_idx, proj->node_idx) == NULL) {
					allHaveIFEdges = false; /* there is no if edge between proj and last_element */
				}
			}

			if(last_element != NULL && allHaveIFEdges) {
				if (get_free_regs(restr_nodes, cls, last_element) <= 4) {
					pqueue_put(restr_nodes_queue, last_element, pbqp_alloc_env->ife_edge_num[get_irn_idx(last_element)]);
				}
				else {
					pqueue_put(queue, last_element, pbqp_alloc_env->ife_edge_num[get_irn_idx(last_element)]);
				}
				plist_erase(temp_list, plist_find_value(temp_list, get_node(pbqp_inst, last_element->node_idx)));
				last_element = NULL;
			}

			/* first insert all restricted proj nodes */
			while (!pqueue_empty(restr_nodes_queue)) {
				plist_insert_front(sorted_list, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(restr_nodes_queue))));
			}

			/* insert proj nodes descending by their number of interference edges */
			while (!pqueue_empty(queue)) {
				plist_insert_front(sorted_list, get_node(pbqp_inst, get_irn_idx(pqueue_pop_front(queue))));
			}

			/* invert sorted list */
			foreach_plist(sorted_list, el) {
				plist_insert_front(temp_list, el->data);
			}

			plist_clear(sorted_list);

		}
		else {
			if (arch_irn_consider_in_reg_alloc(cls, irn)) {
				// remember last colorable node
				last_element = irn;
				plist_insert_front(temp_list, get_node(pbqp_inst, get_irn_idx(irn)));
			}
			else {
				// node not colorable, so ignore it
				last_element = NULL;
			}
		}
	}

	/* insert nodes into reverse perfect elimination order */
	foreach_plist(temp_list, el) {
		plist_insert_back(rpeo, el->data);
	}

	/* free reserved memory */
	ir_nodeset_destroy(&live_nodes);
	plist_free(temp_list);
	plist_free(sorted_list);
	del_pqueue(queue);
	del_pqueue(restr_nodes_queue);
}

static void insert_perms(ir_node *block, void *data)
{
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

static void be_pbqp_coloring(be_chordal_env_t *env)
{
	ir_graph                   	*irg  			= env->irg;
	be_irg_t                   	*birg 			= env->birg;
	const arch_register_class_t *cls  			= env->cls;
	be_lv_t 					*lv				= NULL;
	plist_element_t 		  	*element		= NULL;
	unsigned 					 colors_n 		= arch_register_class_n_regs(cls);
	be_pbqp_alloc_env_t 		 pbqp_alloc_env;
	unsigned 					 row, col;


#if TIMER
	ir_timer_t *t_ra_pbqp_alloc_create     = ir_timer_new();
	ir_timer_t *t_ra_pbqp_alloc_solve      = ir_timer_new();
	ir_timer_t *t_ra_pbqp_alloc_create_aff = ir_timer_new();

	printf("#### ----- === Allocating registers of %s (%s) ===\n", cls->name, get_entity_name(get_irg_entity(irg)));
#endif
	lv = be_assure_liveness(birg);
	be_liveness_assure_sets(lv);
	be_liveness_assure_chk(lv);

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
	pbqp_alloc_env.ife_edge_num = XMALLOCNZ(unsigned, get_irg_last_idx(irg));
	pbqp_alloc_env.env			= env;
	be_put_ignore_regs(birg, cls, pbqp_alloc_env.ignored_regs);				/* get ignored registers */


	/* create costs matrix template for interference edges */
	struct pbqp_matrix *ife_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
	/* set costs */
	for (row = 0, col=0; row < colors_n; row++, col++)
		pbqp_matrix_set(ife_matrix, row, col, INF_COSTS);

	pbqp_alloc_env.ife_matrix_template = ife_matrix;


	if (!use_exec_freq) {
		/* create costs matrix template for affinity edges */
		struct pbqp_matrix *afe_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
		/* set costs */
		for (row = 0; row < colors_n; row++) {
			for (col = 0; col < colors_n; col++) {
				if (row != col)
					pbqp_matrix_set(afe_matrix, row, col, 2);
			}
		}
		pbqp_alloc_env.aff_matrix_template = afe_matrix;
	}


	/* create pbqp instance */
#if TIMER
	ir_timer_reset_and_start(t_ra_pbqp_alloc_create);
#endif
	assure_doms(irg);
	dom_tree_walk_irg(irg, create_pbqp_coloring_instance , NULL, &pbqp_alloc_env);
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_create);
#endif


	/* set up affinity edges */
#if TIMER
	ir_timer_reset_and_start(t_ra_pbqp_alloc_create_aff);
#endif
	foreach_plist(pbqp_alloc_env.rpeo, element) {
		pbqp_node 	*node	= element->data;
		ir_node 	*irn    = get_idx_irn(irg, node->index);

		create_affinity_edges(irn, &pbqp_alloc_env);
	}
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_create_aff);
#endif


#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE *file_before = my_open(env, "", "-pbqp_coloring.html");
	set_dumpfile(pbqp_alloc_env.pbqp_inst, file_before);
#endif

	/* print out reverse perfect eleminiation order */
#if PRINT_RPEO
	plist_element_t *elements;
	foreach_plist(pbqp_alloc_env.rpeo, elements) {
		pbqp_node *node			   = elements->data;
		printf(" %d(%lu);", node->index, get_idx_irn(irg, node->index)->node_nr);
	}
	printf("\n");
#endif


	/* solve pbqp instance */
#if TIMER
	ir_timer_reset_and_start(t_ra_pbqp_alloc_solve);
#endif
	if(use_late_decision) {
		solve_pbqp_heuristical_co_ld(pbqp_alloc_env.pbqp_inst,pbqp_alloc_env.rpeo);
	}
	else {
		solve_pbqp_heuristical_co(pbqp_alloc_env.pbqp_inst,pbqp_alloc_env.rpeo);
	}
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_solve);
#endif
	num solution = get_solution(pbqp_alloc_env.pbqp_inst);
	assert(solution != INF_COSTS && "No PBQP solution found");


	/* assign colors */
	foreach_plist(pbqp_alloc_env.rpeo, element) {
		pbqp_node 				*node	= element->data;
		ir_node 				*irn    = get_idx_irn(irg, node->index);
		num 					 color  = get_node_solution(pbqp_alloc_env.pbqp_inst, node->index);
		const arch_register_t 	*reg 	= arch_register_for_index(cls, color);

		arch_set_irn_register(irn, reg);
	}


#if TIMER
	printf("%-20s: %8.3lf msec\n", "pbqp alloc create",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create) / 1000.0);
	printf("%-20s: %8.3lf msec\n", "pbqp alloc solve",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_solve) / 1000.0);
	printf("%-20s: %8.3lf msec\n", "pbqp alloc create aff",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create_aff) / 1000.0);
#endif


	/* free reserved memory */
#if KAPS_DUMP
	fclose(file_before);
#endif
	bitset_free(pbqp_alloc_env.ignored_regs);
	free_pbqp(pbqp_alloc_env.pbqp_inst);
	plist_free(pbqp_alloc_env.rpeo);
	xfree(pbqp_alloc_env.restr_nodes);
	xfree(pbqp_alloc_env.ife_edge_num);
}


/**
 * Initializes this module.
 */
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_pbqp_coloring);
void be_init_pbqp_coloring(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *coloring_grp = lc_opt_get_grp(chordal_grp, "coloring");
	lc_opt_entry_t *pbqp_grp = lc_opt_get_grp(coloring_grp, "pbqp");

	static be_ra_chordal_coloring_t coloring = {
		be_pbqp_coloring
	};

	lc_opt_add_table(pbqp_grp, options);
	be_register_chordal_coloring("pbqp", &coloring);
}

#endif
