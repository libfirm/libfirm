/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       PBQP based register allocation.
 * @author      Thomas Bersch
 * @date        27.11.2009
 */

/* miscellaneous includes */
#include "panic.h"

#include "irdom.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "irtools.h"
#include "time.h"
#include "execfreq_t.h"
#include "bipartite.h"

/* libfirm/ir/be includes */
#include "bearch.h"
#include "beirg.h"
#include "besched.h"
#include "bemodule.h"
#include "bechordal_common.h"
#include "bechordal.h"
#include "bechordal_t.h"
#include "becopyopt_t.h"
#include "beinsn_t.h"
#include "benode.h"
#include "belive.h"
#include "belive.h"
#include "pdeq.h"
#include "pqueue.h"

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
#include "pbqp_edge_t.h"

#define TIMER                  0
#define PRINT_RPEO             0
#define USE_BIPARTITE_MATCHING 0
#define DO_USEFUL_OPT          1


static bool use_exec_freq     = true;
static bool use_late_decision = false;

typedef struct be_pbqp_alloc_env_t {
	pbqp_t                      *pbqp_inst;         /**< PBQP instance for register allocation */
	ir_graph                    *irg;               /**< The graph under examination. */
	const arch_register_class_t *cls;               /**< Current processed register class */
	be_lv_t                     *lv;
	bitset_t              const *allocatable_regs;
	pbqp_matrix_t               *ife_matrix_template;
	pbqp_matrix_t               *aff_matrix_template;
	deq_t                        rpeo;
	unsigned                    *restr_nodes;
	unsigned                    *ife_edge_num;
	ir_execfreq_int_factors      execfreq_factors;
	be_chordal_env_t            *env;
} be_pbqp_alloc_env_t;


#define insert_edge(pbqp, src_node, trg_node, template_matrix) (add_edge_costs(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node), pbqp_matrix_copy(pbqp, template_matrix)))
#define get_free_regs(restr_nodes, cls, irn)                   ((cls)->n_regs - restr_nodes[get_irn_idx(irn)])

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_BOOL("exec_freq", "use exec_freq",  &use_exec_freq),
	LC_OPT_ENT_BOOL("late_decision", "use late decision for register allocation",  &use_late_decision),
	LC_OPT_LAST
};

#if KAPS_DUMP
static FILE *my_open(const be_chordal_env_t *env, const char *prefix, const char *suffix)
{
	FILE       *result;
	char        buf[1024];
	size_t      i;
	size_t      n;
	char       *tu_name;
	const char *cup_name = be_get_irg_main_env(env->irg)->cup_name;

	n = strlen(cup_name);
	tu_name = XMALLOCN(char, n + 1);
	strcpy(tu_name, cup_name);
	for (i = 0; i < n; ++i)
		if (tu_name[i] == '.')
			tu_name[i] = '_';

	ir_snprintf(buf, sizeof(buf), "%s%s_%F_%s%s", prefix, tu_name, env->irg, env->cls->name, suffix);
	free(tu_name);
	result = fopen(buf, "wt");
	if (result == NULL) {
		panic("couldn't open '%s' for writing", buf);
	}

	return result;
}
#endif


static void create_pbqp_node(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *irn)
{
	arch_register_class_t const *const cls              = pbqp_alloc_env->cls;
	pbqp_t                      *const pbqp_inst        = pbqp_alloc_env->pbqp_inst;
	bitset_t              const *const allocatable_regs = pbqp_alloc_env->allocatable_regs;
	unsigned                     const colors_n         = cls->n_regs;
	unsigned                           cntConstrains    = 0;

	/* create costs vector depending on register constrains */
	vector_t *costs_vector = vector_alloc(pbqp_inst, colors_n);

	/* set costs depending on register constrains */
	unsigned idx;
	for (idx = 0; idx < colors_n; idx++) {
		const arch_register_req_t *req = arch_get_irn_register_req(irn);
		const arch_register_t     *reg = arch_register_for_index(cls, idx);
		if (!bitset_is_set(allocatable_regs, idx)
		    || !arch_reg_is_allocatable(req, reg)) {
			/* constrained */
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
	pbqp_t                      *pbqp                = pbqp_alloc_env->pbqp_inst;
	const arch_register_class_t *cls                 = pbqp_alloc_env->cls;
	pbqp_matrix_t               *ife_matrix_template = pbqp_alloc_env->ife_matrix_template;
	unsigned                    *restr_nodes         = pbqp_alloc_env->restr_nodes;

	if (get_edge(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node)) == NULL) {

		/* increase ife edge counter */
		pbqp_alloc_env->ife_edge_num[get_irn_idx(src_node)]++;
		pbqp_alloc_env->ife_edge_num[get_irn_idx(trg_node)]++;

#if DO_USEFUL_OPT || USE_BIPARTITE_MATCHING
		/* do useful optimization to speed up pbqp solving (we can do this because we know our matrix) */
		if (get_free_regs(restr_nodes, cls, src_node) == 1 && get_free_regs(restr_nodes, cls, trg_node) == 1) {
			assert(vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs) !=
			       vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs) &&
			       "Interfering nodes must not have the same register!");
			return;
		}
		if (get_free_regs(restr_nodes, cls, src_node) == 1 || get_free_regs(restr_nodes, cls, trg_node) == 1) {
			if (get_free_regs(restr_nodes, cls, src_node) == 1) {
				unsigned idx = vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs);
				vector_set(get_node(pbqp, get_irn_idx(trg_node))->costs, idx, INF_COSTS);
			} else {
				unsigned idx = vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs);
				vector_set(get_node(pbqp, get_irn_idx(src_node))->costs, idx, INF_COSTS);
			}
			return;
		}
#endif
		/* insert interference edge */
		insert_edge(pbqp, src_node, trg_node, ife_matrix_template);
	}
}

static void insert_afe_edge(be_pbqp_alloc_env_t *pbqp_alloc_env, ir_node *src_node, ir_node *trg_node, int pos)
{
	pbqp_t                      *pbqp        = pbqp_alloc_env->pbqp_inst;
	const arch_register_class_t *cls         = pbqp_alloc_env->cls;
	unsigned                    *restr_nodes = pbqp_alloc_env->restr_nodes;
	unsigned                     colors_n    = cls->n_regs;
	pbqp_matrix_t               *afe_matrix  = pbqp_matrix_alloc(pbqp, colors_n, colors_n);

	if (get_edge(pbqp, get_irn_idx(src_node), get_irn_idx(trg_node)) == NULL) {
		if (use_exec_freq) {
			/* get exec_freq for copy_block */
			ir_node *root_bl = get_nodes_block(src_node);
			ir_node *copy_bl = is_Phi(src_node) ? get_Block_cfgpred_block(root_bl, pos) : root_bl;
			int      res     = get_block_execfreq_int(&pbqp_alloc_env->execfreq_factors, copy_bl);

			/* create afe-matrix */
			unsigned row, col;
			for (row = 0; row < colors_n; row++) {
				for (col = 0; col < colors_n; col++) {
					if (row != col)
						pbqp_matrix_set(afe_matrix, row, col, (num)res);
				}
			}
		} else {
			afe_matrix = pbqp_alloc_env->aff_matrix_template;
		}
#if DO_USEFUL_OPT || USE_BIPARTITE_MATCHING
		/* do useful optimization to speed up pbqp solving */
		if (get_free_regs(restr_nodes, cls, src_node) == 1 && get_free_regs(restr_nodes, cls, trg_node) == 1) {
			return;
		}
		if (get_free_regs(restr_nodes, cls, src_node) == 1 || get_free_regs(restr_nodes, cls, trg_node) == 1) {
			if (get_free_regs(restr_nodes, cls, src_node) == 1) {
				unsigned regIdx = vector_get_min_index(get_node(pbqp, get_irn_idx(src_node))->costs);
				vector_add_matrix_col(get_node(pbqp, get_irn_idx(trg_node))->costs, afe_matrix, regIdx);
			} else {
				unsigned regIdx = vector_get_min_index(get_node(pbqp, get_irn_idx(trg_node))->costs);
				vector_add_matrix_col(get_node(pbqp, get_irn_idx(src_node))->costs, afe_matrix, regIdx);
			}
			return;
		}
#endif
		/* insert interference edge */
		insert_edge(pbqp, src_node, trg_node, afe_matrix);
	}
}

static void create_affinity_edges(ir_node *irn, void *env)
{
	be_pbqp_alloc_env_t         *pbqp_alloc_env = (be_pbqp_alloc_env_t*)env;
	const arch_register_class_t *cls            = pbqp_alloc_env->cls;
	const arch_register_req_t   *req            = arch_get_irn_register_req(irn);

	if (is_Phi(irn)) { /* Phis */
		foreach_irn_in(irn, pos, arg) {
			if (!arch_irn_consider_in_reg_alloc(cls, arg))
				continue;

			/* no edges to itself */
			if (irn == arg) {
				continue;
			}

			insert_afe_edge(pbqp_alloc_env, irn, arg, pos);
		}
	} else if (is_Perm_Proj(irn)) { /* Perms */
		ir_node *arg = get_Perm_src(irn);
		if (!arch_irn_consider_in_reg_alloc(cls, arg))
			return;

		insert_afe_edge(pbqp_alloc_env, irn, arg, -1);
	} else if (req->should_be_same != 0) {
		const unsigned other = req->should_be_same;
		int            i;

		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_node *other = get_irn_n(skip_Proj(irn), i);
				if (!arch_irn_consider_in_reg_alloc(cls, other))
					continue;

				/* no edges to itself */
				if (irn == other) {
					continue;
				}

				insert_afe_edge(pbqp_alloc_env, irn, other, i);
			}
		}
	}
}

static void create_pbqp_coloring_instance(ir_node *block, void *data)
{
	be_pbqp_alloc_env_t         *pbqp_alloc_env     = (be_pbqp_alloc_env_t*)data;
	be_lv_t                     *lv                 = pbqp_alloc_env->lv;
	const arch_register_class_t *cls                = pbqp_alloc_env->cls;
	deq_t                       *rpeo               = &pbqp_alloc_env->rpeo;
	pbqp_t                      *pbqp_inst          = pbqp_alloc_env->pbqp_inst;
	pbqp_node_t                **temp_list          = NEW_ARR_F(pbqp_node_t*, 0);
	ir_nodeset_t                 live_nodes;
#if USE_BIPARTITE_MATCHING
	int                         *assignment         = ALLOCAN(int, cls->n_regs);
#else
	unsigned                    *restr_nodes        = pbqp_alloc_env->restr_nodes;
	pqueue_t                    *restr_nodes_queue  = new_pqueue();
	pqueue_t                    *queue              = new_pqueue();
	ir_node                     *last_element       = NULL;
	pbqp_node_t                **sorted_list        = NEW_ARR_F(pbqp_node_t*, 0);
#endif

	/* first, determine the pressure */
	/* (this is only for compatibility with copymin optimization, it's not needed for pbqp coloring) */
	create_borders(block, pbqp_alloc_env->env);

	/* calculate living nodes for the first step */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	/* create pbqp nodes, interference edges and reverse perfect elimination order */
	sched_foreach_reverse(block, irn) {
		be_foreach_value(irn, value,
			if (!arch_irn_consider_in_reg_alloc(cls, value))
				continue;

			/* create pbqp source node if it doesn't exist */
			if (!get_node(pbqp_inst, get_irn_idx(value)))
				create_pbqp_node(pbqp_alloc_env, value);

			/* create nodes and interference edges */
			foreach_ir_nodeset(&live_nodes, live, iter) {
				/* create pbqp source node if it doesn't exist */
				if (!get_node(pbqp_inst, get_irn_idx(live)))
					create_pbqp_node(pbqp_alloc_env, live);

				/* no edges to itself */
				if (value == live)
					continue;

				insert_ife_edge(pbqp_alloc_env, value, live);
			}
		);

		/* get living nodes for next step */
		if (!is_Phi(irn)) {
			be_liveness_transfer(cls, irn, &live_nodes);
		}

#if USE_BIPARTITE_MATCHING
		if (get_irn_mode(irn) == mode_T) {
			unsigned     clique_size         = 0;
			unsigned     n_alloc             = 0;
			pbqp_node_t *clique[cls->n_regs];
			bipartite_t *bp                  = bipartite_new(cls->n_regs, cls->n_regs);

			/* add all proj after a perm to clique */
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				/* ignore node if it is not necessary for register allocation */
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;

				/* insert pbqp node into temp rpeo list of this block */
				ARR_APP1(pbqp_node_t*, temp_list, get_node(pbqp_inst, get_irn_idx(proj)));

				if (is_Perm_Proj(proj)) {
					/* add proj to clique */
					pbqp_node_t *clique_member = get_node(pbqp_inst,proj->node_idx);
					vector_t    *costs         = clique_member->costs;
					unsigned     idx           = 0;

					clique[clique_size] = clique_member;

					for (idx = 0; idx < costs->len; idx++) {
						if (costs->entries[idx].data != INF_COSTS) {
							bipartite_add(bp, clique_size, idx);
						}
					}

					/* increase node counter */
					clique_size++;
					n_alloc++;
				}
			}

			if (clique_size > 0) {
				for (size_t i = ARR_LEN(temp_list); i-- > 0;) {
					pbqp_node_t *clique_candidate  = temp_list[i];
					unsigned     idx               = 0;
					bool         isMember          = true;

					/* clique size not bigger then register class size */
					if (clique_size >= cls->n_regs) break;

					for (idx = 0; idx < clique_size; idx++) {
						pbqp_node_t *member = clique[idx];

						if (member == clique_candidate) {
							isMember = false;
							break;
						}

						if (get_edge(pbqp_inst, member->index, clique_candidate->index) == NULL && get_edge(pbqp_inst, clique_candidate->index, member->index) == NULL) {
							isMember = false;
							break;
						}
					}

					/* goto next list element if current node is not a member of the clique */
					if (!isMember) { continue; }

					/* add candidate to clique */
					clique[clique_size] = clique_candidate;

					vector_t *costs = clique_candidate->costs;
					for (idx = 0; idx < costs->len; idx++) {
						if (costs->entries[idx].data != INF_COSTS) {
							bipartite_add(bp, clique_size, idx);
						}
					}

					/* increase node counter */
					clique_size++;
				}
			}

			/* solve bipartite matching */
			bipartite_matching(bp, assignment);

			/* assign colors */
			unsigned nodeIdx = 0;
			for (nodeIdx = 0; nodeIdx < clique_size; nodeIdx++) {
				vector_t *costs = clique[nodeIdx]->costs;
				for (int idx = 0; idx < (int)costs->len; idx++) {
					if (assignment[nodeIdx] != idx) {
						costs->entries[idx].data = INF_COSTS;
					}
				}
				assert(assignment[nodeIdx] >= 0 && "there must have been a register assigned (node not register pressure faithful?)");
			}

			/* free memory */
			bipartite_free(bp);
		} else if (arch_irn_consider_in_reg_alloc(cls, irn)) {
			ARR_APP1(pbqp_node_t*, temp_list, get_node(pbqp_inst, get_irn_idx(irn)));
		}
#else
		/* order nodes for perfect elimination order */
		if (get_irn_mode(irn) == mode_T) {
			bool allHaveIFEdges = true;
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;

				/* insert proj node into priority queue (descending by the number of interference edges) */
				if (get_free_regs(restr_nodes, cls, proj) <= 4) {
					pqueue_put(restr_nodes_queue, proj, pbqp_alloc_env->ife_edge_num[get_irn_idx(proj)]);
				} else {
					pqueue_put(queue, proj, pbqp_alloc_env->ife_edge_num[get_irn_idx(proj)]);
				}

				/* skip last step if there is no last_element */
				if (last_element == NULL)
					continue;

				/* check if proj has an interference edge to last_element (at this time pbqp contains only interference edges) */
				if (get_edge(pbqp_inst, proj->node_idx, last_element->node_idx) == NULL && get_edge(pbqp_inst, last_element->node_idx, proj->node_idx) == NULL) {
					allHaveIFEdges = false; /* there is no interference edge between proj and last_element */
				}
			}

			if (last_element != NULL && allHaveIFEdges) {
				if (get_free_regs(restr_nodes, cls, last_element) <= 4) {
					pqueue_put(restr_nodes_queue, last_element, pbqp_alloc_env->ife_edge_num[get_irn_idx(last_element)]);
				} else {
					pqueue_put(queue, last_element, pbqp_alloc_env->ife_edge_num[get_irn_idx(last_element)]);
				}
				/* remove node from list */
				pbqp_node_t *node = get_node(pbqp_inst, last_element->node_idx);
				for (size_t i = 0, e = ARR_LEN(temp_list); i < e; ++i) {
					if (temp_list[i] == node) {
						temp_list[i] = NULL;
						break;
					}
				}
				last_element = NULL;
			}

			/* first insert all restricted proj nodes */
			while (!pqueue_empty(restr_nodes_queue)) {
				ir_node *node = (ir_node*)pqueue_pop_front(restr_nodes_queue);
				ARR_APP1(pbqp_node_t*, sorted_list, get_node(pbqp_inst, get_irn_idx(node)));
			}

			/* insert proj nodes descending by their number of interference edges */
			while (!pqueue_empty(queue)) {
				ir_node *node = (ir_node*)pqueue_pop_front(queue);
				ARR_APP1(pbqp_node_t*, sorted_list, get_node(pbqp_inst, get_irn_idx(node)));
			}

			/* invert sorted list */
			for (unsigned i = ARR_LEN(sorted_list); i-- > 0;) {
				ARR_APP1(pbqp_node_t*, temp_list, sorted_list[i]);
			}

			ARR_SHRINKLEN(sorted_list, 0);
		} else {
			if (arch_irn_consider_in_reg_alloc(cls, irn)) {
				// remember last colorable node
				last_element = irn;
				ARR_APP1(pbqp_node_t*, temp_list, get_node(pbqp_inst, get_irn_idx(irn)));
			} else {
				// node not colorable, so ignore it
				last_element = NULL;
			}
		}
#endif
	}

	/* add the temp peo list of this block to the global reverse perfect
	 * elimination order list */
	for (size_t i = ARR_LEN(temp_list); i-- > 0;) {
		if (temp_list[i] == NULL)
			continue;
		deq_push_pointer_right(rpeo, temp_list[i]);
	}

	/* free reserved memory */
	ir_nodeset_destroy(&live_nodes);
	DEL_ARR_F(temp_list);
#if USE_BIPARTITE_MATCHING
#else
	DEL_ARR_F(sorted_list);
	del_pqueue(queue);
	del_pqueue(restr_nodes_queue);
#endif
}

static void insert_perms(ir_node *block, void *data)
{
	be_chordal_env_t *env    = (be_chordal_env_t*)data;

	sched_foreach_safe(block, irn) {
		be_insn_t *insn = be_scan_insn(env, irn);
		if (insn)
			pre_process_constraints(env, &insn);
	}
}

static void be_pbqp_coloring(be_chordal_env_t *env)
{
	ir_graph                    *irg = env->irg;
	arch_register_class_t const *cls = env->cls;
#if TIMER
	ir_timer_t *const t_ra_pbqp_alloc_create     = ir_timer_new();
	ir_timer_t *const t_ra_pbqp_alloc_solve      = ir_timer_new();
	ir_timer_t *const t_ra_pbqp_alloc_create_aff = ir_timer_new();

	printf("#### ----- === Allocating registers of %s (%s) ===\n", cls->name, get_entity_name(get_irg_entity(irg)));
#endif
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	be_assure_live_sets(irg);

	/* insert perms */
	dom_tree_walk_irg(irg, insert_perms, NULL, env);

	be_chordal_dump(BE_CH_DUMP_CONSTR, irg, cls, "constr");

	be_pbqp_alloc_env_t pbqp_alloc_env;
	ir_calculate_execfreq_int_factors(&pbqp_alloc_env.execfreq_factors, irg);

	/* initialize pbqp allocation data structure */
	pbqp_alloc_env.pbqp_inst        = alloc_pbqp(get_irg_last_idx(irg));  /* initialize pbqp instance */
	pbqp_alloc_env.cls              = cls;
	pbqp_alloc_env.irg              = irg;
	pbqp_alloc_env.lv               = be_get_irg_liveness(irg);
	pbqp_alloc_env.allocatable_regs = env->allocatable_regs;
	pbqp_alloc_env.restr_nodes      = XMALLOCNZ(unsigned, get_irg_last_idx(irg));
	pbqp_alloc_env.ife_edge_num     = XMALLOCNZ(unsigned, get_irg_last_idx(irg));
	pbqp_alloc_env.env              = env;
	deq_init(&pbqp_alloc_env.rpeo);

	unsigned const colors_n = cls->n_regs;
	/* create costs matrix template for interference edges */
	pbqp_matrix_t *const ife_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
	/* set costs */
	for (unsigned row = 0, col = 0; row < colors_n; row++, col++)
		pbqp_matrix_set(ife_matrix, row, col, INF_COSTS);

	pbqp_alloc_env.ife_matrix_template = ife_matrix;


	if (!use_exec_freq) {
		/* create costs matrix template for affinity edges */
		pbqp_matrix_t *afe_matrix = pbqp_matrix_alloc(pbqp_alloc_env.pbqp_inst, colors_n, colors_n);
		/* set costs */
		for (unsigned row = 0; row < colors_n; row++) {
			for (unsigned col = 0; col < colors_n; col++) {
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
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	dom_tree_walk_irg(irg, create_pbqp_coloring_instance , NULL, &pbqp_alloc_env);
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_create);
#endif


	/* set up affinity edges */
#if TIMER
	ir_timer_reset_and_start(t_ra_pbqp_alloc_create_aff);
#endif
	deq_foreach_pointer(&pbqp_alloc_env.rpeo, pbqp_node_t, node) {
		ir_node     *irn  = get_idx_irn(irg, node->index);

		create_affinity_edges(irn, &pbqp_alloc_env);
	}
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_create_aff);
#endif


#if KAPS_DUMP
	// dump graph before solving pbqp
	FILE* const file_before = my_open(env, "", "-pbqp_coloring.html");
	set_dumpfile(pbqp_alloc_env.pbqp_inst, file_before);
#endif

	/* print out reverse perfect elimination order */
#if PRINT_RPEO
	deq_foreach_pointer(&pbqp_alloc_env.rpeo, pbqp_node_t, node) {
		printf(" %d(%ld);", node->index, get_idx_irn(irg, node->index)->node_nr);
	}
	printf("\n");
#endif

	/* solve pbqp instance */
#if TIMER
	ir_timer_reset_and_start(t_ra_pbqp_alloc_solve);
#endif
	if (use_late_decision) {
		solve_pbqp_heuristical_co_ld(pbqp_alloc_env.pbqp_inst,
		                             &pbqp_alloc_env.rpeo);
	} else {
		solve_pbqp_heuristical_co(pbqp_alloc_env.pbqp_inst,
		                          &pbqp_alloc_env.rpeo);
	}
#if TIMER
	ir_timer_stop(t_ra_pbqp_alloc_solve);
#endif


	num const solution = get_solution(pbqp_alloc_env.pbqp_inst);
	if (solution == INF_COSTS)
		panic("no PBQP solution found");


	/* assign colors */
	deq_foreach_pointer(&pbqp_alloc_env.rpeo, pbqp_node_t, node) {
		ir_node *const irn   = get_idx_irn(irg, node->index);
		num      const color = get_node_solution(pbqp_alloc_env.pbqp_inst, node->index);
		arch_set_irn_register_idx(irn, color);
	}


#if TIMER
	printf("PBQP alloc create:     %10.3lf msec\n",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create) / 1000.0);
	printf("PBQP alloc solve:      %10.3lf msec\n",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_solve) / 1000.0);
	printf("PBQP alloc create aff: %10.3lf msec\n",
	       (double)ir_timer_elapsed_usec(t_ra_pbqp_alloc_create_aff) / 1000.0);
#endif


	/* free reserved memory */
#if KAPS_DUMP
	fclose(file_before);
#endif
	free_pbqp(pbqp_alloc_env.pbqp_inst);
	deq_free(&pbqp_alloc_env.rpeo);
	free(pbqp_alloc_env.restr_nodes);
	free(pbqp_alloc_env.ife_edge_num);
}


/**
 * Initializes this module.
 */
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_pbqp_coloring)
void be_init_pbqp_coloring(void)
{
	lc_opt_entry_t *be_grp       = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp       = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp  = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *coloring_grp = lc_opt_get_grp(chordal_grp, "coloring");
	lc_opt_entry_t *pbqp_grp     = lc_opt_get_grp(coloring_grp, "pbqp");

	static be_ra_chordal_coloring_t coloring = {
		be_pbqp_coloring
	};

	lc_opt_add_table(pbqp_grp, options);
	be_register_chordal_coloring("pbqp", &coloring);
}
