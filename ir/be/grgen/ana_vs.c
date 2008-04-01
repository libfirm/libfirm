/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/ana_vs.c
 * Purpose:     implements the interfaces in analyze.h by a
 *	 			v-structure (vs) statistic (see [D"orr95],[Batz05b])
 * Author:      Veit Batz
 * Created:		22. June 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file	ext/grs/ana_vs.c
 * @brief	implements the interfaces in analyze.h by a
 * 			v-structure (vs) statistic (see [D"orr95],[Batz05b])
 */

#include "iredges_t.h"
#include "irgwalk.h"

#include "common_t.h"
#include "analyze.h"
#include "auxilary_t.h"
#include "base_t.h"
#include "ana_vs_t.h"



/* counter for isomorphic arguments and result uses during analyses */
static int *n_isomorphics;
static int op_dim, mode_dim;
#define N_ISOMORPHICS(opc, mc) n_isomorphics[(opc)*mode_dim + (mc)]


/*  Counts isomorphic edeges incident at the current node
 *  and stores some information in vs table.
 *  To be called by an irg walker
 *
 * node   the current ir node
 * env    the vs statistics object of the current ir graph
 */
static void ana_nodes_vs(ir_node *node, void *env)
{

	ext_grs_vs_stat_t *vs_stat = (ext_grs_vs_stat_t *) env;
	ir_graph *irg = vs_stat->irg;
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);
	ext_grs_irn_private_t *pr_n = _ext_grs_get_irn_private(node);

	const ir_edge_t *edge;

	int j, k;
	int n_ins = get_irn_arity(node);

	int i = -1; /* for non Block nodes start at pos -1 */
	ir_opcode opc = get_irn_opcode(node);
	modecode mc = get_irn_modecode(node);



	assert(vs_stat->irg == get_irn_irg(node) && "got wrong vs statistics");




	/* add the current node to the appropriate node list */
	lc_list_add(& pr_n->node_list, & _ext_grs_NODE_LIST(pr_g, opc, mc));
	/* increment the node counter counting the node in that list */
	(_ext_grs_N_INSTANCES(pr_g, opc, mc))++;
	/* increment the counter counting the instances of an op for ANY mode */
	if (mc != irm_ANY)
		(_ext_grs_N_INSTANCES(pr_g, opc, irm_ANY))++;

	/* count isomorphic arguments of the current node */
	if (get_irn_op(node) == op_Block) i = 0;
	for ( ; i < n_ins; i++) {

		ir_node *arg = get_irn_n(node, i);
		ir_opcode opc_arg = get_irn_opcode(arg);
		modecode mc_arg = get_irn_modecode(arg);
		ir_opcode super_opc_arg;


		int n_super_ops = ARR_LEN(_ext_grs_all_super_ops_of_op[opc_arg]);

		/* increment counters for the apropriate op and all super ops */
		for (j = 0; j < n_super_ops; j++) {
			super_opc_arg = get_op_code(_ext_grs_all_super_ops_of_op[opc_arg][j]);
			N_ISOMORPHICS(super_opc_arg, mc_arg)++;
			N_ISOMORPHICS(super_opc_arg, irm_ANY)++;
		}
	}
	/* store resulting number of v structures */
	i = -1;
	if (get_irn_op(node) == op_Block) i = 0;
	n_ins = get_irn_arity(node);
	for ( ; i < n_ins; i++) {

		ir_node *arg = get_irn_n(node, i);
		ir_opcode opc_arg = get_irn_opcode(arg);
		modecode mc_arg = get_irn_modecode(arg);
		ir_opcode super_opc_arg;


#if(0)
		int n_occ_add = 0;
		int n_occ_any_add = 0;
		int n_any_occ_add = 0;
		int n_any_occ_any_add = 0;
#endif

		int n_super_ops = ARR_LEN(_ext_grs_all_super_ops_of_op[opc_arg]);

		for (j = 0; j < n_super_ops; j++) {
			super_opc_arg = get_op_code(_ext_grs_all_super_ops_of_op[opc_arg][j]);
			if (N_ISOMORPHICS(super_opc_arg, mc_arg) > 0) {
				double x = _log2(N_ISOMORPHICS(super_opc_arg, mc_arg));

#if(0)
				/* count the occurence of nodes with such an edge */
				n_occ_add++;
				if (mc != irm_ANY)
					n_occ_any_add++;
#endif

				/* accumulate multiplicity of such an edge */
				if (x > 0) {
					/* and also add for all super ops of curent nodes op */
					int n_super_ops_func = ARR_LEN(_ext_grs_all_super_ops_of_op[opc]);
					for (k = 0; k < n_super_ops_func; k++) {
						ir_opcode super_opc_func = get_op_code(_ext_grs_all_super_ops_of_op[opc][k]);

						_add_n_v_structures(
							vs_stat, super_opc_func, mc, super_opc_arg, mc_arg, ext_grs_in, x);
						if (mc != irm_ANY)
							_add_n_v_structures(vs_stat,
								super_opc_func, irm_ANY, super_opc_arg, mc_arg, ext_grs_in, x);

					}
				}
				N_ISOMORPHICS(super_opc_arg, mc_arg) = 0;
			}
			if (N_ISOMORPHICS(super_opc_arg, irm_ANY) > 0) {
				double x = _log2(N_ISOMORPHICS(super_opc_arg, irm_ANY));

#if(0)
				/* count the occurence of nodes with such an edge */
				n_any_occ_add++;
				if (mc != irm_ANY)
					n_any_occ_any_add++;
#endif

				/* accumulate multiplicity of such an edge */
				if (x > 0) {
					/* and also add for all super ops of curent nodes op */
					int n_super_ops_func = ARR_LEN(_ext_grs_all_super_ops_of_op[opc]);
					for (k = 0; k < n_super_ops_func; k++) {
						ir_opcode super_opc_func = get_op_code(_ext_grs_all_super_ops_of_op[opc][k]);

						_add_n_v_structures(
							vs_stat, super_opc_func, mc, super_opc_arg, irm_ANY, ext_grs_in, x);
						if (mc != irm_ANY)
							_add_n_v_structures(vs_stat,
								super_opc_func, irm_ANY, super_opc_arg, irm_ANY, ext_grs_in, x);
					}
				}
				N_ISOMORPHICS(super_opc_arg, irm_ANY) = 0;
			}
		}
#if(0)
		if (n_occ_add > 0)
			_add_n_occurence(vs_stat, opc, mc, opc_arg, mc_arg, ext_grs_in, n_occ_add);
		if (n_occ_any_add > 0)
			_add_n_occurence(vs_stat, opc, irm_ANY, opc_arg, mc_arg, ext_grs_in, n_any_occ_add);
		if (n_any_occ_add > 0)
			_add_n_occurence(vs_stat, opc, mc, opc_arg, irm_ANY, ext_grs_in, n_occ_any_add);
		if (n_any_occ_any_add > 0)
			_add_n_occurence(vs_stat, opc, irm_ANY, opc_arg, irm_ANY, ext_grs_in, n_any_occ_any_add);
#endif

	}



	/* count isomorphic uses of the current nodes result */
	foreach_out_edge(node, edge) {

		ir_node *res = get_edge_src_irn(edge);
		ir_opcode opc_res = get_irn_opcode(res);
		modecode mc_res = get_irn_modecode(res);

		modecode super_opc_res;

		int n_super_ops = ARR_LEN( _ext_grs_all_super_ops_of_op[opc_res]);

		/* increment counters for the apropriate op and all super ops */
		for (j = 0; j < n_super_ops; j++) {
			super_opc_res = get_op_code(_ext_grs_all_super_ops_of_op[opc_res][j]);
			N_ISOMORPHICS(super_opc_res, mc_res)++;
			N_ISOMORPHICS(super_opc_res, irm_ANY)++;
		}
	}
	/* store resulting number of v structures */
	foreach_out_edge(node, edge) {

		ir_node *res = get_edge_src_irn(edge);
		ir_opcode opc_res = get_irn_opcode(res);
		modecode mc_res = get_irn_modecode(res);
		modecode super_opc_res;

#if(0)
		int n_occ_add = 0;
		int n_occ_any_add = 0;
		int n_any_occ_add = 0;
		int n_any_occ_any_add = 0;
#endif

		int n_super_ops = ARR_LEN( _ext_grs_all_super_ops_of_op[opc_res]);

		for (j = 0; j < n_super_ops; j++) {
			super_opc_res = get_op_code(_ext_grs_all_super_ops_of_op[opc_res][j]);
			if (N_ISOMORPHICS(super_opc_res, mc_res) > 0) {

				double x = _log2(N_ISOMORPHICS(super_opc_res, mc_res));

#if(0)
				/* count the occurence of nodes with such an edge */
				n_occ_add++;
				if (mc != irm_ANY)
					n_any_occ_add++;
#endif

				/* accumulate multiplicity of such an edge */
				if (x > 0) {
					/* and also add for all super ops of curent nodes op */
					int n_super_ops_func = ARR_LEN(_ext_grs_all_super_ops_of_op[opc]);
					for (k = 0; k < n_super_ops_func; k++) {
						ir_opcode super_opc_func = get_op_code(_ext_grs_all_super_ops_of_op[opc][k]);

						_add_n_v_structures(
							vs_stat, super_opc_func, mc, super_opc_res, mc_res, ext_grs_out, x);
						if (mc != irm_ANY)
							_add_n_v_structures(vs_stat,
								super_opc_func, irm_ANY, super_opc_res, mc_res, ext_grs_out, x);
					}
				}
				N_ISOMORPHICS(super_opc_res, mc_res) = 0;
			}
			if (N_ISOMORPHICS(super_opc_res, irm_ANY) > 0) {

				double x = _log2(N_ISOMORPHICS(super_opc_res, irm_ANY));

#if(0)
				/* count the occurence of nodes with such an edge */
				n_occ_any_add++;
				if (mc != irm_ANY)
					n_any_occ_any_add++;
#endif

				/* accumulate multiplicity of such an edge */
				if (x > 0) {
					/* and also add for all super ops of curent nodes op */
					int n_super_ops_func = ARR_LEN(_ext_grs_all_super_ops_of_op[opc]);
					for (k = 0; k < n_super_ops_func; k++) {
						ir_opcode super_opc_func = get_op_code(_ext_grs_all_super_ops_of_op[opc][k]);

						_add_n_v_structures(
							vs_stat, super_opc_func, mc, super_opc_res, irm_ANY, ext_grs_out, x);
						if (mc != irm_ANY)
							_add_n_v_structures(vs_stat,
								super_opc_func, irm_ANY, super_opc_res, irm_ANY, ext_grs_out, x);
					}
				}
				N_ISOMORPHICS(super_opc_res, irm_ANY) = 0;
			}
		}
#if(0)
		if (n_occ_add > 0)
			_add_n_occurence(vs_stat, opc, mc, opc_res, mc_res, ext_grs_out, n_occ_add);
		if (n_any_occ_add > 0)
			_add_n_occurence(vs_stat, opc, irm_ANY, opc_res, mc_res, ext_grs_out, n_any_occ_add);
		if (n_occ_any_add > 0)
			_add_n_occurence(vs_stat, opc, mc, opc_res, irm_ANY, ext_grs_out, n_occ_add);
		if (n_any_occ_any_add > 0)
			_add_n_occurence(vs_stat, opc, irm_ANY, opc_res, irm_ANY, ext_grs_out, n_any_occ_add);
#endif

	}

}

/* initialize an vs analyzer */
static void init(ext_grs_analyzer_t *alz) {

	ext_grs_ana_vs_private_t *pr_a = malloc(sizeof(*pr_a));

	/* init isomorphic edge counters with 0 */
	op_dim = 170;
	mode_dim = 25;
	n_isomorphics = malloc(170*25*sizeof(int));
	memset(n_isomorphics, 0, 170*25*sizeof(int));

	/* init the pset mapping ir_graphs to that ir_graphs vs statistic */
	pr_a->ana_results = lc_pset_new(_stat_cmp_func, 256);
	/* init the global vs statistic */
	pr_a->global_vs_stat.irg = NULL;
	pr_a->global_vs_stat.stat = lc_pset_new(_vs_cmp_func, 256);
	obstack_init(& pr_a->global_vs_stat.obst);

	alz->data = pr_a;
}

/* perform string v structure analysis for a given graph */
static void analyze(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	int j,k;
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);
	ext_grs_vs_stat_t *vs_stat;

	assert(pr_g->matching_enabled && "matching not enabled for this graph");

	edges_assure(irg);

	/* check whether the array of instance counters is great enough */
	if (_ext_grs_max_opcode >= op_dim) {
		free(n_isomorphics);
		op_dim = _ext_grs_max_opcode + 50;
		n_isomorphics = malloc(op_dim*mode_dim*sizeof(int));
		memset(n_isomorphics, 0, op_dim*mode_dim*sizeof(int));
	}

	if (_ext_grs_max_modecode >= mode_dim) {
		free(n_isomorphics);
		mode_dim = _ext_grs_max_modecode + 50;
		n_isomorphics = malloc(op_dim*mode_dim*sizeof(int));
		memset(n_isomorphics, 0, op_dim*mode_dim*sizeof(int));
	}


	/* init the op list heads */
	for (j = 0; j < _ext_grs_irgpr_op_dim; j++)
		for (k = 0; k < _ext_grs_irgpr_mode_dim; k++) {
			LC_INIT_LIST_HEAD(& _ext_grs_NODE_LIST(pr_g, j, k));
			_ext_grs_N_INSTANCES(pr_g, j, k) = 0;
		}
	/* init the number of (op,mode)-instances with zero */
	memset(pr_g->n_instances, 0,
		_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim *
			sizeof(*(pr_g->n_instances)));

	/* get the statistics object of the given irg and remove its complete contents */
	vs_stat = _get_irg_vs_stat(alz, irg);
	assert(vs_stat->irg == irg);

	lc_pset_del(vs_stat->stat);
	vs_stat->stat = lc_pset_new(_vs_cmp_func, 256);
	obstack_free(& vs_stat->obst, NULL);
	obstack_init(& vs_stat->obst);



	irg_walk_graph(irg, ana_nodes_vs, NULL, vs_stat);
}

static void free_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	ext_grs_vs_stat_t *vs_stat = _get_irg_vs_stat(alz, irg);

	if (vs_stat->stat) {
		lc_pset_del(vs_stat->stat);
		vs_stat->stat = lc_pset_new(_vs_cmp_func, 256);
	}

	obstack_free(& vs_stat->obst, NULL);
	obstack_init(& vs_stat->obst);
}

/* dump the current analysis result for a given ir graph */
static void dump_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	int i,j,k,l,r;
	double x;
	ext_grs_vs_stat_t *vs_stat = _get_irg_vs_stat(alz, irg);
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);

	printf("Dump vs-statistics for ir graph %ld...\n", get_irg_graph_nr(irg));

	for (i=0; i <= _ext_grs_max_opcode; i++)
		for (j=0; j <= _ext_grs_max_modecode; j++)
			for (k=0; k <= _ext_grs_max_opcode; k++)
				for (l=0; l <= _ext_grs_max_modecode; l++) {
					x = _get_n_v_structures(vs_stat,i,j,k,l,ext_grs_in);
					if (x > (double)0) {

						int occ = _get_n_occurence(vs_stat, i, j, k, l, ext_grs_in);
						int n_inst = 0;
						double acc_div_occ = 0.0;
						double acc_div_inst = 0.0;

						/* compute the number of instances present for the
						 * func node (including all sub ops) */
						for (r = 0; r < ARR_LEN(_ext_grs_all_sub_ops_of_op[i]); r++) {
							ir_opcode sub_opc = get_op_code(_ext_grs_all_sub_ops_of_op[i][r]);
							n_inst += _ext_grs_N_INSTANCES(pr_g, sub_opc, j);
						}

						if (occ > 0)
							acc_div_occ = x / occ;

						if (n_inst > 0)
							acc_div_inst = x / (double)n_inst;

						printf("# (%s, %s) -in-> (%s, %s): acc, occ, n_inst, acc/n_inst\n",
							get_op_name(_ext_grs_op_map[i]),
							get_mode_name(_ext_grs_mode_map[j]),
							get_op_name(_ext_grs_op_map[k]),
							get_mode_name(_ext_grs_mode_map[l])
						);
						printf("(%d,%d,%d,%d,in): %lf, %d, %d, %lf\n",
							i,j,k,l,
							x, occ, n_inst,
							acc_div_inst
						);
					}
					x = _get_n_v_structures(vs_stat,i,j,k,l,ext_grs_out);
					if (x > (double)0) {

						int occ = _get_n_occurence(vs_stat, i, j, k, l, ext_grs_out);
						int n_inst = 0;
						double acc_div_occ = 0.0;
						double acc_div_inst = 0.0;

						/* compute the number of instnaces present for the
						 * func node (including all sub ops) */
						for (r = 0; r < ARR_LEN(_ext_grs_all_sub_ops_of_op[i]); r++) {
							ir_opcode sub_opc = get_op_code(_ext_grs_all_sub_ops_of_op[i][r]);
							n_inst += _ext_grs_N_INSTANCES(pr_g, sub_opc, j);
						}

						if (occ > 0)
							acc_div_occ = x / occ;

						if (n_inst > 0)
							acc_div_inst = x / (double)n_inst;

						printf("# (%s, %s) <-out- (%s, %s): acc, occ, n_inst, acc/n_inst\n",
							get_op_name(_ext_grs_op_map[i]),
							get_mode_name(_ext_grs_mode_map[j]),
							get_op_name(_ext_grs_op_map[k]),
							get_mode_name(_ext_grs_mode_map[l])
						);
						printf("(%d,%d,%d,%d,out): %lf, %d, %d, %lf\n",
							i,j,k,l,
							x, occ, n_inst,
							acc_div_inst
						);
					}
				}

	for (i=0; i <= _ext_grs_max_opcode; i++)
		for (j=0; j <= _ext_grs_max_modecode; j++) {
			int n_inst = 0;
			const char *opname;
			const char *modename;

			for (r = 0; r < ARR_LEN(_ext_grs_all_sub_ops_of_op[i]); r++) {
				ir_opcode sub_opc = get_op_code(_ext_grs_all_sub_ops_of_op[i][r]);
				n_inst += _ext_grs_N_INSTANCES(pr_g, sub_opc, j);
			}

			if (_ext_grs_op_map[i] != NULL)
				opname = get_op_name(_ext_grs_op_map[i]);
			else opname = NULL;

			if (_ext_grs_mode_map[j] != NULL)
				modename = get_mode_name(_ext_grs_mode_map[j]);
			else modename = NULL;

			if (opname == NULL) opname = "dummy";
			if (modename == NULL) modename = "dummy";

			if (n_inst != 0 || _ext_grs_N_INSTANCES(pr_g, i,j) != 0)
				printf("Number of instances (%s, %s): direct = %d, with sub ops = %d\n",
					opname, modename, _ext_grs_N_INSTANCES(pr_g, i,j), n_inst);
		}


	printf(" done\n");
}









static ext_grs_analyzer_t vs_analyzer;


/** yields an analyzer performing a v-structure statisitc */
ext_grs_analyzer_t *ext_grs_get_vs_analyzer(void)
{
	memset(&vs_analyzer, 0, sizeof(vs_analyzer));
	vs_analyzer.tag = "vs_analyzer";
	vs_analyzer.dump_ana_result = dump_ana_result;
	vs_analyzer.analyze = analyze;
	vs_analyzer.free_ana_result = free_ana_result;

	init(&vs_analyzer);
	return &vs_analyzer;
}
