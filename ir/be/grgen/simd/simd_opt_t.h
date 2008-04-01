/*******************************************************************************************
* Program:  simd_opt_t.h
* Function: Part of the simd optimization. Searches match of complex operation pattern and
*           inserts the complex operation if possible.
* Author:   Andreas Schoesser
* Date:     05.02.2007
*******************************************************************************************/

#ifndef __SIMD_OPT_H__
#define __SIMD_OPT_H__

#include "simd_presets.h"
#include "simd_opt.h"

#include "pmap.h"
#include "irdump.h"
#include "iroptimize.h"
#include "irgmod.h"
#include "../ir/be/benode_t.h"

#include "grs/grs.h"


#include <time.h>
#include <grs/action_t.h>
#include <grs/match_t.h>

typedef struct
{
	struct list_head list;
	ir_node *node;
} nodes_stack_t;

typedef struct
{
	struct list_head *nodes_stack;
	struct pmap		 *pattern_nodes;
	struct pmap		 *memory_preds;
	ir_graph		 *irg;
} mem_preds_info_t;

typedef struct
{
	int		num_instructions_inserted;
	int     num_matches;
	int     num_matched_nodes;
	int     num_replaced_nodes;
	int		*rewritten_patterns;
	clock_t start_time, end_time;
} statistics_t;

typedef struct
{
	struct pmap *node_map_hash;
	int			found_dependency;
} argdep_ana_info_t;



struct pmap *find_memory_preds(ir_graph *irg, ir_node **match_node_map, int max_node_aiid);
static void find_mem_preds_pre(ir_node *n, void * env);
static void find_mem_preds_post(ir_node *n, void * env);
static ir_node *find_memory_pred(ir_node *n, int *edge_nr, int start_edge);
static int walk_mem_edge_alias(ir_node *orgn, ir_node *cn, struct pmap *memory_preds, struct pmap *node_map_hash, struct pmap *replaced_ptrs);
int test_alias_relation(ir_node **node_map, int max_node_aiid, struct pmap *memory_preds, struct pmap *replaced_ptrs);
struct pmap *generate_nodemap_hash(ir_node **node_map, int max_node_aiid);

static int find_bad_node(ir_node *n);
void connect_complex_memory(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *mem_preds);
void save_complex_operation(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *vec_operations, int nr);
void save_keep_nodes(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *keep_nodes);
static void rename_complex_operations(struct pmap *vec_operations);
static void visit_mem_nodes(ir_node *node, mem_preds_info_t *mem_preds_info);
static void exchange_keep_nodes(struct pmap *keep_nodes);
int perform_rewrite(ir_graph *irg, struct pmap *vec_operations, struct pmap *keep_nodes, int start, statistics_t *statistics, struct pmap *replaced_ptrs);
static void view_statistics(statistics_t *statistics);
int analyze_arguments(ext_grs_action_t *action, ext_grs_match_t *match, int which, ir_graph *irg, struct pmap *memory_preds);
int search_mem_node(ir_node *n, ir_node *start, ir_graph *irg);
void compute_match_plans(ir_graph *irg, ext_grs_planer_t *planer, ext_grs_analyzer_t *analyzer);
static void walk_arg_dep(ir_node *n, void * env);
int ext_result_costs(ext_grs_match_t *match, int which);
int single_node_costs(ir_node *n);

#endif
