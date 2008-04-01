/************************************************************************
* Program:		Search_tree.c
* Project:		SIMD optimization
* Function:		Provides functions to build a search tree to
*				select the patterns for rewrite
* Author:		Andreas Schoesser
* Date:			2007-07-05
************************************************************************/

#ifndef __SEARCH_TREE_T_H__
#define __SEARCH_TREE_T_H__

#include "plist.h"
#include "obstack.h"

#include "grs/grs.h"
#include "grs/match_t.h"
#include "grs/matchplan.h"
#include "grs/matchplan_t.h"

#include "simd_opt_t.h"

ext_grs_match_plan_t *mplans;

#define THIS_MATCHS_COSTSAVINGS 3


/** Entry of a ignore_match list */
typedef struct
{
	pmap			*nodes;
	//ir_edge_t		**edges;
} ignored_match_t;


/** A node of the search tree */
typedef struct search_tree_node_t
{
	ext_grs_match_t		*match;
	int					which_match;
	struct search_tree_node_t	*with;
	struct search_tree_node_t	*without;
	int							cost_savings;
	int							this_matchs_costsavings;
	int							rule_nr;
	struct pmap					*memory_preds;
} search_tree_node_t;

/** Information to delete vproj nodes again */
typedef struct
{
	ir_node *vproj_node;
	ir_node *org_result;
} undo_vproj_t;

/** Environment passed while building the search tree recursively */
typedef struct
{
	ir_graph				*irg;
	int						num_rules;
	ext_grs_match_plan_t	**m_plans;
	int						*priorities;
	ext_grs_action_t        **rules;
	struct pmap				*which_match_vproj;
	struct pmap				*replaced_ptrs;
	char					**firmnode_names;
	struct obstack			*obst;
	statistics_t			*statistics;
	int						*cost_savings;
} search_tree_env_t;

/** To be filled before replacement */
typedef struct
{
	ir_graph			*irg;
	struct pmap			*keep_nodes;
	struct pmap			*vec_operations;
	struct pmap			*which_match_vproj;
	statistics_t		*statistics;
	ext_grs_action_t    **rules;
	char				**firmnode_names;
	struct obstack		*obst;
} replace_st_env_t;

typedef struct
{
	ext_grs_match_t *match;
	int				which;
	int				aiid;
} which_match_vproj_t;


// Public
search_tree_node_t *build_search_tree(search_tree_env_t *search_tree_env, int prio_start);//, int struct pmap *vec_operations, struct pmap *keep_nodes, int start, statistics_t *statistics, struct pmap *replaced_ptrs)
void replace_search_tree(search_tree_node_t *root, replace_st_env_t *rep_env);
void destroy_search_tree(search_tree_node_t *root);

// Private
static search_tree_node_t *rec_search_patterns(search_tree_env_t *st_env, int prio, plist_t **ignored_matches);
static plist_element_t *add_ignored_match(ext_grs_match_t *match, int which, plist_t **ignored_matches, int rule_nr);
static void delete_ignored_match(plist_t **ignored_matches, plist_element_t *match_to_delete, int rule_nr);
static int ignore_match(ext_grs_match_t *match, int which, plist_t **ignored_matches, int rule_nr);
static plist_t *construct_vproj(search_tree_env_t *st_env, search_tree_node_t *st_node, int rule_nr);
static void deconstruct_vproj(plist_t *undo_vproj_info);
static void insert_which_match_vproj(search_tree_env_t *st_env, ir_node *vproj, ext_grs_match_t *match, int which, int aiid);
static void manipulate_match(replace_st_env_t *rep_env, ext_grs_match_t *match, int which);

void dump_search_tree(search_tree_node_t *root, char *file, char *graph_name, search_tree_env_t *st_env);
int rec_dump_search_tree(search_tree_node_t *node, FILE *fp, int this_node_nr, search_tree_env_t *st_env, int best_cost);

void insert_inner_nodes(search_tree_env_t *st_env, search_tree_node_t *st_node);
void remove_inner_nodes(search_tree_env_t *st_env, search_tree_node_t *st_node);

void set_complex_operation_block(ir_graph *irg, ext_grs_match_t *match, int which);

#endif
