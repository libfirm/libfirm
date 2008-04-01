#ifndef _EXT_GRS_ACTION_T_H_
#define _EXT_GRS_ACTION_T_H_

/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/action_t.h
 * Purpose:     provides an internal interface for the creation of actions
 * 				(i.e. rewriting rules and patterns to be found without
 *              performing a rewriting step)
 * Author:      Veit Batz
 * Created:		29. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#include <libcore/lc_bitset.h>

#include "common_t.h"
#include "action.h"






/** the kind of a graph elem (node or edge) */
typedef enum {
	/** a graph node */
	ext_grs_k_node,
	/** a graph edge */
	ext_grs_k_edge,
	/** a pseudo node  */
	ext_grs_k_pseudo_node,
	/** a pseudo edge  */
	ext_grs_k_pseudo_edge
} ext_grs_elem_kind_t;


typedef double ext_grs_edge_cost_t;



/** represents a node in a grs rule */
struct _ext_grs_node_t
{
	ext_grs_elem_kind_t kind; /** mus be ext_grs_k_node or ext_grs_k_pseudo_node */

	/* this nodes graph */
	ext_grs_graph_t *graph;

	char *name; /**< the name of this node */
	/** This nodes action internal id. This id is also used
	 *  as index for match storage. */
	int aiid;
	/** this nodes ir op */
	ir_op *op;
	/** this nodes ir mode */
	ir_mode *mode;
	/** arrays of incoming and outgoing edges */
	lc_list_t edges[2];
	/** represents the memberships in the graphs node list  */
	lc_list_t node_list;
	/** a function representing a condition regarding the op of a firm
	 *  node which shall be matched with this (pattern or negative) node */
	ext_grs_op_condition_func_t op_condition;
	/** a function representing a condition regarding the mode of a firm
	 *  node which shall be matched with this (pattern or negative) node */
	ext_grs_mode_condition_func_t mode_condition;
	/** number of incoming replace edges not associated with a pattern
	 *  edge and having no specified position, these edges will be appended
	 *  to the exisiting in array in the rwrite step (only used in replacement
	 *  graphs) */
	int n_append_edges;

	/** index of the matcher op by which this node is first matched */
	int first_mop_index;

	/** some auxilary data needed to maintain a list of not yet processed nodes
	 * (see impl of Edmonds alogrithm in vs search planing) */
	lc_list_t not_visited_list;

	ext_grs_node_t *related_node;
};

typedef struct {
	ext_grs_elem_kind_t kind;
	union { ext_grs_node_t *n; struct _ext_grs_edge_t *e;} val;
} ext_grs_elem_t;

/** represents an edge in a grs rule */
struct _ext_grs_edge_t
{
	ext_grs_elem_kind_t kind; /** mus be ext_grs_k_edge or ext_grs_k_pseudo_edge */

	/* this edges graph */
	ext_grs_graph_t *graph;

	char *name; /**< the name of this edge */
	/** This edges action internal id. This id is also used
	 *  as index for match storage. */
	int aiid;
	/** this edges position as an argument of its source node */
	int pos;
	/** this edges argument node, i.e. the node the result of which is
	 * processed by the function node */
	ext_grs_node_t *arg;
	/** this edges function node, i.e. the node using the argument for
	 *  further computations */
	ext_grs_node_t *func;
	/** if edge is a pseudo edge it represents follwing an edge against
	 *  its direction or the matching of a start node. Then this member
	 *  is a ptr to the edge or node represented */
	ext_grs_elem_t genuine_elem;
	/** for actual (non pseudo) edges: points to the pseudo edge
	 * representing the backward traversal of this edge */
	ext_grs_edge_t *backward_edge;
	/** for replacement edges, which have to be newly inserted, this function
	 *  computes the position the edge is inserted with at its func-node */
	ext_grs_edge_pos_func_t position_func;
	/* represents the membership in the graphs edge list
	 * (not for pseudo edges) */
	lc_list_t edge_list;
	/** represents the membership in this edge src- and tgt-nodes
	 * in and out lists (not for pseudo edges)  */
	lc_list_t list[2];
	/** index of the matcher op by which this edge is first matched */
	int first_mop_index;


	/** A field needed in the planing process:
	 *  represents this edges mambership in a
	 *  meldable priority queue of edges */
	lc_list_t mpq_list;
	/** A field needed in the planing process:
	 *  represents this edges cost in this actions cost graph during the planing process */
	ext_grs_edge_cost_t cost;

	ext_grs_edge_t *related_edge;
};

typedef enum {
	ext_grs_k_pattern,
	ext_grs_k_negative,
	ext_grs_k_replacement
} ext_grs_graph_kind_t;

/** represents a pattern graph to be matched or inserted or
 *  a negetive pattern graph */
struct _ext_grs_graph_t {
	ext_grs_graph_kind_t kind;
	/** the action this pattern graph belongs to */
	ext_grs_action_t *action;
	/** number of this graphs nodes */
	int n_nodes;
	 /** number of this graphs edges */
	int n_edges;
	/** number of this graphs conditions */
	int n_conditions;
	/**this graphs nodes */
	lc_list_t nodes;
	/**this graphs edges */
	lc_list_t edges;
	/** if this graph is a negative pattern this field represents the
	 *  membership in this graphs actions list of negative patterns */
	lc_list_t negative_list;
	/** A pseudo node needed for the computation of matching plans.
	 *  all edges incident to this node are pseudo edges representing
	 *  some kind of cost used in match planning */
	ext_grs_node_t root_node;
};


/** An action the grs can perform. Possible kinds of actions are rules
 * and tests. A rule describes the replacement of pattern graphs by a
 * replacement graph, a test only describes the lookup for occurences of
 * the pattern graphs, where it is not intended to perform a replacement */
struct _ext_grs_action_t
{
	/** this actions kind (e.g. rule or test)*/
	ext_grs_action_kind_t kind;
	/** this actions id */
	int id;
	/** this actions name */
	char *name;
	/** flag: tells wether this action is mature or not. No changes
	 *  can be performed on the pattern, replacement or negative
	 *  patterns of a mature action */
	int mature;


	/* information concerning the matching */

	/** the pattern graph */
	ext_grs_graph_t *pattern;
	/** list of all negative patterns if this action */
	lc_list_t negatives;
	/** overall number of real nodes of this action without pseudo and negative nodes */
	int n_nodes;
	/** overall number of real edges of this action without pseudo and negative edges */
	int n_edges;
	/** overall number of negative nodes of this action */
	int n_neg_nodes;
	/** overall number of negative edges of this action */
	int n_neg_edges;
	/** overall number of pseudo nodes of this action excluding the root node */
	int n_pseudo_nodes;
	/** overall number of pseudo edges of this action */
	int n_pseudo_edges;

	/** overall number of conditions registered with this action
	*  (without op and mode conditions) */
	int n_conditions;
	/** the conditions registered with this action (without op and mode conditions) */
	ext_grs_condition_func_t *conditions;
	/** the aiids of the nodes involved in the condition with the given index */
	lc_bitset_t **conds_involved_nodes;
	/** the aiids of the edges involved in the condition with the given index */
	lc_bitset_t **conds_involved_edges;
	/** All conditions of this action stored as list of condition descriptors.
	*  Only needed when the action is not yet mature. */
	lc_list_t listed_conditions;

	/** maximum (pattern or negative) node aiid present */
	int max_node_aiid;
	/** maximum (pattern or negative) edge aiid present */
	int max_edge_aiid;
	/** all (positive or negative) nodes of
	 *  this action (no replacement nodes) */
	ext_grs_node_t **nodes;
	/** the indices of the conditions refering the node of a given aiid */
	lc_bitset_t **nodes_refering_conds;
	/** all (positive or negative) edges of this action
	 *  (no replacement edges) */
	ext_grs_edge_t **edges;
	/** the indices of the conditions refering the edge of a given aiid */
	lc_bitset_t **edges_refering_conds;

	/** tells wether two nodes are allowed to be matched homomorphic.<br>
	 *  Usage: <code>act->hom[node1_id][mode2_id]</code> */
	int **hom;

	/** tells wether a positive and a negative node are related
	 *  Usage: <code>act->neg_node_relate[pat_node][neg_node]d</code>
	int *neg_node_d;*/

	/** tells wether a positive and a negative edge are related
	 * Usage: <code>act->neg_edge_relate[pat_edge][neg_edge]d</code>
	int *neg_edge_related;*/

	/** tells wether a node is allowed for multiple matching when
	 *  parallel rule application is intended
	 *  Usage: <code>act->critical[node_id]</code> */
	int *node_critical;

	/** tells wether an edge is allowed for multiple matching when
	 *  parallel rule application is intended
	 *  Usage: <code>act->critical[edge_id]</code> */
	int *edge_critical;



	/* information concerning the replacement */

	/** the replacament graph (if action is a rule) */
	ext_grs_graph_t *replacement;

	/** if pattern node is to be kept
	 * 	pattern_node_kept[pattern_node_index]
	 *  yields the replacement aiid of the corresponding
	 *  replace node and -1 otherwise */
	int *pattern_node_kept;
	/** if pattern edge is to be kept
	 * 	pattern_node_kept[pattern_edge_index]
	 *  yields the replacement aiid of the corresponding
	 *  replace edge and -1 otherwise */
	int *pattern_edge_kept;

	/** number of nodes of the replacement graph */
	int n_replacement_nodes;
	/** The nodes of the replacement graph. The index is the nodes aiid, which is
	 *  unique ONLY for replacement nodes */
	ext_grs_node_t **replacement_nodes;
	/** number of edges of the replacement graph */
	int n_replacement_edges;
	/** the edges of the rplacement graph, index is the nodes aiid, which is
	 *  unique ONLY for replacement nodes */
	ext_grs_edge_t **replacement_edges;
	/** Tells wether a replacement node is to be newly inserted or kept:
	 *  -1 means to be inserted, >=0 means kept. If the node is kept,
	 *  the value equals the aiid of the corresponding pattern node.
	 *  USAGE: replacement_node_kept[replace_node_aiid] */
	int *replacement_node_kept;
	/** Tells wether a replacement edge is to be newly inserted or kept:
	 *  -1 means to be inserted, >=0 means kept. If the edge is kept,
	 *  the value equals the aiid of the corresponding pattern edge.
	 *  USAGE: replacement_edge_kept[replace_edge_aiid] */
	int *replacement_edge_kept;
	/** maximum replacement node aiid present */
	int max_repl_node_aiid;
	/** maximum replacement edge aiid present */
	int max_repl_edge_aiid;
	/** the list of functions to be evaluated directly after each rewrite step,
	 *  the evaluation is done in the order, the functions are stored
	 *  in this list */
	lc_list_t listed_evals;

	/** Stores pairs of nodes and edges associated to be kept nodes/edge.
	 *  Only needed when the action is not yet mature. */
	lc_pset *kept_elems;
};


/** A condition descriptor. Describes which (pattern or negative) nodes
 *  and/or edfes are involved in the condition represented by the given
 *  condition function */
typedef struct {
	/** the action this condition belongs to */
	struct _ext_grs_graph_t *graph;
	/** the function representing the condition */
	ext_grs_condition_func_t condition;
	/** the number of involved nodes */
	int n_nodes;
	/** the involved nodes */
	ext_grs_node_t **nodes;
	/** the number of involved edges */
	int n_edges;
	/** the involved edges */
	ext_grs_edge_t **edges;
	/** represents the list of onditions of an action */
	lc_list_t condition_list;
} ext_grs_cond_descr_t;

/** An eval descriptor. Respresents an eval function in
 *  an actions list of evals */
typedef struct {
	/** the eval-in function */
	ext_grs_eval_in_func_t eval_in;
	/** the eval-out function */
	ext_grs_eval_out_func_t eval_out;
	/** represents the list of evals of an action */
	lc_list_t eval_list;
	/** some auxilary data used when finishing a match */
	void *data;
} ext_grs_eval_descr_t;

/** Initialize the action part of the exr/grs module of the libfirm.
 *  This is an internal function. */
void _ext_grs_act_init(void);

/** Finalize the action part of the exr/grs module of the libfirm.
 *  This is an internal function. */
void _ext_grs_act_finalize(void);



#endif /* _EXT_GRS_ACTION_T_H_ */
