#ifndef _EXT_GRS_ACTION_H_
#define _EXT_GRS_ACTION_H_

/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/action.h
 * Purpose:     provides an interface for the creation of actions
 * 				(i.e. rewriting rules and patterns to be found without
 *              performing a rewriting step)
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		29. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */



/** represents a node in a grs action */
typedef struct _ext_grs_node_t ext_grs_node_t;

/** represents an edge in a grs action */
typedef struct _ext_grs_edge_t ext_grs_edge_t;

/** represents a pattern, negative or replacement graph */
typedef struct _ext_grs_graph_t ext_grs_graph_t;

/** An action the grs can perform. Possible kinds of actions are rules
 * and tests. A rule describes the replacement of pattern graphs by a
 * replacement graph, a test only describes the lookup for occurences of
 * the pattern graphs, where it is not intended to perform a replacement */
typedef struct _ext_grs_action_t ext_grs_action_t;




/** the kind of a grs action */
typedef enum
{
	ext_grs_k_test, /**< a "test" represents a lookup for a given pattern */
	ext_grs_k_rule /**< a "rule" represents a graph rewriting step */
}
ext_grs_action_kind_t;

/** A condition function to be invoked by the matcher. If the condition
 *  represented by the function does not hold, the current potential match
 *  will be rejected by the matcher.
 *
 *  @param node_map	the current node map during the matching process,
 * 					maps the user defined node ids to host graph nodes
 *  @param edge_map	the current edge map during the matching process,
 * 					maps the user defined edge ids to host graph edges
 *
 *  @return Return non zero if the condition holds and zero if it does not.
 *  @note The functions ext_grs_get_pattern_nodes_image() and
 *        ext_grs_get_pattern_edges_image() provide the images
 *        of pattern nodes and edges under the current match. */
typedef int (* ext_grs_condition_func_t)
	(ir_node **node_map, const ir_edge_t **edge_map);

/** A function ptr to be invoked on inserting new edges during the
 *  rewrite step. The function pointed to is supposed to compute the
 *  position of the newly inserted edge.
 *
 *  @param edge		the replacement edge, the position of which is computed
 *  @param node_map The node map of the match, which is used in the
 * 					current rewrite.
 *  @param edge_map	The edge map of the match, which is used in the
 * 					current	rewrite.
 *
 *  @return The position of a newly inserted edge, i.e., a non negative
 * 		integer or ext_grs_NO_EDGE_POS. If the result equals
 * 		ext_grs_NO_EDGE_POS the edge is appended to the exisiting edge
 * 		array.
 *
 *  @note The functions ext_grs_get_pattern_nodes_image() and
 *        ext_grs_get_pattern_edges_image() provide the images
 *        of pattern nodes and edges under the current match. */
typedef int (* ext_grs_edge_pos_func_t)
	(ext_grs_edge_t *edge, ir_node **node_map, const ir_edge_t **edge_map);

/**
 * A condition function to be invoked by the matcher. If the function
 * representing the condition does not hold for the given op, the matching
 * of a node with this op is prohibited
 *
 * @param op	the op to be checked
 * @return nonzero if the condition holds, zero otherwise
 * */
typedef int (* ext_grs_op_condition_func_t) (ir_op *op);

/**
 * A condition function to be invoked by the matcher. If the function
 * representing the condition does not hold for the given mode, the matching
 * of a node with this mode is prohibited
 *
 * @param mode	the mode to be checked
 * @return nonzero if the condition holds, zero otherwise
 * */
typedef int (* ext_grs_mode_condition_func_t) (ir_mode *mode);

/**
 * A function representing the in-part of a computation, which
 * has to be evaluated BEFORE performing a graph rewrite step. Every
 * such function is associated with a function of type
 * ext_grs_eval_out_func_t representing the out-part of the computation.
 *
 *  @param pat_node_map	the node map of the match, the rewriting is performed with.
 *  @param pat_edge_map	the edge map of the match, the rewriting is performed with.
 *
 * 	@returns	A ptr to some user defined data. The ptr returned will
 * 				passed as input data to the respective eval-out function.
 */
typedef void* (* ext_grs_eval_in_func_t) (
	ir_node **pat_node_map, const ir_edge_t **pat_edge_map);

/**
 * A function representing the out-part of a computation, which
 * has to be evaluated AFTER performing a graph rewrite step. Every
 * such function is associated with a function of type
 * ext_grs_eval_in_func_t representing the in-part of the computation.
 *
 *  @param rpl_node_map		the node map of the match, the rewriting is performed with.
 *  @param rpl_edge_map		the edge map of the match, the rewriting is performed with.
 *  @param pat_node_map		the node map of the match, the rewriting is performed with.
 *  @param data				the pointer of user defined data returned by the respective
 * 							eval-in function
 */
typedef void (* ext_grs_eval_out_func_t) (
	ir_node **rpl_node_map, const ir_edge_t **rpl_edge_map,
	ir_node **pat_node_map, void *data);



/** an invalid node or edge index, this is needed for negative
 *  nodes and edges which are NOT matched */
#define ext_grs_EDGE_NODE_INDEX_INVALID -99
/** an invalid edge position */
#define ext_grs_NO_EDGE_POS -99





/** Create a new graph action of the given kind. */
ext_grs_action_t *ext_grs_new_action(ext_grs_action_kind_t kind, char *name);

/** Get the kind of an action. */
ext_grs_action_kind_t ext_grs_act_get_kind(ext_grs_action_t *act);

/** Impose a new negative graph pattern on the given action.
 *  @note The action must NOT be mature. */
ext_grs_graph_t *ext_grs_act_impose_negative(ext_grs_action_t *action);

/** Get the pattern graph of an action. */
ext_grs_graph_t *ext_grs_act_get_pattern(ext_grs_action_t *action);

/** Get the replacement graph of a rule action. If the action is not
 *  of kind rule NULL is returned instead.
 */
ext_grs_graph_t *ext_grs_act_get_replacement(ext_grs_action_t *action);

/** add a new node to a given pattern graph of an unmature action
 *  @param graph	the pattern, negative or replacement graph the node is added to
 *  @param name		the new nodes name
 *  @param op		the new nodes op
 *  @param mode		the new nodes mode
 *  @param id		the id of the new node
 *
 *  @returns		the new node
 *
 *  @note 	the id has to be unique over the hole set of nodes of the pattern
 * 			and all negative graphs of an action, and unique for the set of nodes
 * 			of the replacement graph of an action.
 */
ext_grs_node_t *ext_grs_act_add_node(ext_grs_graph_t *graph,
	char *name, ir_op *op, ir_mode *mode, int id);

/** Add a new node to a negative pattern and relate it with a positive node */
ext_grs_node_t *ext_grs_act_add_related_node(ext_grs_graph_t *graph,
	char *name, ir_op *op, ir_mode *mode, int id, ext_grs_node_t *positive_node);

/** Add a new node to the replacement pattern and announce to keep it in the host graph */
ext_grs_node_t *ext_grs_act_add_node_to_keep(ext_grs_graph_t *graph,
											 char *name, ir_op *op, ir_mode *mode, int id, ext_grs_node_t *pattern_node);

/** Add a new edge to a given pattern graph of an UNmature action.
 *  @param graph	the pattern, negative or replacement graph the node is added to
 *  @param name		the new nodes name
 *  @param arg		the new edges target node
 *  @param func		the new edges source node
 *  @param id		the id of the new edge
 *
 *  @returns		the new edge
 *
 *  @note 	the id has to be unique over the hole set of edges of the pattern
 * 			and all negative graphs of an action, and unique for the set of edges
 * 			of the replacement graph of an action.
 */
ext_grs_edge_t *ext_grs_act_add_edge(ext_grs_graph_t *graph,
	char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id);

/** Add a new edge to a negative pattern and relate it with a positive edge */
ext_grs_edge_t *ext_grs_act_add_related_edge(ext_grs_graph_t *graph,
							 char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id, ext_grs_edge_t *positive_edge);

/** Add a new edge to the replacement pattern and announce to keep it in the host graph */
ext_grs_edge_t *ext_grs_act_add_edge_to_keep(ext_grs_graph_t *graph,
											 char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id, ext_grs_edge_t *pattern_edge);

/** Mature an action. It is not possible to make any
 *  changes to this actions pattern afterwards (except allowing
 *  the homomorphic matching of nodes).
 *  @returns nonzero if succesful */
int ext_grs_act_mature(ext_grs_action_t *action);

/** Get a pattern node by its name. */
ext_grs_node_t *ext_grs_act_get_node(ext_grs_action_t *action, char *name);

/** Get a pattern node aiid by its name. */
int ext_grs_act_get_node_aiid(ext_grs_action_t *action, char *name);
int ext_grs_act_get_node_aiid_n(ext_grs_action_t *action, char *name, int n);

/** Get a pattern edge by its name. */
ext_grs_edge_t *ext_grs_act_get_edge(ext_grs_action_t *action, char *name);

/** Allow homomorphic matching of two pattern nodes,
 *  note that these two nodes MUST have the same op and mode
 *  (including mode ANY) and belong tp the same acion
 *  @returns zero if fails and nonzero otherwise
 *  @note the action the given nodes belong to has to be mature */
int ext_grs_act_allow_nodes_hom(ext_grs_node_t *n1, ext_grs_node_t *n2);

/** Assocites a pattern node and a replacement node of a given rule-action
 *  to be a node which is kept in the rewrting step.
 *
 *  @param e1	the pattern edge
 *  @param e2	the replacement edge
 *
 *  @return zero if an error occured, nonzero otherwise
 */
int ext_grs_act_announce_kept_node(ext_grs_node_t *n1, ext_grs_node_t *n2);

/** Assocites a pattern edge and a replacement edge of a given rule-action
 *  to be an edge which is kept in the rewriting step. The two edges must
 *  have the same position or both no position at all. Otherwise an error
 *  will be reported.
 *
 *  @param e1	the pattern edge
 *  @param e2	the replacement edge
 *
 *  @return zero if an error occurred, nonzero otherwise
 *
 *  @note if the replacement contains an edge without a valid position which is
 *    NOT associated with a pattern edge, an error will occur while maturing the
 *    respective action
 */
int ext_grs_act_announce_kept_edge(ext_grs_edge_t *e1, ext_grs_edge_t *e2);

/** relate a negative node with a pattern node
 *
 *  @param pat_node	the pattern node
 *  @param neg_node	the negative node
 *
 *  @return zero if an error occured, nonzero otherwise
 *
 *  @note	The pattern and the negative node must belong to the same action.
 * 			Furthermore, the action has to be already mature.
 */
int ext_grs_act_relate_neg_node(ext_grs_node_t *pat_node, ext_grs_node_t *neg_node);

/** relate a negative edge with a pattern edge
 *
 *  @param pat_edge	the pattern node
 *  @param neg_edge	the negative node
 *
 *  @return zero if an error occured, nonzero otherwise
 *
 *  @note 	The pattern and the negative edge must belong to the same action.
 * 			Furthermore, the action has to be already mature.
 *
 */
int ext_grs_act_relate_neg_edge(ext_grs_edge_t *pat_edge, ext_grs_edge_t *neg_edge);

/** Sets the position function of a replacement edge. After rewriting the
 *  host edge corresponding to that replace edge gets the position computed
 *  by the position function.
 *
 *  @param edge	the replacement edge
 *  @param func	the function computing the edge pos
 *
 *  @return zero if an error occured, nonzero otherwise
 *
 *  @note	Fails if the given edge is not a replace edge or the
 * 			the action the given edge belongs to is already mature.
 */
int ext_grs_act_set_pos_func(ext_grs_edge_t *edge, ext_grs_edge_pos_func_t func);

/** Sets the op condition function of a (pattern or negative) node.
 *  The function checks wether nodes of certain subops may be matched
 *  with the given (pattern or negative) node or not
 *
 *  @param node	a (pattern or negative) node
 *  @param func	the op condition function
 *
 *  @return zero if an error occured, nonzero otherwise
 */
int ext_grs_act_set_op_condition(
		ext_grs_node_t *node, ext_grs_op_condition_func_t func);

/** Registers a mode condition function of a (pattern or negative) node.
 *  The function checks wether nodes of certain modes may be matched
 *  with the given (pattern or negative) node or not
 *
 *  @param node	a (pattern or negative) node
 *  @param func	the mode condition function
 *
 *  @return zero if an error occured, nonzero otherwise
 */
int ext_grs_act_set_mode_condition_func(
		ext_grs_node_t *node, ext_grs_mode_condition_func_t func);

/** Registers a condition function with some pattern or
 *  negative nodes of an action.
 *
 *  @param func		the function representing the condition
 *  @param graph	the (pattern or negative) graph the given condition
 * 					function is associated with
 *  @param n_nodes	the number of nodes involved in the condition
 *  @param nodes    the array of involved nodes
 *  @param n_edges	the number of edges involved in the condition
 *  @param nodes    the array of involved edges
 *
 *  @return zero if an error occured, nonzero otherwise

 *  @note	The given action must NOT be mature!
 */
int ext_grs_act_register_condition(
		ext_grs_condition_func_t func,
		ext_grs_graph_t *graph,
		int n_nodes, ext_grs_node_t **nodes,
		int n_edges, ext_grs_edge_t **edges);

/** Registers a pair of functions representing a computation,
 *  which shall be evaluated on a rewrite. The in-part is
 *  computed BEFORE the transformation of the host graph,
 *  the out-part is computed AFTER that.
 *
 *  @param action	the action the eval-out function is associated with
 *  @param in_func	the function representing the computations in-part
 *  @param out_func	the function representing the computations out-part
 *
 *  @return zero if an error occurred, nonzero otherwise
 *
 *  @note	The given action must NOT be mature!
 */
int ext_grs_act_register_eval(ext_grs_action_t *action,
		ext_grs_eval_in_func_t in_func, ext_grs_eval_out_func_t out_func);




#endif /*_EXT_GRS_ACTION_H_*/
