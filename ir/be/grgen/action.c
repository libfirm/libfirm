/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/action.c
 * Purpose:     funcytions to construct for libfirm grs actions
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		29. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "common_t.h"
#include "action_t.h"


#define ext_grs_PSEUDO_PREFIX "_pseudo__"
#undef MAX
#define MAX(a,b) (a)>=(b) ? (a) : (b)


/* counter to create action ids from */
static int action_counter = 0;

/* obstack to store actions on */
static struct obstack obst;
/* tells whether the obstack has already been initialized */
static int obst_init = 0;



static int not_injective = 0;
static int multiple_entry = 0;
static int already_exists = 0;


/* a pointer to pointer map elem */
typedef struct {
	/* kind */
	ext_grs_elem_kind_t kind;
	/* the preimage */
	void *pre;
	/* the image */
	void *img;
}
ext_grs_ptr_map_entry_t;

int ptr_map_cmp_func(const void *elt, const void *key) {

	ext_grs_ptr_map_entry_t *entry1 = (ext_grs_ptr_map_entry_t *) elt;
	ext_grs_ptr_map_entry_t *entry2 = (ext_grs_ptr_map_entry_t *) key;

	if (entry1->pre != entry2->pre && entry1->img == entry2->img)
		not_injective = 1;

	if (entry1->pre == entry2->pre && entry1->img != entry2->img)
		multiple_entry = 1;

	if (entry1->pre == entry2->pre && entry1->img == entry2->img)
		already_exists = 1;

	return entry1->pre != entry2->pre;
}




static void init_graph(ext_grs_graph_t *graph,
ext_grs_action_t *act, ext_grs_graph_kind_t kind)
{
	memset(graph, 0, sizeof(*graph));

	graph->kind = kind;
	graph->action = act;
	LC_INIT_LIST_HEAD(& graph->nodes);
	LC_INIT_LIST_HEAD(& graph->edges);
}


/*
 *  Copy all nodes into the action and gen pseudo edges
 *  modeling the explicit matching of a node
 *  PseudoEdge == Edge modelled as a node
 */
static int import_graph_info(ext_grs_action_t *action, ext_grs_graph_t *graph)
{
	int edge_aiid = action->max_edge_aiid;

	lc_list_t *pos, *n;

	assert (action->kind != ext_grs_k_replacement &&
		"import_graph_info() is not meant for calling on replacement graphs");


	/* init the graphs root node */
	graph->root_node.kind = ext_grs_k_pseudo_node;
	graph->root_node.graph = graph;
	LC_INIT_LIST_HEAD(& graph->root_node.edges[ext_grs_in]);
	LC_INIT_LIST_HEAD(& graph->root_node.edges[ext_grs_out]);
	graph->root_node.name = ext_grs_PSEUDO_PREFIX "root_node";

	lc_list_for_each (pos, & graph->nodes) {

		ext_grs_node_t *node = lc_list_entry(pos, ext_grs_node_t, node_list);
		ext_grs_edge_t *pseudo_edge = obstack_alloc(&obst, sizeof(*pseudo_edge));

		memset(pseudo_edge, 0, sizeof(*pseudo_edge));

		/* store the node in the actions node array */
		if (action->nodes[node->aiid] == NULL) {
			action->nodes[node->aiid] = node;
		}
		else {
			printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
			printf("  Multiple assigned node id encountered.\n");
			printf("  You may have added nodes with the same id via ext_grs_act_add_node().\n\n");
			return 0;
		}

		/* for each node setup the pseudo edge modeling the
		 * explicit matching of that node */
		pseudo_edge->graph = graph;
		pseudo_edge->kind = ext_grs_k_pseudo_edge;
		pseudo_edge->aiid = ++edge_aiid;
		pseudo_edge->arg = node;
		pseudo_edge->func = & graph->root_node;
		pseudo_edge->genuine_elem.kind = ext_grs_k_node;
		pseudo_edge->genuine_elem.val.n = node;
		pseudo_edge->name = obstack_alloc(
			& obst,
			(1 + strlen(ext_grs_PSEUDO_PREFIX) + strlen(node->name)) * sizeof(char)
		);
		strcpy(pseudo_edge->name, ext_grs_PSEUDO_PREFIX);
		strcat(pseudo_edge->name, node->name);

		/* add the pseudo edge to the graphs edge list */
		lc_list_add(& pseudo_edge->edge_list, & graph->edges);
		/* add the pseudo edge to the tgt- and src-nodes  in- and out-lists */
		lc_list_add(& pseudo_edge->list[ext_grs_in],
			& pseudo_edge->func->edges[ext_grs_in]);
		lc_list_add(& pseudo_edge->list[ext_grs_out],
			& pseudo_edge->arg->edges[ext_grs_out]);
		/* add the pseudo edge to the action */
		action->edges[edge_aiid] = pseudo_edge;
	}

	/* copy all pattern edges into the action and for every
	 * edge gen a pseudo edge modeling the traversal of the original edge
	 * against its direction */
	lc_list_for_each_safe (pos, n, & graph->edges) {

		ext_grs_edge_t *edge = lc_list_entry(pos, ext_grs_edge_t, edge_list);
		ext_grs_edge_t *pseudo_edge;

		if (edge->kind == ext_grs_k_pseudo_edge)
			continue;

		pseudo_edge = obstack_alloc(&obst, sizeof(*pseudo_edge));
		memset(pseudo_edge, 0, sizeof(*pseudo_edge));

		/* store the node in the actions node array */
		action->edges[edge->aiid] = edge;

		/* for each edge setup the pseudo edge modelling the
		 * against-direction-traversal of that edge  */
		pseudo_edge->graph = edge->graph;
		pseudo_edge->kind = ext_grs_k_pseudo_edge;
		pseudo_edge->aiid = ++edge_aiid;
		pseudo_edge->arg = edge->func;
		pseudo_edge->func = edge->arg;
		pseudo_edge->genuine_elem.kind = ext_grs_k_edge;
		pseudo_edge->genuine_elem.val.e = edge;
		pseudo_edge->name = obstack_alloc(
			& obst,
			(1 + strlen(ext_grs_PSEUDO_PREFIX) + strlen(edge->name)) * sizeof(char)
		);
		strcpy(pseudo_edge->name, ext_grs_PSEUDO_PREFIX);
		strcat(pseudo_edge->name, edge->name);

		/* add the pseudo edge to the graphs edge list */
		lc_list_add(& pseudo_edge->edge_list, & graph->edges);
		/* add the pseudo edge to the tgt- and src-nodes  in- and out-lists */
		lc_list_add(& pseudo_edge->list[ext_grs_in],
			& pseudo_edge->func->edges[ext_grs_in]);
		lc_list_add(& pseudo_edge->list[ext_grs_out],
			& pseudo_edge->arg->edges[ext_grs_out]);
		/* add the pseudo edge to the action */
		action->edges[edge_aiid] = pseudo_edge;
		/* each actual edge knows the respective pseudo edge */
		edge->backward_edge = pseudo_edge;
	}

	action->max_edge_aiid = edge_aiid;

	return 1;
}



static void set_up_condition_stuff(ext_grs_action_t *action) {

	lc_list_t *pos;
	int i;

	/* alloc space for bitsets telling which (pattern or negative) nodes and edges
	 * are involved in which conditions and vice versa wich conditions are refered
	 * by which nodes or edges */
	action->conditions =
		obstack_alloc(&obst, action->n_conditions * sizeof(*action->conditions));
	action->conds_involved_nodes =
		obstack_alloc(&obst, action->n_conditions * sizeof(*action->conds_involved_nodes));
	action->conds_involved_edges =
		obstack_alloc(&obst, action->n_conditions * sizeof(*action->conds_involved_edges));

	action->nodes_refering_conds =
		obstack_alloc(&obst, (action->max_node_aiid+1) * sizeof(*action->nodes_refering_conds));
	action->edges_refering_conds =
		obstack_alloc(&obst, (action->max_edge_aiid+1) * sizeof(*action->edges_refering_conds));

	/* alloc a bitset for every (pattern or negative) node and edge */
	for(i = 0; i <= action->max_node_aiid; i++)
		action->nodes_refering_conds[i] =
			lc_bitset_obstack_alloc(&obst, action->n_conditions);
	for(i = 0; i <= action->max_edge_aiid; i++)
		action->edges_refering_conds[i] =
			lc_bitset_obstack_alloc(&obst, action->n_conditions);

	/* alloc two bitset for every condition (telling which nodes and edges are involved),
	 * and setup the hole condition information */
	i = 0;
	lc_list_for_each(pos, & action->listed_conditions) {

		int j;
		lc_bitset_t *bs;
		ext_grs_cond_descr_t *cond_descr =
			lc_list_entry(pos, ext_grs_cond_descr_t, condition_list);

		action->conditions[i] = cond_descr->condition;

		/* alloc the bitset, which represents the information about the
		 * (pattern or negative) nodes involved in the current condition */
		bs = action->conds_involved_nodes[i] =
				lc_bitset_obstack_alloc(&obst, action->max_node_aiid+1);

		/* store that info in the bitset */
		for (j = 0; j < cond_descr->n_nodes; j++) {
			int aiid = cond_descr->nodes[j]->aiid;
			lc_bitset_set(bs, aiid);
			lc_bitset_set(action->nodes_refering_conds[aiid], i);   // CRASH!!!
		}

		/* alloc the bitset, which represents the information about the
		 * (pattern or negative) edges involved in the current condition */
		bs = action->conds_involved_edges[i] =
				lc_bitset_obstack_alloc(&obst, action->max_edge_aiid+1);

		/* store that info in the bitset */
		for (j = 0; j < cond_descr->n_edges; j++) {
			int aiid = cond_descr->edges[j]->aiid;
			lc_bitset_set(bs, aiid);
			lc_bitset_set(action->edges_refering_conds[aiid], i);
		}

		i++;
	}

	assert(i == action->n_conditions && "inconsistent number of registered condition functions");
}





/** create a new graph action of the given kind */
ext_grs_action_t *ext_grs_new_action(ext_grs_action_kind_t kind, char *name) {

	ext_grs_action_t *action;

	/* allocate a new action */
	action = (ext_grs_action_t *) obstack_alloc(&obst, sizeof(*action));
	if (action == NULL)	{
		printf("module ext/grs: internal ERROR in function ext_grs_act_allow_nodes_hom()\n");
		printf("  Failed to allocate a new action.\n\n");
		return NULL;
	}
	memset(action, 0, sizeof(*action));
	/* set the actions kind and id */
	action->kind = kind;
	action->id = action_counter++;
	action->name = name;
	action->max_node_aiid = -1;
	action->max_edge_aiid = -1;
	action->max_repl_node_aiid = -1;
	action->max_repl_edge_aiid = -1;

	LC_INIT_LIST_HEAD(& action->negatives);
	LC_INIT_LIST_HEAD(& action->listed_conditions);
	LC_INIT_LIST_HEAD(& action->listed_evals);


	/* allocate this actions pattern graph */
	action->pattern = (ext_grs_graph_t *)
		obstack_alloc(&obst, sizeof(*(action->pattern)));
	if (action->pattern == NULL) {
		printf("module ext/grs: internal ERROR in function ext_grs_new_action()\n");
		printf("  Failed to allocate a pattern graph for the new action.\n\n");
		obstack_free(&obst, action);
		return NULL;
	}
	init_graph(action->pattern, action, ext_grs_k_pattern);

	/* if action is a rule, allocate a replacemant graph */
	if (kind == ext_grs_k_rule) {
		action->replacement = (ext_grs_graph_t *)
			obstack_alloc(&obst, sizeof(*(action->replacement)));
		if (action->replacement == NULL) {
			printf("module ext/grs: internal ERROR in function ext_grs_new_action()\n");
			printf("  Failed to allocate a replacement graph for the new action.\n\n");
			obstack_free(&obst, action);
			return NULL;
		}
		init_graph(action->replacement, action, ext_grs_k_replacement);
	}

	action->kept_elems = lc_pset_new(ptr_map_cmp_func, 256);

	return action;
}

/** get the kind of an action */
ext_grs_action_kind_t ext_grs_act_get_kind(ext_grs_action_t *act) {
	return act->kind;
}

/** impose a new negative graph pattern on the given action */
ext_grs_graph_t *ext_grs_act_impose_negative(ext_grs_action_t *action) {

	ext_grs_graph_t *negative;

	/* if action is already mature no further changes are allowed */
	if (action->mature) return NULL;

	/* create new graph */
	negative = (ext_grs_graph_t *) obstack_alloc(&obst, sizeof(*negative));
	if (negative == NULL) {
		printf("module ext/grs: ERROR in function ext_grs_act_impose_negative()\n");
		printf("  failed to allocate a new action!\n");
		return NULL;
	}
	init_graph(negative, action, ext_grs_k_negative);

	/* store the new graph in the given action
	 * as a negative pattern graph */
	lc_list_add(& negative->negative_list, & action->negatives);

	return negative;
}


/** get the pattern graph of an action */
ext_grs_graph_t *ext_grs_act_get_pattern(ext_grs_action_t *action) {
	return action->pattern;
}

/** get the replacement graph of an action */
ext_grs_graph_t *ext_grs_act_get_replacement(ext_grs_action_t *action) {

	if (action->kind == ext_grs_k_rule) {
		assert (action->replacement->kind == ext_grs_k_replacement &&
			"replacement graph has wrong kind");
		return action->replacement;
	}

	/* if action is no rule, no replacement can be returned */
	return NULL;
}

/** add a node to a given pattern graph */
ext_grs_node_t *ext_grs_act_add_node(
	ext_grs_graph_t *graph, char *name, ir_op *op, ir_mode *mode, int id)
{
	ext_grs_node_t *new_node;

	if (!graph) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Given graph was NULL!\n");
		return NULL;
	}
	if (!name) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Given name was NULL!\n");
		return NULL;
	}
	if (!op) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Given op was NULL!\n");
		return NULL;
	}
	if (!mode) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Given mode was NULL!\n");
		return NULL;
	}
	if (id < 0) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Negative node id was given.\n");
		return NULL;
	}
	if (graph->action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_add_node()\n");
		printf("  Given graph belongs to a mature action.\n");
		return NULL;
	}

	new_node = obstack_alloc(&obst, sizeof(*new_node));
	memset(new_node, 0, sizeof(*new_node));

	new_node->kind = ext_grs_k_node;
	new_node->graph = graph;
	new_node->name = name;
	new_node->mode = mode;
	new_node->op = op;
	new_node->aiid = id;
	new_node->related_node = NULL;
	LC_INIT_LIST_HEAD(& new_node->edges[ext_grs_in]);
	LC_INIT_LIST_HEAD(& new_node->edges[ext_grs_out]);

	/* add node to the graph */
	graph->n_nodes++;
	lc_list_add(& new_node->node_list, & graph->nodes);

	/* add node to the graphs action...? */
	/* ...this is done by the the mature function when
	 * pseudo nodes and edges are created! */

	if (graph->kind == ext_grs_k_replacement) {
		graph->action->max_repl_node_aiid = MAX(id, graph->action->max_repl_node_aiid);
	}
	else {
		assert((graph->kind == ext_grs_k_pattern || graph->kind == ext_grs_k_negative)
			&& "given graph has invalid kind");
		graph->action->max_node_aiid = MAX(id, graph->action->max_node_aiid);
	}

	return new_node;
}

ext_grs_node_t *ext_grs_act_add_related_node(ext_grs_graph_t *graph,
		 char *name, ir_op *op, ir_mode *mode, int id, ext_grs_node_t *positive_node)
{
	ext_grs_node_t *added_node = ext_grs_act_add_node(graph, name, op, mode, id);
	ext_grs_act_relate_neg_node(positive_node, added_node);
	return(added_node);
}

ext_grs_node_t *ext_grs_act_add_node_to_keep(ext_grs_graph_t *graph,
											 char *name, ir_op *op, ir_mode *mode, int id, ext_grs_node_t *pattern_node)
{
	ext_grs_node_t *added_node = ext_grs_act_add_node(graph, name, op, mode, id);
	ext_grs_act_announce_kept_node(pattern_node, added_node);
	return(added_node);
}


/** add an edge to a given pattern graph */
ext_grs_edge_t *ext_grs_act_add_edge(ext_grs_graph_t *graph,
	char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id)
{
	ext_grs_edge_t *new_edge;

	if (!graph) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Given graph was NULL.\n\n");
		return NULL;
	}
	if (!name) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Given name was NULL.\n\n");
		return NULL;
	}
	if (!arg) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Given argument node was NULL.\n\n");
		return NULL;
	}
	if (!func) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Given function node was NULL.\n\n");
		return NULL;
	}
	if (id < 0) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Given edge id was negative.\n\n");
		return NULL;
	}
	if (graph->action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_add_edge()\n");
		printf("  Tried to add an edge to a graph of a mature action.\n\n");
		return NULL;
	}

	if (func->op == op_Bad) {
		printf("module ext/grs: WARNING in function ext_grs_add_edge()\n");
		printf("  Added an incoming edge to a Bad node, this leads\n");
		printf("  to a malformed graph.\n\n");
	}


	new_edge = obstack_alloc(&obst, sizeof(*new_edge));
	memset(new_edge, 0, sizeof(*new_edge));

	new_edge->kind = ext_grs_k_edge;
	new_edge->graph = graph;
	new_edge->name = name;
	new_edge->pos = pos;
	new_edge->func = func;
	new_edge->arg = arg;
	new_edge->aiid = id;
	new_edge->related_edge = 0;

	/* add edge to the graph */
	graph->n_edges++;
	lc_list_add(& new_edge->edge_list, & graph->edges);

	/* add edge to tgt- and src-nodes in and out lists */
	lc_list_add(&new_edge->list[ext_grs_in], & new_edge->func->edges[ext_grs_in]);
	lc_list_add(&new_edge->list[ext_grs_out], & new_edge->arg->edges[ext_grs_out]);

	/* add edge to the graphs action...? */
	/* ...this is done by the the mature function when
	 * pseudo nodes and edges are created! */

	if (graph->kind == ext_grs_k_replacement) {
		graph->action->max_repl_edge_aiid = MAX(id, graph->action->max_repl_edge_aiid);
	}
	else {
		assert((graph->kind == ext_grs_k_pattern || graph->kind == ext_grs_k_negative)
			&& "given graph has invalid kind");
		graph->action->max_edge_aiid = MAX(id, graph->action->max_edge_aiid);
	}

	return new_edge;
}

ext_grs_edge_t *ext_grs_act_add_related_edge(ext_grs_graph_t *graph,
							 char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id, ext_grs_edge_t *positive_edge)
{
	ext_grs_edge_t *added_edge = ext_grs_act_add_edge(graph, name, pos, arg, func, id);
	ext_grs_act_relate_neg_edge(positive_edge, added_edge);
	return(added_edge);
}

ext_grs_edge_t *ext_grs_act_add_edge_to_keep(ext_grs_graph_t *graph,
											 char *name, int pos, ext_grs_node_t *arg, ext_grs_node_t *func, int id, ext_grs_edge_t *pattern_edge)
{
	ext_grs_edge_t *added_edge = ext_grs_act_add_edge(graph, name, pos, arg, func, id);
	ext_grs_act_announce_kept_edge(pattern_edge, added_edge);
	return(added_edge);
}

/** mature an action, it is not possible to make any
 *  changes to this actions pattern afterwards */

int ext_grs_act_mature(ext_grs_action_t *action) {

	int i;
	lc_list_t *pos, *pos1;

	int n_nodes = 0;
	int n_edges = 0;
	int n_neg_nodes = 0;
	int n_neg_edges = 0;
	int n_pseudo_nodes = 0;
	int n_pseudo_edges = 0;


	if (action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
		printf("  given action is already mature!\n");
		return 0;
	}

/* ................ 1 ................................................. */

	/* compute the overall number of real nodes and negative nodes */
	n_nodes = action->pattern->n_nodes;
	lc_list_for_each(pos, &action->negatives) {
		ext_grs_graph_t *neg =
			lc_list_entry(pos, ext_grs_graph_t, negative_list);
		n_neg_nodes += neg->n_nodes;
	}
	/* compute the overall number of real edges and negative edges */
	n_edges = action->pattern->n_edges;
	lc_list_for_each(pos, &action->negatives) {
		ext_grs_graph_t *neg =
			lc_list_entry(pos, ext_grs_graph_t, negative_list);
		n_neg_edges += neg->n_edges;
	}

	/* alloc space for real and negative nodes */
	action->nodes =
		obstack_alloc(&obst, (action->max_node_aiid+1) * sizeof(ext_grs_node_t));
	memset(action->nodes, 0 , (action->max_node_aiid+1) * sizeof(ext_grs_node_t));
	/* alloc space for real, negative and pseudo edges */
	action->edges =
		obstack_alloc(&obst,
			(action->max_edge_aiid+1 + n_nodes + n_neg_nodes + n_edges) * sizeof(ext_grs_edge_t));
	memset(action->edges, 0,
		(action->max_edge_aiid+1 + n_nodes + n_neg_nodes + n_edges) * sizeof(ext_grs_edge_t));


/* ................ 2 ................................................. */
	{
		int ret = 0;
		ret = import_graph_info(action, action->pattern);
		if (!ret) return 0;
	}
	lc_list_for_each(pos, &action->negatives) {
		int ret = 0;
		ext_grs_graph_t *neg =
			lc_list_entry(pos, ext_grs_graph_t, negative_list);
		ret = import_graph_info(action, neg);
		if (!ret) return 0;
	}

/* ................ 3 ................................................. */

	/* there's a pseudo edge for each node and a backward pseudo edge
	 * for each real and each negative edge */
	n_pseudo_edges = n_nodes + n_edges + n_neg_edges;
	n_pseudo_nodes = 0;

	/* setup space for information about homomorphic node matching and about critical
	 * nodes and edges. A node or edge is critical when it multiple matching may be
	 * harmful for parallel rule application */
	if (action->max_node_aiid >= 0) {
		action->hom =
			obstack_alloc(&obst, (action->max_node_aiid+1) * sizeof(*(action->hom)));
		action->node_critical =	obstack_alloc(&obst,
			(action->max_node_aiid+1) * sizeof(*(action->node_critical)));
	}
	if (n_nodes > 0)
		action->pattern_node_kept =
			obstack_alloc(&obst, (action->max_node_aiid+1) * sizeof(*action->pattern_node_kept));

	for (i=0; i <= action->max_node_aiid; i++) {

		action->hom[i] = obstack_alloc(
			&obst, (action->max_node_aiid+1) * sizeof(**(action->hom)));
		memset(action->hom[i], 0,
			(action->max_node_aiid+1) * sizeof(*(action->hom[i])));

		if (!action->nodes[i]) continue;

		/* by default every node is critical (unless it can be
		 * proofen that it is not) */
		action->node_critical[i] = 1;
		/* by default no node is kept */
		action->pattern_node_kept[i] = -1;
	}

	if (action->max_edge_aiid >= 0) {
		action->edge_critical =	obstack_alloc(&obst,
			(action->max_edge_aiid+1) * sizeof(*(action->edge_critical)));
		action->pattern_edge_kept =
			obstack_alloc(&obst, (action->max_edge_aiid+1) * sizeof(*action->pattern_edge_kept));
	}

	for (i=0; i <= action->max_edge_aiid; i++) {
		/* by default each node is critical (unless it can be
		 * proofen that it is not) */
		action->edge_critical[i] = 1;

		if (!action->edges[i]) continue;

		/* by default no edge is kept */
		action->pattern_edge_kept[i] = -1;
	}


	/* .................................... 4 .................................. */
	set_up_condition_stuff(action);

	action->n_nodes = n_nodes;
	action->n_edges = n_edges;

	action->n_neg_nodes = n_neg_nodes;
	action->n_neg_edges = n_neg_edges;

	action->n_pseudo_nodes = n_pseudo_nodes;
	action->n_pseudo_edges = n_pseudo_edges;




	/* if the action is of kind test, no replacement information has to be set up */
	if (action->kind == ext_grs_k_test) {
		/* set mature flag */
		action->mature = 1;
		return 1;
	}




	/* setup replacement information */

	/* first: nodes */
	if (action->max_repl_node_aiid >= 0) {
		action->replacement_nodes = obstack_alloc(&obst,
			(action->max_repl_node_aiid+1) * sizeof(*action->replacement_nodes));
		memset(action->replacement_nodes, 0,
			(action->max_repl_node_aiid+1) * sizeof(*action->replacement_nodes));
		action->replacement_node_kept = obstack_alloc(&obst,
			(action->max_repl_node_aiid+1) * sizeof(*action->replacement_node_kept));
	}
	for (i = 0; i <= action->max_repl_node_aiid; i++)
		/* by default ALL nodea are newly inserted */
		action->replacement_node_kept[i] = -1;

	lc_list_for_each(pos1, & action->replacement->nodes) {

		ext_grs_node_t *node = lc_list_entry(pos1, ext_grs_node_t, node_list);

		if (action->replacement_nodes[node->aiid] == NULL) {
			action->replacement_nodes[node->aiid] = node;
		}
		else {
			printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
			printf("  Multiply assigned id of a replacement node encountered!\n\n");
			return 0;
		}
	}
	action->n_replacement_nodes = action->replacement->n_nodes;

	/* second: edges */
	if (action->max_repl_edge_aiid >= 0) {
		action->replacement_edges = obstack_alloc(&obst,
			(action->max_repl_edge_aiid+1) * sizeof(*action->replacement_edges));
		memset(action->replacement_edges, 0,
			(action->max_repl_edge_aiid+1) * sizeof(*action->replacement_edges));
		action->replacement_edge_kept = obstack_alloc(&obst,
			(action->max_repl_edge_aiid+1) * sizeof(*action->replacement_edge_kept));
	}
	/* iterate over all replace edges */
	for (i = 0; i <= action->max_repl_edge_aiid; i++)
		/* by default ALL nodes are newly inserted */
		action->replacement_edge_kept[i] = -1;

	lc_list_for_each(pos, & action->replacement->edges) {
		ext_grs_edge_t *edge = lc_list_entry(pos, ext_grs_edge_t, edge_list);

		if (action->replacement_edges[edge->aiid] == NULL) {
			action->replacement_edges[edge->aiid] = edge;
		}
		else {
			printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
			printf("  Multiply assigned id of a replacement edge encountered!\n\n");
			return 0;
		}
	}
	action->n_replacement_edges = action->replacement->n_edges;





	/* setup information about the nodes and edges, which have to be kept by
	 * the rewriting step */
	{
		/* for this reason iterate over all pairs of nodes/edges stored in the ptr
		 *  to ptr map representing the association of pattern ans replacement elements */
		ext_grs_ptr_map_entry_t *entry = lc_pset_first(action->kept_elems);
		for ( ; entry; entry = lc_pset_next(action->kept_elems) ) {

			if (entry->kind == ext_grs_k_node) {

				ext_grs_node_t *n1 = (ext_grs_node_t *) entry->pre;
				ext_grs_node_t *n2 = (ext_grs_node_t *) entry->img;

				assert(n1->kind == ext_grs_k_node && n2->kind == ext_grs_k_node);
				action->pattern_node_kept[n1->aiid] = n2->aiid;
				action->replacement_node_kept[n2->aiid] = n1->aiid;
			}

			else if (entry->kind == ext_grs_k_edge) {

				ext_grs_edge_t *e1 = (ext_grs_edge_t *) entry->pre;
				ext_grs_edge_t *e2 = (ext_grs_edge_t *) entry->img;

				assert(e1->kind == ext_grs_k_edge && e2->kind == ext_grs_k_edge);
				action->pattern_edge_kept[e1->aiid] = e2->aiid;
				action->replacement_edge_kept[e2->aiid] = e1->aiid;
			}

			else assert(0 && "found an invalid entry in the ptr to ptr map");
		}
	}

	/* for all replacement nodes count the number of incoming edges having
	 * no given position and not being associated with a pattern edge and
	 * having no position function. This
	 * number is stored in each replacement nodes and represents the number
	 * of edges to appended to the matched host nodes in array without a
	 * specific order.
	 */
	for (i = 0; i <= action->max_repl_node_aiid; i++) {
		ext_grs_node_t *node = action->replacement_nodes[i];

		if (!node) continue;


		node->n_append_edges = 0;
		lc_list_for_each (pos, & node->edges[ext_grs_in]) {

			ext_grs_edge_t *in_edge = lc_list_entry(pos, ext_grs_edge_t, list[ext_grs_in]);
			assert(in_edge->aiid >= 0 && "invalid aiid encountered");

			if (in_edge->position_func) {

				if (in_edge->pos != ext_grs_NO_EDGE_POS) {
					assert(in_edge->pos >= 0 && "bad edge position");
					printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
					printf("  There is a position function associated with an edge,\n");
					printf("  which has a given postion.\n\n");
					return 0;
				}
				if (action->replacement_node_kept[in_edge->aiid] >= 0) {
					printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
					printf("  There is a position function associated with an edge,\n");
					printf("  which is not to be newly inserted but kept.\n\n");
					return 0;
				}

			}
			else {

				if
				(
					in_edge->pos == ext_grs_NO_EDGE_POS &&
					action->replacement_edge_kept[in_edge->aiid] == -1
				)
				{
					node->n_append_edges++;
				}

			}

		}
	}




	/* ensure that there is no edge, which has to be preserved while the rewriting
	 * and is incident to a pattern node, which has to be removed and, if not,
	 * ensure that associated edges are incident to associated nodes */
	lc_list_for_each(pos, & action->pattern->edges) {

		ext_grs_edge_t *edge = lc_list_entry(pos, ext_grs_edge_t, edge_list);

		if (
			edge->kind == ext_grs_k_edge &&
			action->pattern_edge_kept[edge->aiid] > -1
		)
		{
			if (action->pattern_node_kept[edge->func->aiid] == -1 ||
				action->pattern_node_kept[edge->arg->aiid] == -1)
			{
				printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
				printf("  Action contains a pattern edge which is announced as to\n");
				printf("  be kept in rewrite steps, but this edge is incident to\n");
				printf("  a pattern node which must be deleted.\n\n");

				return 0;
			}
			else {
				ext_grs_edge_t *rpl_edge;
				ext_grs_node_t *rpl_arg_node, *rpl_func_node;

				assert(
					action->pattern_node_kept[edge->func->aiid] > -1 &&
					action->pattern_node_kept[edge->arg->aiid] > -1 &&
					"there should be no entry less than -1");

				rpl_edge = action->replacement_edges[action->pattern_edge_kept[edge->aiid]];
				rpl_func_node =
					action->replacement_nodes[action->pattern_node_kept[edge->func->aiid]];
				rpl_arg_node =
					action->replacement_nodes[action->pattern_node_kept[edge->arg->aiid]];

				if (rpl_edge->func != rpl_func_node || rpl_edge->arg != rpl_arg_node) {
					printf("module ext/grs: ERROR in function ext_grs_act_mature()\n");
					printf("  Action contains a pattern edge which is announced as to\n");
					printf("  be kept in rewrite steps, but at least one of the replacement\n");
					printf("  nodes incident to the associated replacement edge is not\n");
					printf("  associated with the respective pattern nodes.\n\n");

					return 0;
				}
			}

		}
	}


#if 0
	/* prepare relation between pattern nodes and negative nodes by
	 * initializing with 0 which means that there is no relation yet */
	action->neg_node_related = obstack_alloc(&obst,
			(action->max_node_aiid+1) * sizeof(*action->neg_node_related));
	memset(action->neg_node_related, 0,
			(action->max_node_aiid+1) * sizeof(*action->neg_node_related));

	action->neg_edge_related = obstack_alloc(&obst,
			(action->max_edge_aiid+1) * sizeof(*action->neg_edge_related));
	memset(action->neg_edge_related, 0,
			(action->max_edge_aiid+1) * sizeof(*action->neg_edge_related));
#endif


	/* set mature flag */
	action->mature = 1;

	return 1;
}


/** get a pattern node by its name */
ext_grs_node_t *ext_grs_act_get_node(ext_grs_action_t *action, char *name) {

	ext_grs_node_t *node;
	int i;

	if (action->mature == 0) {
		printf("module ext/grs: ERROR in function ext_grs_act_get_node()\n");
		printf("  The given action is NOT mature!\n\n");
		return NULL;
	}

	// Look for the name in the pattern node array
	for(i = 0; i <= action->max_node_aiid; i++) {
		node = action->nodes[i];
		if(node)
			if ( strcmp(name, node->name) == 0 )
				return node;
	}

	// Look for the name in the replace node array
	for(i = 0; i <= action->max_repl_node_aiid; i++) {
		node = action->replacement_nodes[i];
		if(node)
			if (strcmp(name, node->name) == 0)
				return node;
	}

	return NULL;
}


/** Same as ext_grs_act_get_node, but compares only the first n bytes of the node name */
ext_grs_node_t *ext_grs_act_get_node_n(ext_grs_action_t *action, char *name, int n) {

	ext_grs_node_t *node;
	int i;

	if (action->mature == 0) {
		printf("module ext/grs: ERROR in function ext_grs_act_get_node()\n");
		printf("  The given action is NOT mature!\n\n");
		return NULL;
	}

	// Look for the name in the pattern node array
	for(i = 0; i <= action->max_node_aiid; i++) {
		node = action->nodes[i];
		if(node)
			if ( strncmp(name, node->name, n) == 0 )
				return node;
	}

	// Look for the name in the replace node array
	for(i = 0; i <= action->max_repl_node_aiid; i++) {
		node = action->replacement_nodes[i];
		if(node)
			if (strncmp(name, node->name, n) == 0)
				return node;
	}

	return NULL;
}


/** Get a pattern node aiid by its name. */
int ext_grs_act_get_node_aiid(ext_grs_action_t *action, char *name)
{
	ext_grs_node_t *node = ext_grs_act_get_node(action, name);
	if(node)
		return(node->aiid);
	else
		return(-1);
}

/** Same as ext_grs_act_get_nodes_aiid, but compares only the first n bytes of the nodes name */
int ext_grs_act_get_node_aiid_n(ext_grs_action_t *action, char *name, int n)
{
	ext_grs_node_t *node = ext_grs_act_get_node_n(action, name, n);
	if(node)
		return(node->aiid);
	else
		return(-1);
}



/** get a pattern edge by its name */
ext_grs_edge_t *ext_grs_act_get_edge(ext_grs_action_t *action, char *name) {

	ext_grs_edge_t *edge;
	int i;

	if (action->mature == 0) {
		printf("module ext/grs: ERROR in function ext_grs_act_get_edge()\n");
		printf("  The given action is NOT mature\n\n");
		return NULL;
	}

	for(i = 0; i < action->n_edges; i++) {
		edge = action->edges[i];
		if ( strcmp(name, edge->name) == 0 )
			return edge;
	}
	return NULL;
}


/** allows homomorphic matching of two pattern nodes,
 *  note that these two nodes MUST have the same op and mode
 *  (including mode ANY) and belong tp the same acion
 *  @returns zero if fails and nonzero otherwise
 *  @note the action the given nodes belong to has to be mature */
int ext_grs_act_allow_nodes_hom(ext_grs_node_t *n1, ext_grs_node_t *n2)
{
	ext_grs_action_t *act;

	if (!n1 || !n2) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  NULL node(s) has/have been given.\n\n");
		return 0;
	}

	assert(n1->graph && n2->graph && "every node should belong to a graph");
	assert(n1->graph->action && n2->graph->action &&
		"every graph should belong to an action");
	act = n1->graph->action;

	/* check wether the given nodes are not replacement nodes nodes */
	if (
		n1->graph->kind == ext_grs_k_replacement ||
		n2->graph->kind == ext_grs_k_replacement
	)
	{
		assert(n1->graph == act->replacement);
		assert(n2->graph == act->replacement);

		printf("module ext/grs: ERROR in function ext_grs_act_allow_nodes_hom()\n");
		printf("  At least one given node belongs to a replacement graph.\n\n");
		return 0;
	}

	/* check wether the given nodes belong to the given action */
	if (n1->graph->action != n2->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_allow_nodes_hom()\n");
		printf("  Given nodes belong to different actions.\n\n");
		return 0;
	}
	/* action has to be mature */
	if (! act->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_allow_nodes_hom()\n");
		printf("  Given action is not mature.\n\n");
		return 0;
	}
	/* check wether the given nodes have the same firm op and firm mode */
	if (n1->op != n2->op  ||  n1->mode != n2->mode) {
		printf("module ext/grs: ERROR in function ext_grs_act_allow_nodes_hom()\n");
		printf("  Given nodes have different Firm op and/or different Firm mode.\n\n");
		return 0;
	}
	/* if the given pattern nodes are BOTH associated with replacement
	 * nodes, check wether the ops and modes of the replacement nodes are equal */
	if (act->pattern_node_kept[n1->aiid] >= 0 && act->pattern_node_kept[n2->aiid] >= 0) {

		ext_grs_node_t *rpl_n1, *rpl_n2;

		assert(n1->graph->kind != ext_grs_k_negative &&
			"a negative node should not be marked as kept");
		assert(n2->graph->kind != ext_grs_k_negative &&
			"a negative node should not be marked as kept");

		rpl_n1 = act->replacement_nodes[act->pattern_node_kept[n1->aiid]];
		rpl_n2 = act->replacement_nodes[act->pattern_node_kept[n2->aiid]];
		if (rpl_n1->op != rpl_n2->op || rpl_n1->mode != rpl_n2->mode)
		{
			printf("module ext/grs: ERROR in function ext_grs_act_allow_nodes_hom()\n");
			printf("  Given pattern nodes are assiciated with replacement nodes of\n");
			printf("  different op or mode, this may cause undefined behaviour of\n");
			printf("  the replacement when the nodes are matched homomorphic.\n\n");
			return 0;
		}
	}


	/* allow homomorphic matching of the given node */
	act->hom[n1->aiid][n2->aiid] = 1;

	return 1;
}


int ext_grs_act_announce_kept_node(ext_grs_node_t *n1, ext_grs_node_t *n2)
{
	ext_grs_action_t *act;
	ext_grs_ptr_map_entry_t *new_entry = obstack_alloc(&obst, sizeof(*new_entry));

	if (! new_entry) {
		printf("module ext/grs: Internal ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  Failed to allocate a new node pair representative.\n\n");
		return 0;
	}

	if (!n1 || !n2) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  NULL node(s) has/have been given.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}


	assert(n1->graph->action && n2->graph->action &&
		"every graph should belong to an action");
	act = n1->graph->action;

	/* check wether the given nodes belong to the given action */
	if (n1->graph->action != n2->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  Given nodes belong to different actions.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}
	/* action must not be mature */
	if (act->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  Given action must not be mature.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}
	/* check wether n1 belongs to pattern and n2 to the replacement graph */
	if (n1->graph != act->pattern || n2->graph != act->replacement) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  A pattern node and a replacement node must be given.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	new_entry->kind = ext_grs_k_node;
	new_entry->pre = n1;
	new_entry->img = n2;

	not_injective = 0;
	multiple_entry = 0;
	already_exists = 0;
	lc_pset_insert(act->kept_elems, new_entry, LC_HASH_PTR(new_entry));

	if (not_injective) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  The given replacement node has already been\n");
		printf("  associated with another pattern node.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (multiple_entry) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  The given pattern node has already been\n");
		printf("  associated with another replacement node.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (already_exists) {
		printf("module ext/grs: WARNING in function ext_grs_act_announce_kept_node()\n");
		printf("  The given association has already been announced.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	return 1;
}

int ext_grs_act_announce_kept_edge(ext_grs_edge_t *e1, ext_grs_edge_t *e2)
{
	ext_grs_action_t *act;
	ext_grs_ptr_map_entry_t *new_entry = obstack_alloc(&obst, sizeof(*new_entry));

	if (! new_entry) {
		printf("module ext/grs: Internal ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  Failed to allocate a new edge pair representative.\n\n");
		return 0;
	}

	if (!e1 || !e2) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  NULL edge(s) has/have been given.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (!e1->graph || !e2->graph || !e1->graph->action || !e2->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  At least one of the given edges is corrupted.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	act = e1->graph->action;

	/* check wether the given nodes belong to the given action */
	if (e1->graph->action != e2->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  Given edges belong to different actions.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}
	/* action has to be mature */
	if (act->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  Given action must not be mature.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}
	/* check wether n1 belongs to pattern and n2 to the replacement graph */
	if (e1->graph != act->pattern || e2->graph != act->replacement) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  A pattern edge and a replacement edge must be given.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (e1->pos != e2->pos) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_edge()\n");
		printf("  The pattern and the replacemnt edge must have the same position OR\n");
		printf("  both no position at all.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	new_entry->kind = ext_grs_k_edge;
	new_entry->pre = e1;
	new_entry->img = e2;

	not_injective = 0;
	multiple_entry = 0;
	already_exists = 0;
	lc_pset_insert(act->kept_elems, new_entry, LC_HASH_PTR(new_entry));

	if (not_injective) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  The given replacement node has already been\n");
		printf("  associated with another pattern node.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (multiple_entry) {
		printf("module ext/grs: ERROR in function ext_grs_act_announce_kept_node()\n");
		printf("  The given pattern node has already been\n");
		printf("  associated with another replacement node.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	if (already_exists) {
		printf("module ext/grs: WARNING in function ext_grs_act_announce_kept_node()\n");
		printf("  The given association has already been announced.\n\n");
		obstack_free(&obst, new_entry);
		return 0;
	}

	return 1;
}

int ext_grs_act_relate_neg_node(ext_grs_node_t *pat_node, ext_grs_node_t *neg_node) {

	if (pat_node->graph->action != neg_node->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_node()\n");
		printf("  The given nodes do not belong to the same action.\n\n");
		return 0;
	}

	if (pat_node->graph->kind != ext_grs_k_pattern) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_node()\n");
		printf("  The given pattern node does not belong to a pattern graph.\n\n");
		return 0;
	}

	if (neg_node->graph->kind != ext_grs_k_negative) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_node()\n");
		printf("  The given negative node does not belong to a negative pattern graph.\n\n");
		return 0;
	}

	neg_node->related_node = pat_node;
	return(1);
}

int ext_grs_act_relate_neg_edge(ext_grs_edge_t *pat_edge, ext_grs_edge_t *neg_edge) {

	if (pat_edge->graph->action != neg_edge->graph->action) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_edge()\n");
		printf("  The given edges do not belong to the same action.\n\n");
		return 0;
	}

	if (pat_edge->graph->kind != ext_grs_k_pattern) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_edge()\n");
		printf("  The given pattern edge does not belong to a pattern graph.\n\n");
		return 0;
	}

	if (neg_edge->graph->kind != ext_grs_k_negative) {
		printf("module ext/grs: ERROR in function ext_grs_act_relate_neg_edge()\n");
		printf("  The given negative edge does not belong to a negative pattern graph.\n\n");
		return 0;
	}

	neg_edge->related_edge = pat_edge;
	return(1);
}



int ext_grs_act_register_condition(
		ext_grs_condition_func_t func,
		ext_grs_graph_t *graph,
		int n_nodes, ext_grs_node_t **nodes,
		int n_edges, ext_grs_edge_t **edges)
{
	int i;
	ext_grs_cond_descr_t *cond_descr;
	ext_grs_node_t **nodes_copied = obstack_alloc(&obst, n_nodes * sizeof(*nodes_copied));
	ext_grs_edge_t **edges_copied = obstack_alloc(&obst, n_edges * sizeof(*edges_copied));

	if (!func) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
		printf("  The given condition function was NULL.\n\n");
		return 0;
	}

	if (!graph) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
		printf("  The given (pattern or negative graph) was NULL.\n\n");
		return 0;
	}

	if (graph->action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
		printf("  The given action is already mature, no condition registered.\n\n");
		return 0;
	}

	/* check whether the given nodes and edges are really nodes and edges, AND check
	 * whether these nodes and edges all belong to the given action */
	for (i = 0; i < n_nodes; i++) {

		if (!nodes[i]) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given node was NULL.\n\n");
			return 0;
		}
		if (nodes[i]->kind != ext_grs_k_node) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given node was not a valid node.\n\n");
			return 0;
		}
		if (nodes[i]->graph->action != graph->action) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given node belongs to an action different from the one of the given\n");
			printf("  (pattern or negative) graph.\n\n");
			return 0;
		}

		nodes_copied[i] = nodes[i];
	}
	for (i = 0; i < n_edges; i++) {

		if (!edges[i]) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given edge was NULL.\n\n");
			return 0;
		}
		if (edges[i]->kind != ext_grs_k_edge) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given edge was not a valid edge.\n\n");
			return 0;
		}
		if (edges[i]->graph->action != graph->action) {
			printf("module ext/grs: ERROR in function ext_grs_act_register_condition()\n");
			printf("  A given edge belongs to an action different from one of the given\n");
			printf("  (pattern or negative) graph.\n\n");
			return 0;
		}

		edges_copied[i] = edges[i];
	}

	/* allocate space for a condition descriptor */
	cond_descr = obstack_alloc(&obst, sizeof(*cond_descr));
	memset(cond_descr, 0, sizeof(cond_descr));

	/* init the condition descriptor */
	cond_descr->graph = graph;
	cond_descr->condition = func;

	cond_descr->n_nodes = n_nodes;
	cond_descr->nodes = nodes_copied;
	cond_descr->n_edges = n_edges;
	cond_descr->edges = edges_copied;

	/* add the condition descriptor to the list */
	lc_list_add(&cond_descr->condition_list, & graph->action->listed_conditions);
	/* increment the number of conditions */
	graph->n_conditions++;
	graph->action->n_conditions++; // AS
	return 1;
}


int ext_grs_act_register_eval(ext_grs_action_t *action,
		ext_grs_eval_in_func_t in_func, ext_grs_eval_out_func_t out_func)
{
	ext_grs_eval_descr_t *eval_descr;

	/*if (!in_func) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_eval()\n");
		printf("  The given eval-in function function was NULL.\n\n");
		return 0;
	}
	if (!out_func) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_eval()\n");
		printf("  The given eval-out function function was NULL.\n\n");
		return 0;
	}*/
	if (action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_register_eval_out_func()\n");
		printf("  The given action is already mature, no eval function registered.\n\n");
		return 0;
	}

	eval_descr = obstack_alloc(&obst, sizeof(*eval_descr));
	memset(eval_descr, 0, sizeof(*eval_descr));
	eval_descr->eval_in = in_func;
	eval_descr->eval_out = out_func;
	eval_descr->data = NULL;
	lc_list_add_tail(& eval_descr->eval_list, & action->listed_evals);
	return 1;
}



int ext_grs_act_set_op_condition(
	ext_grs_node_t *node, ext_grs_op_condition_func_t func)
{
	if (!node) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_op_condition()\n");
		printf("  The given node was NULL.\n\n");
		return 0;
	}
	if (!func) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_op_condition()\n");
		printf("  The given op condition function was NULL.\n\n");
		return 0;
	}

	node->op_condition = func;
	return 1;
}

int ext_grs_act_set_mode_condition(
		ext_grs_node_t *node, ext_grs_mode_condition_func_t func)
{
	if (!node) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_mode_condition()\n");
		printf("  The given node was NULL.\n\n");
		return 0;
	}
	if (!func) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_mode_condition()\n");
		printf("  The given mode condition function was NULL.\n\n");
		return 0;
	}

	node->mode_condition = func;
	return 1;
}

int ext_grs_act_set_pos_func(ext_grs_edge_t *edge, ext_grs_edge_pos_func_t func)
{
	if (!edge) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_pos_func()\n");
		printf("  The given edge was NULL.\n\n");
		return 0;
	}
	if (!edge) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_pos_func()\n");
		printf("  The given position function was NULL.\n\n");
		return 0;
	}
	assert(edge->graph && "every edge should have a graph");
	if (edge->graph->kind != ext_grs_k_replacement) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_pos_func()\n");
		printf("  The given edge belongs to a graph, which is not a replacement graph.\n\n");
		return 0;
	}
	assert(edge->graph->action && "every graph should have an action");
	if (edge->graph->action->mature) {
		printf("module ext/grs: ERROR in function ext_grs_act_set_pos_func()\n");
		printf("  The given edge belongs to an action, which is already mature.\n\n");
		return 0;
	}

	edge->position_func = func;
	return 1;
}


/* Initilize the action part of the exr/grs module of the libfirm.
 * This is an internal function. */
void _ext_grs_act_init() {
	/* init obstack if necesarry */
	if (obst_init == 0) {
		obstack_init(&obst);
	}
	obst_init = 1;
}

/* Finalize the action part of the exr/grs module of the libfirm.
 * This is an internal function */
void _ext_grs_act_finalize() {

	/* finalize the action obstack */
	obstack_free(&obst, NULL);
	obstack_finish(&obst);
}
