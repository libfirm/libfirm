/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/match.c
 * Purpose:     provides functions for subgraph matching
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		7. Junly 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdlib.h>
#include <libcore/lc_pset.h>
#include "firm.h"
#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif
#include "firm_types.h"
#include "array.h"
#include "ircons_t.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "iredges.h"
#include "base_t.h"
#include "match_t.h"


/* just a prototype */
static void match_rec(void);


#undef MAX
#define MAX(a,b) ((a) >= (b) ? (a) : (b))



/* the match dumper stores ir nodes and edges of a given match in hash tables
 * such that it can determine whether a ir node or edge has to be highlighted or not */
typedef struct _ext_grs_matched_graph_elem_t {
	/* tells whether a node or an edge is matched */
	ext_grs_elem_kind_t kind;
	/* the matched graph elem (a node or an edge) */
	union {ir_node *irn; const ir_edge_t *ire;} elem;
	/* tells whether this elem is matched for a given match id */
	int *matched;
	/* an array of index lists, tells the indeces in the
	 * espective match (if matched at all) */
	lc_list_t *aiids;
}
ext_grs_matched_graph_elem_t;

typedef struct {
	int aiid;
	lc_list_t list;
} ext_grs_matched_aiids_t;


/* elem of the list of found matches */
struct ext_grs_match_list_head {
	lc_list_t list;
	ir_node **nodes;
	ir_edge_t **edges;
};



/* Some global variables needed by the recursive matching algorithm.
 * This helps saving parameters */

/* the current matcher prog */
static ext_grs_match_op_t *prog = NULL;
/* the length of the current matcher prog */
static int length = 0;
/* the current position in the match program */
static int mpos = 0;
/* the maximum number of matches that shall be found (-1 means ALL) */
static int max_n_matches;
/* tells whether the matching shall guaranty the parallel
 * applicability of the given action for ALL found matches:
 * zero means no, nonzero mens yes */
static int match_compliant;
/* the current visit phase of the current irg. This info is necessary
 * to mark all nodes and edges which have already been matched by a
 * pattern node which does not guaranty parallel applicability in
 * multiple matches */
static unsigned long irg_visit_phase;
/* tells to which op index the backtracking has to brunch back, when a match
 * is found */
static int return_to_mop_index;

/* flag, tells whether a invocation of match_rec is for the pattern or a NAC */
static int match_negative_pattern = 0;
static int found_negative;

/** the node map of the current graph homomorphism */
static ir_node **node_map;
/** the edge map of the current graph homomorphism */
static ir_edge_t **edge_map;

static ir_node	 **positive_node_map;
static ir_edge_t **positive_edge_map;

/* the current match object */
static ext_grs_match_t *match = NULL;
/* private data of the current irg */
static ext_grs_irg_private_t *pr_g = NULL;
/* the list of already found matches (to be copied into the
 * match object at the end of the matching process) */
static struct ext_grs_match_list_head match_list_head;


/* an obstack needed by the match dumper */
static struct obstack dump_obst;
/* the hash table needed in the dump process mentioned above */
static lc_pset *dump_node_map, *dump_edge_map;
/* the number of matches in the match which is currently dumped */
static int dump_n_matches;







int matched_graph_elem_cmp_func(const void *o1, const void *o2) {
	ext_grs_matched_graph_elem_t *e1, *e2;
	e1 = (ext_grs_matched_graph_elem_t *) o1;
	e2 = (ext_grs_matched_graph_elem_t *) o2;

	assert (e1->kind == e2->kind);

	return (((unsigned) e1->elem.irn) - ((unsigned) e2->elem.irn));
}

int dump_matched_node_vcgattr(FILE *F, ir_node *node, ir_node *local) {

	ext_grs_matched_graph_elem_t *elem = alloca(sizeof(*elem));

	/* check wether the given node is matched or not */
	elem->kind = ext_grs_k_node;
	elem->elem.irn = node;
	elem = lc_pset_find(dump_node_map, elem, HASH_PTR(node));
	/* if it is matched... */
	if (elem) {
		fprintf (F, "color: magenta");
		return 1;
	}
	return 0;
}

int dump_matched_edge_vcgattr(FILE *F, ir_node *node, int to) {

	ext_grs_matched_graph_elem_t *elem = alloca(sizeof(*elem));

	const ir_edge_t *edge =  get_irn_edge(get_irn_irg(node), node, to);

	/* check whether the given node is matched or not */
	elem->kind = ext_grs_k_edge;
	elem->elem.ire = edge;
	elem = lc_pset_find(dump_edge_map, elem, HASH_PTR(edge));
	/* if it is matched... */
	if (elem) {
		fprintf (F, "color:magenta");
		return 1;
	}
	return 0;
}

int dump_graph_match_nodes(FILE *F, ir_graph *irg) {

	int which;
	for (which = 0; which < dump_n_matches; which++) {
		fprintf(F, "node: {title: \"match%d\" label: \"Match %d\" ", which, which);
		fprintf(F, "color: cyan}\n");
	}

	return 0;
}

int dump_match_edges(FILE *F, ir_node *node) {

	ext_grs_matched_graph_elem_t *elem = alloca(sizeof(*elem));
	int which;
	lc_list_t *pos;

	/* check wether the given node is matched or not */
	elem->kind = ext_grs_k_node;
	elem->elem.irn = node;
	elem = lc_pset_find(dump_node_map, elem, HASH_PTR(node));
	/* if it is matched... */
	if (elem)
		for (which = 0; which < dump_n_matches; which++)
			if (elem->matched[which])
				lc_list_for_each (pos, & elem->aiids[which]) {
					/* get the index in the node map */
					ext_grs_matched_aiids_t *aiid_wrapper =
						lc_list_entry(pos, ext_grs_matched_aiids_t, list);
					/* gen an edge from match node to matched node */
					fprintf(F, "edge: {sourcename: \"match%d\" targetname: \"", which);
					PRINT_NODEID(node);
					fprintf(F, "\" ");
					fprintf(F, "label: \"%d\" color:cyan}\n", aiid_wrapper->aiid);
				}

	return 0;
}

void ext_grs_dump_match(ext_grs_match_t *match, const char *suffix) {

	int i;
	int which, aiid;
	ext_grs_matched_graph_elem_t *elem;
	ext_grs_matched_aiids_t *aiid_wrapper;
	DUMP_NODE_EDGE_FUNC remember;

	obstack_init(& dump_obst);

	/* establish hash tables to determine, whether a nodes
	 *  or edges have been matched or not */
	dump_node_map =
		lc_pset_new(matched_graph_elem_cmp_func, match->n_nodes * match->n_matches);
	dump_edge_map =
		lc_pset_new(matched_graph_elem_cmp_func, match->n_edges * match->n_matches);

	for (which = 0; which < match->n_matches; which++) {
		for (aiid = 0; aiid < match->n_nodes; aiid ++) {

			ir_node *irn = match->nodes[which][aiid];

			elem = alloca(sizeof(*elem));
			elem->kind = ext_grs_k_node;
			elem->elem.irn = irn;
			elem = lc_pset_find(dump_node_map, elem, HASH_PTR(irn));

			if (!elem) {

				elem = obstack_alloc(& dump_obst, sizeof(*elem));
				elem->matched = obstack_alloc(
					& dump_obst, match->n_matches * sizeof(*elem->matched));
				elem->aiids = obstack_alloc(
					& dump_obst, match->n_matches * sizeof(*elem->aiids));
				/* init to 'not matched' for each match */
				memset(elem->matched, 0, match->n_matches * sizeof(*elem->matched));
				/* init the list heads in the aiid array */
				for (i = 0; i < match->n_matches; i++)
					LC_INIT_LIST_HEAD( &(elem->aiids[i]) );

				elem->kind = ext_grs_k_node;
				elem->elem.irn = irn;
				lc_pset_insert(dump_node_map, elem, HASH_PTR(irn));
			}
			elem->matched[which] = 1;
			aiid_wrapper = obstack_alloc(& dump_obst, sizeof(*aiid_wrapper));
			aiid_wrapper->aiid = aiid;
			lc_list_add(& aiid_wrapper->list, & (elem->aiids[which]));
		}
		for (aiid = 0; aiid < match->n_edges; aiid ++) {

			const ir_edge_t *ire = match->edges[which][aiid];

			elem = alloca(sizeof(*elem));
			elem->kind = ext_grs_k_edge;
			elem->elem.ire = ire;
			elem = lc_pset_find(dump_edge_map, elem, HASH_PTR(ire));

			if (!elem) {
				elem = obstack_alloc(& dump_obst, sizeof(*elem));
				elem->matched = obstack_alloc(
					& dump_obst, match->n_matches * sizeof(*elem->matched));
				elem->aiids = obstack_alloc(
					& dump_obst, match->n_matches * sizeof(*elem->aiids));
				/* init to 'not matched' for each match */
				memset(elem->matched, 0, match->n_matches * sizeof(*elem->matched));
				/* init the list heads in the indices array */
				for (i = 0; i < match->n_matches; i++)
					LC_INIT_LIST_HEAD( &(elem->aiids[i]) );

				elem->kind = ext_grs_k_edge;
				elem->elem.ire = ire;
				lc_pset_insert(dump_edge_map, elem, HASH_PTR(ire));
			}
			elem->matched[which] = 1;
			aiid_wrapper = obstack_alloc(& dump_obst, sizeof(*aiid_wrapper));
			aiid_wrapper->aiid = aiid;
			lc_list_add(& aiid_wrapper->list, & (elem->aiids[which]));
		}
	}

	/* set some hooks to produce the needed output in the generated vcg file */
	set_dump_node_vcgattr_hook(dump_matched_node_vcgattr);
	set_dump_edge_vcgattr_hook(dump_matched_edge_vcgattr);
	set_dump_ir_graph_hook(dump_graph_match_nodes);

	remember = get_dump_node_edge_hook();
	set_dump_node_edge_hook(dump_match_edges);

	/* dump the ir graph */
	dump_n_matches = match->n_matches;
	dump_ir_graph(match->irg, suffix);

	/* reset the hooks */
	set_dump_node_vcgattr_hook(NULL);
	set_dump_edge_vcgattr_hook(NULL);
	set_dump_ir_graph_hook(NULL);
	set_dump_node_edge_hook(remember);

	/* delete auxillary data structures */
	lc_pset_del(dump_node_map);
	lc_pset_del(dump_edge_map);
	obstack_free(& dump_obst, NULL);
	obstack_finish(& dump_obst);
}





/* initialize the matching mechanism */
void _ext_grs_match_init(void) {
	/* init the node map */
	node_map = NEW_ARR_F(ir_node*, 100);
	memset(node_map, 0, sizeof(*node_map) * 100);
	/* init the edge map */
	edge_map = (ir_edge_t **) NEW_ARR_F(ir_edge_t*, 100);
	memset(edge_map, 0, sizeof(*edge_map) * 100);
}



static INLINE void store_match(void) {

	int aiid;
	ir_node *node;
	ir_edge_t *edge;
	struct ext_grs_match_list_head *match_list_elem;
	ext_grs_iredges_private_t *pr_e;

	int n_all_pattern_nodes = match->action->max_node_aiid + 1;
	int n_all_pattern_edges = match->action->max_edge_aiid + 1;

	/* allocate new list elem on the match internal obstack */
	match_list_elem = obstack_alloc(& match->obst, sizeof(*match_list_elem));
	memset(match_list_elem, 0, sizeof(match_list_elem));

	/* add this new wrapper for the found match to the list */
	lc_list_add(& match_list_elem->list, & match_list_head.list);

	/* allocate space for the arrays representing the node and edge map */
	match_list_elem->nodes =
		obstack_alloc(& match->obst, n_all_pattern_nodes * sizeof(ir_node *));
	match_list_elem->edges =
		obstack_alloc(& match->obst, n_all_pattern_edges * sizeof(ir_edge_t *));

	memset(match_list_elem->nodes, 0, n_all_pattern_nodes * sizeof(*match_list_elem->nodes));
	memset(match_list_elem->edges, 0, n_all_pattern_edges * sizeof(*match_list_elem->edges));

	/* copy node map into match object */
	for (aiid = 0; aiid < n_all_pattern_nodes; aiid++) {

		ext_grs_node_t *pat_node = match->action->nodes[aiid];

		if (pat_node->graph->kind != ext_grs_k_pattern) continue;
		node = node_map[aiid];

		assert(node && "a pattern node should have been matched");
		assert(pat_node->kind == ext_grs_k_node &&
			"a pseudo node should not have been matched");

		match_list_elem->nodes[aiid] = node;

		if (match_compliant && match->action->node_critical[aiid])
			if (node) {
				set_irn_visited(node, irg_visit_phase);
				return_to_mop_index =
					return_to_mop_index < match->action->nodes[aiid]->first_mop_index ?
						return_to_mop_index : match->action->nodes[aiid]->first_mop_index;
			}
	}

	/* copy edge map into match object */
	for (aiid = 0; aiid < n_all_pattern_edges; aiid++) {

		ext_grs_edge_t *pat_edge = match->action->edges[aiid];

		/* check wether the current edge is a not pseudo-edge */
		if (pat_edge->kind != ext_grs_k_edge) continue;
		/* check wether the current edge belongs to the pattern graph
		 * (and not to e.g. a negative graph) */
		if (pat_edge->graph->kind != ext_grs_k_pattern) continue;

		edge = edge_map[aiid];

		assert(edge && "a pattern edge should have been matched");
		assert(pat_edge->kind == ext_grs_k_edge &&
			"a pseudo edge should not have been matched");

 		match_list_elem->edges[aiid] = edge;

		if (match_compliant && match->action->edge_critical[aiid]) {

			pr_e = get_edge_private_data(edge, _ext_grs_private_ofs_e);
			pr_e->visited = irg_visit_phase;
			return_to_mop_index =
				return_to_mop_index < match->action->edges[aiid]->first_mop_index ?
					return_to_mop_index : match->action->edges[aiid]->first_mop_index;
		}
	}

	/* increment counter for the found matches */
	match->n_matches++;
}

static void fill_match_result(ext_grs_match_plan_t *plan) {

	int match_counter = 0;
	struct ext_grs_match_list_head *list_elem;

	ext_grs_match_t *restore_match;
	ext_grs_match_t *neg_match;

	lc_list_t *pos;

	/* allocate space for the two arrays containing the node and edge maps */
	match->nodes = obstack_alloc(& match->obst, match->n_matches * sizeof(*match->nodes));
	match->edges = obstack_alloc(& match->obst, match->n_matches * sizeof(*match->edges));
	match->repl_nodes = obstack_alloc(& match->obst, match->n_matches * sizeof(*match->repl_nodes));

	restore_match = match;
	neg_match = alloca(sizeof(*neg_match));

	/* init that match object */


	/* store the node and edge maps */
	match_counter = 0;
	/* Walk through the list of matches */
	lc_list_for_each(pos, & match_list_head.list) {
		int i;
		lc_list_t *neg_pattern_list;

		list_elem = lc_list_entry(pos, struct ext_grs_match_list_head, list);

		/* for the current positive match ensure that no NAC occurs in the host graph */
		match = neg_match;
		neg_pattern_list = &plan->action->negatives;
		neg_pattern_list = neg_pattern_list->next;
		/* act->negatives is the list head, call next node to find the first object in list */
		for(i = 1; i < plan -> num_progs; i++)
		{
			ext_grs_graph_t *pat = lc_list_entry(neg_pattern_list, ext_grs_graph_t, negative_list);

			memset(match, 0, sizeof(*neg_match));
			match->n_nodes = pat->n_nodes;
			match->n_edges = pat->n_edges;
			match->action = plan->action;
			match->irg = restore_match->irg;
			match->max_node_id = plan->action->max_node_aiid;
			match->max_edge_id = plan->action->max_edge_aiid;
			match->compliant = ext_grs_REGARDLESS;


			return_to_mop_index = plan->length[i] - 1;
			length = plan->length[i];
			prog = plan->progs[i];
			mpos = 0;
			match_negative_pattern = 1;
			found_negative = 0;
			match_compliant = 0;
			max_n_matches = 1;
			positive_node_map = list_elem->nodes;
			positive_edge_map = list_elem->edges;

			/* if program has length 0 ... do nothing */
			if(plan->length[i] <= 0)
				break;

			match_rec();
			if(found_negative == 1)
				break;
			neg_pattern_list = neg_pattern_list->next;
		}
		match_negative_pattern = 0;
		match = restore_match;

		if(found_negative == 1)
			continue;


		match->nodes[match_counter] = list_elem->nodes; /* node_map */
		match->edges[match_counter] = list_elem->edges; /* edge_map */

		match_counter++;
	}
	/* Some positive matches could have been eliminated due to NAC's,
	   so adapt number of matches */
	match->n_matches = match_counter;
	assert(match_counter <= match->n_matches && "wrong number of matches stored");
}



/* ---------------
   What does this?
   --------------- */
static INLINE int match_preset_rec(
	 ext_grs_node_t *pattern_node, ext_grs_match_op_t *current_mop, ir_node *host_node)
{
	ext_grs_irn_private_t *pr_n;
	ext_grs_node_t *alredy_matched_pattern_node;

	/* Get the private ir data of the host node */
	pr_n = get_irn_data(host_node, ext_grs_irn_private_t, _ext_grs_private_ofs_n);

	assert(pattern_node->mode == mode_ANY || get_irn_mode(host_node) == pattern_node->mode);

	assert(get_irn_opcode(host_node) == get_op_code(pattern_node->op) ||
		_ext_grs_OP_IS_A(get_irn_opcode(host_node),	get_op_code(pattern_node->op))
	);

	assert(
		match->action->node_critical[pattern_node->aiid] == 0 ||
		match->action->node_critical[pattern_node->aiid] == 1
	);

	assert(!pr_n->deleted &&
		"a node deleted by a rewrite step should not be a member of a node list");

	/* if pattern is negative, compliant matching makes no sense */
	assert(! (match_compliant && match_negative_pattern));

	if (match_compliant && match->action->node_critical[pattern_node->aiid]) {
		assert(get_irn_visited(host_node) <= irg_visit_phase &&
			"visited in a future phase?");
		if (get_irn_visited(host_node) == irg_visit_phase) return;  // TODO! Return a value here!
	}

	/* check whether the current host graph node has not already
	* been matched. But if it has, the according host nodes have
	* to be allowed to be matched homomorphic with the current
	* host node */
	alredy_matched_pattern_node = pr_n->preimage;
	if (alredy_matched_pattern_node == NULL ||
		match->action->hom[pattern_node->aiid][alredy_matched_pattern_node->aiid])
	{

		/* if the host node has NOT already been matched before
		* remember the pattern node as already matched to the host node */
		int matched_first = 0;
		if (alredy_matched_pattern_node == NULL) {
			matched_first = 1;
		}

		pr_n->preimage = pattern_node;

		/* add the pair (pattern_node, host_node) to the node map */
		node_map[pattern_node->aiid] = host_node;



		/* check the condition if present */
		if (!current_mop->condition || current_mop->condition(node_map, edge_map)) {
			/* call recursively to process the next matcher op */
			mpos++;
			match_rec();
			mpos--;
		}


		/* remove the pair (pattern_node, host_node) from the node map */
		node_map[pattern_node->aiid] = NULL;
		/* remember whether this matching is no more */


		/* if the pattern node was the first node to be matched with
		* the host node,  */
		if (matched_first)
			pr_n->preimage = NULL;

		/* check whether matching is already complete */
		if (match->n_matches == max_n_matches) return 1;
		assert(match->n_matches < max_n_matches || max_n_matches == -1);
		/* check whether backjumping is needed
		* (this only happens for compliant matching) */
		if (return_to_mop_index < mpos) return 1;
		return_to_mop_index = length - 1;
	}


	/* matching not yet complete (or no backjumping needed),
	* do more matching if possible */
	return 0;
}


static INLINE int match_node_rec(
	ext_grs_node_t *pattern_node, ext_grs_match_op_t *current_mop, ir_opcode opc, modecode mc)
{
	lc_list_t *node_list_head = & _ext_grs_NODE_LIST(pr_g, opc, mc);
	lc_list_t *pos;
	ir_node *host_node;

	/* FOR all ir nodes in the node list of the current op
	 * and the pattern nodes mode DO... */
	/* In other words: Start a new match for each host-node with the current op. (Backtracking) */

	lc_list_for_each (pos, node_list_head) {

		ext_grs_irn_private_t *pr_n;
		ext_grs_node_t *alredy_matched_pattern_node;

#if DUMP_WHILE_MATCHING
		printf("Next node...\n");
#endif

		pr_n = lc_list_entry(pos, ext_grs_irn_private_t, node_list);
		host_node = get_irn_data_base(pr_n, _ext_grs_private_ofs_n);

		assert(pattern_node->mode == mode_ANY || get_irn_mode(host_node) == pattern_node->mode);

		assert(get_irn_opcode(host_node) == get_op_code(pattern_node->op) ||
			_ext_grs_OP_IS_A(
					get_irn_opcode(host_node),
					get_op_code(pattern_node->op)
				)
		);

		assert(
			match->action->node_critical[pattern_node->aiid] == 0 ||
			match->action->node_critical[pattern_node->aiid] == 1);

		assert(!pr_n->deleted &&
			"a node deleted by a rewrite step should not be a member of a node list");

		if (match_compliant && match->action->node_critical[pattern_node->aiid]) {
			assert(get_irn_visited(host_node) <= irg_visit_phase &&
				"visited in a future phase?");
			if (get_irn_visited(host_node) == irg_visit_phase) continue;
		}

		/* check whether the current host graph node has not already
		 * been matched. But if it has, the according host nodes have
		 * to be allowed to be matched homomorphic with the current
		 * host node */
		alredy_matched_pattern_node = pr_n->preimage;
		if (alredy_matched_pattern_node == NULL ||
			match->action->hom[pattern_node->aiid][alredy_matched_pattern_node->aiid])
		{

			/* if the host node has NOT already been matched before
			 * remember the pattern node as already matched to the host node */
			int matched_first = 0;
			if (alredy_matched_pattern_node == NULL) {
				matched_first = 1;
			}

			pr_n->preimage = pattern_node;

			/* add the pair (pattern_node, host_node) to the node map */
			node_map[pattern_node->aiid] = host_node;

			/* check the condition if present */
			if (!current_mop->condition || current_mop->condition(node_map, edge_map)) {
					/* call recursively to process the next matcher op */
					mpos++;
					match_rec();
					mpos--;
				}



			/* remove the pair (pattern_node, host_node) from the node map */
			node_map[pattern_node->aiid] = NULL;
			/* remember whether this matching is no more */


			/* if the pattern node was the first node to be matched with
			 * the host node,  */
			if (matched_first)
				pr_n->preimage = NULL;

			/* check whether matching is already complete */
			if (match->n_matches == max_n_matches) return 1;
			assert(match->n_matches < max_n_matches || max_n_matches == -1);
			/* check whether backjumping is needed
			 * (this only happens for compliant matching) */
			if (return_to_mop_index < mpos) return 1;
			return_to_mop_index = length - 1;
		}
	}
	/* matching not yet complete (or no backjumping needed),
	 * do more matching if possible */
	return 0;
}



static void match_edge_rec(
	ext_grs_node_t *pattern_node,
	ext_grs_edge_t *pattern_edge,
	ext_grs_node_t *other_pattern_node,
	ir_node *host_node, ir_edge_t *host_edge, ir_node *other_host_node)
{

	ext_grs_irn_private_t *pr_n;
	ext_grs_iredges_private_t *pr_e =
		get_edge_private_data(host_edge, _ext_grs_private_ofs_e);

	int matched_first = 0;
	int both_nodes_matched = 0;
	ext_grs_edge_t *already_matched_pattern_edge;
	ext_grs_node_t *already_matched_other_pattern_node;

#if DUMP_WHILE_MATCHING
	printf("%d: Checking %s%d -> %s%d (%s->%s) (%s->%s)\n", mpos, get_op_name(get_irn_op(host_node)), host_node->node_nr, get_op_name(get_irn_op(other_host_node)), other_host_node->node_nr, get_op_name(pattern_node->op), get_op_name(other_pattern_node->op), pattern_node->name, other_pattern_node->name);
#endif

	assert(
		match->action->edge_critical[pattern_edge->aiid] == 0 ||
		match->action->edge_critical[pattern_edge->aiid] == 1);

	/* if node has already been delted on a rewrite step and it still reached
	 * (maybe by a matching comming from the igrs Bad node) it is nevertheless
	 * NOT matched! */
	pr_n = _ext_grs_get_irn_private(other_host_node);
	if (pr_n->deleted) {
		assert(host_node == get_irg_bad(match->irg) &&
			"this node has been deleted by a rewrite step, the only chance" &&
			"to meet it should have been coming from the irgs bad node");
		return;
	}

	if (match_compliant) {
		if (match->action->edge_critical[pattern_edge->aiid]) {
			assert(pr_e->visited <= irg_visit_phase && "visited in a future phase?");
			if (pr_e->visited == irg_visit_phase) return;
		}
		if (match->action->node_critical[other_pattern_node->aiid]) {
			assert(get_irn_visited(other_host_node) <= irg_visit_phase &&
				"visited in a future phase?");
			if (get_irn_visited(other_host_node) == irg_visit_phase) return;
		}
	}
	/* check whether the op of the host edge matches
	 * the op of the current pattern edge */
	if (get_irn_op(other_host_node) != other_pattern_node->op &&
		! _ext_grs_OP_IS_A(get_irn_opcode(other_host_node), get_op_code(other_pattern_node->op)))
		return;

	/* check whether the mode of the node at the other end of the host edge
	 * matches the mode of the pattern node at the other end of the pattern edge */
	if (
		/* if the other pattern node allows any mode, dont check the mode
		 * of the other host node at all */
		other_pattern_node->mode != mode_ANY &&
		/* otherwise check whether the modecode of the other
		 * pattern and the other host node are equal */
		other_pattern_node->mode != get_irn_mode(other_host_node)
	) return;

	/* check whether the current host edge has not already been matched */
	already_matched_pattern_edge = pr_e->preimage;
	if ( already_matched_pattern_edge != NULL ) return;


	/* if both pattern nodes have already been matched, check whether the other
	 * host nodes preimage equals the other pattern node */
	if (node_map[other_pattern_node->aiid] != NULL) {
		/* in the case that both nodes are already matched check whether the
		 * current host edge connects these nodes */
		if (node_map[other_pattern_node->aiid] != other_host_node) return;
		both_nodes_matched = 1;
	}
	else {
		/* if the current host edge is NOT loop... */
		if (other_host_node != host_node) {

			/* check whether the host node at the other end of the current
			 * host edge has not already been matched OR, if it HAS, whether the already
			 * matched pattern node is permitted to be matched homomorphic with the
			 * pattern node at the other end of the current pattern edge */
			already_matched_other_pattern_node = pr_n->preimage;
			if (already_matched_other_pattern_node != NULL) {
				if (! match->action->hom
						[other_pattern_node->aiid][already_matched_other_pattern_node->aiid]
				) return;
			}
			else {
				/* if the other host node has not already been matched, this
				 * is matching is for the first time */
				pr_n->preimage = other_pattern_node;
				matched_first = 1;
			}

			/* add the pair (other_pattern_node, other_host_node) to the node map */
			node_map[other_pattern_node->aiid] = other_host_node;
		}
	}


	/* add the pair (pattern_edge, host_edge) to the node map */
	edge_map[pattern_edge->aiid] = host_edge;
	pr_e->preimage = pattern_edge;

	/* check the condition if present */
	if (!prog[mpos].condition || prog[mpos].condition(node_map, edge_map)) {
		/* call recursively to process the next matcher op */
		mpos++;
#if DUMP_WHILE_MATCHING
		printf("found it.\n");
#endif
		match_rec();
		mpos--;
	}
#if DUMP_WHILE_MATCHING
	else if(prog[mpos].condition)
		printf("Condition failed\n");
#endif

	/* if the current host edge is NOT loop... */
	if (! both_nodes_matched)
		if (other_host_node != host_node) {
			/* remove the pair (pattern_node, host_node) from the node map */
			node_map[other_pattern_node->aiid] = NULL;
		}
	edge_map[pattern_edge->aiid] = NULL;
	pr_e->preimage = NULL;

	/* if the pattern node was the first node to be matched with
	 * the host node... */
	if (matched_first)
		pr_n->preimage = NULL;
}










static void match_preset_edge_rec(
						   ext_grs_node_t *pattern_node,
						   ext_grs_edge_t *pattern_edge,
						   ext_grs_node_t *other_pattern_node,

						   ir_node *host_node, ir_edge_t *host_edge, ir_node *other_host_node)
{

	ext_grs_irn_private_t *pr_n;
	ext_grs_iredges_private_t *pr_e =
		get_edge_private_data(host_edge, _ext_grs_private_ofs_e);

	int matched_first = 0;
	int both_nodes_matched = 0;
	ext_grs_edge_t *already_matched_pattern_edge;
	ext_grs_node_t *already_matched_other_pattern_node;

	assert(
		match->action->edge_critical[pattern_edge->aiid] == 0 ||
		match->action->edge_critical[pattern_edge->aiid] == 1);

	/* if node has already been deleted on a rewrite step and it still reached
	* (maybe by a matching coming from the irgs Bad node) it is nevertheless
	* NOT matched! */

	pr_n = _ext_grs_get_irn_private(other_host_node);
	if (pr_n->deleted) {
		assert(host_node == get_irg_bad(match->irg) &&
			"this node has been deleted by a rewrite step, the only chance" &&
			"to meet it should have been coming from the irgs bad node");
		return;
	}

	if (match_compliant) {
		if (match->action->edge_critical[pattern_edge->aiid]) {
			assert(pr_e->visited <= irg_visit_phase && "visited in a future phase?");
			if (pr_e->visited == irg_visit_phase) return;
		}
		if (match->action->node_critical[other_pattern_node->aiid]) {
			assert(get_irn_visited(other_host_node) <= irg_visit_phase &&
				"visited in a future phase?");
			if (get_irn_visited(other_host_node) == irg_visit_phase) return;
		}
	}
	/* check wether the op of the host edge matches
	* the op of the current pattern edge */
	if (
		get_irn_op(other_host_node) != other_pattern_node->op &&
		! _ext_grs_OP_IS_A(get_irn_opcode(other_host_node), get_op_code(other_pattern_node->op))
		) return;

	/* check wether the mode of the node at the other end of the host edge
	* matches the mode of the pattern node at the other end of the pattern edge */
	if (
		/* if the other pattern node allows any mode, dont check the mode
		* of the other host node at all */
		other_pattern_node->mode != mode_ANY &&
		/* otherwise check wether the modecode of the other
		* pattern and the other host node are equal */
		other_pattern_node->mode != get_irn_mode(other_host_node)
		) return;

	/* check wether the current host edge has not already been matched */
	already_matched_pattern_edge = pr_e->preimage;
	if ( already_matched_pattern_edge != NULL ) return;


	/* if both pattern nodes have already been matched, check wether the other
	* host nodes preimage equals the other pattern node */
	if (node_map[other_pattern_node->aiid] != NULL) {
		/* in the case that both nodes are already matched check wether the
		* current host edge connects these nodes */
		if (node_map[other_pattern_node->aiid] != other_host_node) return;
		both_nodes_matched = 1;
	}
	else {
		/* if the current host edge is NOT loop... */
		if (other_host_node != host_node) {

			/* check wether the host node at the other end of the current
			* host edge has not alredy been matched OR, if it HAS, wether the already
			* matched pattern node is permitted to be matched homomorphic with the
			* pattern node at the other end of the current pattern edge */
			already_matched_other_pattern_node = pr_n->preimage;
			if (already_matched_other_pattern_node != NULL) {
				if (! match->action->hom
					[other_pattern_node->aiid][already_matched_other_pattern_node->aiid]
				) return;
			}
			else {
				/* if the other host node has not already been matched, this
				* is matching is for the first time */
				pr_n->preimage = other_pattern_node;
				matched_first = 1;
			}

			/* add the pair (other_pattern_node, other_host_node) to the node map */
			node_map[other_pattern_node->aiid] = other_host_node;
		}
	}


	/* add the pair (pattern_edge, host_edge) to the node map */
	edge_map[pattern_edge->aiid] = host_edge;
	pr_e->preimage = pattern_edge;

	/* check the condition if present */
	if (!prog[mpos].condition || prog[mpos].condition(node_map, edge_map)) {
		/* call recursively to process the next matcher op */
		mpos++;
		match_rec();
		mpos--;
	}

	/* if the current host edge is NOT loop... */
	if (! both_nodes_matched)
		if (other_host_node != host_node) {
			/* remove the pair (pattern_node, host_node) from the node map */
			node_map[other_pattern_node->aiid] = NULL;
		}
		edge_map[pattern_edge->aiid] = NULL;
		pr_e->preimage = NULL;

		/* if the pattern node was the first node to be matched with
		* the host node... */
		if (matched_first)
			pr_n->preimage = NULL;
}
















/* The matching loop, actually recursive but implemented as
 * while loop by using a (only conceptually existing) stack.
 * The stack is not implicitly materialized by appropriate
 * fields present in the match op structs.
 * */
static void match_rec(void) {

	ext_grs_match_op_t *current_mop = NULL;

	ext_grs_edge_t *pattern_edge = NULL;
	ext_grs_node_t *pattern_node = NULL;
	ext_grs_node_t *other_pattern_node = NULL;

	ir_node *host_node = NULL;
	ir_node *other_host_node = NULL;
	ir_edge_t *host_edge = NULL;


	assert(prog != NULL && "global ptr to matcher program is NULL");

	/* if the hole match prog has been processed... */
	if (mpos >= length) {
		/* ...a complete match has been found */
		/* so add this complete match to the match object */

		if(match_negative_pattern == 0)
			store_match();
		else
		{
			found_negative = 1;
			match->n_matches = 1;
		}
		return;
	}

	current_mop = & prog[mpos];
	/* execute the current match op */
	/* if the current match op is a node-op... */

	if(current_mop->kind == ext_grs_k_mop_preset)
	{
		/* loop counters */
		ir_op *current_op;
		ir_mode *current_mode;
		ir_opcode opc;
		modecode mc;

		int matching_complete_or_backjump;

		pattern_node = current_mop->node;
		current_mode = pattern_node->mode;

		host_node = positive_node_map[pattern_node->related_node->aiid];
		current_op = host_node->op;

		/* Pattern_node == NAC node */
		assert(pattern_node->related_node != NULL && "preset node is not related!!");

		opc = get_op_code(current_op);
		mc  = get_mode_modecode(current_mode);


		/* if there is an op condition, check whether it holds */
		if (current_mop->op_condition && !current_mop->op_condition(current_op)) return;

		/* if there is an mode condition, check whether it holds */
		if (current_mop->mode_condition && !current_mop->mode_condition(current_mode)) return;

		matching_complete_or_backjump =
			match_preset_rec(pattern_node, current_mop, host_node);

		if (matching_complete_or_backjump) return;

		return;
	}
	else if(current_mop->kind == ext_grs_k_mop_preset_edge)
	{
		ir_edge_t *host_edge;
		ir_graph *irg = match->irg;

		pattern_edge = current_mop->edge;
		pattern_node = current_mop->node;

		/* check  whether the given pattern node is incident to the given pattern edge */
		assert(pattern_node == pattern_edge->func || pattern_node == pattern_edge->arg);

		host_edge = positive_edge_map[pattern_edge->related_edge->aiid];
		edge_map[pattern_edge->aiid] = host_edge;
		if(current_mop->condition && !current_mop->condition(node_map, edge_map))
		{
			mpos++;
			match_rec();
			mpos--;
		}

	}
	else if (current_mop->kind == ext_grs_k_mop_node) {

		/* loop counters */
		int i,j;
		int n_sub_ops;
		/* inheritance related stuff */
		ir_op **sub_op_array;
		pattern_node = current_mop->node;


		sub_op_array = _ext_grs_all_sub_ops_of_op[ get_op_code(pattern_node->op) ];
		n_sub_ops = ARR_LEN(sub_op_array);

		/* FOR all sub ops of the pattern nodes op DO... */
		/* Find all nodes in the host graph that have the same op or a sub_op as the pattern node: */
		for (i = 0; i < n_sub_ops; i++) {

			ir_op *current_op = sub_op_array[i];
			ir_mode *current_mode = pattern_node->mode;
			ir_opcode opc = get_op_code(current_op);
			modecode mc = get_mode_modecode(current_mode);

			int matching_complete_or_backjump;

			/* if there is an op condition, check whether it holds */
			if (current_mop->op_condition && !current_mop->op_condition(current_op)) continue;

			/* if pattern nodes mode is ANY match for ALL modes */
			if (current_mode == mode_ANY) {
				for (j = 0; j <= _ext_grs_max_modecode; j++) {
					/* if no mode condition present, simply perform matching */
					if (
						current_mop->mode_condition &&
						!current_mop->mode_condition(_ext_grs_mode_map[j])
					) continue; /* ??? */

					/* Start backtracking for all nodes in the host graph that have the current mop */
					matching_complete_or_backjump =
						match_node_rec(pattern_node, current_mop, opc, j);

					if (matching_complete_or_backjump) return;
				}
			}
			else {
				/* Even theres only ONE possible mode, the mode condition has to
				 * be checked all the same! E.g. the user may have registered a
				 * mode condition function which depends on a global state... */
				if (
					current_mop->mode_condition &&
					! current_mop->mode_condition(current_mode) /* <<<  NOT may be WRONG!!!! */
				) continue;

				matching_complete_or_backjump =
					match_node_rec(pattern_node, current_mop, opc, mc);

				if (matching_complete_or_backjump) return;
			}

		}
		return;
	}
	else if (current_mop->kind == ext_grs_k_mop_edge) {

		ir_graph *irg = match->irg;

		pattern_edge = current_mop->edge;
		pattern_node = current_mop->node;

		/* check  whether the given pattern node is incident to the given pattern edge */
		assert(pattern_node == pattern_edge->func || pattern_node == pattern_edge->arg);



		/* if the pattern edge is an outgoing (i.e. in-flow) edge... */
		if (pattern_node == pattern_edge->func) {

			int i = -1; /* -1 for the block edge */
			int upper_pos_bound;

			assert(node_map[pattern_node->aiid] != NULL &&
				"this node should have been matched already");

			other_pattern_node = pattern_edge->arg;
			host_node = node_map[pattern_node->aiid];

			/* only Block nodes have NO block edge (i.e. an edge at pos = -1) */
			if (get_irn_op(host_node) == op_Block) i = 0;

			/* if the current pattern edge gives a fixed edge pos... */
			upper_pos_bound = get_irn_arity(host_node);
			if (pattern_edge->pos != ext_grs_NO_EDGE_POS) {
				/* ...then we do NOT have to iterate over the edges.
				 * Instead of that we can access and check the host edge directly: */
				i = pattern_edge->pos;
				upper_pos_bound = i + 1;
			}

			/* FOR all outgoing (i.e. in-flow) edges of the pattern
			 * nodes current image DO */
			for ( ; i < upper_pos_bound; i++) {

				other_host_node = get_irn_n(host_node, i);
				host_edge = get_irn_edge(irg, host_node, i);

				/* if theres no such edge this is no valid match */
				if (! host_edge) continue;

				match_edge_rec(
					pattern_node, pattern_edge, other_pattern_node,
					host_node, host_edge, other_host_node);

				if (match->n_matches == max_n_matches) return;
				assert(match->n_matches < max_n_matches || max_n_matches == -1);

				if (return_to_mop_index < mpos) return;
				return_to_mop_index = length - 1;

			}
			return;
		}

		/* or an incoming (i.e. out-flow) edge otherwise */
		else {

			assert(pattern_node == pattern_edge->arg &&
				"pattern node not incident to pattern edge");

			assert(node_map[pattern_node->aiid] != NULL &&
				"this node should have been matched already");

			other_pattern_node = pattern_edge->func;
			host_node = node_map[pattern_node->aiid];

			foreach_out_edge(host_node, host_edge) {
				int edg_src_pos;

				/* if the pattern edge determines a fixed pos as outgoing
				 * (i.e. in-flow) edge  of its func node, check wether the
				 * pos of the current edge does equal this determined pos */
				if (pattern_edge->pos != ext_grs_NO_EDGE_POS &&
					(edg_src_pos = get_edge_src_pos(host_edge)) != pattern_edge->pos)
					continue;

				other_host_node = get_edge_src_irn(host_edge);

				match_edge_rec(
					pattern_node, pattern_edge, other_pattern_node,
					host_node, host_edge, other_host_node);

				if (match->n_matches == max_n_matches) return;
				assert(match->n_matches < max_n_matches || max_n_matches == -1);

				if (return_to_mop_index < mpos) return;
				return_to_mop_index = length - 1;

			}
			return;


		}


	}
	else {
		assert(current_mop->kind == ext_grs_k_mop_condition);
		assert(current_mop->condition && "a conditon mop should have a condition function");

		/* check the condition */
		if ( ! current_mop->condition(node_map, edge_map) ) return ;

		/* call recursively to process the next matcher op */
		mpos++;
		match_rec();
		mpos--;

		return;
	}

}

ext_grs_match_t *ext_grs_match(
	ir_graph *irg, ext_grs_match_plan_t *plan, int n, int compliant)
{
	ext_grs_graph_t *pattern = plan->action->pattern;

	if (!irg) {
		printf("module ext/grs: WARNING in function ext_grs_match()\n");
		printf("  Given ir graph was NULL.\n\n");
		return NULL;
	}

	if (!plan) {
		printf("module ext/grs: WARNING in function ext_grs_match()\n");
		printf("  Given match plan was NULL.\n\n");
		return NULL;
	}

	/* get the private_data of the current irg */
	pr_g = _ext_grs_get_irg_private(irg);

	if (!pr_g->matching_enabled) {
		printf("module ext/grs: ERROR in function ext_grs_match()\n");
		printf("  Matching is disabeled for the given irg.\n\n");
		return NULL;
	}

	/* allocate a new match object */
	match = (ext_grs_match_t *) malloc(sizeof(*match));
	if (match == NULL)
		return NULL;
	memset(match, 0, sizeof(*match));
	/* init that match object */
	match->action = plan->action;
	match->irg = irg;
	match->n_nodes = pattern->n_nodes;
	match->n_edges = pattern->n_edges;
	match->max_node_id = plan->action->max_node_aiid;
	match->max_edge_id = plan->action->max_edge_aiid;
	match->compliant = compliant;

	/* initialize this match's internal obstack */
	obstack_init(& match->obst);

	/* if program has length 0 just return an empty match */
	if(plan->length[0] <= 0 || n == 0) {
		match->n_matches = 0;
		return match;
	}

	/* ..................... WHAT DOES THE FOLLOWING???? ............................. */


	max_n_matches = n;
	match_compliant = compliant;

	irg_visit_phase = get_irg_visited(irg) + 1;
	set_irg_visited(irg, irg_visit_phase);

	/* if node map is not great enough extend it */
#if 0
	old_size = ARR_LEN(node_map);
	ARR_EXTO(ir_node*, node_map, match->action->max_node_aiid + 1);
	/* if array resized reset it to '0' */
	if (old_size < ARR_LEN(node_map))
		memset(node_map, 0, sizeof(*node_map) * ARR_LEN(node_map));
	/* if edge map is not great enough extend it */
	old_size = ARR_LEN(edge_map);
	ARR_EXTO(ir_edge_t*, edge_map, match->action->max_edge_aiid + 1);
	/* if array resized reset it to '0' */
	if (old_size < ARR_LEN(edge_map))
		memset(edge_map, 0, sizeof(*edge_map) * ARR_LEN(edge_map));
#endif

	node_map = (ir_node **) alloca((match->action->max_node_aiid + 1) * sizeof(*node_map));
	memset(node_map, 0, sizeof(*node_map) * (match->action->max_node_aiid + 1));
	edge_map = (ir_edge_t **) alloca((match->action->max_edge_aiid + 1) * sizeof(*edge_map));
	memset(edge_map, 0, sizeof(*edge_map) * (match->action->max_edge_aiid + 1));


	/* init the list of found matches */
	LC_INIT_LIST_HEAD(& match_list_head.list);

	/* .................................................................................... */

	/* execute the search plan */
	return_to_mop_index = plan->length[0] - 1;
	length = plan->length[0];
	prog = plan->progs[0];
	mpos = 0;
	match_negative_pattern = 0;
	match_rec();

	/* transfer the found matches to the match result object */
	fill_match_result(plan);

	pr_g = NULL;

	return match;
}

static int _get_op_n_args(ir_op *op) {

	switch (op->opar)
	{
		case oparity_zero: return 0;
		case oparity_unary: return 1;
		case oparity_binary: return 2;
		case oparity_trinary: return 3;
		default: return 0;
	}

	return 0;
}

int compute_new_arity(ir_node **node_map, ir_node **repl_node_map, ext_grs_node_t *pat_node, ext_grs_node_t *replace_node, int repl_node_greatest_edge_pos)
{
	int n_free_edges = 0;
	int old_arity =0, new_arity = 0;
	int k;
	ir_node *host_node;


	/* get the ir node matched by the the pattern node associated with the
	* current replacement node */
	host_node = node_map[pat_node->aiid];
	assert(host_node && "replacement node associated with NULL pattern node");
	old_arity = get_irn_arity(host_node);

/*	if(host_node->node_nr == 675)
		printf("found\n"); */

	/* compute the number of free slots in the current
	* in array (free means NULL or pointing to a Bad node) */
	for (k = 0; /*(get_irn_opcode(host_node) == iro_Block) ? 0 : -1;*/ k < old_arity; k++) {
		ir_node *arg = get_irn_n(host_node, k);
		if (!arg || get_irn_op(arg) == op_Bad)
			n_free_edges++;
	}


	if (repl_node_greatest_edge_pos == ext_grs_NO_EDGE_POS) {
		new_arity = old_arity + replace_node->n_append_edges - n_free_edges; // New arity may also be shorter!
			//old_arity + MAX(0, (replace_node->n_append_edges - n_free_edges));
	}
	else {
		assert(repl_node_greatest_edge_pos >= -1 && "invalid edge pos encountered");
		new_arity = MAX(repl_node_greatest_edge_pos + 1,  old_arity + replace_node->n_append_edges - n_free_edges);
		   //MAX(repl_node_greatest_edge_pos + 1, old_arity) + MAX(0, (replace_node->n_append_edges - n_free_edges));
	}
	// new_arity = MAX(new_arity, _get_op_n_args(replace_node->op));
	return(new_arity);
}


void ext_grs_finish_match(ext_grs_match_t *match, int n, int *which) {

	ext_grs_action_t *action;
	ir_node **node_map;
	const ir_edge_t **edge_map;

	ir_node **repl_node_map;
	ir_edge_t **repl_edge_map;
	ir_node *host_node;

	int n_matches;
	int i, j, k;
	lc_list_t *pos;

	ir_node *bad;
	ext_grs_irg_private_t *pr_g;
	ir_graph *remeber_current_irg = current_ir_graph;

	/* array of positions where the next edge without given pos can be appended, the
	 * index is the replacement aiid of the replacement node the edge is incoming on */
	int *next_append_pos;

	int *repl_edge_positions;
	int *repl_node_greatest_edge_pos;


	if (!match) {
		printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
		printf("  Given match object was NULL.\n");
		printf("  No rewrite has been performed.\n\n");
		return;
	}


	action = match->action;
	pr_g = _ext_grs_get_irg_private(match->irg);

	if (!pr_g->matching_enabled && action->kind != ext_grs_k_test) {
		printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
		printf("  Tried to finish an action which is not of kind test, while\n");
		printf("  matching has been disabled for the respective ir graph.\n");
		printf("  This means that some internal data needed in this is no more");
		printf("  there. So no transformation of the ir graph has been done.\n\n");

		obstack_free(& match->obst, NULL);
		free(match);

		return;
	}

	bad = get_irg_bad(match->irg);

	//repl_node_map = alloca(sizeof(*repl_node_map) * (action->max_repl_node_aiid + 1));	Allocated on the obst now to be available after rewrite
	repl_edge_map = alloca(sizeof(*repl_edge_map) * (action->max_repl_edge_aiid + 1));
	next_append_pos = alloca(sizeof(*next_append_pos) * (action->max_repl_node_aiid + 1));
	repl_edge_positions = alloca(sizeof(*repl_edge_positions) * (action->max_repl_edge_aiid + 1));
	repl_node_greatest_edge_pos =
		alloca(sizeof(*repl_node_greatest_edge_pos) * (action->max_repl_node_aiid + 1));

	/* if action is a test no replacement will be done */
	if (action->kind == ext_grs_k_test) n = 0;

	current_ir_graph = match->irg;

	if (n == ext_grs_ALL) {
		n_matches = match->n_matches;
	}
	else if (n >= 0) {
		if (n > match->n_matches) {
			printf("module ext/grs: WARNING in function ext_grs_finish_match()\n");
			printf("  Given number of matches (%d) to be replaced exceeds the", n);
			printf("  number of matches found (%d).\n\n", match->n_matches);
			n_matches = match->n_matches;
		}
		else {
			n_matches = n;
		}
	}
	else {
		printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
		printf("  Given number of matches to be replaced is invalid: %d.\n", n);
		printf("  No rewrite has been performed.\n\n");

		obstack_free(& match->obst, NULL);
		free(match);

		return;
	}

	/* parallel rewrites are forbidden for match objects,
	 * which are not prepared for such things */
	if ( !match->compliant && n_matches > 1 ) {
		n_matches = 1;
		printf("module ext/grs: WARNING in function ext_grs_finish_match()\n");
		printf("  Parallel rewriting was demanded but the given match object\n");
		printf("  is not compliant. So only ONE match is replaced.\n\n");
	}

	/* check for empty which array */
	if (n != ext_grs_ALL && n != 0 && !which) {
		printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
		printf("  The set of match numbers to be replaced was NULL.\n");
		printf("  No rewrite has been performed.\n\n");

		obstack_free(& match->obst, NULL);
		free(match);

		return;
	}



	/* perform replacement of several matches */
	//i = 0;
	for(i = 0; i < n_matches; i++)
	{
		// while (i < n_matches) {

		repl_node_map = obstack_alloc(&(match->obst), sizeof(*repl_node_map) * (action->max_repl_node_aiid + 1)); //)alloca(sizeof(*repl_node_map) * (action->max_repl_node_aiid + 1));
		memset(repl_node_map, 0, sizeof(*repl_node_map) * (action->max_repl_node_aiid + 1));
		memset(repl_edge_map, 0, sizeof(*repl_edge_map) * (action->max_repl_edge_aiid + 1));

		/* get the node and edge map of the current match */
		if (n == ext_grs_ALL) {
			match->repl_nodes[i] = repl_node_map;
			node_map = match->nodes[i];
			edge_map = match->edges[i];
		}
		else {
			if (which[i] > match->n_matches) {
				printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
				printf("  A given match number exceeds the number of found matches.\n");
				printf("  No rewrite is performed for this number.\n\n");

//				obstack_free(& match->obst, NULL);
//				free(match);

				continue;
			}
			match->repl_nodes[which[i]] = repl_node_map;
			node_map = match->nodes[which[i]];
			edge_map = match->edges[which[i]];
		}



		/* Before any graph transformations are done...
		 * ...the eval-in functions of the current action are evaluated for
		 * the current node and edge map */
		lc_list_for_each(pos, &action->listed_evals) {
			/* get the current eval descriptor...  */
			ext_grs_eval_descr_t *descr =
				lc_list_entry(pos, ext_grs_eval_descr_t, eval_list);
			/* get the current eval-in function... */
			if(descr->eval_in != NULL) // AS
			{
				ext_grs_eval_in_func_t in_func = descr->eval_in;
				/* ...and call it */
				descr->data = in_func(node_map, edge_map);
			}

		}





		/* First: remove all remove edges of the pattern */
		lc_list_for_each(pos, & action->pattern->edges) {

			ir_node *n, *in;
			int edge_pos;
			const ir_edge_t *remove_edge;
			ext_grs_edge_t *pattern_edge =
				lc_list_entry(pos, ext_grs_edge_t, edge_list);

			assert(
				(
					pattern_edge->kind == ext_grs_k_edge ||
					pattern_edge->kind == ext_grs_k_pseudo_edge
				) && "wrong kind encountered in edge list"
			);

			/* pseudo edges can't be removed (because they are not mapped) */
			if (pattern_edge->kind != ext_grs_k_edge) continue;
			/* check whether the pattern edge is to be kept */
			if (action->pattern_edge_kept[pattern_edge->aiid] >= 0) continue;
			remove_edge = edge_map[pattern_edge->aiid];

			n = get_edge_src_irn(remove_edge);
			in = get_irn_n(n, 0);
			edge_pos = get_edge_src_pos(remove_edge);

			if(n->node_nr == 675)
				printf("found");

			/* let the remove_edges src node point to a Bad node, for
			 * a complete removal is not permitted by the FIRM syntax */
		set_irn_n(
				get_edge_src_irn(remove_edge),
				get_edge_src_pos(remove_edge),
				bad);
		}





		/* Second: Go through all replacement node, insert the new ones and
		 * retype the kept ones if necessary. Furthermore establish a map
		 * which maps replacement node aiids to ir node pointers. */
		for (j = 0; j <= action->max_repl_node_aiid; j++) {

			ext_grs_node_t *replace_node = action->replacement_nodes[j];
			ir_node *block = NULL;

			ir_node **new_in;
			int old_arity = 0;
			int new_arity;

			/* private area of a host node */
			ext_grs_irn_private_t *pr_n;

			repl_node_greatest_edge_pos[j] = ext_grs_NO_EDGE_POS;

			/* check for gaps in the set of aiids */
			if (!replace_node) continue;

			assert (action->replacement_node_kept[j] >= -1 && "invalid entry found");


			/* for the current replacement node determine the positions of the
			 * outgoing (i.e. inflowing) edges to be newly inserted and the
			 * greatest pos among the outgoing (i.e. in-flowing) edges of each
			 * node */
			lc_list_for_each (pos, & replace_node->edges[ext_grs_in]) {
				ext_grs_edge_t *in_edge = lc_list_entry(pos, ext_grs_edge_t, list[ext_grs_in]);

				if (in_edge->position_func) {
					int h;
					assert(in_edge->pos == ext_grs_NO_EDGE_POS &&
						"a replacement edge with pos function should not have a fixed position");
					assert(action->replacement_edge_kept[in_edge->aiid] == -1 &&
						"a replacement edge with pos function is supposed to be newly inserted");
					h = in_edge->position_func(in_edge, node_map, edge_map);
					repl_edge_positions[in_edge->aiid] = h;
					repl_node_greatest_edge_pos[j] = MAX(h, repl_node_greatest_edge_pos[j]);
				}
				else if (in_edge->pos != ext_grs_NO_EDGE_POS) {

					/*	Was soll diese Assertion?
						Kept-Edges duerfen keine feste Position haben oder wie?
						Besser: Pruefe die neue Position auf Gleichheit mit der alten!

						Warum trat diese Assertion frueher nicht auf? */

					/*assert(action->replacement_edge_kept[in_edge->aiid] == -1 &&
						"a replacement edge with fixed position supposed to be newly inserted"); */
					repl_edge_positions[in_edge->aiid] = in_edge->pos;
					repl_node_greatest_edge_pos[j] =
						MAX(in_edge->pos, repl_node_greatest_edge_pos[j]);
				}
			}


			/* if node is to be newly inserted */
			if (action->replacement_node_kept[j] == -1) {

				ir_opcode opc;
				modecode mc;

				if (repl_node_greatest_edge_pos[j] == ext_grs_NO_EDGE_POS) {
					new_arity = replace_node->n_append_edges;
				}
				else {
					assert(repl_node_greatest_edge_pos[j] >= -1 && "invalid edge pos encountered");
					new_arity = repl_node_greatest_edge_pos[j] + 1 + replace_node->n_append_edges;
				}
				new_arity = MAX(new_arity, _get_op_n_args(replace_node->op));

				/* non Block nodes initially get a Bad block, block nodes a NULL block */
				if (replace_node->op != op_Block)
					block = bad;
				else
					block = NULL;

				if (new_arity > 0) {
					new_in = alloca( sizeof(*new_in) * new_arity );
					for (k = 0; k < new_arity; k++)
						new_in[k] = bad;
					/* create a new node */
					host_node = new_ir_node(
								NULL, match->irg, block,
								replace_node->op, replace_node->mode,
								new_arity, new_in);
				}
				else {
					host_node = new_ir_node(
								NULL, match->irg, block,
								replace_node->op, replace_node->mode,
								0, NULL);
				}

				/* insert the new node in the respective node list */
				opc = get_irn_opcode(host_node);
				mc = get_irn_modecode(host_node);

				pr_n = _ext_grs_get_irn_private(host_node);
				lc_list_add(&pr_n->node_list, & _ext_grs_NODE_LIST(pr_g, opc, mc));
			}
			/* otherwise give the already existing node a new op or mode (if necessary) */
			else {

				/* the pattern node associated with the kept replace node */
				ext_grs_node_t *pat_node = action->nodes[action->replacement_node_kept[j]];
				/* tells whether a preserved node is changed somehow (op or mode) */
				int op_or_mode_changed = 0;
				int old_arity, new_arity;
				ir_node *old_node = NULL;

				host_node = node_map[pat_node->aiid];
				old_arity = get_irn_arity(host_node);
				new_arity = compute_new_arity(node_map, repl_node_map, pat_node, replace_node, repl_node_greatest_edge_pos[j]);

#if 0
				int n_free_edges = 0;


				/* get the ir node matched by the the pattern node associated with the
				 * current replacement node */
				host_node = node_map[pat_node->aiid];
				assert(host_node && "replacement node associated with NULL pattern node");
				old_arity = get_irn_arity(host_node);

				/* compute the number of free slots in the current
				 * in array (free means NULL or pointing to a Bad node) */
				for (k = -1; k < old_arity; k++) {
					ir_node *arg = get_irn_n(host_node, k);
					if (!arg || get_irn_op(arg) == op_Bad)
						n_free_edges++;
				}



				if (repl_node_greatest_edge_pos[j] == ext_grs_NO_EDGE_POS) {
					new_arity =
						old_arity + MAX(0, (replace_node->n_append_edges - n_free_edges));
				}
				else {
					assert(repl_node_greatest_edge_pos[j] >= -1 && "invalid edge pos encountered");
					new_arity = MAX(repl_node_greatest_edge_pos[j] + 1, old_arity) +
										MAX(0, (replace_node->n_append_edges - n_free_edges));
				}
				new_arity = MAX(new_arity, _get_op_n_args(replace_node->op));
#endif



				/* change op if necessary */
				if ( pat_node->op != replace_node->op || pat_node->mode != replace_node->mode) {

					/* if op changes from Block to something else
					 * a bad block will be inserted */
					if (get_irn_op(host_node) == op_Block && replace_node->op != op_Block) {
						assert(0 && "Not possible right now");
						set_irn_op(host_node, replace_node->op);
						set_nodes_block(host_node, bad);
					}
					/* if op changes to Block from something else
					 * the Block edge will be deleted */
					else if (get_irn_op(host_node) != op_Block && replace_node->op == op_Block) {
						assert(0 && "Not possible right now");
						assert(get_irn_op(host_node) != op_Bad && "nodes op should change");
						set_irn_n(host_node, -1, NULL);
						set_irn_op(host_node, replace_node->op);
					}
					/* simply change the op otherwise */
					else {

						ir_node *exchange_node;
						//new_arity = compute_new_arity(node_map, repl_node_map, pat_node, replace_node, repl_node_greatest_edge_pos[j]);

						/* if the edges to be inserted require a greater in-array, create one */
						//if (old_arity != new_arity) {
							new_in = alloca( sizeof(*new_in) * new_arity );
							for (k = 0; k < new_arity; k++) {
								if (k < old_arity)
									new_in[k] = get_irn_n(host_node, k);
								else
									new_in[k] = bad;
							}
						//}
						exchange_node = new_ir_node(NULL, current_ir_graph, get_nodes_block(host_node), replace_node->op, replace_node->mode/*get_irn_mode(host_node)*/, new_arity, new_in);
						old_node = host_node;
						exchange(host_node, exchange_node);
						repl_node_map[j] = exchange_node;
						host_node = exchange_node;

						//set_irn_op(host_node, replace_node->op);
					}
					/* remember op or mode was changed */
					op_or_mode_changed = 1;
				}
				else if(old_arity < new_arity)
				{
					/* if the edges to be inserted require a greater in-array, create one */
					// op or mode are not changed, we do not need to insert a new node and exchange it
					// This is the case e.g. for Phi, Sync or Block nodes.
					if (old_arity < new_arity) {
						new_in = alloca( sizeof(*new_in) * new_arity );
						for (k = 0; k < new_arity; k++) {
							if (k < old_arity)
								new_in[k] = get_irn_n(host_node, k);
							else
								new_in[k] = bad;
						}
						/* note: a nodes block is preserved by set_irn_in() */
						set_irn_in(host_node, new_arity, new_in);
					}
				}
#if 0
				// This is done by inserting a new node and exchanging now.
				/* or change mode if necessary */
				if ( pat_node->mode != replace_node->mode ) {
					set_irn_mode(host_node, replace_node->mode);
					/* remember node was changed */
					op_or_mode_changed = 1;
				}
#endif

				/* if mode or op was changed this node must be
				 * moved to another node list */

				/* Lists:	 each node contains in its private data the list head of the
							 list he resides in. */

				if (op_or_mode_changed) {

					ir_opcode opc = get_irn_opcode(host_node);
					modecode mc = get_irn_modecode(host_node);

					pr_n = _ext_grs_get_irn_private(host_node);

					if(old_node == NULL)
					{
						lc_list_move(&pr_n->node_list, & _ext_grs_NODE_LIST(pr_g, opc, mc));
					}
					else
					{
						// exchange was used
						ext_grs_irn_private_t *pr_n_old;

						pr_n_old = _ext_grs_get_irn_private(old_node);
						lc_list_add(&pr_n->node_list, & _ext_grs_NODE_LIST(pr_g, opc, mc));
						lc_list_del(&pr_n_old->node_list/*, & _ext_grs_NODE_LIST(pr_g, opc, mc)*/);
					}

				}
			}

			/* store the pos where the first edge with unspecified pos can be inserted */
			for(k = (get_irn_opcode(host_node) == iro_Block) ? 0 : -1; k < get_irn_arity(host_node); k++) {
				ir_node *arg = get_irn_n(host_node, k);
				if (!arg || get_irn_op(arg) == op_Bad)
				{
					next_append_pos[j] = k;
					break; // AS
				}
			}

			/* set up the map of replace nodes to host graph nodes */
			repl_node_map[replace_node->aiid] = host_node;

			assert(
				(
					get_op_code(get_irn_op(host_node)) == get_op_code(replace_node->op) ||
					_ext_grs_OP_IS_A(
						get_op_code(get_irn_op(host_node)), get_op_code(replace_node->op)
					)
				)
				&& "replace map should map nodes with compatible op");
		}






		/* Third: insert all edges not associated with an pattern edge */
		for (j = 0; j <= action->max_repl_edge_aiid; /* AS n_replacement_edges;*/ j++) {

			ext_grs_edge_t *replace_edge = action->replacement_edges[j];
			ext_grs_node_t *rpl_src_node, *rpl_tgt_node;

			ir_node *host_src_node, *host_tgt_node;

/*			if(j == 12)
				printf("found!"); */

			/* there might be gaps in the array, if an potential aiid
			 * is not assigned to an edge */
			if (!replace_edge) continue;

			/* if edge not to be newly inserted, skip this edge */
			if (action->replacement_edge_kept[replace_edge->aiid] >= 0) continue;

			rpl_src_node = replace_edge->func;
			rpl_tgt_node = replace_edge->arg;
			host_src_node = repl_node_map[rpl_src_node->aiid];
			host_tgt_node = repl_node_map[rpl_tgt_node->aiid];

			assert(
				(
					get_op_code(get_irn_op(host_src_node)) == get_op_code(rpl_src_node->op) ||
					_ext_grs_OP_IS_A(
						get_op_code(get_irn_op(host_src_node)), get_op_code(rpl_src_node->op)
					)
				)
				&& "replace map should map nodes with compatible op");

			assert(
				(
					get_op_code(get_irn_op(host_tgt_node)) == get_op_code(rpl_tgt_node->op) ||
					_ext_grs_OP_IS_A(
						get_op_code(get_irn_op(host_tgt_node)), get_op_code(rpl_tgt_node->op)
					)
				)
				&& "replace map should map nodes with compatible op");

			/* if the host src (i.e. func) node is a Bad node */
			if (get_irn_op(host_src_node) == op_Bad) {
				printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
				printf("  malformed replacement graph: Bad node cannot have an incoming\n");
				printf("  edge, edge was not inserted!\n");
				continue;
			}

			/* insert and edge with specified position */
			if (replace_edge->pos != ext_grs_NO_EDGE_POS) {
				if (replace_edge->pos == -1 && get_irn_op(host_tgt_node) != op_Block) {
					printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
					printf("  malformed replacement graph: incomming edge has pos -1, but\n");
					printf("  the argument is NOT a Block node! No edge inserted.\n");
					continue;
				}
				if (replace_edge->pos == -1 && get_irn_op(host_src_node) == op_Block) {
					printf("module ext/grs: ERROR in function ext_grs_finish_match()\n");
					printf("  malformed replacement graph: incomming edge has pos -1, but!\n");
					printf("  the src node is a Block node! No edge inserted.\n");
					continue;
				}
				set_irn_n(host_src_node, replace_edge->pos, host_tgt_node);
			}
			/* if no position is specified the edge is inserted to
			 * free places of the in-array */
			else {

				ir_node *arg;
				for (k = next_append_pos[rpl_src_node->aiid]; ; k++) {
					arg = get_irn_n(host_src_node, k);
					if (!arg || get_irn_op(arg) == op_Bad) break;
				}

				assert(k < get_irn_arity(host_src_node) && "append pos exceeds in-array");
				set_irn_n(host_src_node, k, host_tgt_node);
				next_append_pos[rpl_src_node->aiid] =
						MAX(k, next_append_pos[rpl_src_node->aiid] + 1);
			}
		}


		/* Fourth: remove all nodes to be removed and all incident edges */
		lc_list_for_each(pos, & action->pattern->nodes) {

			ext_grs_node_t *pat_node =
				lc_list_entry(pos, ext_grs_node_t, node_list);

			/* private area of a host node */
			ext_grs_irn_private_t *pr_n;

			if (pat_node->kind == ext_grs_k_node) {

				const ir_edge_t *edge;
				const ir_edge_t *ne;
				ir_node *remove_host_node;

				/* if node is to be kept it is skiped */
				if (action->pattern_node_kept[pat_node->aiid] >= 0) continue;
				assert(pat_node->kind == ext_grs_k_node && "no pseudo node expected");

				/* get the host the pat node is maped to, then remove it */
				remove_host_node = node_map[pat_node->aiid];
				assert(remove_host_node && "a pattern node should have been matched");
				/* all out edges of that node have to be changed that they
				 * point to the graphs bad node */
				foreach_out_edge_safe(remove_host_node, edge, ne) {
					set_irn_n(get_edge_src_irn(edge), get_edge_src_pos(edge), bad);
				}
				/* all in edges of the remove node have also altered that they point to bad */
				for (j = 0; j < get_irn_arity(remove_host_node); j++) {
					set_irn_n(remove_host_node, j, bad);
				}
				/* remove the node from the node list */
				pr_n = _ext_grs_get_irn_private(remove_host_node);
				if ( ! pr_n->deleted ) {
					lc_list_del(&pr_n->node_list);
					pr_n->deleted = 1;
				}
			}
		}


		/* After each graph transformations ...
		 * ...the eval-out functions of the current action are evaluated for
		 * the current node and edge map of the replacement graph biuld during
		 * the graph transforamtion */
		lc_list_for_each(pos, &action->listed_evals) {
			/* get the current eval descriptor...  */
			ext_grs_eval_descr_t *descr =
				lc_list_entry(pos, ext_grs_eval_descr_t, eval_list);
			/* get the current eval-in function... */
			if(descr->eval_out != NULL) // AS
			{
				ext_grs_eval_out_func_t in_func = descr->eval_out;
				/* ...and call it */
				in_func(repl_node_map, repl_edge_map, node_map, descr->data);
			}
		}

		//i++;

	}
	current_ir_graph = remeber_current_irg;
}

/* AS: Added to keep the match node map etc after a match
   When this information is not needed any more, ext_grs_free_match has to be called. */

void ext_grs_free_match(ext_grs_match_t *match)
{
	/* clear the match object */
	/* first: free the match objects obstack */
	obstack_free(&(match->obst), NULL);
	/* second: free the match object itself */
	free(match);
}



int ext_grs_get_n_matches(ext_grs_match_t *match) {
	if (! match) {
		printf("module ext/grs: ERROR in function ext_grs_get_n_matches()\n");
		printf("  the match id given was to large\n");
		assert(0);
		return 0;
	}

	return match->n_matches;
}


ir_graph *ext_grs_get_match_irg(ext_grs_match_t *match) {
	if (! match) {
		printf("module ext/grs: ERROR in function ext_grs_get_match_irg()\n");
		printf("  the match id given was to large\n");
		assert(0);
		return NULL;
	}

	return match->irg;
}


ir_node **ext_grs_get_match_node_map(ext_grs_match_t *match, int which) {

	if (which >= match->n_matches) {
		printf("module ext/grs: ERROR in function ext_grs_get_match_node_map()\n");
		printf("  the match id given was to large\n");
		assert(0);
		return NULL;
	}

	return match->nodes[which];
}

ir_node **ext_grs_get_replace_node_map(ext_grs_match_t *match, int which) {

	if (which >= match->n_matches) {
		printf("module ext/grs: ERROR in function ext_grs_get_match_node_map()\n");
		printf("  the match id given was to large\n");
		assert(0);
		return NULL;
	}

	return match->repl_nodes[which];
}



const ir_edge_t **ext_grs_get_match_edge_map(ext_grs_match_t *match, int which) {

	if (which >= match->n_matches) {
		printf("module ext/grs: ERROR in function ext_grs_get_match_node_map()\n");
		printf("  the match id given was to large\n");
		assert(0);
		return NULL;
	}

	return match->edges[which];
}


int ext_grs_get_match_max_node_aiid(ext_grs_match_t *match)
{
	return(match->max_node_id);
}

int ext_grs_get_match_max_edge_aiid(ext_grs_match_t *match)
{
	return(match->max_edge_id);
}
