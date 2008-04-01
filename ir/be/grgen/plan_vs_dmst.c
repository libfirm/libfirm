/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/plan_vs.c
 * Purpose:     provides a planar wich performs a bypassing of
 * 				v-structures by real valued costs
 * Author:      Veit Batz
 * Created:		11. Junly 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "common_t.h"

#include "auxilary_t.h"
#include "base_t.h"
#include "action_t.h"
#include "analyze_t.h"
#include "ana_vs_t.h"
#include "matchplan_t.h"

#include "uf_mpq.h"

#include "plan_vs_dmst.h"
#include "action_t.h"




typedef struct _ext_grs_vs_dmst_planer_private_t {
	/* tells wether an edge has been chosen */
	int *chosen;
	/* tells wether a node has been already reached by the MDST */
	int *mdst_reached;
 	/* An array of edge ptrs. Each ptr
 	 * denotes the edge visited during the MDST computation before the edge,
 	 * with a given aiid, i.e. the path successor of this edge in forward direction.
	 * If the edge belongs to a circle found during the MDST computation, this ptr
	 * points to the next circle edge. If the edges tgt node is a super node,
	 * the ptr points to the next edge on the same level, i.e. if the edge belongs
	 * not to a super node itself, the ptr points to the path successor in
	 * forward direction, if the edge belongs to a super node itself, the
	 * ptr points to the next circle edge. */
	ext_grs_edge_t **edge_path_succ;
 	/* An array of edge ptrs.
	 * If an edge with a given aiid is an outgoing edge of a super node build
	 * during the MDST computation the respective ptr points to the last edge
	 * of the circle on forward direction */
	ext_grs_edge_t **super_node_last_edge;
	/* needed in the actual plan generation, to remember which condition
	 * functions have already been placed in a matching op */
	lc_bitset_t *already_placed_conditions;
	/* needed in the actual plan generation, to remember which nodes have
	 * alredy been matvhed by a generated mop */
	lc_bitset_t *already_matched_nodes;
	/* needed in the actual plan generation, to remember which edges have
	 * alredy been matvhed by a generated mop */
	lc_bitset_t *already_matched_edges;
	/* tells wether a node with a given node id has been visited yet,
	 * if not the value in the array is '0', if yes the value is the
	 * number of the  */
	int *visited;
	/* an array of edge ptr, each pointing to a root of an edge path builded
	 * during the MDST computation */
	ext_grs_edge_t **edge_path_roots;
	/* number of edge path roots stored in that array */
	int n_edge_path_roots;

} ext_grs_vs_dmst_planer_private_t;



#define UF_FIND(e) (ext_grs_uf_find((e)+1) - 1)
#define UF_FIND_VALUE(e) (ext_grs_uf_find_value((e)+1))
#define UF_UNITE(r1,r2)  (ext_grs_uf_unite((r1)+1,(r2)+1) - 1)
#define UF_CHANGE_VALUE(r,a) (ext_grs_uf_change_value((r)+1,(a)))


/* auxilary function for debugging purposes */
static void dump_edge_path(ext_grs_vs_dmst_planer_private_t *p, ext_grs_edge_t *edge) {

	ext_grs_edge_t *current_edge = edge;
	assert(edge);

	printf("An edge path:\n");
	do
	{
		printf("  edge %s\n", current_edge->name);
		current_edge = p->edge_path_succ[current_edge->aiid];
	}
	while (current_edge != NULL && current_edge != edge);
	printf("\n\n");
}














/* auxiliary function needed in the expansion step of the MDST algorithm,
 * checks whether there is a reached node in a super node */
static int examine_super_node(
	ext_grs_vs_dmst_planer_private_t *p,
	ext_grs_edge_t *start_edge)
{
	ext_grs_edge_t *current_edge = NULL;
	int already_reached_node_found = 0;

	/* walk the circle forming the current super node */
	assert(start_edge && "NULL edge given");
	current_edge = start_edge;
	do {

		/* if the current edges real target node is already reached by an MDST edge,
		* report, that a reached node is found in the current super node */
		if ( p->mdst_reached[current_edge->arg->aiid] ) return 1;

		/* if the current edges target node is a super node,
		 * recursively examine that super node too */
		if (p->super_node_last_edge[current_edge->aiid]) {
			already_reached_node_found =
				examine_super_node(p,
					p->edge_path_succ[
						p->super_node_last_edge[current_edge->aiid]->aiid
					]
				);
		}
		if (already_reached_node_found) return 1;

		/* process the next edge of the circle */
		current_edge = p->edge_path_succ[current_edge->aiid];
		assert(current_edge && "in a circle the edge path should form a circle too");
	}
	while (current_edge != start_edge);

	/* report that no reached node has been found in the current super node */
	return 0;
}











/* auxiliary function needed in the expansion step of the MDST algorithm,
 * walks a super node and kills one specific edge */
static void expand_super_node(
	ext_grs_vs_dmst_planer_private_t *p,
	ext_grs_edge_t *start_edge)
{
	ext_grs_edge_t *current_edge;

	assert(start_edge);

	/* Walk the circle and choose every edge marking its target node as reached, except...
	 * ...the edge already has an already reached real target node OR
	 * ...the edges target node is a super node containing an already reached real node.
	 * If an edge has a super node as target node recursively expand that super node too. */
	current_edge = start_edge;
	do {

		/* tells whether the current edges target node is a super node
		 * containing an already reached real node */
		int super_node_contains_reached_node = 0;
		/* represents the start edge of a super node circle */
		ext_grs_edge_t *super_node_start_edge = NULL;
		/* represents the last edge of a super node circle */
		ext_grs_edge_t *super_node_end_edge = NULL;


		super_node_end_edge = p->super_node_last_edge[current_edge->aiid];
		if (super_node_end_edge)
			super_node_start_edge = p->edge_path_succ[ super_node_end_edge->aiid ];

		/* if the current edges target is a super node, check whether
		 * this super node contains an already reached real node */
		super_node_contains_reached_node = 0;
		if (super_node_start_edge) {
			super_node_contains_reached_node = examine_super_node(p, super_node_start_edge);
		}

		/* mark the current edge as chosen and its real target node as reached, except... (see above) */
		if (
			! p->mdst_reached[current_edge->arg->aiid] &&
			! super_node_contains_reached_node
		)
		{
			p->chosen[current_edge->aiid] = 1;
			p->mdst_reached[current_edge->arg->aiid] = 1;
		}

		/* if the current edges target node is a super node, then recursively expand it */
		if (super_node_start_edge) {
			expand_super_node(p, super_node_start_edge);
		}

		/* process the next edge of the super node circle */
		current_edge = p->edge_path_succ[current_edge->aiid];
		assert(current_edge);
	}
	while (current_edge != start_edge);
}















#if 0



/* auxiliary function needed in the expansion step of the MDST algorithm,
 * walks a super node where the edge to be killed is initially not known */
static void expand_super_node_examine(
	ext_grs_vs_dmst_planer_private_t *p,
	ext_grs_edge_t *start_edge)
{

	ext_grs_edge_t *current_edge = NULL;
	int edge_killed = 0;

	#ifdef EXT_GRS_DEBUG
		printf("Expanding a super node (examine). Start edge is %s\n", start_edge->name);
		printf("The circle of edges forming the super node: ");
	#endif


	/* walk the circle forming the current super node */
	assert(start_edge && "NULL edge given");
	edge_killed = 0;

	current_edge = start_edge;
	#ifdef EXT_GRS_DEBUG
		printf("%s ", current_edge->name);
	#endif
	do {

		/* if the current edges target node is a super node, expand that super node */
		if (p->super_node_last_edge[current_edge->aiid] != NULL) {

			#ifdef EXT_GRS_DEBUG
				printf(" Recursively stepping into super node:\n{\n");
			#endif

			expand_super_node_examine(p,
				p->edge_path_succ[
					p->super_node_last_edge[current_edge->aiid]->aiid
				]
			);

			#ifdef EXT_GRS_DEBUG
				printf("}\n");
			#endif
		}
		/* if the current edges real target node is already reached by an MDST edge,
		 * do NOT choose the current edge for the MDST but remember that there's already
		 * an edge in the current circle, which has not been chosen for the MDST */
		if ( p->mdst_reached[current_edge->arg->aiid] ) {
			/* all but exactly one edge of a circle have to be chosen for the MDST */
			assert( !edge_killed && "more than one circle edge not chosen for the MDST");
			/* remember that theres already is an edge not chosen */
			edge_killed = 1;
		}
		else {
			/* otherwise the current edge is chosen, except
			 * ... OR
			 * ...it is the last edge of the current circle and no edge edge has been not killed yet */
			if ( p->edge_path_succ[current_edge->aiid] != start_edge || edge_killed )
			{
				p->chosen[current_edge->aiid] = 1;
				p->mdst_reached[current_edge->arg->aiid] = 1;
			}

		}
		/* process the next edge of the circle */
		current_edge = p->edge_path_succ[current_edge->aiid];
		assert(current_edge && "in a circle the edge path should form a circle too");
		#ifdef EXT_GRS_DEBUG
			printf("%s ", current_edge->name);
		#endif
	}
	while (current_edge != start_edge);

	#ifdef EXT_GRS_DEBUG
		printf("\n");
	#endif
}





/* auxilary function needed in the expansion step of the MDST algorithm,
 * walks a super node where the given edge is the edge to be killed from
 * the MDST and the other edges are to be chosen for the MDST */
static void expand_super_node_killing(
	ext_grs_vs_dmst_planer_private_t *p,
	ext_grs_edge_t *kill_edge)
{
	ext_grs_edge_t *current_edge;

	#ifdef EXT_GRS_DEBUG
		printf("\nExpanding a super node (kill). ");
	#endif

	assert(kill_edge);

	#ifdef EXT_GRS_DEBUG
		printf("Kill-edge is %s\n", kill_edge->name);
		printf("The circle of edges forming the super node: ");
	#endif

	/* walk the circle for the first time:
	 * every edge except the edge to be killed is marked as chosen, the real
	 * target node of such an edge is to be marked es reached by the MDST */
	current_edge = p->edge_path_succ[kill_edge->aiid];
	assert(current_edge);
	#ifdef EXT_GRS_DEBUG
		printf("%s ", current_edge->name);
	#endif
	assert(current_edge != kill_edge); /* no loops allowed! */

	do {
		p->mdst_reached[current_edge->arg->aiid] = 1;
		p->chosen[current_edge->aiid] = 1;
		current_edge = p->edge_path_succ[current_edge->aiid];
		assert(current_edge);
		#ifdef EXT_GRS_DEBUG
			printf("%s ", current_edge->name);
		#endif
	}
	while (current_edge != kill_edge);

	#ifdef EXT_GRS_DEBUG
		printf("\n");
	#endif

	/* walk the circle for the second time...
	 * while every super node is expanded. Note that there is a different
	 * treatment of supernodes pointed to by the edge to be killed and by
	 * the other edges: two different functions are called */
	current_edge = p->edge_path_succ[kill_edge->aiid];
	assert(current_edge);
	assert(current_edge != kill_edge); /* no loops allowed! */

	do {
		if (p->super_node_last_edge[current_edge->aiid])
			expand_super_node_killing(p, p->super_node_last_edge[current_edge->aiid]);
		current_edge = p->edge_path_succ[current_edge->aiid];
	}
	while (current_edge != kill_edge);
	/* now the super node pointed to by the edge to be killed is expanded
	 * (if there is one) */
	if (p->super_node_last_edge[kill_edge->aiid])
		expand_super_node_examine(p,
			p->edge_path_succ[
				p->super_node_last_edge[kill_edge->aiid]->aiid
			]
		);
}

#endif



/* Place all conditions in the mop, which can now be computed
* and have not been computed yet. If there are more than one
* such conditions generate additional condition mops. */
static INLINE void emit_nodes_conditions(
	ext_grs_action_t *act, ext_grs_node_t *node,
	ext_grs_vs_dmst_planer_private_t *p,  ext_grs_match_op_t *prog, int *prog_pos)
{
	/* flag, tells whether there has already been placed a condition
	* in the current edges mop (so additional condition mops have
	* to be created) */
	int already_placed = 0;

	/* a condition index from a set of involved conditions */
	lc_bitset_pos_t cond = (act->nodes_refering_conds[node->aiid])->size;

	/* a matcher op */
	ext_grs_match_op_t *mop;

	if (act->n_conditions > 0) {
		lc_bitset_foreach(act->nodes_refering_conds[node->aiid], cond) {

			/* if current condition has already been placed in the plan
			* currently generated, then skip */
			if (lc_bitset_is_set(p->already_placed_conditions, cond)) continue;

			/* otherwise check, whether all involved nodes
			* and edges have already been matched  */
			if (
				! lc_bitset_contains(
				act->conds_involved_nodes[(int)cond],
				p->already_matched_nodes)
				) continue;

			/* otherwise place the condition in the mop or (if there has already
			* been placed one) create a condition mop. */
			if (already_placed) {
				int j = ++(*prog_pos);   // Create a condition mop;
				mop = & prog[j];
				mop->kind = ext_grs_k_mop_condition;
				mop->condition = act->conditions[(int)cond];
				lc_bitset_set(p->already_placed_conditions, cond);				// AS
			}
			else {
				mop = &prog[*prog_pos]; // AS
				mop->condition = act->conditions[(int)cond];
				already_placed = 1;
				lc_bitset_set(p->already_placed_conditions, cond);  // AS
			}
		}
	}
}



/* Place all conditions in the mop, which can now be computed
 * and have not been computed yet. If there are more than one
 * such conditions generate additional condition mops. */
static INLINE void emit_edges_conditions(
	ext_grs_action_t *act, ext_grs_edge_t *edge,
	ext_grs_vs_dmst_planer_private_t *p, ext_grs_match_op_t *prog, int *prog_pos)
{
	/* flag, tells whether there has already been placed a condition
	 * in the current edges mop (so additional condition mops have
	 * to be created) */
	int already_placed = 0;

	/* a condition index from a set of involved conditions */
	lc_bitset_pos_t cond;

	/* a matcher op */
	ext_grs_match_op_t *mop;

	if (act->n_conditions > 0) {

		// Place conditions for source and target of the edge, if they have not been
		// placed yet while emitting the "node operation".
		if(edge->func != NULL)												// AS
			emit_nodes_conditions(act, edge->func, p, prog, prog_pos);		// AS
		if(edge->arg != NULL)												// AS
			emit_nodes_conditions(act, edge->arg, p, prog, prog_pos);		// AS

		lc_bitset_foreach(act->edges_refering_conds[edge->aiid], cond) {
			/* if current condition has already been placed in the plan
			 * currently generated, then skip */
			if (lc_bitset_is_set(p->already_placed_conditions, cond)) continue;
			/* otherwise check, whether all involved nodes
			 * and edges have already been matched  */
			if (
				! lc_bitset_contains(
					act->conds_involved_nodes[(int)cond],
					p->already_matched_nodes)
			) continue;
			/* otherwise check, whether all involved edges
			 * and edges have already been matched  */
			if (
				! lc_bitset_contains(
					act->conds_involved_edges[(int)cond],
					p->already_matched_edges)
			) continue;
			/* otherwise place the condition in the mop or (if there has already
			 * been placed one) create a condition mop. */
			if (already_placed) {
				int j = ++(*prog_pos);
				mop = & prog[j];
				mop->kind = ext_grs_k_mop_condition;
				mop->condition = act->conditions[(int)cond];
				lc_bitset_set(p->already_placed_conditions, cond); // AS
			}
			else {
				mop = &prog[*prog_pos];  // AS
				mop->condition = act->conditions[(int)cond];
				already_placed = 1;
				lc_bitset_set(p->already_placed_conditions, cond); // AS
			}
		}
	}
}



static void emit_mop(
	ext_grs_vs_dmst_planer_private_t *p,
	ext_grs_action_t *act, ext_grs_match_op_t *prog,
	ext_grs_edge_t *edge,  int *prog_pos)
{

	/* a matcher op */
	ext_grs_match_op_t *mop;
	int j = *prog_pos;

	mop = &prog[j];

	/* write the respective node or edge into the mop */
	if (edge->kind == ext_grs_k_edge) {
		ext_grs_edge_t *back_edge;

		mop->kind = ext_grs_k_mop_edge;
		mop->edge = edge;
		mop->node = edge->func;

		mop->edge->first_mop_index = j;

		/* mark the pseudo edge representing the backward
		 * traversal of the edge also as chosen such that
		 * it can not be used for the generation of matcher
		 * ops anymore, or if already chosen, remove it
		 * from the priority queue of allowed edges */
		back_edge = edge->backward_edge;
		if (p->chosen[back_edge->aiid] == 2) {
			ext_grs_mpq_delete(back_edge);
		}
		else {
			p->chosen[edge->backward_edge->aiid] = 2;
		}

		/* mark the genuine node as already been matched */
		lc_bitset_set(p->already_matched_edges, edge->aiid);
		/* mark the genuine node as already been matched */
		lc_bitset_set(p->already_matched_nodes, edge->arg->aiid);

		emit_edges_conditions(act, edge, p, prog, prog_pos);
	}
	else {
		ext_grs_elem_t *genuine_elem;

		assert(edge->kind == ext_grs_k_pseudo_edge && "invalid kind");
		/* get the genuine elem */
		genuine_elem = & edge->genuine_elem;
		if (genuine_elem->kind == ext_grs_k_edge) {
			/* pseudo edge represents a reverse edge matching */
			ext_grs_edge_t *genuine_edge = genuine_elem->val.e;

			if(genuine_edge->related_edge != NULL)
				mop->kind = ext_grs_k_mop_preset_edge;
			else
				mop->kind = ext_grs_k_mop_edge;
			mop->edge = genuine_edge;
			mop->node = genuine_edge->arg;

			mop->edge->first_mop_index = j;

			/* mark the actual edge belonging to the
			 * pseudo edge representing the backward
			 * traversal of the that actual edge also as chosen
			 * such that it can not be used for the generation
			 * of matcher ops anymore, or if already chosen,
			 * remove it from the priority queue of allowed edges */
			if (p->chosen[genuine_edge->aiid] == 2) {
				ext_grs_mpq_delete(genuine_edge);
			}
			else {
				p->chosen[genuine_edge->aiid] = 2;
			}

			/* mark the genuine node as already been matched */
			lc_bitset_set(p->already_matched_edges, genuine_edge->aiid);
			/* mark the genuine node as already been matched */
			lc_bitset_set(p->already_matched_nodes, genuine_edge->func->aiid);

			emit_edges_conditions(act, genuine_edge, p, prog, prog_pos);

		}
		else {
			/* pseudo edge represents a node matching */
			ext_grs_node_t *genuine_node = genuine_elem->val.n;

			assert(genuine_elem->kind == ext_grs_k_node && "invalid kind");
			assert(genuine_node == edge->arg);

			if(genuine_node->related_node != NULL)
				mop->kind = ext_grs_k_mop_preset;
			else
				mop->kind = ext_grs_k_mop_node;
			mop->edge = NULL;
			mop->node = genuine_node;

			if (genuine_node->op_condition)
				mop->op_condition = genuine_node->op_condition;
			if (genuine_node->mode_condition)
				mop->mode_condition = genuine_node->mode_condition;

			/* mark the genuine node as already been matched */
			lc_bitset_set(p->already_matched_nodes, genuine_node->aiid);

			emit_nodes_conditions(act, genuine_node, p, prog, prog_pos);
		}
	}
}

#if 0
static ext_grs_match_op_t **emit_negative_plans(ext_grs_action_t *act)
{
	lc_list_t *pos;
	/* Compute a search plan for all negative patterns */
	/* Count number of negative patterns */
	int num_negatives = 0, i;
	ext_grs_match_op_t **result;



	lc_list_for_each(pos, (&act->negatives))
		num_negatives++;

	result = (ext_grs_match_op_t **) malloc(num_negatives * sizeof(ext_grs_match_op_t *));

	i = 0;
	/* Handle all negative patterns, there can be several */
	lc_list_for_each(pos, (&act->negatives))
	{
		struct ext_grs_graph_t *neg_pattern;

		result[i] = (ext_grs_match_op_t *) malloc((act->n_nodes + act->n_edges + act->n_conditions) * sizeof(ext_grs_match_op_t));
		neg_pattern = lc_list_entry(pos, ext_grs_graph_t, negative_list);

		/* TODO: Now walk through that graph and create search plan */


		i++;
	}
	return(result);
}
#endif


/* ---------------------------------------------------------
   Allocate the memory for the plan object and for the array
   representing the match program all at once
   --------------------------------------------------------- */
static ext_grs_match_plan_t *alloc_plan(ext_grs_action_t *act)
{
	ext_grs_match_plan_t *plan;
	lc_list_t *neg_patterns;
	lc_list_t *list_pos;
	int num_negatives = 0, num_all_plans;
	int i;

	plan = malloc(sizeof(*plan));
	memset(plan, 0, sizeof(*plan));
	plan->action = act;


	/* Compute a search plan for all negative patterns */

	/* Count number of negative patterns */
	lc_list_for_each(list_pos, (&act->negatives))
		num_negatives++;
	num_all_plans = num_negatives + 1;

	plan->progs = (ext_grs_match_op_t **) malloc(num_all_plans * sizeof(ext_grs_match_op_t *));

	/* Alloc mem for all search plans, 1 positive and n negative */
	plan->progs[0] = (ext_grs_match_op_t *) malloc((act->n_nodes + act->n_edges + act->n_neg_nodes + act->n_neg_edges + act->n_conditions) * sizeof(ext_grs_match_op_t));
	memset(plan->progs[0], 0, (act->n_nodes + act->n_edges + act->n_neg_nodes + act->n_neg_edges + act->n_conditions) * sizeof(ext_grs_match_op_t));


	neg_patterns = &act->negatives;
	neg_patterns = neg_patterns->next;
	for(i = 1; i < num_all_plans; i++)
	{
		ext_grs_graph_t *pat = (ext_grs_graph_t *) lc_list_entry(neg_patterns, ext_grs_graph_t, negative_list);
		plan->progs[i] = (ext_grs_match_op_t *) malloc((act->n_nodes + act->n_edges + act->n_neg_nodes + act->n_neg_edges + act->n_conditions) * sizeof(ext_grs_match_op_t));
		memset(plan->progs[i], 0, (act->n_nodes + act->n_edges + act->n_neg_nodes + act->n_neg_edges + act->n_conditions) * sizeof(ext_grs_match_op_t));
		neg_patterns = neg_patterns->next;
	}


	plan -> prog = plan -> progs[0];

	plan -> length = (int *) malloc(num_all_plans * sizeof(int));
	memset(plan -> length, 0, num_all_plans * sizeof(int));

	plan->num_progs = num_all_plans;

	return(plan);
}



/*
 * Generation of the matching plan usind the precoomputed MDST.
 * HERE WE GO: comming from the root chose one edge after another as the
 *             next search plan edge by a modified Best-First manner:
 *             An edge can only be chosen iff
 *              - if it is an pseudo or benuine OUT edge AND
 *              - at least one of its incident nodes has already
 *                been visited by the final plan generation AND
 *              - it is part of the computed MDSR or BOTH of
 *                its incident nodes have already been chosen
 * This is done by one big Priority queue were all edges
 * are put in the moment they become allowed to be chosen */
static int
emit_plan( ext_grs_match_op_t *prog /*ext_grs_match_plan_t *plan*/, ext_grs_graph_t *pattern /*ext_grs_action_t *act*/, ext_grs_vs_dmst_planer_private_t *p, int phase)
{
	ext_grs_action_t *act = pattern->action;
	ext_grs_node_t *current_node;
	int j = 0;

	/* create the pq containing the allowed edges */
	/* a priority queue for edges used after MDST computation in
	 * the final plan generation step  */
	ext_grs_mpq_t *edge_queue = ext_grs_mpq_create();
	/* initialize the uf structure. This is because after the init
	 * a zero cost modification of incoming edges is associated to
	 * every pattern node. This MUST be done, because the cost
	 * modification stored on the if structure are used by the priority
	 * queue by to compute the actual cost of an edge. So if these
	 * modifications are NOT zero the order in the queue will be WRONG!!! */
	ext_grs_uf_init(act->max_node_aiid + 1);


	/* the current pos in the generated match program */


	/* START from the patterns root */
	current_node = &pattern->root_node;
	while (1) {
		lc_list_t *pos;
		ext_grs_edge_t *cheapest_edge;
		ext_grs_match_op_t *mop;

		/* put all out (that is in-flowing) edges in pq that are allowed
		 * to be matched by the fact, that they belong to the MDST */
		lc_list_for_each(pos, & current_node->edges[ext_grs_in]) {

			/* ATTENTION: if ported to GrGen these 'in' indeces have
			 *            to be changed to 'out' !!!! */
			ext_grs_edge_t *some_edge =
				lc_list_entry(pos, ext_grs_edge_t, list[ext_grs_in]);

			if (
				/* some edge belongs to the MDST, but no match opereation
				 * has been generated yet */
				p->chosen[some_edge->aiid] == 1
			)
			{
				ext_grs_mpq_insert(edge_queue, some_edge);
				/* mark edge as processed
				 * (so it will not be inserted in the queue again) */
				p->chosen[some_edge->aiid] = 2;
			}
		}
		/* put all out (that is in-flowing) AND in (that is out-flowing)
		 * edges in pq that are allowed to be matched by the fact,
		 * that both of their incident nodes have already been matched.
		 * The in edges are added too, because the matching of the REAL
		 * edge coming from the (already matched) node at the other end
		 * of the plan graph edge may be cheaper */
		lc_list_for_each(pos, & current_node->edges[ext_grs_in]) {

			/* ATTENTION: if ported to GrGen these 'in' indeces have
			 *            to be changed to 'out' !!!! */
			ext_grs_edge_t *some_edge =
				lc_list_entry(pos, ext_grs_edge_t, list[ext_grs_in]);

			if (
				/* both incident nodes of some edge have arealready been
				 * processed by the final plan genration phase, but no match
				 * operation has been generated yet */
				(p->chosen[some_edge->aiid] == 0 && p->visited[some_edge->arg->aiid] > phase)
			)
			{
				ext_grs_mpq_insert(edge_queue, some_edge);
				/* mark edge as processed
				 * (so it will not be inserted in the queue again) */
				p->chosen[some_edge->aiid] = 2;


				/* also insert the backward edge */
				/* ...in case of a real edge its in fact the backward edge */
				if (some_edge->kind == ext_grs_k_edge) {
					ext_grs_mpq_insert(edge_queue, some_edge->backward_edge);
					p->chosen[some_edge->backward_edge->aiid] = 2;
				}
				/* ...otherwise its the genuine edge */
				else {
					assert (some_edge->kind == ext_grs_k_pseudo_edge && "invalid kind");
					if (some_edge->genuine_elem.kind == ext_grs_k_edge) {
						ext_grs_edge_t *genuine_edge = some_edge->genuine_elem.val.e;
						ext_grs_mpq_insert(edge_queue, genuine_edge);
						p->chosen[genuine_edge->aiid] = 2;
					}
					else {
						/* just check for wrong kind and do nothing */
						assert (some_edge->genuine_elem.kind == ext_grs_k_node && "invalid kind");
					}
				}
			}
		}

		/* get the cheapest edge */
		cheapest_edge = ext_grs_mpq_delete_min(edge_queue);
		if (cheapest_edge == NULL) break;/* if all edges processed... */

		assert(j < (act->n_nodes + act->n_edges + act->n_neg_edges + act->n_neg_nodes + act->n_conditions) && "prog position out of bounds");

		/* add new match op to the plan */
		mop = & prog[j];

		if (p->visited[cheapest_edge->arg->aiid] <= phase)
			cheapest_edge->arg->first_mop_index = j;

		/* mark the target node of the edge as visited */
		p->visited[cheapest_edge->arg->aiid] = phase + 1;


		emit_mop(p, act, prog, cheapest_edge, &j);

		/* increment the pos in the match program */
		j++;
		/* increment the length of the search plan */
		/*plan->length++; */
		/* the next node to insert allowed incident out edges in the queue */
		current_node = cheapest_edge->arg;
	}
	return (j);
}

/* annotate the edges of the plan graph with edge costs due to the
 * result of a previous v-structure analysis */
static void annotate_edge_costs(
	ext_grs_graph_t *pattern /*ext_grs_action_t *act*/, ext_grs_irg_private_t *pr_g, ext_grs_vs_stat_t *vs_stat, int is_negative)
{

	/*int i; */

	/* annotate all real, negative and pseudo edges with edge costs
	 * additionally do some init stuff with each edge */
	lc_list_t *pos;

	/* Go through all edges of this pattern */
	lc_list_for_each(pos, &pattern->edges)
	{
	/*for (i = 0; i <= act->max_edge_aiid; i++) { */

		ext_grs_edge_t *edge = lc_list_entry(pos, ext_grs_edge_t, edge_list); /*act->edges[i]; */

		ir_opcode opc_arg;
		modecode mc_arg;
		ir_opcode opc_func;
		modecode mc_func;
		ir_opcode opc;
		modecode mc;

		int l;

		/* there might be gaps in the array of edges */
		/*if (!edge) continue; */
		assert(edge != NULL && "Edge was NULL!");

		/* for regular and pseudo edges annotate the v-structure costs */
		if (edge->kind == ext_grs_k_edge) {
			double n_vs;
			int n_inst = 0;


			opc_arg = get_op_code(edge->arg->op);
			mc_arg = get_mode_modecode(edge->arg->mode);
			opc_func = get_op_code(edge->func->op);
			mc_func = get_mode_modecode(edge->func->mode);



			/* get v-structures of the function node */

			/* compute the number of instnaces present for the
			 * func node (including all sub ops) */
			for (l = 0; l < ARR_LEN(_ext_grs_all_sub_ops_of_op[opc_func]); l++) {
				ir_opcode super_opc_func = get_op_code(_ext_grs_all_sub_ops_of_op[opc_func][l]);
				n_inst += _ext_grs_N_INSTANCES(pr_g, super_opc_func, mc_func);
			}

			edge->cost = 0.0;
			if (edge->pos == ext_grs_NO_EDGE_POS)
				if (n_inst != 0) {
					n_vs = _get_n_v_structures(
								vs_stat, opc_func, mc_func, opc_arg, mc_arg, ext_grs_in);
					edge->cost = (n_vs / (double)n_inst) + (double)is_negative;
				}
		}

		/* for pseudo edges... */
		else {
			ext_grs_elem_t *elem;
			assert(edge->kind == ext_grs_k_pseudo_edge && "invalid kind found");

			/* ...find out, whether they represent a node matching or the
			 * following of an edge in reverse direction */
			elem = & edge->genuine_elem;
			/* to do that, check the kind tag, which is stored first */
			if ( elem->kind == ext_grs_k_node) {
				double log_n_instances;
				int n_inst = 0;

				opc = get_op_code(edge->arg->op);
				mc = get_mode_modecode(edge->arg->mode);
				/* the genuine elem the pseudo edge belongs to is a node... */
				/* thus the the costs are the log2 of the number of instances */

				/* compute the number of instances present for the
				 * node (including all sub ops) */
				for (l = 0; l < ARR_LEN(_ext_grs_all_sub_ops_of_op[opc]); l++) {
					ir_opcode super_opc = get_op_code(_ext_grs_all_sub_ops_of_op[opc][l]);
					n_inst += _ext_grs_N_INSTANCES(pr_g, super_opc, mc);
				}

				if (n_inst > 0)
					log_n_instances = _log2(n_inst);
				else log_n_instances = 0.0;
				edge->cost = log_n_instances + (double)is_negative;

				/* check (for negative graphs) whether the node is related to
				   a positive node; if so, set the costs of the pseudo edge to zero. */
				if (is_negative)
					if (edge->arg->related_node)
						edge->cost = 0.0;
			}
			else {
				double n_vs;
				int n_inst = 0;
				opc_arg = get_op_code(edge->arg->op);
				mc_arg = get_mode_modecode(edge->arg->mode);
				opc_func = get_op_code(edge->func->op);
				mc_func = get_mode_modecode(edge->func->mode);

				/* the genuine elem the pseudo edge belongs to is an edge... */
				assert(elem->kind == ext_grs_k_edge && "pattern elem of a wrong kind");

				/* get v-structures of the argument node */
				n_vs = _get_n_v_structures(
						vs_stat, opc_func, mc_func, opc_arg, mc_arg, ext_grs_out);

				/* compute the number of instances present for the
				 * func node (including all sub ops) */
				for (l = 0; l < ARR_LEN(_ext_grs_all_sub_ops_of_op[opc_func]); l++) {
					ir_opcode super_opc_func = get_op_code(_ext_grs_all_sub_ops_of_op[opc_func][l]);
					n_inst += _ext_grs_N_INSTANCES(pr_g, super_opc_func, mc_func/* MAYBE WRONG!!! */ );
				}

				if (n_inst != 0)
					edge->cost = n_vs / (double)n_inst;
				else
					edge->cost = 0.0;

				edge->cost += (double)is_negative;
			}
		}
	}
}

static void prepare_mdst_contraction_phase(
	/*ext_grs_action_t *act*/ext_grs_graph_t *pattern, lc_list_t *not_visited_nodes, ext_grs_mpq_t **pq)
{
	lc_list_t *pattern_pos;
	int i;

	/* for all pattern nodes v do:
	 * 		- create a pq(v), put all the nodes edges in pq(v)
	 *      - add v to the list of not yet processed nodes */
	LC_INIT_LIST_HEAD(not_visited_nodes);

	lc_list_for_each(pattern_pos, &pattern->nodes)
	{
	/*for (i = 0; i <= act->max_node_aiid; i++) {*/

		lc_list_t *pos;


		ext_grs_node_t *current_node = lc_list_entry(pattern_pos, ext_grs_node_t, node_list); /*act->nodes[i];*/
		i = current_node->aiid;

		/* there might be gaps in the array of nodes */
		/*if (!current_node)
			continue;*/

		assert(current_node != 0 && "NULL Node in node List!!");

		/*assert(current_node->aiid == i && "found node with an inconsistent action internal id"); */
		/* create a new pq for each pattern node */
		pq[i] = ext_grs_mpq_create();
		/* fill the new pq with all graph theoretical incoming
		 * edges of the node, i.e. with all out-flowing edges */
		lc_list_for_each(pos, & current_node->edges[ext_grs_out]) {
			ext_grs_edge_t *current_edge =
				lc_list_entry(pos, ext_grs_edge_t, list[ext_grs_out]);
			ext_grs_mpq_insert(pq[i], current_edge);
		}

		/* add the current node to a list of nodes not yet
		 * processed by Edmonds Algor. */
		lc_list_add(& current_node->not_visited_list, not_visited_nodes);
	}
}

static ext_grs_edge_t *
process_found_circle(
	ext_grs_graph_t *pattern, ext_grs_vs_dmst_planer_private_t *p, ext_grs_mpq_t **pq,
	ext_grs_node_t *start_node, int phase)
{
	ext_grs_action_t *act = pattern->action;
	ext_grs_node_t *current_node = start_node;

	/* the current circle node */
	ext_grs_node_t *circle_node;

	ext_grs_edge_t *cheapest_edge;
	ext_grs_edge_t *current_edge;
	int node_id;


	/* an auxilary pq item */
	ext_grs_mpq_t *meld_pq;

	/* the first edge of the edges forming the circle, the last edge forming the
	 * circle (both seen in edge direction) and the edge processed before processing
	 * the last edge */
	ext_grs_edge_t *first_circle_edge, *last_circle_edge, *from_edge;

	/* cheapest edge of the new made super node, it is incident
	 * to the same node as kill edge */
	ext_grs_edge_t *continue_edge;
	/* the repsective incident incomming circle edge */
	ext_grs_edge_t *current_end_edge;

	/* check for a consistent uf state */
	assert(p->visited[current_node->aiid] == phase && "visited by a future phase?!");



	/* walk the circle and unite all (super)nodes uf sets. By uniting
	 * the uf structure a second walk through the circle can find out
	 * the cheapest edge entering the super node formed by the
	 * current circle, such that this cheapest edge is not a loop edge. */
/*
#ifdef EXT_GRS_DEBUG
	printf("Walk circle the 1st time. The processed edges are:\n");
#endif
 */
	cheapest_edge = ext_grs_mpq_delete_min(pq[current_node->aiid]);
	assert(cheapest_edge && "there should have been a circle edge");

/*
	#ifdef EXT_GRS_DEBUG
		printf("  %s\n", cheapest_edge->name);
	#endif
 */

	/* the first of the processed cheapest edges is the last circle edge
	 * followinf the edge direction (as the last of these edge is the
	 * first circle edge in edge direction) */
	last_circle_edge = cheapest_edge;
	/* the edge processed before processing the last circle edge for
	 * the first time */
	from_edge = p->edge_path_succ[last_circle_edge->aiid];

	/* store the pq of the cheapest edges target (super)node temporarily
	 * in the real target node of the cheapest edge */
	pq[cheapest_edge->arg->aiid] = pq[current_node->aiid];

	/* get the minimal cost edges source (super) node */
	node_id = UF_FIND(cheapest_edge->func->aiid);
	assert(node_id <= act->max_node_aiid && "node id found in uf structure to large");
	circle_node = act->nodes[node_id];

	/* check wether all circle nodes have been visited in the current phase */
	assert(p->visited[circle_node->aiid] == phase &&
		"all nodes of a circle should have been visited in the same phase");

	/* store modification of incoming edge costs in node */
	UF_CHANGE_VALUE(
		current_node->aiid,
		cheapest_edge->cost - UF_FIND_VALUE(cheapest_edge->arg->aiid)
	);

	while (circle_node != current_node) { /* walk the hole circle */

		/* get a minimum cost edge of some_node, this edge must be the
		 * first edge of the circle because the priviously chosen one
		 * remains to be the first elem of the double linked lists */
		cheapest_edge = ext_grs_mpq_delete_min(pq[circle_node->aiid]);
/*
		#ifdef EXT_GRS_DEBUG
			printf("  %s\n", cheapest_edge->name);
		#endif
 */
		/* check: there is a cheapest edge and as a circle edge it
		 * has to be a chosen one */
		assert(cheapest_edge && "there should have been a circle edge");

		/* store the pq of the cheapest edges target (super)node temporarily
		 * in the real target node of the cheapest edge */
		pq[cheapest_edge->arg->aiid] = pq[circle_node->aiid];

		/* store the change of the costs of the incoming edges of
		 * the current circle node as value in the nodes uf structure */
		UF_CHANGE_VALUE(
			circle_node->aiid,
			cheapest_edge->cost - UF_FIND_VALUE(cheapest_edge->arg->aiid)
		);

		/* unite circle nodes to a super node */
		node_id = UF_UNITE(circle_node->aiid, current_node->aiid);
		/* ensure that the next node is united super node */
		current_node = act->nodes[node_id];

		/* get ready for the next iteration step... */
		/* ...and the current one to the tgt node of the cheapest edge */
		node_id = UF_FIND(cheapest_edge->func->aiid);
		assert(node_id < act->n_nodes + act->n_neg_nodes + act->n_pseudo_nodes && "node id found in uf structure to large");
		circle_node = act->nodes[node_id];

		/* check wether all circle nodes have been visited in the current phase */
		assert(p->visited[circle_node->aiid] == phase &&
			"all nodes of a circle should have been visited in the same phase");

	}


	/* the last of the processed cheapest edges is the first circle edge
	 * following the edge direction (as the first of these edge is the
	 * last circle edge in edge direction) */
	first_circle_edge = cheapest_edge;

	p->edge_path_succ[last_circle_edge->aiid] = first_circle_edge;




	/* walk the circle again, this time using the stored edge path.
	 * while walking the circle look for the cheapest incomming edge
	 * which is not a loop. The loop property can be checked using
	 * the already united uf sets representing the (super)nodes */
/*
	#ifdef EXT_GRS_DEBUG
		printf("Walk circle the 2nd time. The processed edges are:\n");
	#endif
 */

	meld_pq = pq[last_circle_edge->arg->aiid];
	current_edge = first_circle_edge;

	continue_edge = ext_grs_mpq_find_min( meld_pq );
	assert (continue_edge && "there's always an edge from the root node");
	while (
		UF_FIND(continue_edge->func->aiid) == current_node->aiid  &&
		continue_edge->func != &pattern->root_node
	)
	{
		/* a loop edge has to be deleted... */
		ext_grs_mpq_delete_min( meld_pq );
		/* ...and a new minimal one has to be chosen */
		continue_edge = ext_grs_mpq_find_min( meld_pq );
		assert (continue_edge != NULL && "there's always an edge from the root node");
	}
	current_end_edge = last_circle_edge;


	while (current_edge != last_circle_edge) {

		ext_grs_edge_t *some_edge = continue_edge;

		meld_pq =  ext_grs_mpq_meld(pq[current_edge->arg->aiid], meld_pq);

		continue_edge = ext_grs_mpq_find_min( meld_pq );
		assert (continue_edge && "there's always an edge from the root node");
		while (
			UF_FIND(continue_edge->func->aiid) == current_node->aiid  &&
			continue_edge->func != &pattern->root_node
		)
		{
			/* a loop edge has to be deleted... */
			ext_grs_mpq_delete_min( meld_pq );
			/* ...and a new minimal one has to be chosen */
			continue_edge = ext_grs_mpq_find_min( meld_pq );
			assert (continue_edge != NULL && "there's always an edge from the root node");
		}
		/* if continue_edge has changed... */
		if (some_edge != continue_edge) {
			some_edge = continue_edge;
			current_end_edge = current_edge;
		}

		current_edge = p->edge_path_succ[current_edge->aiid];
/*
		#ifdef EXT_GRS_DEBUG
			printf("  %s\n", current_edge->name);
		#endif
 */

	}

	/* store the melded pq as new pq of the super node in the representative
	 * node of the super node. */
	pq[current_node->aiid] = meld_pq;





	p->edge_path_succ[continue_edge->aiid] = from_edge;
	p->super_node_last_edge[continue_edge->aiid] = current_end_edge;


	/* continue with cheapest edge leaving the circle */
	return continue_edge;
}


/* First phase of the MDST computation: contract all circles in
 * the plangraph to super nodes */
static int mdst_contraction_phase(ext_grs_graph_t *pattern, ext_grs_vs_dmst_planer_private_t *p)
{

	ext_grs_action_t *act = pattern->action;
	/* a list of nodes not yet processed by Edmonds Algo */
	lc_list_t not_visited_nodes;
	/* needed for walking the list of unprocessed nodes */
	lc_list_t *pos;

	/* an array of meldable priority queues. Each mpq
	 * will contain the edges of the node the id of which equals the
	 * index in the mpq array. The edge costs will serve as key. */
	ext_grs_mpq_t **pq = alloca((act->max_node_aiid + 1) * sizeof(*pq));
	/* the current pattern node */
	ext_grs_node_t *current_node;
	/* visit phases of the contraction phase */
	int phase = 1;


	memset(pq, 0, (act->max_node_aiid + 1) * sizeof(*pq));


	prepare_mdst_contraction_phase(pattern, & not_visited_nodes, pq);


	#ifdef EXT_GRS_DEBUG
		printf(">>>> The MDST contraction phase: <<<<\n");
	#endif



	while (! lc_list_empty(& not_visited_nodes))
	{

		/* a minimum cost edge */
		ext_grs_edge_t *cheapest_edge;
		/* be minimum cost edge processed in the last step */
		ext_grs_edge_t *previous_edge = NULL;

		pos = not_visited_nodes.next;


		current_node = lc_list_entry(pos, ext_grs_node_t, not_visited_list);
		#ifdef EXT_GRS_DEBUG
			printf("\nTaking a not yet visited node: %s\nChoosing edges: ", current_node->name);
		#endif

		/* mark the current node as visited (with the number of the
		 * corresponding visit phase)... */
		p->visited[current_node->aiid] = phase;
		/* del the current node from the list of unvisited nodes */
		lc_list_del( & current_node->not_visited_list);

		cheapest_edge = ext_grs_mpq_find_min(pq[current_node->aiid]);
		p->edge_path_succ[cheapest_edge->aiid] = NULL;

		#ifdef EXT_GRS_DEBUG
			printf("\n%s ", cheapest_edge->name);
		#endif


		/* Edmonds Algo unites nodes to super nodes, thus the number
		 * of nodes decreases, further more the root node has no incomming
		 * (that is out-flow) edges, thus in that case one has to start with
		 * another not yet visited node */
		/* note that root node has no in-edges (that is out-flow edges) */
		while (1) {

			/* an auxilary id */
			int node_id;

			assert (cheapest_edge != NULL && "every node except the root has an incomming edge");

			/* if tgt node of the cheapest edge is the root,
			 * try another not yet visited node */
			if (cheapest_edge->func == & pattern->root_node) {
				p->edge_path_roots[p->n_edge_path_roots++] = cheapest_edge;
				break;
			}

			/* find the next current node to choose a cheapest incoming edge from */
			node_id = UF_FIND(cheapest_edge->func->aiid);
			assert (node_id < act->n_nodes + act->n_neg_nodes + act->n_pseudo_nodes && "node id found in uf structure to large");
			current_node = act->nodes[node_id];

			/* check wether the src node of the minimal cost edge has already been
			 * visited, and if so wether this has happend in a previous or in current
			 * visit phase */
			if (p->visited[current_node->aiid] == 0) {/* not visited yet... */

				previous_edge = cheapest_edge;

				cheapest_edge = /* ...so get the next cheapest edge... */
					ext_grs_mpq_find_min(pq[current_node->aiid]);

				#ifdef EXT_GRS_DEBUG
					printf("%s ", cheapest_edge->name);
				#endif

				p->edge_path_succ[cheapest_edge->aiid] = previous_edge;

				/* mark current node as visited by the current visit phase */
				p->visited[current_node->aiid] = phase;
				/* remove the node from the list of unvisited nodes */
				lc_list_del( & current_node->not_visited_list);
				continue; /* ...and go on further */
			}
			else if (current_node->aiid == UF_FIND(cheapest_edge->arg->aiid)) { /* a loop edge found */
				/* remove every found loop from the (super)nodes pq */
				ext_grs_mpq_delete_min(pq[current_node->aiid]);

				#ifdef EXT_GRS_DEBUG
					printf("Loop!");
				#endif

				cheapest_edge = ext_grs_mpq_find_min(pq[current_node->aiid]);

				#ifdef EXT_GRS_DEBUG
					printf("%s ", cheapest_edge->name);
				#endif

				p->edge_path_succ[cheapest_edge->aiid] = previous_edge;
				continue;
			}
			else if (p->visited[current_node->aiid] < phase) { /* visited in a previous phase... */
				p->edge_path_roots[p->n_edge_path_roots++] = cheapest_edge;
				break; /* ...so start from a nother not yet visited node */
			}
			else {

				#ifdef EXT_GRS_DEBUG
					printf("Circle! ");
				#endif

				/* a circle has been formed */
				cheapest_edge = process_found_circle(pattern, p, pq, current_node, phase);

				#ifdef EXT_GRS_DEBUG
					printf("Cheapest outgoing edge: %s\n", cheapest_edge->name);
				#endif

				previous_edge = p->edge_path_succ[cheapest_edge->aiid];

			}
		}
		/* next visit phase has a greater marking number, that means that
		 * we can decide in O(1) wether this node belongs to a circle
		 * or has reverse a path to the root node of the current action */
		phase++;
	}

	return phase;
}


/* Second phase of the MDST computation: expand the condensed super nodes
 * and choose an appropriate entering edge of each super node expanded */
static void mdst_expansion_phase(ext_grs_vs_dmst_planer_private_t *p) {

	int i;
	ext_grs_edge_t *current_edge;

	#ifdef EXT_GRS_DEBUG
		printf("\n>>>> The MDST expansion phase: <<<<\n");
		printf("Top level: ");
	#endif


	/* for all edge paths found, do... */
	for (i = 0; i < p->n_edge_path_roots; i++) {

		ext_grs_edge_t *end_edge;
		ext_grs_edge_t *start_edge;

		/* walk the hole edge path till it ends */
		current_edge = p->edge_path_roots[i];
		while (current_edge != NULL) {

			assert(p->mdst_reached[current_edge->arg->aiid] == 0 &&
				"there should be no circles on top level");

			#ifdef EXT_GRS_DEBUG
				printf("%s ", current_edge->name);
			#endif


			/* follow the edge path until the target node of the current
			 * edge is a super node and mark the real target nodes of the
			 * edge path as already reached by an MDST edge (this is because
			 * on the top level the edge do NOT belong to a super node, thus
			 * all super nodes on the current edge path have exactly ONE ocomming
			 * edge, and this edge must be chosen for the MDST!) */
			p->chosen[current_edge->aiid] = 1;
			p->mdst_reached[current_edge->arg->aiid] = 1;

			/* if the current edges target node is a super node, expand this super node! */
			end_edge = p->super_node_last_edge[current_edge->aiid];
			if (end_edge) {
				start_edge = p->edge_path_succ[end_edge->aiid];
				expand_super_node(p, start_edge);
			}

			/* process the next edge on the current edge path */
			current_edge = p->edge_path_succ[current_edge->aiid];
		}

	}
}




static void init(ext_grs_planer_t *planer, ext_grs_analyzer_t *alz) {
	planer->analyzer = alz;
}




static ext_grs_match_plan_t *compute_plan(
	ext_grs_planer_t *planer, ir_graph *irg, ext_grs_action_t *act)
{

	lc_list_t *neg_pattern;
	/* the private data area of the planer */
	ext_grs_vs_dmst_planer_private_t *p = (ext_grs_vs_dmst_planer_private_t *) planer->data;
	/* the plan to be returned */
	ext_grs_match_plan_t *plan;
	int i;

	/* keeps the number of the visit phase of the Edmonds MDST impl */
	int phase;


	/* Alloc memory for plan object and positive and negative search plan progs */
	plan = alloc_plan(act);



	/* init bitsets needed to store information needed in emitting condition  */
	p->already_placed_conditions = lc_bitset_alloca(act->n_conditions);
	p->already_matched_nodes =  lc_bitset_alloca(act->max_node_aiid+1);
	p->already_matched_edges =  lc_bitset_alloca(act->max_edge_aiid+1);

	p->edge_path_succ = alloca((act->max_edge_aiid + 1) * sizeof(* p->edge_path_succ));
	p->super_node_last_edge = alloca((act->max_edge_aiid + 1) * sizeof(* p->super_node_last_edge));

	p->visited = alloca((act->max_node_aiid + 1) * sizeof(*p->visited));
	/* tells wether an edge has been chosen */
	p->chosen = alloca((act->max_edge_aiid + 1) * sizeof(* p->chosen));
	/* tells wether a node has been already reached by the MDST */
	p->mdst_reached = alloca((act->max_node_aiid + 1) * sizeof(* p->mdst_reached));

	p->edge_path_roots = alloca((act->max_edge_aiid + 1) * sizeof(*p->edge_path_roots));




	if (act->mature == 0) {
		printf("ERROR in module ext/grs: invoked planing for a nonmature action\n");
		return NULL;
	}

	/* for an empty pattern return an empty plan */
	if (act->n_nodes + act->n_neg_nodes <= 0) {
		plan = (ext_grs_match_plan_t *) malloc(sizeof(*plan));
		memset(plan, 0, sizeof(*plan));
		plan->action = act;
		plan->length[0] = 0;
		plan->prog = NULL;
		plan->progs = 0;
		plan->num_progs = 0;
		return plan;
	}


	/* Compute Progs for all patterns (positive and negative) */

	/* act->negatives is the list head, call next node to find the first object in list */
	neg_pattern = &act->negatives;
	neg_pattern = neg_pattern->next;
	for(i = 0; i < plan->num_progs; i++)
	{
		/* the current positive or negayive pattern graph */
		ext_grs_graph_t *pat;

		/* do some init */
		memset(p->visited, 0, (act->max_node_aiid + 1) * sizeof(* p->visited));
		memset(p->chosen, 0, (act->max_edge_aiid + 1) * sizeof(* p->chosen));
		memset(p->mdst_reached, 0, (act->max_node_aiid + 1) * sizeof(* p->mdst_reached));
		memset(p->edge_path_succ, 0, (act->max_edge_aiid + 1) * sizeof(* p->edge_path_succ));
		memset(p->super_node_last_edge, 0, (act->max_edge_aiid + 1) * sizeof(* p->super_node_last_edge));
		memset(p->edge_path_roots, 0, (act->max_edge_aiid + 1) * sizeof(*p->edge_path_roots));
		p->n_edge_path_roots = 0;

		/* init the mpq structure */
		ext_grs_mpq_init();

		/* Init the union find structure used to represent the united nodes
		* used by an implementation of Edmond's MDST-Algorithm used in this
		* planning procedure. Nodes and united nodes are represented by
		* node ids which are the elements of the union find structure */
		ext_grs_uf_init(act->max_node_aiid + 1);

		/* annotate the plan graph with edge costs due to the analysis
		* results stored by the analyzer registered with the given
		* planer */


		/* first phase of the MDST computation: contract the all circles in the
		* plan graph to super nodes */

		if(i == 0)
		{
			pat = act->pattern;
			annotate_edge_costs(pat, _ext_grs_get_irg_private(irg), _get_irg_vs_stat(planer->analyzer, irg), 0);
			phase = mdst_contraction_phase(pat, p);
		}
		else
		{
			pat = (ext_grs_graph_t *) lc_list_entry(neg_pattern, ext_grs_graph_t, negative_list);
			annotate_edge_costs(pat, _ext_grs_get_irg_private(irg),	_get_irg_vs_stat(planer->analyzer, irg), 1);
			phase = mdst_contraction_phase(pat, p);
			neg_pattern = neg_pattern->next;
		}

		/* second phase of the MDST computation: expand the condensed super nodes
		* and choose an appropriate entering edge of each super node expanded */
		mdst_expansion_phase(p);

	#ifdef EXT_GRS_DEBUG
		{
			/* dump the MDST */
			double c = 0;
			int i;
			printf("dump the MDST: name, aiid, acc_cost\n");
			for (i = 0; i <= act->max_edge_aiid; i++)
				if (act->edges[i])
					if (
						(act->edges[i]->kind == ext_grs_k_edge && act->edges[i]->graph == pat) ||
						(act->edges[i]->kind == ext_grs_k_pseudo_edge && act->edges[i]->genuine_elem.val.e->graph == pat)
						)
						if (p->chosen[ act->edges[i]->aiid ]) {
							c += act->edges[i]->cost;
							printf("edge %s, %d, %lf\n", act->edges[i]->name, act->edges[i]->aiid, c);
						}
			printf("\n");
		}
	#endif

		/* The MDST is now computed (all edges with chosen[edge->id] != 0 belong
		* to the MDST). Now the final plan generation can be done */

		/* Compute search plans for positive and negative pattern */
		plan->length[i] = emit_plan(plan->progs[i], pat, p, phase);
	}
	return(plan);
}








/** yields an analyzer performing a v-structure statisitc */
ext_grs_planer_t *ext_grs_get_vs_dmst_planer(void)
{
	ext_grs_planer_t *planer = malloc(sizeof(*planer));
	planer->data = malloc( sizeof(ext_grs_vs_dmst_planer_private_t) );
	planer->tag = "vs_dmst_planer";
	planer->init = init;
	planer->compute_plan = compute_plan;

	return planer;
}
