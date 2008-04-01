/************************************************************************
 * Program:		Search_tree.c
 * Project:		SIMD optimization
 * Function:		Provides functions to build a search tree to
 *				select the patterns for rewrite
 * Author:		Andreas Schoesser
 * Date:			2007-07-05
 ************************************************************************/

#include <stdio.h>
#include <conio.h>

#include "irgraph.h"
#include "pmap.h"
#include "irtools.h"

#include "grs/match.h"

#include "simd_presets.h"
#include "search_tree_t.h"
#include "firm_node_ext.h"


struct pmap *applied_nodes;
static int st_number = 0;


/************************************************************************
 * Builds a search tree
 * TODO: Check if a match is valid, only then insert a search tree node,
 *       but insert it into the ignore list
 ************************************************************************/

search_tree_node_t *build_search_tree(search_tree_env_t *search_tree_env, int prio_start)
//void build_search_tree(search_tree_env_t *search_tree_env)//, int struct pmap *vec_operations, struct pmap *keep_nodes, int start, statistics_t *statistics, struct pmap *replaced_ptrs)
{
	plist_t **ignored_matches = alloca(search_tree_env->num_rules * sizeof(plist_t *));
	search_tree_node_t *root;
	int i;
	char st_filename[255];

	applied_nodes = pmap_create();
	#if VERBOSE_REWRITE
		printf("\n\nBuilding search tree for priority %d:\n\n", search_tree_env->priorities[1]);
	#endif

	// Create "list of matches to ignore" for each rule type
	for(i = 0; i < search_tree_env->num_rules; i++)
		ignored_matches[i] = plist_new();

	root = rec_search_patterns(search_tree_env, search_tree_env->priorities[1], ignored_matches);

	// Free "list of matches to ignore" for each rule type
	for(i = 0; i < search_tree_env->num_rules; i++)
	{
		plist_clear(ignored_matches[i]);
		plist_free(ignored_matches[i]);
	}

	#if VERBOSE_REWRITE
		sprintf(st_filename, "%s-searchtree-%d.vcg", get_entity_name(get_irg_entity(search_tree_env->irg)), st_number++);
		dump_search_tree(root, st_filename, "testtree", search_tree_env);
	#endif

	#if VERBOSE_REWRITE
		printf("\n");
	#endif

	pmap_destroy(applied_nodes);

	return(root);
}



/************************************************************************
 * Frees the memory occupied by the search tree data structure
 * Params: root: The root node of the search tree
 ************************************************************************/

void destroy_search_tree(search_tree_node_t *root)
{
	// Free memory preds
	if(root->memory_preds != NULL)
		pmap_destroy(root->memory_preds);

	// Free the match and free subsequent search nodes recursively
	if(root->match != NULL)
	{
		destroy_search_tree(root->with);
		destroy_search_tree(root->without);
		ext_grs_free_match(root->match);
	}

	// Free the current search tree node itself
	free(root);
}



/************************************************************************
 * Searches patterns recursively and tests what costs appear when
 * the pattern we found would be applied or not.
 ************************************************************************/

static search_tree_node_t *rec_search_patterns(search_tree_env_t *st_env, int prio, plist_t **ignored_matches)
{
	int i, j, *prio_pos = st_env->priorities;

	// Search all patterns in this priority
	search_tree_node_t *st_node = malloc(sizeof(*st_node));

	memset(st_node, 0, sizeof(*st_node));

	for(; prio_pos[1] == prio; ) // TODO! Nach oben begrenzen!
	{
		int rule_nr = prio_pos[0];
		ext_grs_match_t *match;

		#if VERBOSE_REWRITE
			printf("\nSearching pattern %s (%d).", st_env->firmnode_names[rule_nr], rule_nr);
		#endif

		// Get ALL matches
		match = ext_grs_match(st_env->irg, st_env->m_plans[rule_nr], ext_grs_ALL, ext_grs_REGARDLESS);
		assert(match != NULL && "Panic! No memory for match!");
		st_env->statistics->num_matches += ext_grs_get_n_matches(match);
		st_env->statistics->num_matched_nodes += ext_grs_get_n_matches(match) * ext_grs_get_match_max_node_aiid(match);

		// Now find the first one not to be ignored;
		for(i = 0; i < ext_grs_get_n_matches(match); i++)
		{
			if(!ignore_match(match, i, ignored_matches, rule_nr))
			{
				// Insert this match into ignore list
				plist_element_t *inserted_match = add_ignored_match(match, i, ignored_matches, rule_nr);
				plist_t *undo_vproj_info;
				int aa;

				#if VERBOSE_REWRITE
					printf(" Found! %p", st_node);
				#endif

				// ** First look if we're allowed to apply the match **
				// Look if the arguments are in dominating blocks

				if((aa = analyze_arguments(st_env->rules[rule_nr], match, i, st_env->irg, NULL)) < 0)
				{

					#if VERBOSE_REWRITE
						if(aa == -1)
							printf(" Invalid arguments.");
						else
							printf(" Argument dependency.");
					#endif
					return(st_node);
				}


				// Find the memory predecessors to be able to schedule the complex operation
				st_node->memory_preds = find_memory_preds(st_env->irg, ext_grs_get_match_node_map(match, i), st_env->rules[rule_nr]->max_node_aiid);

				// Do alias analysis and "LOAD usage" analysis" to see if pattern is really appliable.
				if(test_alias_relation(match->nodes[i], match->max_node_id, st_node->memory_preds, st_env->replaced_ptrs) != 0)
				{
					#if VERBOSE_REWRITE
						printf(" Rejected: Alias conflict!");
					#endif
					return(st_node);
				}


				// Arriving here means we're allowed to apply the match. Save information and look for
				// following matches with or without the current match.
				st_node->match = match;
				st_node->which_match = i;
				st_node->rule_nr = rule_nr;

				// call recursively
				st_node->without = rec_search_patterns(st_env, prio, ignored_matches);

				// Delete this pattern from the ignored ones
				delete_ignored_match(ignored_matches, inserted_match, rule_nr);


				// Insert vProj nodes
				undo_vproj_info = construct_vproj(st_env, st_node, rule_nr);

				// call again recursively
				#if VERBOSE_REWRITE
					printf("\nApplying %p ", st_node);
				#endif
				insert_inner_nodes(st_env, st_node);
				st_node->with = rec_search_patterns(st_env, prio, ignored_matches);
				remove_inner_nodes(st_env, st_node);
				#if VERBOSE_REWRITE
					printf("\nFinished Applying %p\n", st_node);
				#endif

				// Destroy VProj nodes
				deconstruct_vproj(undo_vproj_info);


				// Build the search tree node for this match

				st_node->this_matchs_costsavings = st_env->cost_savings[rule_nr] - ext_result_costs(st_node->match, st_node->which_match);
				st_node->cost_savings = MAX((st_node->without->cost_savings), (st_node->with->cost_savings + st_node->this_matchs_costsavings));


				return(st_node);
			}
		}
		prio_pos += 2;
	}
	return(st_node);
}



/*_                                _                   _
 (_) __ _ _ __   ___  _ __ ___  __| |  _ __   ___   __| | ___  ___
 | |/ _` | '_ \ / _ \| '__/ _ \/ _` | | '_ \ / _ \ / _` |/ _ \/ __|
 | | (_| | | | | (_) | | |  __/ (_| | | | | | (_) | (_| |  __/\__ \
 |_|\__, |_| |_|\___/|_|  \___|\__,_| |_| |_|\___/ \__,_|\___||___/
	|___/													        */


/************************************************************************
 * Adds a certain match to the list of ignored matches
 * Prarms:	match:	The match object to be inserted
 *			which:	The match number inside the match object to be inserted
 *			ignored_matches: Array of list of matches to be ignored
 *			rule_nr:	The rule which found the match
 * Returns: List element containing the ignored match
 ************************************************************************/

static plist_element_t *add_ignored_match(ext_grs_match_t *match, int which, plist_t **ignored_matches, int rule_nr)
{
	ignored_match_t *imatch = malloc(sizeof(*imatch));
	//memset(imatch, 0, sizeof(*imatch));

	imatch->nodes = generate_nodemap_hash(ext_grs_get_match_node_map(match, which), ext_grs_get_match_max_node_aiid(match));
	// imatch->edges = ext_grs_get_match_edge_map(match, which);

	plist_insert_back(ignored_matches[rule_nr], imatch);
	return(plist_last(ignored_matches[rule_nr]));
}



/************************************************************************
 * Removes a certain match to the list of ignored matches
 * Params: ignored_matches: List of matches to be ignored
 *		   match_to_delete: List element of ignored_matches to be deleted
 ************************************************************************/

static void delete_ignored_match(plist_t **ignored_matches, plist_element_t *match_to_delete, int rule_nr)
{
	ignored_match_t *imatch = match_to_delete->data;
	pmap_destroy(imatch->nodes);
	free(imatch);
	plist_erase(ignored_matches[rule_nr], match_to_delete);
}



/************************************************************************
 * Test, if a certain match is in the ignore list
 * Returns: 0 if the match is not in the list
 *          1 if the match has to be ignored
 * Prarms:	match:	The match object to be tested
 *			which:	The match number inside the match object to be tested
 *			ignored_matches: Array of list of matches to be ignored
 *			rule_nr:	The rule which found the match
 ************************************************************************/

static int ignore_match(ext_grs_match_t *match, int which, plist_t **ignored_matches, int rule_nr)
{
	int j;
	plist_element_t *el;
	pmap_entry *pen = NULL;
	ir_node **node_map = ext_grs_get_match_node_map(match, which);
	ir_edge_t **edge_map = ext_grs_get_match_edge_map(match, which);
	int max_aiid = ext_grs_get_match_max_node_aiid(match);

	// Iterate over all matches to be ignored of rule type "rule_nr"
	foreach_plist(ignored_matches[rule_nr], el)
	{
		ignored_match_t *imatch = (ignored_match_t *) el->data;
		int difference = 0;

		for(j = 0; j <= max_aiid; j++)
			// if((pen = pmap_get(imatch->nodes, node_map[j])) == NULL) /// HAEEE? Why does this not work????
			if(!pmap_contains(imatch->nodes, node_map[j]))
			{
				difference = 1; // We have a difference. Proceed with next saved match
				break;
			}

		if(difference == 0)
		{
			#if VERBOSE_REWRITE
				printf(" Ignoring: Already matched in %p", pen);
			#endif
			return(1); // There is no difference beetween the two, that means we found 2 identical matches
		}
	}

	// Now look if match's nodes don't collide with inner nodes of already applied matches
	for(j = 0; j <= max_aiid; j++)
		if((pen = pmap_get(applied_nodes, node_map[j])) != 0)
		{
			#if VERBOSE_REWRITE
				printf(" Overlapping with %p", pen);
			#endif
			return(1);
		}


	return(0);
}


/************************************************************************
 * Insert inner nodes of an applied pattern into a global pmap
 ************************************************************************/

void insert_inner_nodes(search_tree_env_t *st_env, search_tree_node_t *st_node)
{
	ir_node **nodes = ext_grs_get_match_node_map(st_node->match, st_node->which_match);
	int max_id = ext_grs_get_match_max_node_aiid(st_node->match, st_node->which_match);
	int i;
	ext_grs_node_t **act_nodes = st_node->match->action->nodes;
	int len = strlen(VECTOR_RESULT_NAME);

	for(i = 0; i < max_id; i++)
	{
		// Ignore Agrument nodes of the pattern
		if(act_nodes[i]->op == op_IrNode) // Generic argument nodes
			continue;
		if(strncmp(act_nodes[i]->name, "Arg_", 4) == 0) // explicit argument nodes, that is Vector Base
			continue;
		if(get_irn_opcode(nodes[i]) == iro_Const)
			continue;
		if(get_irn_opcode(nodes[i]) == iro_VProj)
			continue;
		if(get_irn_opcode(nodes[i]) == iro_Block)
			continue;
		pmap_insert(applied_nodes, nodes[i], (void *) st_node); // Annotate which node produced that node
	}
}



/************************************************************************
 *
 ************************************************************************/

void remove_inner_nodes(search_tree_env_t *st_env, search_tree_node_t *st_node)
{
	ir_node **nodes = ext_grs_get_match_node_map(st_node->match, st_node->which_match);
	int max_id = ext_grs_get_match_max_node_aiid(st_node->match, st_node->which_match);
	int i;

	for(i = 0; i < max_id; i++)
		if(pmap_contains(applied_nodes, nodes[i]))
			pmap_insert(applied_nodes, nodes[i], 0); // Ugly, can't remove from pmap!
}



/* ___                     _    __     ______            _
  |_ _|_ __  ___  ___ _ __| |_  \ \   / /  _ \ _ __ ___ (_)
   | || '_ \/ __|/ _ \ '__| __|  \ \ / /| |_) | '__/ _ \| |
   | || | | \__ \  __/ |  | |_    \ V / |  __/| | | (_) | |
  |___|_| |_|___/\___|_|   \__|    \_/  |_|   |_|  \___// |
                                                      |__/ */

/************************************************************************
 * Insert "artificial" VProj and Complex Instruction nodes
 * Annotate at these nodes which match "created" them.
 ************************************************************************/

static plist_t *construct_vproj(search_tree_env_t *st_env, search_tree_node_t *st_node, int rule_nr)
{
	char node_name[50];
	int result_nr = 0, result_aiid;
	plist_t *undo_info = plist_new();
	ext_grs_match_t *match = st_node->match;
	int which =	st_node->which_match;

	// Find out if the match has VProj results and how many
	// This is done by looking for the node names Result_0_... - Result_n_...
	// The right node names have to be created by the pattern creator before!

	sprintf(node_name, "%s%d", VECTOR_RESULT_NAME, result_nr);
	if((result_aiid = ext_grs_act_get_node_aiid_n(st_env->rules[rule_nr], node_name, strlen(node_name))) >= 0)
	{																		// Yes, it has vector results
		ir_node **nodes = ext_grs_get_match_node_map(match, which);
		ir_node *ins[1], *dataproj, *block, *complex;
		pmap_entry *pen;

		block = get_nodes_block(nodes[result_aiid]);

		// Insert artificial complex instruction
		ins[0] = NULL;
		pmap_foreach(st_node->memory_preds, pen)
		{
			ins[0] = get_irn_n(pen->key, 0); //(pen->key);
			pmap_break(st_node->memory_preds);
		}
		complex = new_ir_node(NULL, st_env->irg, block, op_FakeComplex, mode_T, (ins[0] == NULL) ? 0 : 1, &ins[0]);
		ins[0] = complex;
		dataproj = new_ir_node_with_update(NULL, st_env->irg, block, op_Proj, mode_LLu, 1, ins);
		insert_which_match_vproj(st_env, dataproj, st_node->match, st_node->which_match, -1);
		ins[0] = dataproj;

		// Insert the VProjs, connect them with the complex instruction node
		do
		{
			ir_node *result_node = nodes[result_aiid];
			ir_node *new_vproj;
			ir_edge_t *res_succ, **outs;
			int n_edges, i = 0;
			undo_vproj_t *undo_vproj = malloc(sizeof(*undo_vproj));

			// Create a new VProj node
			new_vproj = new_ir_node_with_update(NULL, st_env->irg, get_nodes_block(result_node), op_VProj, get_irn_mode(result_node), 1, ins);
			set_VProj_proj(new_vproj, result_nr);

			// Save undo info for that VProj node
			undo_vproj -> vproj_node = new_vproj;
			undo_vproj -> org_result = result_node;
			plist_insert_back(undo_info, undo_vproj);

			// Save for each VProj, which match "created" them
			//pmap_insert(st_env->which_match_vproj, new_vproj, st_node);
			insert_which_match_vproj(st_env, new_vproj, st_node->match, st_node->which_match, result_aiid);

			// Now connect the result's successors to the Vproj, remember the old connections
			n_edges = get_irn_n_edges(result_node);
			NEW_ARR_A(ir_edge_t *, outs, n_edges);

			foreach_out_edge(result_node, res_succ)
			{
				assert(i < n_edges);
				outs[i++] = res_succ;
			}

			for(i = 0; i < n_edges; i++)
			{
				// Rewire
				set_irn_n(outs[i]->src, outs[i]->pos, new_vproj);
			}


			// Search the next node.
			sprintf(node_name, "%s%d", VECTOR_RESULT_NAME, ++result_nr);
		} while((result_aiid = ext_grs_act_get_node_aiid_n(st_env->rules[rule_nr], node_name, strlen(node_name))) >= 0);
	}

	//dump_ir_block_graph(st_env->irg, "inserted_vproj");

	return(undo_info);

}



/************************************************************************
 * Connects the successors of the inserted VProjs with their original
 * arguments again. Does NOT erase the VProj nodes from the graph,
 * they are still needed.
 * Frees the undo info
 ************************************************************************/

static void deconstruct_vproj(plist_t *undo_vproj_info)
{
	plist_element_t *el;

	// Undo every VProj inserted
	foreach_plist(undo_vproj_info, el)
	{
		undo_vproj_t *undo_vproj = el->data;
		ir_edge_t **outs, *vproj_succ;
		int i = 0, n_edges;

		assert(get_irn_opcode(undo_vproj->vproj_node) == iro_VProj);

		// Save all outs
		n_edges = get_irn_n_edges(undo_vproj->vproj_node);
		NEW_ARR_A(ir_edge_t *, outs, n_edges);
		foreach_out_edge(undo_vproj->vproj_node, vproj_succ)
		{
			assert(i < n_edges);
			outs[i++] = vproj_succ;
		}

		// Rewire
		for(i = 0; i < n_edges; i++)
			set_irn_n(outs[i]->src, outs[i]->pos, undo_vproj->org_result);

		// TODO!! Remove VProjs from matchers nodes list!

		// Free undo info
		free(undo_vproj);

	}

	plist_clear(undo_vproj_info);
	plist_free(undo_vproj_info);
}



/************************************************************************
 * Creates an entry in the which_match_vproj pmap to indicate
 * which match created which vproj node and where to find the
 * the REAL vproj node that was created by the replacement
 ************************************************************************/

static void insert_which_match_vproj(search_tree_env_t *st_env, ir_node *vproj, ext_grs_match_t *match, int which, int aiid)
{
	which_match_vproj_t *wmv = obstack_alloc(st_env->obst, sizeof(*wmv));

	wmv->match = match;
	wmv->which = which;
	wmv->aiid  = aiid;

	pmap_insert(st_env->which_match_vproj, vproj, wmv);
}


/*              _                                     _
 _ __ ___ _ __ | | __ _  ___ ___ _ __ ___   ___ _ __ | |_
| '__/ _ \ '_ \| |/ _` |/ __/ _ \ '_ ` _ \ / _ \ '_ \| __|
| | |  __/ |_) | | (_| | (_|  __/ | | | | |  __/ | | | |_
|_|  \___| .__/|_|\__,_|\___\___|_| |_| |_|\___|_| |_|\__|
         |_|                                                  */

/************************************************************************
 * Walk over search tree
 * Replace those patterns with the greates saving of costs
 * Params: root: root of the search tree
 ************************************************************************/

void replace_search_tree(search_tree_node_t *root, replace_st_env_t *rep_env)
{
	char force_apply, suggestion;

	if(root->match == NULL)
		return;

	#if VERBOSE_REWRITE
	suggestion = ((root->without->cost_savings) > (root->with->cost_savings + root->this_matchs_costsavings)) ? 'n' : 'y';
	printf("\nApply RULE %s? Suggested: %c (y/n): ", rep_env->firmnode_names[root->rule_nr], suggestion);
		#if PROMPT_REWRITE
			force_apply = getch();
		#endif
	#endif


	#if VERBOSE_REWRITE && PROMPT_REWRITE
	if(force_apply == 'n' || force_apply == 'N')  // User driven
	#else
	if((root->without->cost_savings) >= (root->with->cost_savings + root->this_matchs_costsavings)) // Cost driven
	#endif
	{
		#if VERBOSE_REWRITE
			printf("n");
		#endif
		// Go on with the "not replaced" edge
		replace_search_tree(root->without, rep_env);
	}
	else
	{
		int which[1];
		char match_postfix[100];

		#if VERBOSE_REWRITE
			printf("y");
		#endif

		// Manipulate the match to use the VProjs of its predecessor
		manipulate_match(rep_env, root->match, root->which_match);

		// Rewrite the Match
		which[0] = root->which_match;
		ext_grs_finish_match(root->match, 1, &which[0]);

		connect_complex_memory(rep_env->rules[root->rule_nr], root->match, root->which_match, root->memory_preds);
		save_complex_operation(rep_env->rules[root->rule_nr], root->match, root->which_match, rep_env->vec_operations, root->rule_nr);
		save_keep_nodes(rep_env->rules[root->rule_nr], root->match, root->which_match, rep_env->keep_nodes);
		set_complex_operation_block(rep_env->irg, root->match, root->which_match);

		//sprintf(match_postfix, "-REWRITTEN-%d-%d", i, j);
		//dump_ir_block_graph(irg, match_postfix);
		rep_env->statistics -> num_instructions_inserted++;
		rep_env->statistics -> num_replaced_nodes += root->match->n_nodes;
		rep_env->statistics -> rewritten_patterns[root->rule_nr]++;

		// Go on with the "replaced" edge
		replace_search_tree(root->with, rep_env);
	}
}



/************************************************************************
 * Manipulate Match object before replacement to not use the
 * temporarily created VProj nodes but the VProj nodes created by
 * the previous replacement.
 ************************************************************************/

static void manipulate_match(replace_st_env_t *rep_env, ext_grs_match_t *match, int which)
{
	/*	TODO:
		* Find out which nodes and edges have to be manipulated
		  - Iterate through matches array
		  - Look if a  node is inside the which_match_vproj map
		  - Retrieve the preceeding match object
		  -
	*/

	int max_aiid = ext_grs_get_match_max_node_aiid(match);
	ir_node **node_map = ext_grs_get_match_node_map(match, which);
	which_match_vproj_t *wmv;
	int i;

	// Manipulate nodes
	for(i = 0; i < max_aiid; i++)
	{
		if((wmv = (which_match_vproj_t *) pmap_get(rep_env->which_match_vproj, node_map[i])) != NULL)
		{
			// We found an entry that has to be rewired.
			if(wmv->aiid >= 0)
			{
				// It's a retyped result node. Follow "kept" array that points to the replace_node_map
				ir_node **repl_node_map = wmv->match->repl_nodes[wmv->which];
				node_map[i] = repl_node_map[wmv->match->action->pattern_node_kept[wmv->aiid]];
			}
			else
			{
				// It's the vector proj, which is not in the "kept" array since it was newly inserted.
				// Search for it by name in the replace_node_map
				int aiid = ext_grs_act_get_node_aiid(wmv->match->action, VECTOR_OP_PROJDATA_NAME);
				ir_node **repl_node_map = wmv->match->repl_nodes[wmv->which];
				assert(aiid >= 0 && "Vector_Op_ProjData not found!");
				node_map[i] = repl_node_map[aiid];
			}
		}
	}

	// Manipulate edges! Maybe not even necessary! As well as Vprojs, we just need the
	// Vector_op_ProjData node!
}



/************************************************************************
* Find the block to place the complex operation in
* We have to think here:
* Maybe we should prefer the blocks of the memory nodes. If no memory
* nodes there, the algorithm should be correct.
************************************************************************/

void set_complex_operation_block(ir_graph *irg, ext_grs_match_t *match, int which)
{
	ir_node **nodes = ext_grs_get_replace_node_map(match, which);
	int max_id = match->action->max_repl_node_aiid;
	int i, aiid;

	ir_node *current_block = NULL;

	assure_doms(irg);

	for(i = 0; i <= max_id; i++)
	{
		if(nodes[i] != NULL)
		{
			ir_opcode opc = get_irn_opcode(nodes[i]);
			if(opc != iro_Conv && match->action->replacement_nodes[i]->op != op_IrNode /* Its a generic Vector Base */ && opc != iro_VProj && opc != iro_MultipleAdd && opc != iro_Block && opc != iro_Const && strncmp(match->action->replacement_nodes[i]->name, "Arg_", 4) != 0)
			{
				if(current_block == NULL)
					current_block = get_nodes_block(nodes[i]);
				else
				{
					ir_node *this_block = get_nodes_block(nodes[i]);
					if(this_block && this_block != current_block && block_dominates(this_block, current_block))
						current_block = this_block;
				}
			}
		}
	}

	// Set the Block for the complex operation and it's projs
	assert(current_block != NULL);
	set_nodes_block(nodes[ext_grs_act_get_node_aiid(match->action,  VECTOR_OP_PROJDATA_NAME)], current_block);
	set_nodes_block(nodes[ext_grs_act_get_node_aiid(match->action,  VECTOR_OP_NAME)], current_block);
	if((aiid = ext_grs_act_get_node_aiid(match->action,  VECTOR_OP_PROJM_NAME)) >= 0)
		set_nodes_block(nodes[aiid], current_block);
}




/*                       _       _                       _
 ___  ___  __ _ _ __ ___| |__   | |_ _ __ ___  ___    __| |_   _ _ __ ___  _ __   ___ _ __
/ __|/ _ \/ _` | '__/ __| '_ \  | __| '__/ _ \/ _ \  / _` | | | | '_ ` _ \| '_ \ / _ \ '__|
\__ \  __/ (_| | | | (__| | | | | |_| | |  __/  __/ | (_| | |_| | | | | | | |_) |  __/ |
|___/\___|\__,_|_|  \___|_| |_|  \__|_|  \___|\___|  \__,_|\__,_|_| |_| |_| .__/ \___|_|
                                                                   	   	  |_|                  */

/***********************************************************************
 * Dumps search trees in VCG Format
 ************************************************************************/

int glob_node_nr = 0;
void dump_search_tree(search_tree_node_t *root, char *file, char *graph_name, search_tree_env_t *st_env)
{
	FILE *fp = fopen(file, "wt");
	assert(fp && "Could not open file to dump");

	// Print vcg header
	fprintf(fp,
		"graph: { title: \"ir graph of %s\"\n"
		"display_edge_labels: yes\n"
		"layoutalgorithm: mindepth\n"
		"manhattan_edges: yes\n"
		"port_sharing: no\n"
		//"orientation: %s\n"
		"infoname 1: \"Test\"\n",
		graph_name);//, label, orientation);
	fprintf(fp, "\n");        /* a separator */

	glob_node_nr = 0;
	rec_dump_search_tree(root, fp, 0, st_env, 1);

	// Dump Footer
	fprintf(fp, "}\n");
	fclose(fp);
}



/************************************************************************
 * Called for each search tree node recursively and dumps its
 * information
 * Params: bestcost:	1 If we're on the path with the best costs
 ************************************************************************/

int rec_dump_search_tree(search_tree_node_t *node, FILE *fp, int this_node_nr, search_tree_env_t *st_env, int best_cost)
{
	int next_node_nr = this_node_nr;
	ir_node **nodes;
	char args[1000], arg_name[20], tmp[1000];
	int i = 0, aiid, replace;
	args[0] = 0;

	if(node->match != NULL)
	{
		nodes = ext_grs_get_match_node_map(node->match, node->which_match);

		do
		{
			sprintf(arg_name, "Arg_%d", i);
			aiid = ext_grs_act_get_node_aiid(node->match->action, arg_name);
			if(aiid >= 0)
			{
				sprintf(tmp, "Arg_%d: %s %d ", i, get_irn_opname(nodes[aiid]), get_irn_node_nr(nodes[aiid]));
				strcat(args, tmp);
				strcat(args, "\n");
			}
			i++;
		} while(aiid >= 0);
		sprintf(tmp, "st_node: %p\n", node);
		strcat(args, tmp);
		sprintf(tmp, "CostSavings: %d\n", node->cost_savings);
		strcat(args, tmp);
		sprintf(tmp, "CostSavings this node %d\n", node->this_matchs_costsavings);
		strcat(args, tmp);

		replace = ((node->without->cost_savings) > (node->with->cost_savings + node->this_matchs_costsavings)) ? 0 : 1;

		fprintf(fp, "node: { title: \"n%d\" label: \"%s\" color:%s info1: \"%s\" }\n", this_node_nr, st_env->rules[node->rule_nr]->name, (replace && best_cost) ? "red" : "white", args);

		if(node->with->match != NULL)
		{
			fprintf(fp, "edge: { sourcename: \"n%d\" targetname: \"n%d\" label: \"1\" color:%s}\n", this_node_nr, this_node_nr+1, (replace==1 && best_cost) ? "red" : "black");
			next_node_nr = rec_dump_search_tree(node->with, fp, this_node_nr + 1, st_env, (replace==1 && best_cost));
		}

		if(node->without->match != NULL)
		{
			fprintf(fp, "edge: { sourcename: \"n%d\" targetname: \"n%d\" label: \"0\" color:%s}\n", this_node_nr, next_node_nr+1, (replace==0 && best_cost) ? "red" : "black");
			next_node_nr = rec_dump_search_tree(node->without, fp, next_node_nr + 1, st_env, (replace==0 && best_cost));
		}
	}

	return(next_node_nr);
}
