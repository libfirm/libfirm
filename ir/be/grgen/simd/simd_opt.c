/*******************************************************************************************
 * Program:  simd_opt.c
 * Function: Part of the simd optimization. Searches match of complex operation pattern and
 *           inserts the complex operation if possible.
 * Author:   Andreas Schoesser
 * Date:     06.12.2006
 *******************************************************************************************/


#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <conio.h>

#include "irgopt.h"
#include "pmap.h"
#include "obstack.h"

#include "simd_opt_t.h"

#include "firm_node_ext.h"
#include "normalize.h"
#include "search_tree_t.h"

#ifdef INCLUDE_GENERATED_PATTERNS
	#include "generated/simd_rules.h"
#endif

extern ir_op *grs_op_Complex;
extern ext_grs_action_init_complex_instructions();

#ifdef INCLUDE_GENERATED_PATTERNS
	ext_grs_match_plan_t *m_plans[NUM_RULES];
#endif


/*          _                          _ _           _   _
_ __  _   _| | ___    __ _ _ __  _ __ | (_) ___ __ _| |_(_) ___  _ __
| '__| | | | |/ _ \  / _` | '_ \| '_ \| | |/ __/ _` | __| |/ _ \| '_ \
| |  | |_| | |  __/ | (_| | |_) | |_) | | | (_| (_| | |_| | (_) | | | |
|_|   \__,_|_|\___|  \__,_| .__/| .__/|_|_|\___\__,_|\__|_|\___/|_| |_|
		                  |_|   |_|
*/

int srt_cmp(const void *v1, const void *v2)
{
	int *i1 = (int *) v1;
	int *i2 = (int *) v2;

	return  i1[1] - i2[1];
}


/************************************************************************/
/* Performs the pattern lookup and pattern replacement if possible,     */
/* for all actions defined by the user in the "create pattern" step.    */
/************************************************************************/

void ext_grs_simd_opt()
{
#ifdef INCLUDE_GENERATED_PATTERNS

	ext_grs_planer_t *planer;
	ext_grs_analyzer_t *analyzer;
	char c;
	int irg_nr, start;
	statistics_t statistics;

	memset(&statistics, 0, sizeof(statistics_t));
	statistics.rewritten_patterns = alloca(NUM_RULES * sizeof(int));
	memset(statistics.rewritten_patterns, 0, NUM_RULES * sizeof(int));

	printf("\n\nSearching for rich instructions\n===============================\n\n");

	statistics.start_time = clock();

	// Insert additional FIRM nodes, like VProj
	ext_firm_nodes();

	// Init the actions
	ext_grs_action_init_complex_instructions();
	fill_rulename_array();

	// "priorites" is a 2-dimensional array, containing
	// [RULE_NUMBER][priority]
	// Sort this array by priority here.
	qsort((void *) &priorities, (size_t) NUM_RULES, ( size_t) (sizeof(int) * 2), srt_cmp);

	// Set up the graph matcher
	analyzer = ext_grs_get_vs_analyzer();
	planer = ext_grs_get_vs_dmst_planer();

	// Match defined rules.
	// Rules are ordered by priority in the rules[] array.
	for(irg_nr = get_irp_n_irgs() - 1; irg_nr >= 0; irg_nr--)
	{
		struct pmap *vec_operations = pmap_create(); /* Maps all created vector operations to their destination op */
		struct pmap *keep_nodes = pmap_create();
		ir_graph *irg = get_irp_irg(irg_nr);
		struct pmap *norm_undo_info;

		#if VERBOSE_REWRITE
			printf("\n ** Analyzing function %s **\n", get_entity_name(get_irg_entity(irg)));
		#endif

		current_ir_graph = irg;


		// Dump the graph to see what it was like before the replacement
		dump_ir_block_graph(irg, "-pre-simd");
		remove_critical_cf_edges(irg);
		norm_undo_info = normalize_address_calculation(irg, 1);
		edges_deactivate(irg);
		edges_activate(irg);


		compute_match_plans(irg, planer, analyzer);

		// Normal rewrite
		//start = perform_rewrite(irg, vec_operations, keep_nodes, 0, &statistics, norm_undo_info);
		start = perform_rewrite_experimental(irg, vec_operations, keep_nodes, 0, &statistics, norm_undo_info);
		#if VERBOSE_REWRITE
			printf("\nRenaming complex operations...\n");
		#endif
		#if VERBOSE_REWRITE
			dump_ir_block_graph(irg, "-REWRITTEN0-PRE-IA23-NODES");
		#endif

		rename_complex_operations(vec_operations);
		#if VERBOSE_REWRITE
				dump_ir_block_graph(irg, "-REWRITTEN1-IA23-NODES");
		#endif
		exchange_keep_nodes(keep_nodes);

		// Rewriting may have left a mess of a graph behind, optimize control flow and ophaned LOADs

		optimize_load_store(irg);
		optimize_cf(irg);

		#if VERBOSE_REWRITE
			dump_ir_block_graph(irg, "-REWRITTEN2-POST-LOAD-STORE-OPT");
		#endif

		edges_activate(irg); /* HACK! optimize_cf deactivates edges!! */

		// Clean up the remaining vProjs
		#if VERBOSE_REWRITE
			printf("\nClean-up step:");
		#endif

		pmap_destroy(vec_operations);
		pmap_destroy(keep_nodes);
		vec_operations = pmap_create(); /* Maps all created vector operations to their destination op */
		keep_nodes = pmap_create();
		perform_rewrite(irg, vec_operations, keep_nodes, start, &statistics, norm_undo_info);
		rename_complex_operations(vec_operations);

		exchange_keep_nodes(keep_nodes);

		// Remove MultipleAdd nodes
		#if VERBOSE_REWRITE
			printf("\nUndoing normalization...\n");
		#endif
		decompose_normalization(norm_undo_info);

		#if VERBOSE_REWRITE
			dump_ir_block_graph(irg, "-REWRITTEN3-FINAL");
		#endif

		// Clean up data structures
		pmap_destroy(vec_operations);
		pmap_destroy(keep_nodes);
		edges_deactivate(irg);
	}
	statistics.end_time = clock();

	printf("\n ** Rewriting finished. **\n\n");

	view_statistics(&statistics);
#else

	printf("SIMD optimization turned off...\n<--'\n");
	getchar();
	ext_firm_nodes();

#endif
}

void compute_match_plans(ir_graph *irg, ext_grs_planer_t *planer, ext_grs_analyzer_t *analyzer)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	int rule_nr;

	#if VERBOSE_REWRITE
		printf("Computing search plans...\n");
	#endif

	ext_grs_init_planer(planer, analyzer);
	ext_grs_enable_irg(irg); // Alloc memory in ir_graph for match analysis result.
	ext_grs_analyze(analyzer, irg);

	// This returns a newly allocated "matchplan" oject.
	for(rule_nr = 0; rule_nr < NUM_RULES; rule_nr++)
	{
		m_plans[rule_nr] = ext_grs_compute_plan(planer, irg, rules[rule_nr]);

		#if DUMP_SEARCH_PLAN
			ext_grs_dump_match_plan(m_plans[rule_nr]);
		#endif
	}
#endif
}



/************************************************************************************
 * Params:	irg: The ir graph to be optimized
 *			vec_operations: A pmap with all "Complex" nodes to be exchanged
 *							by ia32 nodes later
 *			keep_nodes:		A pmap containing the keep nodes to be inserted
 *			start:			rule number to start with
 *			statistics:		Statistics structure to save statistics information in
 *			replaced_ptrs:	Addresses of Load and Store nodes replaced by MultipleAdd
 *							Nodes
 ************************************************************************************/

int perform_rewrite(ir_graph *irg, struct pmap *vec_operations, struct pmap *keep_nodes, int start, statistics_t *statistics, struct pmap *replaced_ptrs)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	int i, j;
	ext_grs_match_t *match;
	char c;

	for(i = start; i < NUM_RULES; i++)
	{
		int rule_nr = priorities[i][0];
		int num_matches;
		char match_postfix[100];
		j = 0;

		if(priorities[i][1] > 1000 && start == 0)
			break;

			do
			{
				#if VERBOSE_REWRITE
					printf("\nApplying RULE %s: ", firmnode_names[rule_nr]);
				#endif

				// This returns a newly allocated "matchplan" oject.
				//m_plan = ext_grs_compute_plan(planer, irg, rules[rule_nr]); */

				#if DUMP_SEARCH_PLAN
					ext_grs_dump_match_plan(m_plans[rule_nr]);
				#endif

				// Match the pattern given in the action inited above
				// Compliant: Only NON-overlapping matches allowed.
				// TODO: Describe case of overlapping patterns. Which to chose? Point to backend implementations
				match = ext_grs_match(irg, m_plans[rule_nr], ext_grs_ALL, ext_grs_COMPLIANT);
				statistics->num_matches++;
				statistics->num_matched_nodes += match->n_nodes;

				#if VERBOSE_REWRITE
					printf(" Found matches is %d.", ext_grs_get_n_matches(match));
					sprintf(match_postfix, "-MATCHED-%d-%d", i, j);
					ext_grs_dump_match(match, match_postfix);
				#endif

				// Look if a match has been found
				num_matches = ext_grs_get_n_matches(match);
				if(num_matches > 0)
				{
					struct pmap *memory_preds;
					// Do alias analysis and "LOAD usage" analysis" to see if pattern is really appliable.

					if(analyze_arguments(rules[rule_nr], match, 0, irg, NULL) < 0)
					{
						#if VERBOSE_REWRITE
							printf(" Invalid arguments.");
						#endif
					}
					else
					{

						// Find the memory predecessors to be able to schedule the complex operation
						memory_preds = find_memory_preds(irg, ext_grs_get_match_node_map(match, 0), rules[rule_nr]->max_node_aiid);

						if(test_alias_relation(match->nodes[0], match->max_node_id, memory_preds, replaced_ptrs) == 0)
						{
							// Everything seems ok, do the rewrite
							#if VERBOSE_REWRITE
								printf(" Rewrite? (y/n) ");
								#if PROMPT_REWRITE
									c = getch();
								#else
									c = 'y';
									printf("y");
								#endif
							#else
								c = 'y';
							#endif

							if(c == 'y' || c == 'Y')
							{
								// Do the rewriting step. Rewrite ALL matches (for now...).
								int which[5] = { 0, 1, 2, 3, 4 };
								ext_grs_finish_match(match, 1, &which[0]);
								connect_complex_memory(rules[rule_nr], match, 0, memory_preds);
								save_complex_operation(rules[rule_nr], match, 0, vec_operations, rule_nr);
								save_keep_nodes(rules[rule_nr], match, 0, keep_nodes);
								sprintf(match_postfix, "-REWRITTEN-%d-%d", i, j);
								dump_ir_block_graph(irg, match_postfix);
								statistics -> num_instructions_inserted++;
								statistics -> num_replaced_nodes += match->n_nodes;
								statistics -> rewritten_patterns[rule_nr]++;
							}
							else
							{
								num_matches = 0;
							}
						}
						else
						{
							#if VERBOSE_REWRITE
								printf("Rejected. ");
							#endif
						}
					}
					ext_grs_free_match(match);
				}
#if PROMPT_NO_PATTERN_FOUND
				else
					getch();
#endif
			//ext_grs_disable_irg(irg); // Free analysis memory
			j++;
		} while(num_matches > 0);
	}
	return(i);
#endif
}


int perform_rewrite_experimental(ir_graph *irg, struct pmap *vec_operations, struct pmap *keep_nodes, int start, statistics_t *statistics, struct pmap *replaced_ptrs)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	search_tree_env_t st_env;
	replace_st_env_t  rep_env;
	search_tree_node_t *root;
	int rule_nr = start;
	struct obstack obst;

	obstack_init(&obst);

	while(rule_nr < NUM_RULES && priorities[rule_nr][1] != PRIORITY_CLEANUP)
	{
		int old_rulenr = rule_nr;

		// Build the search tree
		st_env.irg = irg;
		st_env.priorities = (int *) &priorities[rule_nr];
		st_env.m_plans = m_plans;
		st_env.num_rules = NUM_RULES;
		st_env.rules = rules;
		st_env.which_match_vproj = pmap_create();
		st_env.replaced_ptrs = replaced_ptrs;
		st_env.firmnode_names = firmnode_names;
		st_env.obst = &obst;
		st_env.statistics = statistics;
		st_env.cost_savings = cost_savings;
		root = build_search_tree(&st_env, priorities[rule_nr][1]);

		// Rewrite using search tree we built:
		rep_env.irg = irg;
		rep_env.keep_nodes = keep_nodes;
		rep_env.vec_operations = vec_operations;
		rep_env.statistics = statistics;
		rep_env.rules = rules;
		rep_env.which_match_vproj = st_env.which_match_vproj;
		rep_env.firmnode_names = firmnode_names;
		rep_env.obst = &obst;
		replace_search_tree(root, &rep_env);

		// Destroy the search tree
		destroy_search_tree(root);
		pmap_destroy(st_env.which_match_vproj);

		//dump_irgraph_complete_grgen(current_ir_graph);

		// Search rule with next higher priority to start the next search with
		while(rule_nr < NUM_RULES && priorities[rule_nr][1] == priorities[old_rulenr][1])
			rule_nr++;
	}

	// TODO: Return the rule_nr up to which we searched
	// This will be used in a second step to search for the rest of the
	// rules which are cleanup operations
	obstack_free(&obst, NULL);
	obstack_finish(&obst);
	return(rule_nr);
#endif
}



/************************************************************************
 * View statistics about the rewrite
 ************************************************************************/

static void view_statistics(statistics_t *statistics)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	int i;

	printf("***************************\n* Optimization Statistics *\n***************************\n\n\n");
	printf("Rich instructions inserted: %d\n", statistics->num_instructions_inserted);
	printf("Matches:                    %d\n", statistics->num_matches);
	printf("Nodes matched:              %d\n", statistics->num_matched_nodes);
	printf("Nodes probably replaced:    %d\n", statistics->num_replaced_nodes);
	printf("Time elapsed:               %g s\n", ((double) (statistics->end_time - statistics->start_time)) / CLOCKS_PER_SEC);

	printf("\nReplaced patterns:\n");
	for(i = 0; i < NUM_RULES; i++)
	{
		if(statistics->rewritten_patterns[i] > 0)
			printf("  %3dx %s\n", statistics->rewritten_patterns[i], firmnode_names[i]);
	}

	printf("\n\nPress <--' to generate code.");
	getchar();
#endif
}




/* 		 												   	   _           _
 _ __ ___   ___ _ __ ___   ___  _ __ _   _    __ _ _ __   __ _| |_   _ ___(_)___
| '_ ` _ \ / _ \ '_ ` _ \ / _ \| '__| | | |  / _` | '_ \ / _` | | | | / __| / __|
| | | | | |  __/ | | | | | (_) | |  | |_| | | (_| | | | | (_| | | |_| \__ \ \__ \
|_| |_| |_|\___|_| |_| |_|\___/|_|   \__, |  \__,_|_| |_|\__,_|_|\__, |___/_|___/
     							     |___/                       |___/
*/

#if 0

/* This is used when we have parallel memory edges */

/************************************************************************/
/* Tests, if two operations using the memory edge are                   */
/* 0: parallel regarding the memory edge                                */
/* 1: node b is above node a regarding the memory edge                  */
/* 2: node a is above node b regarding the memory edge                  */
/************************************************************************/

int test_mem_rel(ir_node *a, ir_node *b)
{
	ir_graph *irg = get_irn_irg(get_nodes_block(a));
	inc_irg_visited(irg);

	if(search_mem_node(b, a, irg))
		return(1); // b is above a

	if(search_mem_node(a, b, irg))
		return(2); // a is above b

	return(0);     // nodes are parallel


}

int search_mem_node(ir_node *n, ir_node *start, ir_graph *irg)
{
	int i, found = 0;

	if(get_irn_visited(start) == get_irg_visited(irg))
		return(0);

	set_irn_visited(start, get_irg_visited(irg));

	if(start == n);
		return(1); // Found it!

	// Walk up the memory chain
	if(get_irn_mode(start) == mode_M)
	{
		for(i = 0; i < get_irn_arity(start) && found == 0; i++)
			found |= search_mem_node(n, get_irn_n(start, i), irg);
	}
	else
	{
		for(i = 0; i < get_irn_arity(start) && found == 0; i++)
		{
			ir_node *pred = get_irn_n(start, i);
			if(get_irn_mode(pred) == mode_M)
				 found |= search_mem_node(n, get_irn_n(start, i), irg);
		}
	}

	return(found);
}
#endif




/************************************************************************
 * Finds all pattern nodes, which are scheduled first
 * The indicator of this is that these nodes have a memory-predecessor
 * which lies outside of the pattern and there is no pattern node
 * scheduled 'above' that predecessor. We call those nodes
 * 'incoming memory nodes'
 ************************************************************************/

struct pmap *find_memory_preds(ir_graph *irg, ir_node **match_node_map, int max_node_aiid)
{
	mem_preds_info_t mem_preds_info;  // Holds analysis information during the firm walk
	ir_node *start = get_irg_start(irg);
	inc_irg_visited(irg);

	mem_preds_info.pattern_nodes = generate_nodemap_hash(match_node_map, max_node_aiid); // Generate a hash out of the nodemap array for faster access
	mem_preds_info.memory_preds = pmap_create(); // Save the memory predecessors we will find during the firm walk here
	mem_preds_info.irg = irg;                    // Hand the irg over to the firm walker

	visit_mem_nodes(start, &mem_preds_info);

	// Return the 'incoming memory nodes' that we found.
	pmap_destroy(mem_preds_info.pattern_nodes);
	return(mem_preds_info.memory_preds);

}

static void visit_mem_nodes(ir_node *node, mem_preds_info_t *mem_preds_info)
{
	ir_graph *irg = mem_preds_info->irg;

	if(irn_visited(node))
		return;
	set_irn_visited(node, get_irg_visited(irg));

	// The end node is matched, but not really part of the pattern. Only needed to connect
	// keep nodes to. So Skip the end node when searching memory predecessors
	if(get_irn_opcode(node) == iro_End)
		return;

	// Falls in pattern nodes-> return;
	if(pmap_contains(mem_preds_info->pattern_nodes, node))
	{
		pmap_insert(mem_preds_info->memory_preds, node, (void *) pmap_get(mem_preds_info->pattern_nodes, node) /*=aiid*/);
		return;
	}

	if(get_irn_mode(node) == mode_M)
	{
		const ir_edge_t *edge;
		foreach_out_edge(node, edge)
			visit_mem_nodes(get_edge_src_irn(edge), mem_preds_info);
	}
	else
	{
		const ir_edge_t *edge;
		foreach_out_edge(node, edge)
		{
			ir_node *tgt = get_edge_src_irn(edge);
			if(tgt != NULL && get_irn_mode(tgt) == mode_M)
				visit_mem_nodes(tgt, mem_preds_info);
		}
	}
}




/************************************************************************
 * Find and return the memory in edge of a given node
 * Returns the in-array index for the edge in edge_nr
 * CAUTION: Seems not to work for PHI nodes!!!!!!!!!!!!!!!!!!!!!
 ************************************************************************/

static ir_node *find_memory_pred(ir_node *n, int *edge_nr, int start_edge)
{
	int i;
	ir_node *mem_pred = NULL;

	// Look for ProjMs. ProjM's predecessors don't have the "mode M".
	if(get_irn_arity(n) == 1 && get_irn_modecode(n) == irm_M && start_edge == 0)
	{
		if(edge_nr != NULL)
			*edge_nr = 0;
		return(get_irn_n(n, 0));
	}

	// It's not a ProjM node. Look for a predecessor with "mode_M".
	for(i = start_edge; i < get_irn_arity(n); i++)
	{
		ir_node *pred = get_irn_n(n, i);
		if(get_irn_modecode(pred) == irm_M /*&& get_irn_opcode(pred) != iro_NoMem <-- Do we really need this? */)
		{
			if(edge_nr != NULL)
				*edge_nr = i;
			return(pred);
		}
	}
	return(NULL);
}



/************************************************************************
 * Generates a hash map out of a node_map array to have faster
 * access on matched nodes later.
 ************************************************************************/

struct pmap *generate_nodemap_hash(ir_node **node_map, int max_node_aiid)
{
	struct pmap *node_hash = pmap_create();
	int i;

	for(i = 0; i <= max_node_aiid; i++)
		if(node_map[i])
			pmap_insert(node_hash, node_map[i], (void *) i);
	return(node_hash);
}



/***************************************************************************
 * Returns the first predecessor nr, that is an Bad node, -2 otherwise
 ***************************************************************************/

static int find_bad_node(ir_node *n)
{
	int in_pos = -2, i;

	for(i = 0; i < get_irn_arity(n); i++)
		if(get_irn_opcode(get_irn_n(n, i)) == iro_Bad)
			return(i);

	return(in_pos);
}



/*
    _    _ _             ____      _       _   _
   / \  | (_) __ _ ___  |  _ \ ___| | __ _| |_(_) ___  _ __
  / _ \ | | |/ _` / __| | |_) / _ \ |/ _` | __| |/ _ \| '_ \
 / ___ \| | | (_| \__ \ |  _ <  __/ | (_| | |_| | (_) | | | |
/_/   \_\_|_|\__,_|___/ |_| \_\___|_|\__,_|\__|_|\___/|_| |_| */


/************************************************************************
 * Starts the alias check for each node of the pattern
 * Returns: 1 if pattern is not appliable due to alias conflicts
 *          0 if the pattern is appliable
 ************************************************************************/

int test_alias_relation(ir_node **node_map, int max_node_aiid, struct pmap *memory_preds, struct pmap *replaced_ptrs)
{
	struct pmap *node_map_hash = generate_nodemap_hash(node_map, max_node_aiid);
	int i;

	for(i = 0; i < max_node_aiid; i++)
		if(node_map[i])
		{
			int edge_nr = -1;
			ir_node *pred;

			while((pred = find_memory_pred(node_map[i], &edge_nr, edge_nr + 1)))
			{
				if(walk_mem_edge_alias(node_map[i], pred, memory_preds, node_map_hash, replaced_ptrs) == 1)
					return(1);
			}
		}
	return(0);
}


/************************************************************************
 * For a specific pattern node: Walks up the memory edge until start or
 * a memory predecessor of the pattern is reached and checks each memory
 * node found for alias relation
 * How can start be reached? Should not happen IMO.
 ************************************************************************/

static int walk_mem_edge_alias(ir_node *orgn, ir_node *cn, struct pmap *memory_preds, struct pmap *node_map_hash, struct pmap *replaced_ptrs)
{
	int edge_nr = -1;
	ir_node *pred;

	if(pmap_contains(memory_preds, orgn))
		return(0);

	if(pmap_contains(node_map_hash, cn))
	{
		if(get_multadd_alias_relation(orgn, cn) != no_alias) //check_alias(orgn, cn, replaced_ptrs) != no_alias)
		{
			if(get_irn_opcode(orgn) != iro_Store || get_irn_opcode(cn) == iro_Load)
				return(1); // Bad
			// go on otherwise, STORE after LOAD allowed
		}
	}

	// We reached a memory pred or the start node?
	if(pmap_contains(memory_preds, cn) || get_irn_opcode(cn) == iro_Start) // Caution, memory preds are inside the pattern
		return(0);

	// Check if the two nodes alias
	if(!pmap_contains(node_map_hash, cn))
	{
		if(get_multadd_alias_relation(orgn, cn) != no_alias) //check_alias(orgn, cn, replaced_ptrs) != no_alias)
			return(1);
	}

	// Walk on
	while((pred = find_memory_pred(cn, &edge_nr, edge_nr + 1)))
		if(walk_mem_edge_alias(orgn, pred, memory_preds, node_map_hash, replaced_ptrs) == 1)
			return(1);

	// Checked all paths and did not find an alias conflict, so return 0
	return(0);
}


/************************************************************************
 * Checks if 2 nodes of kind LOAD or STORE alias. Uses the old addresses
 * instead of the MultipleAdd used for normalization
 ************************************************************************/

static ir_alias_relation check_alias(ir_node *n1, ir_node *n2, struct pmap *replaced_ptrs)
{
	ir_node *addr1 = NULL, *addr2 = NULL, *org_addr1, *org_addr2;
	ir_mode *mode1, *mode2;

	switch(get_irn_opcode(n1))
	{
		case iro_Store: addr1 = get_Store_ptr(n1); mode1 = get_irn_mode(get_Store_value(n1)); break;
		case iro_Load:  addr1 = get_Load_ptr(n1);  mode1 = mode_F; /* Hack */ break;
		default: return(no_alias);
	}

	switch(get_irn_opcode(n2))
	{
		case iro_Store: addr2 = get_Store_ptr(n2); mode2 = get_irn_mode(get_Store_value(n1)); break;
		case iro_Load:  addr2 = get_Load_ptr(n2);  mode2 = mode_F; /* Hack */break;
		default: return(no_alias);
	}

	org_addr1 = get_org_addr(addr1, replaced_ptrs);
	org_addr2 = get_org_addr(addr2, replaced_ptrs);

	return(get_alias_relation(current_ir_graph, org_addr1, mode1, org_addr2, mode2));
}



/*  _                                         _                       _           _
   / \   _ __ __ _ _   _ _ __ ___   ___ _ __ | |_    __ _ _ __   __ _| |_   _ ___(_)___
  / _ \ | '__/ _` | | | | '_ ` _ \ / _ \ '_ \| __|  / _` | '_ \ / _` | | | | / __| / __|
 / ___ \| | | (_| | |_| | | | | | |  __/ | | | |_  | (_| | | | | (_| | | |_| \__ \ \__ \
/_/   \_\_|  \__, |\__,_|_| |_| |_|\___|_| |_|\__|  \__,_|_| |_|\__,_|_|\__, |___/_|___/
			  |___/                                                      |___/
*/


/************************************************************************
 * Looks, if a argument node is in a block that dominates the block
 * the complex node has to be inserted to!
 * Return: 0 if so
 *        -1 if the arguments are not in a dominator, which is illegal
 *		  -2 if an argument is mem dependent of an inner pattern node
 * TODO: Analyse where to put the complex operation first! Now we do that
 *       by hand!
 ************************************************************************/

int analyze_arguments(ext_grs_action_t *action, ext_grs_match_t *match, int which, ir_graph *irg, struct pmap *memory_preds)
{
	char arg_name[100];
	int  arg_aiid, i, block_aiid;
	ir_node **node_map = ext_grs_get_match_node_map(match, which);
	ir_node *arg, *block, *arg_block;
	struct pmap *node_map_hash = generate_nodemap_hash(ext_grs_get_match_node_map(match, which), ext_grs_get_match_max_node_aiid(match));

	block_aiid = ext_grs_act_get_node_aiid(action, COMPLEX_OPERATION_BLOCK_NAME);
	assert(block_aiid > 0 && "Block of complex operations has not been dumped!");
	block = node_map[block_aiid];
	assert(get_irn_opcode(block) == iro_Block);

	for(i = 0; ; i++)
	{
		sprintf(arg_name, "Arg_%d", i);
		arg_aiid = ext_grs_act_get_node_aiid(action, arg_name);
		if(arg_aiid < 0)
			break;			// All arguments have been processed.

		// Retrieve the node corresponding to an argument and check it for consistency
		arg = node_map[arg_aiid];

		if(is_arg_mem_dependent(arg, node_map_hash))
		{
			pmap_destroy(node_map_hash);
			return(-2);
		}


		// First check if the argument is dependent from inner pattern nodes
		arg_block = get_nodes_block(arg);
		assert(get_irn_opcode(arg_block) == iro_Block);

		if(arg_block == block)
			continue;

		assure_doms(irg);
		if(block_dominates(arg_block, block))
			continue;

		pmap_destroy(node_map_hash);
		return(-1); // Argument not in dominator of complex operation block! It's unsafe to rewrite.
	}

	pmap_destroy(node_map_hash);
	return(0); // Everything is ok, match can be rewritten
}



/**************************************************************************
* Looks if a Argument node of the complex node is memory dependent of any
* memory node of the pattern. This would lead to a dead lock and has to
* be prevented.
**************************************************************************/

int is_arg_mem_dependent(ir_node *arg, struct pmap *node_map_hash)
{
	argdep_ana_info_t ana_info;

	// First find out the highest block in dom tree

	// First start firm walker up till
	// - a memory nodes is reached
	// - or we reached the highest dom block
	ana_info.found_dependency = 0;
	ana_info.node_map_hash = node_map_hash;
	irg_walk(arg, walk_arg_dep, NULL, &ana_info);

	return(ana_info.found_dependency);


	// For all mem nodes we reached
	// Walk up mem edge
	// Check if we hit an inner pattern node. If so: error!

	// Problems: Mem edge walker: Phis? Sync? Have to walk all ways
}



/************************************************************************
 *
 ************************************************************************/

static void walk_arg_dep(ir_node *n, void * env)
{
	// If n is a memory node, start walking up only all memory edges incident to this node
	int edge_nr = -1;
	argdep_ana_info_t *ana_info = env;
	ir_node *pred;

	// Dependency has been found on another path. Break walker
	if(ana_info->found_dependency)
	{
		for(edge_nr = (is_Block(n)) ? 0 : -1; edge_nr < get_irn_arity(n); edge_nr++)
			set_irn_visited(get_irn_n(n, edge_nr), get_irg_visited(get_irn_irg(n)));
		return;
	}

	// If we have a memory node, walk up memory edge(s) and don't touch the above data preds
	while((pred = find_memory_pred(n, &edge_nr, edge_nr + 1)))
//		if(ana_info->found_dependency == 0)
			if(walk_mem_edge_argdep(pred, ana_info->node_map_hash) == 1)
			{
				ana_info->found_dependency = 1;
				break;
			}
}



/************************************************************************
 * Walks up the memory edge for argument dependency analysis
 ************************************************************************/

int walk_mem_edge_argdep(ir_node *n, struct pmap *node_map_hash)
{
	int edge_nr = -1;
	ir_node *pred;

	if(get_irn_visited(n) == get_irg_visited(get_irn_irg(n)))
		return(0);
	set_irn_visited(n, get_irg_visited(get_irn_irg(n)));

	// Did we reach a pattern node?
	if(pmap_contains(node_map_hash, n))
		return(1);							// Yes

	// TODO: Check if max dominator reached
	// TODO: Check if we walked over a backedgemake


	// Walk on
	while((pred = find_memory_pred(n, &edge_nr, edge_nr + 1)))
		if(walk_mem_edge_argdep(pred, node_map_hash) == 1)
			return(1);

	return(0);								// We reached no pattern node, everything ok
}



/*         _                                        _ _
  _____  _| |_ ___ _ __ _ __    _ __ ___  ___ _   _| | |_
 / _ \ \/ / __/ _ \ '__| '_ \  | '__/ _ \/ __| | | | | __|
|  __/>  <| ||  __/ |  | | | | | | |  __/\__ \ |_| | | |_
 \___/_/\_\\__\___|_|  |_| |_| |_|  \___||___/\__,_|_|\__| */

/************************************************************************
 * Computes the additional costs that arise when pattern nodes have to
 * be maintained due to external usage nodes outside the pattern
 * We just compute the costs of THAT single node
 ************************************************************************/

int ext_result_costs(ext_grs_match_t *match, int which)
{
	ir_node **nodes = ext_grs_get_match_node_map(match, which);
	int maxid = ext_grs_get_match_max_node_aiid(match), i;
	struct pmap *node_map_hash = generate_nodemap_hash(nodes, maxid);
	int add_costs = 0;

	for(i = 0; i < maxid; i++) // Iterate through all pattern nodes
	{
		if(match->action->nodes[i]->op != op_IrNode)  // Node is not one of the generic vector base nodes. Those stay in the graph anyway.
		{
			// Has the node been matched and is the node NOT a Result of the Pattern?
			if(nodes[i] && strncmp(match->action->nodes[i]->name, VECTOR_RESULT_NAME, strlen(VECTOR_RESULT_NAME)) != 0 && strncmp(match->action->nodes[i]->name, "Arg_", 4) != 0)
			{
				if(get_irn_modecode(nodes[i]) != irm_M && get_irn_opcode(nodes[i]) != iro_Const && get_irn_opcode(nodes[i]) != iro_Block && get_irn_opcode(nodes[i]) != iro_VProj) // Memory result not interesting here
				{
					ir_edge_t *edge;
					foreach_out_edge(nodes[i], edge) // Look at all out edges of the pattern node
					{
						if(get_irn_modecode(edge->src) != irm_M) // Memory result not interesting here
						{
							if(!pmap_contains(node_map_hash, edge->src))	// Look, if the source of the out edge is outside the pattern
								add_costs += single_node_costs(edge->src);
						}
					}
				}
			}
		}
	}

	pmap_destroy(node_map_hash);
	return(add_costs);
}



/************************************************************************
 * Returns the costs for each individual FIRM node type.
 * Returns always 3 for now.
 ************************************************************************/

int single_node_costs(ir_node *n)
{
	return(3);
}




/*			 	 _                    _                                     _
 _ __   ___  ___| |_   _ __ ___ _ __ | | __ _  ___ ___ _ __ ___   ___ _ __ | |_
| '_ \ / _ \/ __| __| | '__/ _ \ '_ \| |/ _` |/ __/ _ \ '_ ` _ \ / _ \ '_ \| __|
| |_) | (_) \__ \ |_  | | |  __/ |_) | | (_| | (_|  __/ | | | | |  __/ | | | |_
| .__/ \___/|___/\__| |_|  \___| .__/|_|\__,_|\___\___|_| |_| |_|\___|_| |_|\__|
|_|                            |_|
*/

/************************************************************************
 * Connect the complex operation with the memory edge
 ************************************************************************/

void connect_complex_memory(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *mem_preds)
{
	ir_node **repl_node_map = ext_grs_get_replace_node_map(match, which);
	int vop_aiid = ext_grs_act_get_node_aiid(action, VECTOR_OP_NAME); // Find the vector operation
	int vop_projm_aiid = ext_grs_act_get_node_aiid(action, VECTOR_OP_PROJM_NAME); // Find it's ProjM node
	ir_node *vec, *projm;
	ir_node *memory_base = NULL;
	pmap_entry *entry;
	int i = 0;


	// The complex node needs the memory edge, if it has a ProjM node attached.
	if(vop_projm_aiid > -1)
	{
		// ProjM is there: Connect the Memory edge
		assert(vop_aiid > -1 && "Vector operation not found!");
		vec = repl_node_map[vop_aiid];
		projm = repl_node_map[vop_projm_aiid];

		// Mem_preds are the nodes INSIDE the pattern. Check their successors and
		// look if its always the same. Well, this loop makes no sense right now
		// it assumes that there is only 1 mem_pred (Speichereingaenger) and therefore
		// also one one Speichervorgaenger.
		pmap_foreach(mem_preds, entry)
		{
			ir_node *n; // = entry->key;
			int     aiid = (int) entry->value;
			ir_node *vop_mem_pred;
			int edge_nr = 0, bad_nr = 0;

			// Find the right n if n was replaced
			n = repl_node_map[action->pattern_node_kept[aiid]];

			if(memory_base == NULL)
				memory_base = n;
			if(n != memory_base)
				assert(0 && "Parallel incoming memory edges with different roots not supported right now. Need to SYNC...");

			assert(get_irn_opcode(n) != iro_Phi && "Incoming Mem node is a PHI. Strange.");
			vop_mem_pred = find_memory_pred(n, &edge_nr, 0);
			assert(vop_mem_pred != NULL && "Incoming memory node should have a memory edge!");
			bad_nr = find_bad_node(vec);
			assert(bad_nr >= 0 && "Dummy slot for connecting the Mem edge not found!");
			set_irn_n(vec, bad_nr, vop_mem_pred);  // Connect the vector op with its memory predecessor
			set_irn_n(n, edge_nr, projm);
		}
	}

	/*sprintf(keep_name, "IR_Keep%d", i);
	while((keep_aiid = ext_grs_act_get_node_aiid(action, keep_name)) > -1)
	{
		ir_node *keep, *new_keep;
		ir_node **ins;

		// Find the keep node
		keep = repl_node_map[keep_aiid];
		ins = get_irn_in(keep);
		new_keep = be_new_Keep(NULL, get_irn_irg(keep), get_nodes_block(keep), get_irn_arity(keep), &ins[1]);
		exchange(keep, new_keep);

		//add_register_req(keep);

		sprintf(keep_name, "be_Keep%d", ++i);
	}*/
}


/************************************************************************
 * Called for each "Complex" node inserted into the graph.
 * Saves all complex operations that have been inserted into a ir_graph.
 * After no additional match can be found in the current ir_graph,
 * the saved operations are exchanged by their backend nodes.
 ************************************************************************/

void save_complex_operation(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *vec_operations, int nr)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	ir_node **repl_node_map = ext_grs_get_replace_node_map(match, which);
	int vop_aiid = ext_grs_act_get_node_aiid(action, VECTOR_OP_NAME); // Find it's ProjM node
	ir_node *vec = repl_node_map[vop_aiid];

	pmap_insert(vec_operations, vec, ia32_construct_funcs[nr]);
#endif
}



/*************************************************************************
 * Saves the intermediate keep nodes along with their complex operations
 * so that they can be exchanged by real be_nodes later.
 *************************************************************************/

void save_keep_nodes(ext_grs_action_t *action, ext_grs_match_t *match, int which, struct pmap *keep_nodes)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	ir_node **repl_node_map = ext_grs_get_replace_node_map(match, which);
	char keep_name[255];
	int keep_aiid, i = 0;
	int vop_aiid = ext_grs_act_get_node_aiid(action, VECTOR_OP_NAME); // Find it's ProjM node
	ir_node *vec = repl_node_map[vop_aiid];

	sprintf(keep_name, "IR_Keep%d", i);
	while((keep_aiid = ext_grs_act_get_node_aiid(action, keep_name)) > -1)
	{
		ir_node *keep = repl_node_map[keep_aiid];

		pmap_insert(keep_nodes, keep, vec);  // Save the keep node and link to it's complex operation
		sprintf(keep_name, "be_Keep%d", ++i);
	}
#endif
}


/************************************************************************
 * Renames the complex operation to its destination op
 ************************************************************************/

void rename_complex_operations(struct pmap *vec_operations)
{
#ifdef INCLUDE_GENERATED_PATTERNS
	pmap_entry *entry;

	pmap_foreach(vec_operations, entry)
	{
		const ir_node *c_vec = entry -> key;
		ir_node *vec = (ir_node *) c_vec;
		ir_node *ben;
		ia32_construct_func_t ia32_construct_func = entry -> value;

		switch(get_irn_arity(vec))
		{
			case 1:
				// ir_node *new_rd_ia32_vadd_2_32(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode);
				ben = ia32_construct_func(NULL, get_irn_irg(get_nodes_block(c_vec)), get_nodes_block(c_vec), get_irn_n(c_vec, 0), get_irn_mode(c_vec));
				exchange(vec, ben);
				break;

			case 2:
				ben = ia32_construct_func(NULL, get_irn_irg(get_nodes_block(c_vec)), get_nodes_block(c_vec), get_irn_n(c_vec, 0), get_irn_n(c_vec, 1), get_irn_mode(c_vec));
				exchange(vec, ben);
				break;

			case 3:
				{
					ir_graph *irg;
					ir_node *block, *op1, *op2, *op3;
					ir_mode *mode;

					block = get_nodes_block(vec);
					irg = get_irn_irg(block);
					op1 = get_irn_n(c_vec, 0);
					op2 = get_irn_n(c_vec, 1);
					op3 = get_irn_n(c_vec, 2);
					mode = get_irn_mode(vec);

					ben = ia32_construct_func(NULL, irg, block, op1, op2, op3, mode);
					exchange(vec, ben);
				}
				break;
			default: assert(0 && "This arity is not supported!");
		}
		//set_irn_op(vec, op);
	}
#endif
}


/************************************************************************
 * Exchanges the intermediate keep nodes by real be_Keep nodes, asking
 * the complex operation for the register constraints.
 ************************************************************************/

static void exchange_keep_nodes(struct pmap *keep_nodes)
{
	pmap_entry *entry;

	pmap_foreach(keep_nodes, entry)
	{
		ir_node *keep = (ir_node *) entry->key;
		ir_node *vec_op;
		ir_node *new_keep, **ins;

		// TODO: Ask the vecop for register constraints.
		vec_op = get_irn_n(get_irn_n(keep, 0), 0);

		ins = get_irn_in(keep);
		new_keep = be_new_Keep(NULL /*&ia32_reg_classes[CLASS_ia32_xmm]*/, get_irn_irg(keep), get_nodes_block(keep), get_irn_arity(keep), &ins[1]);
		exchange(keep, new_keep);
	}
}
