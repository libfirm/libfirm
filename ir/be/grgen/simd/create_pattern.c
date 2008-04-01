/***************************************************************************
 * Program:  create_pattern.c
 * Function: Transforms the given ir_graph into the pattern (search) graph.
 *			Extracts nodes to be dumped by the grgen_dumper and other
 *			information into a graph_ana structure.
 * Author:   Andreas Schoesser
 * Date:     2006-12-07
 ***************************************************************************/

#include <dos.h>
#include <assert.h>
#include <malloc.h>
#include <time.h>
#include <stdlib.h>

#include "simd_presets.h"

#include "obstack.h"
#include "pmap.h"
#include "irgmod.h"
#include "irprog_t.h"
#include "irdump.h"
#include "iredges.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "iroptimize.h"
#include "tv.h"
#include "irtools.h"

#include "simd_presets.h"
#include "firm_node_ext.h"
#include "create_pattern_t.h"
#include "grgen_dumper.h"
#include "be_spec_dumper.h"
#include "normalize.h"
#include "rule_info_dumper.h"



/************************************************************************
 * Generates pattern for one given function
 * representing the C-specification of a complex instruction
 ************************************************************************/

void ext_grs_create_pattern() {
	graph_ana_info_t graph_ana_info;
	char c, grgen_commandline[1000] = "";
	int i, variants, rule_nr = 0;
	struct obstack obst;							 // General obst for objects generated during analysis
	int last_priority = 0;

	// Environments of the used modules
	be_spec_env_t *be_spec_env;
	grgen_dumper_env_t *grgen_dumper_env;
	rule_info_env_t *rule_info_env;

	clock_t start_time, end_time;

	#ifdef DUMP_EVERYTHING
		for(i = 0; i < get_irp_n_irgs(); i++)
		{
			char filename[1000];
			sprintf(filename, "%s.grg", get_entity_name(get_irg_entity(irg)));
			dump_irgraph_complete_grgen(get_irp_irg(i), filename, 0);
		}
		return;
	#endif

	// Generate Header of rule pointers file
	rule_info_env = init_rule_info_dumper(get_irp_n_irgs() * MAX_ADDRESS_VARIANTS);
	be_spec_env = init_be_spec();
	grgen_dumper_env = init_grgen_dumper(GRGEN_GRAPH_FILE, 0);

	// Create VProj FIRM nodes etc.
	ext_firm_nodes();

	start_time = clock();
	for(i = 0; i < get_irp_n_irgs(); i++)
	{
		struct pmap *nodes_to_dump = pmap_create();      // Saves all nodes that have to be dumped after analysis
		struct pmap *register_access = pmap_create();    // Saves LOAD and STORE nodes that access registers instead of memory,
													 	 // along with their array index.
		struct pmap *memory_access = pmap_create();      // Saves LOAD and STORE nodes that access memory along
		struct pmap *argument_nodes = pmap_create();	 // Saves nodes that deliver pointers for memory access

		obstack_init(&obst);

		// Init the walker_info structure, which contains data needed or filled during
		// the analysis of the graph
		graph_ana_info.irg = get_irp_irg(i);
		graph_ana_info.nodes_to_dump = nodes_to_dump;
		graph_ana_info.register_access = register_access;
		graph_ana_info.memory_access = memory_access;
		graph_ana_info.argument_nodes = argument_nodes;
		graph_ana_info.destroyed_regs = pmap_create();
		graph_ana_info.obst = &obst;
		graph_ana_info.num_simd_arguments = 0;
		graph_ana_info.complex_operation_block = NULL;
		graph_ana_info.node_to_match_block = NULL;
		graph_ana_info.emit_statement = NULL;
		graph_ana_info.has_result = 0;
		graph_ana_info.variant_nr = 0;
		graph_ana_info.priority = last_priority; // Maybe the user did not specify a priority in this pattern. Use the last priority then.
		graph_ana_info.cost_savings = 3;
		graph_ana_info.dump_everything = 0;

		printf("*** %s ***:\n", get_entity_name(get_irg_entity(graph_ana_info.irg)));

		// Dump the graph before we change anything
		dump_ir_block_graph(graph_ana_info.irg, "-pre-create-pattern");

		// Normalize the Address calculation for LOADs and STOREs
		set_irg_memory_disambiguator_options(graph_ana_info.irg, aa_opt_no_alias);
		optimize_load_store(graph_ana_info.irg);
		remove_critical_cf_edges(graph_ana_info.irg);
		normalize_address_calculation(graph_ana_info.irg, 0);


		// Dump the graph before we change anything
		dump_ir_block_graph(graph_ana_info.irg, "-post-norm-pattern");


		// Analyze graph
		irg_walk_graph(graph_ana_info.irg, walk_pre, walk_post, &graph_ana_info);
		if(graph_ana_info.emit_statement == NULL)
			assert(0 && "No emit statement given!");

		edges_activate(graph_ana_info.irg);
		for(variants = 0; variants < MAX_ADDRESS_VARIANTS; variants++)
		{
			int uses_memory; // TODO: Maybe find the out during analysis.

			// Dump rule information
			dump_rule_info(rule_info_env, &graph_ana_info, graph_ana_info.num_simd_arguments);
			rule_nr++;

			// Dump graph (that is pattern) in grGen format
			uses_memory = dump_irgraph_grgen(grgen_dumper_env, &graph_ana_info);

			// Dump the backend specification
			dump_be_spec(be_spec_env, &graph_ana_info, uses_memory);

			// Add an address variant
			if(variants < MAX_ADDRESS_VARIANTS - 1)
			{
				if(!add_address_variant(&graph_ana_info))
					break;
				graph_ana_info.variant_nr++;
			}
		}

		// New code placement
		//	set_opt_global_cse(1);
		//	optimize_graph_df(walker_info.irg);
		//	place_code(walker_info.irg);
		//	set_opt_global_cse(0);

		// Dump graph in .vcg format
		dump_ir_block_graph(graph_ana_info.irg, "-post-create-pattern");

		//dump_type_graph (walker_info.irg, "-TYPES");

		// Clean up
		pmap_destroy(nodes_to_dump);
		pmap_destroy(register_access);
		pmap_destroy(argument_nodes);
		pmap_destroy(memory_access);
		pmap_destroy(graph_ana_info.destroyed_regs);

		obstack_free(&obst, NULL);
		obstack_finish(&obst);
		printf("\n");

		last_priority = graph_ana_info.priority;
	}

	deinit_rule_info_dumper(rule_info_env);
	deinit_grgen_dumper(grgen_dumper_env);
	deinit_be_spec(be_spec_env);
	end_time = clock();

	printf("*******************************\n* Pattern Creation Statistics *\n*******************************\n\n");

	printf("Time elapsed:   %g s\n", ((double) (end_time - start_time)) / CLOCKS_PER_SEC);
	printf("Rules created:  %d\n", rule_nr);

	printf("\n\n ** Finished pattern creation **\n\n");

	printf("Run grGen now? (Y/N) ");
	c = getchar();
	if(c == 'y' || c == 'Y')
		run_grgen();
}



/************************************************************************
 * Build a command line in order to run grgen
 * Parameters for the GrGen call are globally set in simd_presets.h
 ************************************************************************/

static void run_grgen()
{
	char grgen_commandline[1000];

	strcpy(grgen_commandline, "java -classpath \"");
	strcat(grgen_commandline, GRGEN_LOCATION);
	strcat(grgen_commandline, "grgen.jar;");
	strcat(grgen_commandline, GRGEN_LOCATION);
	strcat(grgen_commandline, "jars/jargs.jar;");
	strcat(grgen_commandline, GRGEN_LOCATION);
	strcat(grgen_commandline, "jars/antlr.jar\" de.unika.ipd.grgen.Main -n -j -i -b de.unika.ipd.grgen.be.C.SearchPlanBackend -o \"");
	strcat(grgen_commandline, GENERATION_DEST);
	strcat(grgen_commandline, "\" \"");
	strcat(grgen_commandline, GRGEN_GRAPH_FILE);
	strcat(grgen_commandline, "\"");

	printf("%s\n", grgen_commandline);

	// Call grGen to create C source for searching the pattern
	system(grgen_commandline);
}



/*			 		   _                         _           _
  __ _ _ __ __ _ _ __ | |__     __ _ _ __   __ _| |_   _ ___(_)___
 / _` | '__/ _` | '_ \| '_ \   / _` | '_ \ / _` | | | | / __| / __|
| (_| | | | (_| | |_) | | | | | (_| | | | | (_| | | |_| \__ \ \__ \
 \__, |_|  \__,_| .__/|_| |_|  \__,_|_| |_|\__,_|_|\__, |___/_|___/
 |___/          |_|                                |___/               */



/************************************************************************
 * Analyze LOAD and STORE nodes, mark nodes for dumping
 ************************************************************************/

static void walk_pre(ir_node *n, void * env)
{
	graph_ana_info_t *graph_ana_info = (graph_ana_info_t *) env;
	struct pmap *nodes_to_dump = graph_ana_info->nodes_to_dump;
	struct pmap *register_access = graph_ana_info->register_access;
	struct pmap *memory_access = graph_ana_info->memory_access;
	struct pmap *argument_nodes = graph_ana_info->argument_nodes;

	ir_graph *host_irg = graph_ana_info->irg;
	//nodes_list_t *node_to_dump;

	// **** Special case: We have a Store Node
	if(get_irn_opcode(n) == iro_Store)
	{
		ir_node *call_node = NULL, *add_node = NULL, *pointer_root = NULL;
		unsigned int array_index = 0;

		call_node = search_call(n, &array_index, &add_node, &pointer_root);
		if(is_memory_access(call_node))
		{
			memory_access_descr_t *memory_access_descr = obstack_alloc(graph_ana_info->obst, sizeof(memory_access_descr_t));

			// Operations on memory vectors HAVE to access the vector index 0!
			if(array_index == 0)
				mark_argument_node(graph_ana_info, call_node, pointer_root, add_node);

			// Memory STORE, nothing has to be done
			memory_access_descr->load_store = MEMORY_STORE;
			memory_access_descr->array_index = array_index;
			pmap_insert(memory_access, n, (void *) memory_access_descr);
		}
		else
		{
			register_access_descr_t *register_access_descr = obstack_alloc(graph_ana_info->obst, sizeof(register_access_descr_t));

			// Store information about the argument node
			mark_argument_node(graph_ana_info, call_node, pointer_root, add_node);

			// Fill the register access descriptor
			register_access_descr -> array_index = array_index;
			register_access_descr -> load_store  = REGISTER_STORE;
			register_access_descr -> replace_node_name = NULL;
			register_access_descr -> pointer_base = pointer_root;

			// Mark the predecessor of the store node as the result to be stored in a register
			if(get_irn_opcode(get_irn_n(n, 2)) == iro_Conv)
			{
				/* Hack */
				ir_node *conv = get_irn_n(n, 2);
				exchange(conv, get_irn_n(conv, 0));
			}

			pmap_insert(register_access, get_irn_n(n, 2), (void *) register_access_descr);

			// Prevent the Address of the STORE node to be dumped.
			set_irn_visited(get_irn_n(n, 1), get_irg_visited(get_irn_irg(n)));

			// Don't dump the STORE NODE
			return;
		}
	}


	// **** Special case: We have a Load node
	if(get_irn_opcode(n) == iro_Load)
	{
		ir_node *call_node = NULL, *add_node = NULL, *pointer_root = NULL;
		unsigned int array_index = 0;

		//	kill_mem_pred(n);
		call_node = search_call(n, &array_index, &add_node, &pointer_root);

		if(is_memory_access(call_node))
		{
			memory_access_descr_t *memory_access_descr = obstack_alloc(graph_ana_info->obst, sizeof(memory_access_descr_t));

			// Operations on memory vectors HAVE to access the vector index 0!
			if(array_index == 0)
				mark_argument_node(graph_ana_info, call_node, pointer_root, add_node);

			// Memory access, nothing has to be done

			memory_access_descr->load_store = MEMORY_LOAD;
			memory_access_descr->array_index = array_index;
			pmap_insert(memory_access, n, (void *) memory_access_descr);
		}
		else
		{
			// Register access, save information about this load
			register_access_descr_t *register_access_descr = obstack_alloc(graph_ana_info->obst, sizeof(register_access_descr_t));

			mark_argument_node(graph_ana_info, call_node, pointer_root, add_node);

			// Fill the register access descriptor
			register_access_descr->load_store  = REGISTER_LOAD;
			register_access_descr->array_index = array_index;
			register_access_descr -> replace_node_name = NULL;
			register_access_descr -> pointer_base = pointer_root;

			// Mark this LOAD node, which has to be exchanged by a VProj node
			pmap_insert(register_access, n, (void *) (register_access_descr));

			if(add_node != NULL)
				set_irn_visited(add_node, get_irg_visited(get_irn_irg(add_node)));

			pmap_insert(nodes_to_dump, pointer_root, NULL); // TODO: If the value is already there, will it be overwritten?

			// Don't dump this LOAD node.
			return;
		}
	}

	// Mark the current node for Dumping
	switch(get_irn_opcode(n))
	{

		case iro_Call:
			search_emit_statement(n, graph_ana_info);
			break;
		case iro_End: //We match also the end node.
		case iro_Start:
		case iro_Return:
		case iro_Block:
		case iro_SymConst:
		//case iro_Jmp:
		case iro_Sync:
		case iro_Bad:
			break;

		case iro_Phi:
			if(get_irn_modecode(n) != irm_M)
				mark_node_for_dumping(n, graph_ana_info);
			break;

		case iro_Proj:
			// Proj's beyond LOADs and STOREs will be dumped in walk-post method
			// to be sure that all information about the LOAD and STORE has been already computed.
			if(get_irn_opcode(get_irn_n(n, 0)) == iro_Load || get_irn_opcode(get_irn_n(n, 0)) == iro_Store)
				break;
			if(get_irn_opcode(get_irn_n(n, 0)) == iro_Call || get_irn_opcode(get_irn_n(n, 0)) == iro_Start)
				break;
			if(get_irn_modecode(n) == irm_X && get_irg_start_block(graph_ana_info->irg) == get_nodes_block(n)) // We don't need the initial ProjX
				break; // TODO: This is only valid of only one block is there!!!

			// Else Fall through

		default:
			mark_node_for_dumping(n, graph_ana_info);
			break;
	}
}



/************************************************************************
 * Take special care of Proj nodes beyond LOAD and STORE
 * Here we can be sure that the LOAD or STORE node has already been
 * analyzed.
 ************************************************************************/

static void walk_post(ir_node *n, void * env)
{
	graph_ana_info_t *graph_ana_info = (graph_ana_info_t *) env;
	struct pmap *nodes_to_dump = graph_ana_info->nodes_to_dump;
	struct pmap *register_access = graph_ana_info->register_access;
	struct pmap *memory_access = graph_ana_info->memory_access;
	ir_graph *host_irg = graph_ana_info->irg;

	if(get_irn_opcode(n) == iro_Proj)
	{
		ir_node *proj_pred = get_irn_n(n, 0);


		if(get_irn_opcode(proj_pred) == iro_Store)
		{
			ir_mode *store_mode = get_irn_mode(get_Store_value(proj_pred));

			if(pmap_contains(memory_access, proj_pred))
			{
				memory_access_descr_t *memory_access_descr = pmap_get(memory_access, proj_pred);
				memory_access_descr->projm = n;
				memory_access_descr->array_index /= get_mode_size_bytes(store_mode);

				printf("MEMORY STORE at array index %d\n", memory_access_descr->array_index);

				// Mark the current proj for dumping.
				mark_node_for_dumping(n, graph_ana_info);
				//pmap_insert(memory_access, n, memory_access_descr);
			}
			else
			{
				// Adapt array index
				register_access_descr_t *register_access_descr = pmap_get(register_access, get_Store_value(proj_pred));
				register_access_descr->array_index /= get_mode_size_bytes(store_mode);
				printf("REGISTER STORE at array index %d\n", register_access_descr->array_index);
				// Register STORE, don't dump it's ProjM
				return;
			}
		}

		if(get_irn_opcode(proj_pred) == iro_Load)
		{
			ir_mode *load_mode = get_irn_mode(n);

			if(pmap_contains(register_access, proj_pred))
			{
				// Register LOAD: Take special care of proj's


				if(get_irn_modecode(n) != irm_M)
				{
					ir_node *vproj_node;
					register_access_descr_t *register_access_descr = pmap_get(register_access, proj_pred);
					register_access_descr -> array_index /= get_mode_size_bytes(load_mode);
					printf("REGISTER LOAD at array index %d\n", register_access_descr->array_index);

					// We cannot use 'exchange' here, because the node n could already be in
					// the register_access or memory_access list. Exchanging would result in a wrong pointer
					// in those lists. Luckily Proj an VProj use the same node data, so we can just
					// retype the proh to vproj without danger.

					set_irn_n(n, 0, register_access_descr->pointer_base);
					set_irn_op(n, op_VProj);
					vproj_node = n;
					set_VProj_proj(n, register_access_descr->array_index);
				}
				else
				{
					// It's the memory proj. Just don't dump it
					return;
				}
			}
			else
			{
				if(get_irn_modecode(n) != irm_M)
				{
					memory_access_descr_t *memory_access_descr = pmap_get(memory_access, proj_pred);
					memory_access_descr -> array_index /= get_mode_size_bytes(load_mode);
					printf("MEMORY LOAD at array index %d\n", memory_access_descr->array_index);
				}
			}

			// Mark the current proj for dumping.
			mark_node_for_dumping(n, graph_ana_info);
		}
	}
}

void mark_node_for_dumping(ir_node *n, graph_ana_info_t *graph_ana_info)
{
	analyze_complex_operation_block(n, graph_ana_info);
	pmap_insert(graph_ana_info->nodes_to_dump, n, NULL);
	if(get_irn_opcode(n) != iro_Block && get_irn_opcode(n) != iro_Const)
	{
		ir_node *block = get_nodes_block(n);
		pmap_insert(graph_ana_info->nodes_to_dump, block, NULL);
	}
}



/************************************************************************
 * Analyzes the behavior of a LOAD or STORE node
 * Also detects ConvP node and prevents further dumping
 * start_node:  LOAD or STORE node to analyze
 * array_index: Returns the array index of the LOAD or STORE
 * result:      CALL node defining the behavior of the LOAD OR STORE
 ************************************************************************/

static ir_node *search_call(ir_node *start_node, unsigned int *array_index, ir_node **add, ir_node **pointer_root)
{
	ir_node *add_node;
	//ir_node *convP_node;  // If there's convP involved, replace proj_node by conP_node
	ir_node *proj_node;
	ir_node *call_node;
	ir_node *proj_pred_node;
	int     aindex = 0;

	assert(get_irn_opcode(start_node) == iro_Load || get_irn_opcode(start_node) == iro_Store);

	add_node = get_irn_n(start_node, 1);

	if(get_irn_opcode(add_node) != /*iro_Add*/ iro_MultipleAdd)
	{
		// Add node is not there if array Index is 0
		proj_node = add_node;
		add_node = NULL;
		assert(0); // Must not happen when multiple Adds are there
	}
	else
	{
		// Analyze MultipleAdd predecessors
		if(get_irn_opcode(get_irn_n(add_node, 0)) == iro_Proj)
		{
			proj_node = get_irn_n(add_node, 0);
			aindex = get_tarval_long(get_Const_tarval(get_irn_n(add_node, 1))); // get_mode_size_bytes(get_irn_mode()) /*4*/;  // TODO: varibale TARVALS
		}
		else
		{
			proj_node = get_irn_n(add_node, 1);
			aindex = get_tarval_long(get_Const_tarval(get_irn_n(add_node, 0)));// / 4;  // TODO: varibale TARVALS
		}
	}

	assert(get_irn_opcode(proj_node) == iro_Proj);
	//call_node = get_irn_n(get_irn_n(get_irn_n(convP_node, 0), 0), 0); // ConvP -> ProjIs -> ProjT -> Call
	call_node = get_irn_n(get_irn_n(proj_node, 0), 0); // ProjIs -> ProjT -> Call
	assert(get_irn_opcode(call_node) == iro_Call && "Call node not found! Pattern not valid.");

	*array_index = aindex;
	*add = add_node;
	*pointer_root = proj_node;

	// Prevent further dumping of nodes starting from that convP Node
	proj_pred_node = get_irn_n(proj_node, 0);
	set_irn_visited(proj_pred_node, get_irg_visited(get_irn_irg(proj_pred_node)));

	return(call_node);
}



/************************************************************************
 * Marks a vector base pointer to be used as an argument for the
 * complex operation afterwards
 ************************************************************************/

static void mark_argument_node(graph_ana_info_t *graph_ana_info, ir_node *call_node, ir_node *argument_node, ir_node *add_node)
{
	const char *arg_name;
	int  arg_nr = -1, i;

	// Look if we marked that node already
	if(!pmap_contains(graph_ana_info->argument_nodes, argument_node))
	{
		// No. Find out which argument number this node will be
		// for complex operation
		arg_name = get_entity_name(get_SymConst_entity(get_irn_n(call_node, 1)));

		// Is it the result pointer?
		if(strcmp(arg_name, RESULT_NAME) == 0)
		{
			// Arg nr is -1 since we don't know how much
			char *register_class;
			argument_descr_t *argument_descr = obstack_alloc(graph_ana_info->obst, sizeof(argument_descr_t));

			register_class = obstack_alloc(graph_ana_info->obst, get_SymConst_strlen(get_irn_n(call_node, 4)));
			get_SymConst_string(get_irn_n(call_node, 4), register_class);

			argument_descr -> arg_nr = -1;
			argument_descr -> argument_location = ARGUMENT_RESULT;			// TODO: This is wrong, Memory or Register here
			argument_descr -> argument_type = (is_vector(call_node) ? ARGUMENT_VECTOR : ARGUMENT_SCALAR);
			argument_descr -> register_class = register_class;
			argument_descr -> vec_op_input = add_node; // Only important for memory access nodes

			pmap_insert(graph_ana_info->argument_nodes, argument_node, (void *) argument_descr);
			graph_ana_info->has_result = 1;
			return;
		}

		for(i = 0; i < MAX_SIMD_ARGUMENTS; i++)
			if(strcmp(arg_name, SIMD_ARGUMENTS[i]) == 0)
			{
				char *register_class;
				argument_descr_t *argument_descr = obstack_alloc(graph_ana_info->obst, sizeof(argument_descr_t));

				register_class = obstack_alloc(graph_ana_info->obst, get_SymConst_strlen(get_irn_n(call_node, 4)));
				get_SymConst_string(get_irn_n(call_node, 4), register_class);

				argument_descr -> arg_nr = i;
				argument_descr -> argument_location = (!is_memory_access(call_node) ? ARGUMENT_SIMD_REGISTER : ARGUMENT_MEMORY);
				argument_descr -> argument_type = (is_vector(call_node) ? ARGUMENT_VECTOR : ARGUMENT_SCALAR);
				argument_descr -> register_class = register_class;
				argument_descr -> vec_op_input = add_node;

				pmap_insert(graph_ana_info->argument_nodes, argument_node, (void *) argument_descr);
				graph_ana_info->num_simd_arguments = MAX(i, graph_ana_info->num_simd_arguments);
				break;
			}
	}
}



/************************************************************************
 * Tests, if the given call_node is the emit-statement
 * If so, saves the emit-statement for dumping later in the process.
 ************************************************************************/

void search_emit_statement(ir_node *call_node, graph_ana_info_t *graph_ana_info)
{
	ir_node *symC_name;
	ir_node *symC_arg;
	const char *function_name;
	int i;

	assert(get_irn_opcode(call_node) == iro_Call);

	// Prevent walker to consider arguments of the call node.
	for(i = 1; i < get_irn_arity(call_node); i++)
		set_irn_visited(get_irn_n(call_node, i), get_irg_visited(get_irn_irg(call_node)));

	// Check the function name
	symC_name = get_irn_n(call_node, 1);
	function_name = get_entity_name(get_SymConst_entity(symC_name));

	// Save the emit statement
	if(strcmp(function_name, EMIT) == 0)
	{
		// Allocate space for the emit statement and save it.
		char *emit_statement;
		symC_arg = get_irn_n(call_node, 2);
		emit_statement = obstack_alloc(graph_ana_info->obst, get_SymConst_strlen(symC_arg));
		get_SymConst_string(symC_arg, emit_statement);
		graph_ana_info->emit_statement = emit_statement;
		return;
	}

	// Save the extra registers, the complex operation destroys
	if(strcmp(function_name, DESTROYS) == 0)
	{
		char *destroy_statement;
		symC_arg = get_irn_n(call_node, 2);
		destroy_statement = obstack_alloc(graph_ana_info->obst, get_SymConst_strlen(symC_arg));

		get_SymConst_string(symC_arg, destroy_statement);
		pmap_insert(graph_ana_info->destroyed_regs, call_node, destroy_statement);
	}

	if(strcmp(function_name, PRIORITY) == 0)
	{
		ir_node *c = get_irn_n(call_node, 2);		// Get the constant argument
		assert(get_irn_opcode(c) == iro_Const && "Something is wrong with the priority node");
		graph_ana_info->priority = get_tarval_long(get_Const_tarval(c));
	}

	if(strcmp(function_name, COST_SAVINGS) == 0)
	{
		ir_node *c = get_irn_n(call_node, 2);		// Get the constant argument
		assert(get_irn_opcode(c) == iro_Const && "Something is wrong with the CostSavings node");

		graph_ana_info->cost_savings = get_tarval_long(get_Const_tarval(c));
	}
}


#if 0

Not needed any more

/************************************************************************/
/* Cut's of the Memory predecessor of a LOAD node                       */
/* We don't need that node in the final pattern.                        */
/************************************************************************/

static void kill_mem_pred(ir_node *load)
{
	assert(get_irn_opcode(load) == iro_Load);

	// TODO: Think about this. Is this really enough?
	if(get_irn_opcode(get_irn_n(get_irn_n(load, 0), 0)) == iro_Call)
		set_irn_visited(get_irn_n(load, 0), get_irg_visited(get_irn_irg(load)));
}

#endif



/************************************************************************
 * Returns for a given call node if the underlying LOAD or STORE access
 * memory (1) or not (0)
 ************************************************************************/
static int is_memory_access(ir_node *call)
{
	char symC_string[255];
	ir_node *symC;

	assert(get_irn_opcode(call) == iro_Call);
	symC = get_irn_n(call, 3);
	assert(get_irn_opcode(symC) == iro_SymConst);

	get_SymConst_string(symC, symC_string);
	if(strstr(symC_string, MEMORY_ARRAY) != 0)
		return(1);
	return(0);
}


/************************************************************************/
/* Returns 1 if the given argument represents a vector                  */
/************************************************************************/

static int is_vector(ir_node *call)
{
	char symC_string[255];
	ir_node *symC;

	assert(get_irn_opcode(call) == iro_Call);
	symC = get_irn_n(call, 2);
	assert(get_irn_opcode(symC) == iro_SymConst);
	get_SymConst_string(symC, symC_string);
	if(strstr(symC_string, "vector") != 0)
		return(1);
	return(0);
}



/************************************************************************
 * Analyze, if the current pattern node is in the immediate post
 * dominator block of the start block
 ************************************************************************/

/*
 Der Block jedes aufgenommenen Knotens wird ueberprueft.
 Falls dieser der direkte Nachdominator des Startblocks ist, ist das der Block, in den der komplexe Befehl soll.

 Problem:
 Wird der Block dann auch wirklich mit einem Knoten des Musters verbunden?
 Beim Component-Fall anscheinend nicht.
 */

void analyze_complex_operation_block(ir_node *n, graph_ana_info_t *graph_ana_info)
{
	if(graph_ana_info->complex_operation_block == NULL && get_irn_opcode(n) != iro_Block)
	{
		ir_node *block = get_nodes_block(n);
		if(block != get_irg_start_block(graph_ana_info->irg))
		{
			ir_node *block_pred = get_irn_n(block, 0);
			if(get_irn_opcode(block_pred) != iro_Block)
				block_pred = get_nodes_block(block_pred);
			if(block_pred == get_irg_start_block(graph_ana_info->irg))
			{
				if(!pmap_contains(graph_ana_info->argument_nodes, n))
				{
					graph_ana_info->complex_operation_block = block;
					graph_ana_info->node_to_match_block = n;
					//pmap_insert(walker_info->nodes_to_dump, block, NULL);
				}
			}
		}
	}
}



/*               _             _
__   ____ _ _ __(_) __ _ _ __ | |_ ___
\ \ / / _` | '__| |/ _` | '_ \| __/ __|
 \ V / (_| | |  | | (_| | | | | |_\__ \
  \_/ \__,_|_|  |_|\__,_|_| |_|\__|___/  */

/************************************************************************
 * Generates an address mode variant
 * Returns 0 if no variant was generated because no address calculation
 * is made.
 ************************************************************************/

int add_address_variant(graph_ana_info_t *graph_ana_info)
{
	int i;
	struct pmap *argument_nodes = graph_ana_info->argument_nodes;
	int found = 0;

	pmap_entry *entry;

	pmap_foreach(argument_nodes, entry)
	{
		ir_node *arg = (ir_node *) entry->key;
		argument_descr_t *arg_descr = entry->value;
		const ir_edge_t *edge;
		ir_node *generic_node;

		if(arg_descr -> argument_type == ARGUMENT_VECTOR && arg_descr -> argument_location == ARGUMENT_MEMORY)
		{
			generic_node = new_ir_node(NULL, graph_ana_info->irg, get_nodes_block(arg), op_IrNode, mode_ANY, 0, NULL);
			set_irn_mode(generic_node, mode_ANY);
			pmap_insert(graph_ana_info->nodes_to_dump, generic_node, NULL);
			foreach_out_edge_kind(arg, edge, EDGE_KIND_NORMAL)
			{
				ir_node *mult_add = get_edge_src_irn(edge);
				int new_arity = get_irn_arity(mult_add) + 1;
				ir_node **new_ins = alloca(new_arity * sizeof(ir_node *));

				found = 1;
				assert(get_irn_opcode(mult_add) == iro_MultipleAdd);

				for(i = 0; i < get_irn_arity(mult_add); i++)
					new_ins[i] = get_irn_n(mult_add, i);
				new_ins[i] = generic_node;
				set_irn_in(mult_add, new_arity, new_ins);
			}

		}
	}
	return(found);
}



/*	   	    _
  _ __ ___ (_)___  ___
 | '_ ` _ \| / __|/ __|
 | | | | | | \__ \ (__
 |_| |_| |_|_|___/\___| */


/************************************************************************
 * Concatenates the rule name out of the irg name and the variant
 * number
 ************************************************************************/

void get_rule_name(graph_ana_info_t *graph_ana_info, char *rule_name)
{
	ir_graph *irg = graph_ana_info->irg;
	sprintf(rule_name, "%s_variant%d", get_entity_name(get_irg_entity(irg)), graph_ana_info->variant_nr);
}



/************************************************************************
 * Gets the number of characters contained in a string SymConst
 * including the trailing 0-Character
 ************************************************************************/

int get_SymConst_strlen(ir_node *symC)
{
	ir_entity *ent = get_SymConst_entity(symC);
	return(get_compound_ent_n_values(ent) + 1);
}



/************************************************************************
 * Reads the string assiciated with a sym_const and stores it in
 * 'string'
 ************************************************************************/

void get_SymConst_string(ir_node *symC, char *string)
{
	ir_entity *ent = get_SymConst_entity(symC);
	int i, n;

	n = get_compound_ent_n_values(ent);

	for (i = 0; i < n-1; i++) {
		ir_node *irn;
		int c;

		irn = get_compound_ent_value(ent, i);
		c = (int) get_tarval_long(get_Const_tarval(irn));
		string[i] = c;
	}
	string[i] = 0;
}
