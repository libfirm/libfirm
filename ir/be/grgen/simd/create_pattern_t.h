/*************************************************************************
* Program:  create_pattern_t.h
* Function: Extracts nodes to be dumped as search pattern out of the
*			the initial pattern
* Author:   Andreas Schoesser
* Date:     2006-12-07
*************************************************************************/

#ifndef CREATE_PATTERN_T_H
#define CREATE_PATTERN_T_H

#include "irgraph.h"

// * This struct contains information that was collected during the pattern
// * analysis.
typedef struct
{
	ir_graph *irg;					// The irg that was analysed
	struct pmap *nodes_to_dump;		// Contains all nodes which have to be dumped to create the pattern
	struct pmap *register_access;	// Contains additional information for nodes which represent a register access
	struct pmap *memory_access;		// Contains additional information for nodes which represent a memory access
	struct pmap *argument_nodes;    // Contains additional information for nodes which represent an arument for the complex operatione
	struct pmap *destroyed_regs;     // Contains an entry for each extra register a complex operation destroys (except the target register which is clear to be destroyed)
	struct obstack *obst;			// Memory allocated during analysis, will be killed after analysis is finished
	int num_simd_arguments;         // Number of arguments the complex operation will have
	ir_node *complex_operation_block, // Used to match the block the complex operation will be placed in
			*node_to_match_block;	  // Currently, only one pattern node's block is matched
	char *emit_statement;			// String that describes the assembly emit statement
	int  has_result;                // Is set to 1 if the complex operation has a result
	int  variant_nr;
	int  priority;
	int  cost_savings;
	int  dump_everything;
} graph_ana_info_t;


// * Additional information for nodes which represent a register access
typedef struct
{
	int load_store;				// REGISTER_STORE or REGISTER_LOAD
	int array_index;			// The vector component of that access
	char *replace_node_name;	// The node name of the VProj that replaces the node that generates the result (Register STORE)
	ir_node *pointer_base;      // Represents the complex operation in case of a register LOAD
} register_access_descr_t;


// * Additional information for nodes which represent a memory access
typedef struct
{
	int load_store;				// MEMORY_STORE or MEMORY_LOAD
	int array_index;			// The vector component of that access
	ir_node *projm;				// Represents the projm node in case of a memory store
} memory_access_descr_t;


typedef enum
{
	ARGUMENT_SIMD_REGISTER = 0,
	ARGUMENT_GP_REGISTER,
	ARGUMENT_MEMORY,
	ARGUMENT_RESULT
} argument_location_t;

typedef enum
{
	ARGUMENT_SCALAR = 0,
	ARGUMENT_VECTOR
} argument_type_t;



// * Additional information for nodes which represent an argument for the complex operation
typedef struct
{
	argument_location_t argument_location;      // ARGUMENT_SIMD_REGISTER or MEMORY
	argument_type_t     argument_type;          // VECTOR or SCALAR
    int arg_nr;							        // The argument number, this node will be for the complex operation
	char *register_class;					    // The register class used for the backend specification
	ir_node *vec_op_input;                      // This is important for variants:
												// Points to the node that computes the vector address inside a
												// (multi dim) array.
} argument_descr_t;


// Internal prototypes

static ir_node *search_call(ir_node *start_node, unsigned int *array_index, ir_node **add, ir_node **pointer_root);
static int is_memory_access(ir_node *call);
void search_emit_statement(ir_node *call_node, graph_ana_info_t *walker_info);
static void mark_argument_node(graph_ana_info_t *walker_info, ir_node *call_node, ir_node *argument_node, ir_node *add_node);
void get_operation_name(const char *irg_name, char *operation_name);
void get_firmnode_name(const char *irg_name, char *firmnode_name);
void analyze_complex_operation_block(ir_node *n, graph_ana_info_t *walker_info);
int get_SymConst_strlen(ir_node *symC);
void get_SymConst_string(ir_node *symC, char *string);
static void walk_pre(ir_node *n, void * env);
static void walk_post(ir_node *n, void * env);
static int is_vector(ir_node *call);
int add_address_variant(graph_ana_info_t *walker_info);
void get_rule_name(graph_ana_info_t *walker_info, char *rule_name);
void mark_node_for_dumping(ir_node *n, graph_ana_info_t *walker_info);
static void run_grgen();

#endif
