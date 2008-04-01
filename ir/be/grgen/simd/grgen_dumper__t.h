/*************************************************************************
* Program:  grgen_dumper_t.h
* Function: Dumps parts of a firm graph (those which have to be extracted
*			 as search and replace patterns) as a grgen rule.
* Author:   Andreas Schoesser
* Date:     2007-01-11
*************************************************************************/

#ifndef GRGEN_DUMPER_H_T
#define GRGEN_DUMPER_H_T

#include "grgen_dumper.h"

#define MAX_NODENAME_LEN 40

typedef struct
{
	ir_graph *irg;
	struct pmap *mode_edge_map;
	struct pmap *edge_name_map;
	struct pmap *node_name_map;  // Contains the mapping firm node -> node name
	struct obstack node_names;   // Contains the node name data
	struct pmap *mode_name_map;  // Contains the mapping firm mode -> mode node name
	struct obstack mode_names;   // Contains the "mode node name" data
	struct pmap *register_access;  // Contains all nodes that are register access and their vector component
	struct pmap *memory_access;    // Contains all nodes that are memory access and their vector component
	struct pmap *nodes_to_dump;         // Contains firm nodes, that have to be dumped
	struct pmap *argument_nodes;    // Contains "pointer root nodes" (usually ProjP), which have to be transformed to a generic firm node mode P
	struct pmap *retyped_nodes;         // Contains all nodes that were retyped in the replacement step and therefore no edges have to be dumped.
										// (Retyped nodes get completely new edges in our case)
	struct pmap *dump_condition;        // Contains all nodes for which a conditions has to be dumped afterwards.
	struct pmap *eval_statements;       // Contains strings of eval statements dropped during replacement dumping and to be dumped at the end
	struct pmap *destroyed_regs;
	ir_node     *complex_operation_block, *node_to_match_block;
	char        *node_name;
	int         has_result;
	int			priority;
	int			dump_everything;	// Used to dump whole FIRM graphs with memory egdes ....
} dump_info_t;

static int dump_pattern(dump_info_t *dump_info, FILE *fp);
static void dump_replacement(dump_info_t *dump_info, int uses_memory, FILE *fp);
void dump_grgen_node(ir_node *n, dump_info_t *dump_info, FILE *fp);
static char *dump_grgen_retyped_node(ir_node *n, ir_op *new_op, dump_info_t *dump_info, FILE *fp);
static void dump_grgen_block(ir_node *n, dump_info_t *dump_info, FILE *fp);
void dump_grgen_edge(ir_node *n, int n_edge, dump_info_t *dump_info, FILE *fp);
static void dump_grgen_mode(ir_node *n, dump_info_t *dump_info, FILE *fp, ir_mode *alt_mode);
char *dump_grgen_mode_node(ir_mode *irn_mode, dump_info_t *dump_info, FILE *fp);
void dump_grgen_condition(ir_node *n, dump_info_t *dump_info, FILE *fp);
static void dump_retyping_to_vproj(ir_node *n, ir_node *complex_operation, dump_info_t *dump_info, char *vector_op_node_name, FILE *fp);
static void dump_retyping_to_proj(ir_node *n, ir_node *complex_operation, dump_info_t *dump_info, char *vector_op_node_name, FILE *fp);
static void dump_vproj_nr(dump_info_t *dump_info, FILE *fp);
void dump_complex_operation_block_match(dump_info_t *dump_info, FILE *fp);
void dump_complex_operation(dump_info_t *dump_info, FILE *fp, int uses_memory);
void dump_mandatory_evals(dump_info_t *dump_info, int uses_memory, FILE *fp);
void add_eval(dump_info_t *dump_info, char *eval);
void change_mode(ir_node *n, ir_mode *m, dump_info_t *dump_info, FILE *fp);
void dump_grgen_delete_incoming_edges(ir_node *n, dump_info_t *dump_info, FILE *fp);
static void collect_nodes(ir_node *n, void * env);

void set_indent(int i);
int  get_indent(void);

#endif
