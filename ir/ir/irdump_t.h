

#ifndef __IRDUMPT_T_H__
#define __IRDUMPT_T_H__

#include "irdump.h"

/* Attributes of nodes */
#define PRINT_DEFAULT_NODE_ATTR
#define DEFAULT_NODE_ATTR " "
#define DEFAULT_TYPE_ATTRIBUTE " "
#define DEFAULT_ENUM_ITEM_ATTRIBUTE " "

/* Attributes of edges between Firm nodes */
#define INTRA_DATA_EDGE_ATTR "class:1  priority:50"
#define INTER_DATA_EDGE_ATTR "class:16 priority:10"
#define BLOCK_EDGE_ATTR      "class:2  priority:50 linestyle:dotted"
#define CF_EDGE_ATTR         "class:13 priority:60 color:red"
#define EXC_CF_EDGE_ATTR     "class:18 priority:60 color:blue"
#define INTRA_MEM_EDGE_ATTR  "class:14 priority:50 color:blue"
#define INTER_MEM_EDGE_ATTR  "class:17 priority:10 color:blue"
#define DOMINATOR_EDGE_ATTR  "class:15 color:red"

#define BACK_EDGE_ATTR "linestyle:dashed "

/* Attributes of edges between Firm nodes and type/entity nodes */
#define NODE2TYPE_EDGE_ATTR "class:2 priority:2 linestyle:dotted"

/* Attributes of edges in type/entity graphs. */
#define TYPE_METH_NODE_ATTR      "color: lightyellow"
#define TYPE_CLASS_NODE_ATTR     "color: green"
#define TYPE_DESCRIPTION_NODE_ATTR "color: lightgreen"
#define ENTITY_NODE_ATTR         "color: yellow"
#define ENT_TYPE_EDGE_ATTR       "class: 3 label: \"type\" color: red"
#define ENT_OWN_EDGE_ATTR        "class: 4 label: \"owner\" color: black"
#define METH_PAR_EDGE_ATTR       "class: 5 label: \"param %d\" color: green"
#define METH_RES_EDGE_ATTR       "class: 6 label: \"res %d\" color: green"
#define TYPE_SUPER_EDGE_ATTR     "class: 7 label: \"supertype\" color: red"
#define UNION_EDGE_ATTR          "class: 8 label: \"component\" color: blue"
#define PTR_PTS_TO_EDGE_ATTR     "class: 9 label: \"points to\" color:green"
#define ARR_ELT_TYPE_EDGE_ATTR   "class: 10 label: \"arr elt tp\" color:green"
#define ARR_ENT_EDGE_ATTR        "class: 10 label: \"arr ent\" color: green"
#define ENT_OVERWRITES_EDGE_ATTR "class: 11 label: \"overwrites\" color:red"
#define ENT_VALUE_EDGE_ATTR      "label: \"value %d\""
#define ENT_CORR_EDGE_ATTR       "label: \"value %d corresponds to \" "
#define TYPE_MEMBER_EDGE_ATTR    "class: 12 label: \"member\" color:blue"
#define ENUM_ITEM_NODE_ATTR      "color: green"
#define CALLGRAPH_EDGE_ATTR      "calls"

#define PRINT_NODEID(X)       fprintf(F, "n%ld", get_irn_node_nr(X))
#define PRINT_TYPEID(X)       fprintf(F, "\"t%ld\"", get_type_nr(X))
#define PRINT_ENTID(X)        fprintf(F, "e%ld", get_entity_nr(X))
#define PRINT_IRGID(X)        fprintf(F, "g%ld", get_irg_graph_nr(X))
#define PRINT_CONSTID(X,Y)    fprintf(F, "\"n%ldn%ld\"", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_CONSTBLKID(X,Y) fprintf(F, "n%ldb%ld", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_LOOPID(X)       fprintf(F, "l%d", get_loop_loop_nr(X))
#define PRINT_ITEMID(X,Y)     fprintf(F, "i%ldT%d", get_type_nr(X), (Y))

extern int dump_dominator_information_flag;
extern bool opt_dump_pointer_values_to_info;

FILE *vcg_open (ir_graph *irg, const char * suffix1, const char *suffix2);
FILE *vcg_open_name (const char *name, const char *suffix);
void dump_vcg_header(FILE *F, const char *name, const char *orientation);
const char *get_irg_dump_name(ir_graph *irg);
void vcg_close (FILE *F);


const char *get_ent_dump_name(entity *ent);
const char *get_type_name_ex(type *tp, int *bad);
const char *get_mode_name_ex(ir_mode *mode, int *bad);
/**
 * dump the name of a node n to the File F.
 */
int
dump_node_opcode(FILE *F, ir_node *n);

#endif /* __IRDUMPT_T_H__ */
