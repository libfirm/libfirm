/*
 * Project:     libFIRM
 * File name:   ir/ir/irdump.c
 * Purpose:     Write vcg representation of firm to file.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Hubert Schmidt
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <string.h>
# include <stdlib.h>
# include <stdarg.h>

# include "irnode_t.h"
# include "irgraph_t.h"
# include "entity_t.h"
# include "irop_t.h"
# include "firm_common_t.h"

# include "irdump.h"

# include "irgwalk.h"
# include "typewalk.h"
# include "irprog.h"
# include "tv_t.h"
# include "type_or_entity.h"
# include "irouts.h"
# include "irdom.h"
# include "irloop.h"

# include "panic.h"
# include "array.h"
# include "pmap.h"

# include "exc.h"

//#define HEAPANAL
#ifdef HEAPANAL
void dump_chi_term(FILE *FL, ir_node *n);
void dump_state(FILE *FL, ir_node *n);
int  get_opt_dump_abstvals(void);
typedef unsigned long SeqNo;
SeqNo get_Block_seqno(ir_node *n);
#endif

/* Attributes of nodes */
#define PRINT_DEFAULT_NODE_ATTR
#define DEFAULT_NODE_ATTR " "
#define DEFAULT_TYPE_ATTRIBUTE " "

/* Attributes of edges between Firm nodes */
#define BLOCK_EDGE_ATTR     "class: 2 priority: 2 linestyle: dotted"
#define CF_EDGE_ATTR        "color: red"
#define MEM_EDGE_ATTR       "color: blue"
#define DOMINATOR_EDGE_ATTR "color: red"

#define BACK_EDGE_ATTR "linestyle: dashed "

/* Attributes of edges between Firm nodes and type/entity nodes */
#define NODE2TYPE_EDGE_ATTR "class: 2 priority: 2 linestyle: dotted"

/* Attributes of edges in type/entity graphs. */
#define TYPE_METH_NODE_ATTR  "color: lightyellow"
#define TYPE_CLASS_NODE_ATTR "color: green"
#define TYPE_DESCRIPTION_NODE_ATTR "color: lightgreen"
#define ENTITY_NODE_ATTR     "color: yellow"
#define ENT_TYPE_EDGE_ATTR   "class: 3 label: \"type\" color: red"
#define ENT_OWN_EDGE_ATTR    "class: 4 label: \"owner\" color: black"
#define METH_PAR_EDGE_ATTR   "class: 5 label: \"param %d\" color: green"
#define METH_RES_EDGE_ATTR   "class: 6 label: \"res %d\" color: green"
#define TYPE_SUPER_EDGE_ATTR "class: 7 label: \"supertype\" color: red"
#define UNION_EDGE_ATTR      "class: 8 label: \"component\" color: blue"
#define PTR_PTS_TO_EDGE_ATTR "class: 9 label: \"points to\" color:green"
#define ARR_ELT_TYPE_EDGE_ATTR "class: 10 label: \"arr elt tp\" color:green"
#define ARR_ENT_EDGE_ATTR    "class: 10 label: \"arr ent\" color: green"
#define ENT_OVERWRITES_EDGE_ATTR "class: 11 label: \"overwrites\" color:red"
#define ENT_VALUE_EDGE_ATTR "label: \"value %d\""
#define ENT_CORR_EDGE_ATTR "label: \"value %d corresponds to \" "
#define TYPE_MEMBER_EDGE_ATTR "class: 12 label: \"member\" color:blue"


#if DEBUG_libfirm && NODEID_AS_LABEL
#define PRINT_NODEID(X) fprintf(F, "n%ld", get_irn_node_nr(X))
#define PRINT_TYPEID(X) fprintf(F, "\"t%ld\"", get_type_nr(X))
#define PRINT_ENTID(X)  fprintf(F, "e%ld", get_entity_nr(X))
#define PRINT_IRGID(X)  fprintf(F, "g%ld", get_irg_graph_nr(X))
#define PRINT_CONSTID(X,Y) fprintf(F, "\"n%ldn%ld\"", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_LOOPID(X) fprintf(F, "l%p", (void *)X)

#else
#define PRINT_NODEID(X) fprintf(F, "n%p", (void*) X)
#define PRINT_TYPEID(X) fprintf(F, "\"t%p\"", (void *) X)
#define PRINT_ENTID(X)  fprintf(F, "e%p", (void*) X)
#define PRINT_IRGID(X)  fprintf(F, "g%p",(void*) X)
#define PRINT_CONSTID(X,Y) fprintf(F, "\"n%pn%p\"", (void*) X, (void*) Y)
#define PRINT_LOOPID(X) fprintf(F, "l%p", (void *)X)
#endif

static void print_type_type_edge(FILE *F, type *S, type *T, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: "); PRINT_TYPEID(S);
  fprintf(F, " targetname: "); PRINT_TYPEID(T);
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

static void print_type_ent_edge(FILE *F, type *T, entity *E, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: "); PRINT_TYPEID(T);
  fprintf(F, " targetname: \""); PRINT_ENTID(E); fprintf(F, "\"");
  vfprintf(F, fmt, ap);
  fprintf(F, "}\n");
  va_end(ap);
}

static void print_ent_ent_edge(FILE *F, entity *E, entity *T, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: \""); PRINT_ENTID(E);
  fprintf(F, "\" targetname: \""); PRINT_ENTID(T);  fprintf(F, "\"");
  vfprintf(F, fmt, ap);
  fprintf(F, "}\n");
  va_end(ap);
}

static void print_ent_type_edge(FILE *F, entity *E, type *T, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: \""); PRINT_ENTID(E);
  fprintf(F, "\" targetname: "); PRINT_TYPEID(T);
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

static void print_node_type_edge(FILE *F, const ir_node *N, type *T, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: \""); PRINT_NODEID(N);
  fprintf(F, "\" targetname: "); PRINT_TYPEID(T);
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

static void print_node_ent_edge(FILE *F, const ir_node *N, entity *E, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: \""); PRINT_NODEID(N);
  fprintf(F, "\" targetname: \""); PRINT_ENTID(E);
  fprintf(F, "\"");
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

static void print_ent_node_edge(FILE *F, entity *E, const ir_node *N, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: \""); PRINT_ENTID(E);
  fprintf(F, "\" targetname: \""); PRINT_NODEID(N); fprintf(F, "\"");
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

/*******************************************************************/
/* global and ahead declarations                                   */
/*******************************************************************/

/* A suffix to manipulate the file name. */
char *dump_file_suffix = "";

/* file to dump to */
static FILE *F;

static void dump_whole_node(ir_node *n, void* env);
static INLINE void dump_loop_info(ir_graph *irg);

/*******************************************************************/
/* Helper functions.                                                */
/*******************************************************************/

/* Use private link attr to be able to call dumper anywhere without
   destroying link fields. */

static pmap *irdump_link_map = NULL;

static void init_irdump(void) {
  /* We need a new, empty map. */
  if (irdump_link_map) pmap_destroy(irdump_link_map);
  irdump_link_map = pmap_create();
}


void *ird_get_irn_link(ir_node *n) {
  void *res = NULL;
  if (!irdump_link_map) return NULL;

  if (pmap_contains(irdump_link_map, (void *)n))
    res = pmap_get(irdump_link_map, (void *)n);
  return res;
}

void ird_set_irn_link(ir_node *n, void *x) {
  if (!irdump_link_map) init_irdump();
  pmap_insert(irdump_link_map, (void *)n, x);
}

void *ird_get_irg_link(ir_graph *irg) {
  void *res = NULL;
  if (!irdump_link_map) return NULL;

  if (pmap_contains(irdump_link_map, (void *)irg))
    res = pmap_get(irdump_link_map, (void *)irg);
  return res;
}

void ird_set_irg_link(ir_graph *irg, void *x) {
  if (!irdump_link_map) init_irdump();
  pmap_insert(irdump_link_map, (void *)irg, x);
}

static void clear_link(ir_node * node, void * env) {
  ird_set_irn_link(node, NULL);
}


static int node_floats(ir_node *n) {
  return ((get_op_pinned(get_irn_op(n)) == floats) &&
	  (get_irg_pinned(current_ir_graph) == floats));
}

static const char *get_ent_dump_name (entity *ent) {
  /* Don't use get_entity_ld_ident (ent) as it computes the mangled name! */
  if (ent->ld_name) return get_id_str(ent->ld_name);
  return get_id_str(ent->name);
}

static const char *get_irg_dump_name (ir_graph *irg) {
  /* Don't use get_entity_ld_ident (ent) as it computes the mangled name! */
  entity *ent = get_irg_ent(irg);
  return get_ent_dump_name(ent);
}

static void collect_node(ir_node * node, void *env) {
  if (is_Block(node)
      || node_floats(node)
      || get_irn_op(node) == op_Bad
      || get_irn_op(node) == op_Unknown) {
    ir_node ** arr = (ir_node **) ird_get_irg_link(get_irn_irg(node));
    if (!arr) arr = NEW_ARR_F(ir_node *, 0);
    ARR_APP1(ir_node *, arr, node);
    ird_set_irg_link(get_irn_irg(node), arr);    /* arr is an l-value, APP_ARR might change it! */
  } else {
    ir_node * block = get_nodes_block(node);
    ird_set_irn_link(node, ird_get_irn_link(block));
    ird_set_irn_link(block, node);
  }
}

/** Construct lists to walk ir block-wise.
 *
 * Collects all blocks, nodes not pinned,
 * Bad and Unknown into a flexible array in link field of
 * irg they belong to.  Sets the irg link field to NULL in all
 * graphs not visited.
 * Free the list with DEL_ARR_F.  */
static ir_node ** construct_block_lists(ir_graph *irg) {
  int i;
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  for (i = 0; i < get_irp_n_irgs(); i++)
    ird_set_irg_link(get_irp_irg(i), NULL);

  irg_walk_graph(current_ir_graph, clear_link, collect_node, current_ir_graph);

  current_ir_graph = rem;
  return ird_get_irg_link(irg);
}

/*******************************************************************/
/* flags to steer output                                           */
/*******************************************************************/

/* A compiler option to turn off edge labels */
int edge_label = 1;
/* A compiler option to turn off dumping values of constant entities */
int const_entities = 1;
/* A compiler option to dump the keep alive edges */
int dump_keepalive = 0;
/* Compiler options to dump analysis information in dump_ir_graph */
int dump_out_edge_flag = 0;
int dump_dominator_information_flag = 0;
int dump_loop_information_flag = 0;
int dump_backedge_information_flag = 1;
int dump_const_local = 0;
bool opt_dump_analysed_type_info = 1;
bool opt_dump_pointer_values_to_info = 0;  /* default off: for test compares!! */

INLINE bool get_opt_dump_const_local(void) {
  if (!dump_out_edge_flag && !dump_loop_information_flag)
    return dump_const_local;
  else
    return false;
}

/* To turn off display of edge labels.  Edge labels offen cause xvcg to
   abort with a segmentation fault. */
void turn_off_edge_labels(void) {
  edge_label = 0;
}

void dump_consts_local(bool b) {
  dump_const_local = b;
}

void turn_off_constant_entity_values(void) {
  const_entities = 0;
}

void dump_keepalive_edges(bool b) {
  dump_keepalive = b;
}

bool get_opt_dump_keepalive_edges(void) {
  return dump_keepalive;
}

void dump_out_edges(void) {
  dump_out_edge_flag = 1;
}

void dump_dominator_information(void) {
  dump_dominator_information_flag = 1;
}

void dump_loop_information(void) {
  dump_loop_information_flag = 1;
}

void dont_dump_loop_information(void) {
  dump_loop_information_flag = 0;
}

void dump_backedge_information(bool b) {
  dump_backedge_information_flag = b;
}

/* Dump the information of type field specified in ana/irtypeinfo.h.
 * If the flag is set, the type name is output in [] in the node label,
 * else it is output as info.
 */
void dump_analysed_type_info(bool b) {
  opt_dump_analysed_type_info = b;
}

void dump_pointer_values_to_info(bool b) {
  opt_dump_pointer_values_to_info = b;
}

/*******************************************************************/
/* Routines to dump information about a single ir node.            */
/*******************************************************************/

static INLINE void
dump_node_opcode (ir_node *n)
{

  switch(get_irn_opcode(n)) {

  case iro_Const: {
    int res;
    char buf[1024];
    res = tarval_snprintf(buf, sizeof(buf), get_Const_tarval(n));
    assert(res < sizeof(buf) && "buffer to small for tarval_snprintf");
    fprintf(F, buf);
  } break;

  case iro_SymConst: {
    if (get_SymConst_kind(n) == linkage_ptr_info) {
      /* don't use get_SymConst_ptr_info as it mangles the name. */
      fprintf (F, "SymC %s", get_id_str(get_SymConst_ptrinfo(n)));
    } else {
      assert(get_kind(get_SymConst_type(n)) == k_type);
      assert(get_type_ident(get_SymConst_type(n)));
      fprintf (F, "SymC %s ", get_type_name(get_SymConst_type(n)));
      if (get_SymConst_kind(n) == type_tag)
        fprintf (F, "tag");
      else
        fprintf (F, "size");
    }
  } break;

  case iro_Filter: {
    if (!interprocedural_view) fprintf(F, "Proj'");
    else                       fprintf(F, "%s", get_irn_opname(n));
  } break;

  case iro_Start: {
    if (interprocedural_view) {
      fprintf(F, "%s %s", get_irn_opname(n), get_ent_dump_name(get_irg_ent(get_irn_irg(n))));
      break;
    }
  } /* fall through */

  default: {
    fprintf (F, "%s", get_irn_opname(n));
  }

  }  /* end switch */
}

static INLINE void
dump_node_mode (ir_node *n)
{
  switch (get_irn_opcode(n)) {
  case iro_Phi:
  case iro_Const:
  case iro_Id:
  case iro_Proj:
  case iro_Filter:
  case iro_Conv:
  case iro_Tuple:
  case iro_Add:
  case iro_Sub:
  case iro_Mul:
  case iro_And:
  case iro_Or:
  case iro_Eor:
  case iro_Shl:
  case iro_Shr:
  case iro_Abs:
  case iro_Cmp:
  case iro_Confirm:
    fprintf (F, "%s", get_mode_name(get_irn_mode(n)));
    break;
  default:
    ;
  }
}

static void dump_node_typeinfo(ir_node *n) {
  if (!opt_dump_analysed_type_info) return;
  if (get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_consistent  ||
      get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_inconsistent  ) {
    type *tp = get_irn_type(n);
    if (tp != none_type)
      fprintf (F, " [%s]", get_type_name(tp));
    else
      fprintf (F, " []");
  }
}

static INLINE void
dump_node_nodeattr (ir_node *n)
{
  switch (get_irn_opcode(n)) {
  case iro_Start:
    if (false && interprocedural_view) {
      fprintf (F, "%s", get_ent_dump_name(get_irg_ent(current_ir_graph)));
    }
    break;
  case iro_Proj:
    if (get_irn_opcode(get_Proj_pred(n)) == iro_Cmp) {
      fprintf (F, "%s", get_pnc_string(get_Proj_proj(n)));
    } else {
      fprintf (F, "%ld", get_Proj_proj(n));
    }
    break;
  case iro_Filter:
    fprintf (F, "%ld", get_Filter_proj(n));
    break;
  case iro_Sel: {
    fprintf (F, "%s", get_ent_dump_name(get_Sel_entity(n)));
    } break;
  case iro_Cast: {
    fprintf (F, "(%s)", get_type_name(get_Cast_type(n)));
    } break;
  case iro_Confirm: {
    fprintf (F, "%s", get_pnc_string(get_Confirm_cmp(n)));
    } break;

  default:
    ;
  } /* end switch */
}

static INLINE void
dump_node_vcgattr (ir_node *n)
{
  switch (get_irn_opcode(n)) {
  case iro_Start:
  case iro_EndReg:
    /* fall through */
  case iro_EndExcept:
    /* fall through */
  case iro_End:
    fprintf (F, "color: blue");
    break;
  case iro_Block:
    fprintf (F, "color: lightyellow");
    break;
  case iro_Phi:
    fprintf (F, "color: green");
    break;
  case iro_Const:
  case iro_Proj:
  case iro_Filter:
  case iro_Tuple:
    fprintf (F, "color: yellow");
    break;
  default:
    PRINT_DEFAULT_NODE_ATTR;
  }
}

static INLINE void
dump_node_info (ir_node *n) {
  int i;
  ir_graph *irg;
  fprintf (F, " info1: \"");
  if (opt_dump_pointer_values_to_info)
    fprintf (F, "addr:    %p \n", (void *)n);
  fprintf (F, "visited: %ld \n", get_irn_visited(n));
  irg = get_irn_irg(n);
  if (irg != get_const_code_irg())
    fprintf (F, "irg:     %s\n", get_ent_dump_name(get_irg_entity(irg)));

  /* Source types */
  switch (get_irn_opcode(n)) {
  case iro_Start: {
    type *tp = get_entity_type(get_irg_ent(get_irn_irg(n)));
    fprintf(F, "start of method of type %s \n", get_type_name(tp));
    for (i = 0; i < get_method_n_params(tp); ++i)
      fprintf(F, "  param %d type: %s \n", i, get_type_name(get_method_param_type(tp, i)));
  } break;
  case iro_Alloc: {
    fprintf(F, "allocating entity of type %s \n", get_type_name(get_Alloc_type(n)));
  } break;
  case iro_Free: {
    fprintf(F, "freeing entity of type %s \n", get_type_name(get_Free_type(n)));
  } break;
  case iro_Sel: {
    fprintf(F, "Selecting entity of type %s \n", get_type_name(get_entity_type(get_Sel_entity(n))));
    fprintf(F, "  from entity of type %s \n", get_type_name(get_entity_owner(get_Sel_entity(n))));
  } break;
  case iro_Call: {
    type *tp = get_Call_type(n);
    fprintf(F, "calling method of type %s \n", get_type_name(tp));
    for (i = 0; i < get_method_n_params(tp); ++i)
      fprintf(F, "  param %d type: %s \n", i, get_type_name(get_method_param_type(tp, i)));
    for (i = 0; i < get_method_n_ress(tp); ++i)
      fprintf(F, "  resul %d type: %s \n", i, get_type_name(get_method_res_type(tp, i)));
    if (Call_has_callees(n)) {
      fprintf(F, "possible callees: \n");
      for (i = 0; i < get_Call_n_callees(n); i++) {
	if (!get_Call_callee(n, i)) {
	  fprintf(F, "  %d external method\n", i);
	} else {
	  fprintf(F, "  %d: %s\n", i, get_ent_dump_name(get_Call_callee(n, i)));
	}
      }
    }
  } break;
  case iro_CallBegin: {
    ir_node *call = get_CallBegin_call(n);
    if (Call_has_callees(call)) {
      fprintf(F, "possible callees: \n");
      for (i = 0; i < get_Call_n_callees(call); i++) {
	if (!get_Call_callee(call, i)) {
	  fprintf(F, "  %d external method\n", i);
	} else {
	  fprintf(F, "  %d: %s\n", i, get_ent_dump_name(get_Call_callee(call, i)));
	}
      }
    }
  } break;
  case iro_Return: {
    if (!interprocedural_view) {
      type *tp = get_entity_type(get_irg_ent(get_irn_irg(n)));
      fprintf(F, "return in method of type %s \n", get_type_name(tp));
      for (i = 0; i < get_method_n_ress(tp); ++i)
	fprintf(F, "  res %d type: %s \n", i, get_type_name(get_method_res_type(tp, i)));
    }
    } break;
  case iro_Const: {
    type *tp = get_Const_type(n);
    assert(tp != none_type);
    fprintf(F, "Const of type %s \n", get_type_name(get_Const_type(n)));
  } break;
  case iro_Filter: {
    int i;
    if (interprocedural_view) {
      fprintf(F, "intra predecessor nodes:\n");
      for (i = 0; i < get_irn_intra_arity(n); i++) {
	ir_node *pred = get_irn_intra_n(n, i);
	fprintf(F, "  %s%s %ld\n", get_irn_opname(pred), get_irn_modename(pred), get_irn_node_nr(pred));
      }
    } else {
      fprintf(F, "inter predecessor nodes:\n");
      for (i = 0; i < get_irn_inter_arity(n); i++) {
	ir_node *pred = get_irn_inter_n(n, i);
	fprintf(F, "  %s%s %ld \tin graph %s\n", get_irn_opname(pred), get_irn_modename(pred),
		get_irn_node_nr(pred), get_ent_dump_name(get_irg_entity(get_irn_irg(pred))));
      }
    }
  } break;
  default: ;
  }


  if (get_irg_typeinfo_state(get_irn_irg(n)) == irg_typeinfo_consistent  ||
      get_irg_typeinfo_state(get_irn_irg(n)) == irg_typeinfo_inconsistent  )
    if (get_irn_type(n) != none_type)
      fprintf (F, "\nAnalysed type: %s", get_type_name(get_irn_type(n)));

  fprintf (F, "\"");
}


static INLINE
bool is_constlike_node(ir_node *n) {
  ir_op *op = get_irn_op(n);
  return (op == op_Const || op == op_Bad || op == op_SymConst || op == op_Unknown);
}


/* outputs the predecessors of n, that are constants, local.  I.e.,
   generates a copy of the constant predecessors for each node called with. */
static void dump_const_node_local(ir_node *n) {
  int i;
  if (!get_opt_dump_const_local()) return;

  /* Use visited flag to avoid outputting nodes twice.
     initialize it first. */
  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con)) {
      set_irn_visited(con, get_irg_visited(current_ir_graph)-1);
    }
  }

  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con) && irn_not_visited(con)) {
      mark_irn_visited(con);
      /* Generate a new name for the node by appending the names of
	 n and const. */
      fprintf (F, "node: {title: "); PRINT_CONSTID(n, con);
      fprintf(F, " label: \"");
      dump_node_opcode(con);
      dump_node_mode (con);
      dump_node_typeinfo(con);
      fprintf (F, " ");
      dump_node_nodeattr(con);
      fprintf (F, " %ld", get_irn_node_nr(con));
      fprintf (F, "\" ");
      dump_node_vcgattr(con);
      dump_node_info(con);
      fprintf (F, "}\n");
    }
  }
}

static void
dump_node (ir_node *n) {
  if (get_opt_dump_const_local() && is_constlike_node(n)) return;
  /* dump this node */
  fprintf (F, "node: {title: \""); PRINT_NODEID(n); fprintf(F, "\" label: \"");

  dump_node_opcode(n);
  dump_node_mode (n);
  dump_node_typeinfo(n);
  fprintf (F, " ");
  dump_node_nodeattr(n);
  fprintf (F, " %ld", get_irn_node_nr(n));
  fprintf (F, "\" ");
  dump_node_vcgattr(n);
  dump_node_info(n);
  fprintf (F, "}\n");
  dump_const_node_local(n);
#ifdef HEAPANAL
  dump_chi_term(F, n);
  dump_state(F, n);
#endif
}

/* dump the edge to the block this node belongs to */
static void
dump_ir_block_edge(ir_node *n)  {
  if (get_opt_dump_const_local() && is_constlike_node(n)) return;
  if (is_no_Block(n)) {
    fprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"");
    PRINT_NODEID(get_nodes_block(n));
    fprintf (F, "\" "	BLOCK_EDGE_ATTR "}\n");
  }
}

static void print_edge_vcgattr(ir_node *from, int to) {
  assert(from);

  if (dump_backedge_information_flag && is_backedge(from, to))
    fprintf (F, BACK_EDGE_ATTR);

  switch (get_irn_opcode(from)) {
  case iro_Block:
    fprintf (F, CF_EDGE_ATTR);
    break;
  case iro_Start:   break;
  case iro_End:
    if (to >= 0) {
      if (get_irn_mode(get_End_keepalive(from, to)) == mode_BB)
	fprintf (F, CF_EDGE_ATTR);
      if (get_irn_mode(get_End_keepalive(from, to)) == mode_X)
	fprintf (F, MEM_EDGE_ATTR);
    }
    break;
  case iro_EndReg: break;
  case iro_EndExcept: break;
  case iro_Jmp:     break;
  case iro_Break:   break;
  case iro_Cond:    break;
  case iro_Return:
  case iro_Raise:
    if (to == 0) fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Const:   break;
  case iro_SymConst:break;
  case iro_Sel:
  case iro_Call:
    if (to == 0) fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_CallBegin: break;
  case iro_Add:     break;
  case iro_Sub:     break;
  case iro_Minus:   break;
  case iro_Mul:     break;
  case iro_Quot:
  case iro_DivMod:
  case iro_Div:
  case iro_Mod:
    if (to == 0) fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Abs:    break;
  case iro_And:    break;
  case iro_Or:     break;
  case iro_Eor:    break;
  case iro_Shl:    break;
  case iro_Shr:    break;
  case iro_Shrs:   break;
  case iro_Rot:    break;
  case iro_Cmp:    break;
  case iro_Conv:   break;
  case iro_Phi:
    if (get_irn_modecode(from) == irm_M) fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Load:
  case iro_Store:
  case iro_Alloc:
  case iro_Free:
    if (to == 0) fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Sync:
    fprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Tuple:  break;
  case iro_Proj:
  case iro_Filter:
    switch (get_irn_modecode(from)) {
    case irm_X:
      fprintf (F, CF_EDGE_ATTR);
      break;
    case irm_M:
      fprintf (F, MEM_EDGE_ATTR);
      break;
    default: break;
    }
    break;
  case iro_Bad:    break;
  case iro_Unknown: break;
  case iro_Id:     break;
  default:
    ;
  }
}

/* dump edges to our inputs */
static void
dump_ir_data_edges(ir_node *n)  {
  int i, visited = get_irn_visited(n);

  if ((get_irn_op(n) == op_End) && (!dump_keepalive))
    return;

  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node * pred = get_irn_n(n, i);
    assert(pred);

    if ((interprocedural_view && get_irn_visited(pred) < visited))
      continue; /* pred not dumped */

    if (dump_backedge_information_flag && is_backedge(n, i))
      fprintf (F, "backedge: {sourcename: \"");
    else
      fprintf (F, "edge: {sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: ");
    if ((get_opt_dump_const_local()) && is_constlike_node(pred)) {
      PRINT_CONSTID(n, pred);
    } else {
      fprintf(F, "\""); PRINT_NODEID(pred); fprintf(F, "\"");
    }
    fprintf (F, " label: \"%d\" ", i);
    print_edge_vcgattr(n, i);
    fprintf (F, "}\n");
  }
}

/** Dumps a node and its edges but not the block edge
 */
static INLINE void
dump_node_wo_blockedge (ir_node *n, void* env) {
  dump_node(n);
  dump_ir_data_edges(n);
}

/** Dumps a node and its edges.
 */
static void
dump_whole_node (ir_node *n, void* env) {
  dump_node_wo_blockedge(n, env);
  if (!node_floats(n)) dump_ir_block_edge(n);
}

static void
dump_const_node(ir_node *n, void *env) {
  if (is_Block(n)) return;
  dump_node_wo_blockedge(n, env);
}

/***********************************************************************/
/* the following routines dump the nodes/irgs bracketed to graphs.     */
/***********************************************************************/

/** Dumps a constant expression as entity initializer, array bound ...
 */
static void dump_const_expression(ir_node *value) {
  ir_graph *rem = current_ir_graph;
  int rem_dump_const_local = dump_const_local;
  dump_const_local = 0;
  current_ir_graph = get_const_code_irg();
  irg_walk(value, dump_const_node, NULL, NULL);
  /* Decrease visited flag so that we walk with the same flag for the next
     expresssion.  This guarantees that we don't dump the same node twice,
     as for const expressions cse is performed to save memory. */
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph) -1);
  current_ir_graph = rem;
  dump_const_local = rem_dump_const_local;
}

/** Dump a block as graph containing its nodes.
 *
 *  Expects to find nodes belonging to the block as list in its
 *  link field.
 *  Dumps the edges of all nodes including itself. */
static void
dump_whole_block(ir_node *block) {
  ir_node *node;
  assert(is_Block(block));

  fprintf(F, "graph: { title: \"");
  PRINT_NODEID(block);
  fprintf(F, "\"  label: \"");
  dump_node_opcode(block);
  fprintf (F, " %ld", get_irn_node_nr(block));
#ifdef HEAPANAL
  if (get_opt_dump_abstvals())
    fprintf (F, " seqno: %d", (int)get_Block_seqno(block));
#endif
  fprintf(F, "\" status:clustered color:%s \n",
	   get_Block_matured(block) ? "yellow" : "red");

  /* dump the blocks edges */
  dump_ir_data_edges(block);

  /* dump the nodes that go into the block */
  for (node = ird_get_irn_link(block); node; node = ird_get_irn_link(node)) {
    dump_node(node);
    dump_ir_data_edges(node);
  }

  /* Close the vcg information for the block */
  fprintf(F, "}\n");
  dump_const_node_local(block);
#ifdef HEAPANAL
  dump_chi_term(F, block);
#endif
  fprintf(F, "\n");
}

/** dumps a graph block-wise. Expects all blockless nodes in arr in irgs link.
 *  The outermost nodes: blocks and nodes not pinned, Bad, Unknown. */
static void
dump_block_graph (ir_graph *irg) {
  int i;
  ir_graph *rem = current_ir_graph;
  ir_node **arr = ird_get_irg_link(irg);
  current_ir_graph = irg;

  for (i = ARR_LEN(arr) - 1; i >= 0; --i) {
    ir_node * node = arr[i];
    if (is_Block(node)) {
      /* Dumps the block and all the nodes in the block, which are to
	 be found in Block->link. */
      dump_whole_block(node);
    } else {
      /* Nodes that are not in a Block. */
      dump_node(node);
      dump_ir_data_edges(node);
    }
  }

  if (dump_loop_information_flag) dump_loop_info(irg);

  current_ir_graph = rem;
}

/** Dumps an irg as a graph.
 *  If interprocedural view edges can point to nodes out of this graph.
 */
static void dump_graph(ir_graph *irg) {

  fprintf(F, "graph: { title: \"");
  PRINT_IRGID(irg);
  fprintf(F, "\" label: \"%s\" status:clustered color:white \n",
	  get_ent_dump_name(get_irg_ent(irg)));

  dump_block_graph (irg);

  /* Close the vcg information for the irg */
  fprintf(F, "}\n\n");
}

/*******************************************************************/
/* Basic type and entity nodes and edges.                          */
/*******************************************************************/

/* dumps the edges between nodes and their type or entity attributes. */
static void dump_node2type_edges (ir_node *n, void *env)
{
  assert(n);

  switch (get_irn_opcode(n)) {
  case iro_Const :
    /* @@@ some consts have an entity */
    break;
  case iro_SymConst:
    if (   (get_SymConst_kind(n) == type_tag)
	   || (get_SymConst_kind(n) == size))
      {
	print_node_type_edge(F,n,get_SymConst_type(n),NODE2TYPE_EDGE_ATTR);
      }
    break;
  case iro_Sel: {
      print_node_ent_edge(F,n,get_Sel_entity(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Call: {
      print_node_type_edge(F,n,get_Call_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Alloc: {
      print_node_type_edge(F,n,get_Alloc_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Free: {
      print_node_type_edge(F,n,get_Free_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Cast: {
      print_node_type_edge(F,n,get_Cast_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  default:
    break;
  }
}


static void print_type_info(type *tp) {
  if (get_type_state(tp) == layout_undefined) {
    fprintf(F, "state: layout_undefined\n");
  } else {
    fprintf(F, "state: layout_fixed,\n");
  }
  if (get_type_mode(tp))
    fprintf(F, "mode: %s,\n", get_mode_name(get_type_mode(tp)));
  fprintf(F, "size: %dB,\n", get_type_size(tp));
}

static void print_typespecific_info(type *tp) {
  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    {
      fprintf(F, "peculiarity: %s\n", get_peculiarity_string(get_class_peculiarity(tp)));
    } break;
  case tpo_struct:
    {
    } break;
  case tpo_method:
    {
    } break;
  case tpo_union:
    {
    } break;
  case tpo_array:
    {
    } break;
  case tpo_enumeration:
    {
    } break;
  case tpo_pointer:
    {
    } break;
  case tpo_primitive:
    {
    } break;
  default: break;
  } /* switch type */
}


static void print_typespecific_vcgattr(type *tp) {
  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    {
      if (peculiarity_existent == get_class_peculiarity(tp))
	fprintf (F, " " TYPE_CLASS_NODE_ATTR);
      else
	fprintf (F, " " TYPE_DESCRIPTION_NODE_ATTR);
    } break;
  case tpo_struct:
    {
      fprintf (F, " " TYPE_METH_NODE_ATTR);
    } break;
  case tpo_method:
    {
    } break;
  case tpo_union:
    {
    } break;
  case tpo_array:
    {
    } break;
  case tpo_enumeration:
    {
    } break;
  case tpo_pointer:
    {
    } break;
  case tpo_primitive:
    {
    } break;
  default: break;
  } /* switch type */
}

static void print_type_node(type *tp)
{
  fprintf (F, "node: {title: ");
  PRINT_TYPEID(tp);
  fprintf (F, " label: \"%s %s\"", get_type_tpop_name(tp), get_type_name(tp));
  fprintf (F, " info1: \"");
  print_type_info(tp);
  print_typespecific_info(tp);
  fprintf (F, "\"");
  print_typespecific_vcgattr(tp);
  fprintf (F, "}\n");
}

#define X(a)	case a: fprintf(F, #a); break
void dump_entity_node(entity *ent)
{
  fprintf (F, "node: {title: \"");
  PRINT_ENTID(ent); fprintf(F, "\"");
  fprintf (F, DEFAULT_TYPE_ATTRIBUTE);
  fprintf (F, "label: ");
  fprintf (F, "\"ent %s\" " ENTITY_NODE_ATTR , get_ent_dump_name(ent));
  fprintf (F, "\n info1: \"\nid: "); PRINT_ENTID(ent);

  fprintf (F, "\nallocation:  ");
  switch (get_entity_allocation(ent)) {
    X(allocation_dynamic);
    X(allocation_automatic);
    X(allocation_static);
    X(allocation_parameter);
  }

  fprintf (F, "\nvisibility:  ");
  switch (get_entity_visibility(ent)) {
    X(visibility_local);
    X(visibility_external_visible);
    X(visibility_external_allocated);
  }

  fprintf (F, "\nvariability: ");
  switch (get_entity_variability(ent)) {
    X(variability_uninitialized);
    X(variability_initialized);
    X(variability_part_constant);
    X(variability_constant);
  }

  fprintf (F, "\nvolatility:  ");
  switch (get_entity_volatility(ent)) {
    X(volatility_non_volatile);
    X(volatility_is_volatile);
  }

  fprintf(F, "\npeculiarity: %s", get_peculiarity_string(get_entity_peculiarity(ent)));
  fprintf(F, "\nname:    %s\nld_name: %s",
	  get_ent_dump_name(ent), ent->ld_name ? get_entity_ld_name(ent) : "no yet set");
  fprintf(F, "\noffset:  %d", get_entity_offset(ent));
  if (is_method_type(get_entity_type(ent))) {
    if (get_entity_irg(ent))   /* can be null */
      { fprintf (F, "\nirg = "); PRINT_IRGID(get_entity_irg(ent)); }
    else
      { fprintf (F, "\nirg = NULL"); }
  }
  fprintf(F, "\"\n}\n");
}
#undef X

/* dumps a type or entity and it's edges. */
static void
dump_type_info (type_or_ent *tore, void *env) {
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */

  switch (get_kind(tore)) {
  case k_entity:
    {
      entity *ent = (entity *)tore;
      ir_node *value;
      /* The node */
      dump_entity_node(ent);
      /* The Edges */
      /* skip this to reduce graph.  Member edge of type is parallel to this edge. *
      fprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_OWN_EDGE_ATTR "}\n", ent, get_entity_owner(ent));*/
      print_ent_type_edge(F,ent, get_entity_type(ent), ENT_TYPE_EDGE_ATTR);
      if(is_class_type(get_entity_owner(ent))) {
	for(i = 0; i < get_entity_n_overwrites(ent); i++){
	  print_ent_ent_edge(F,ent, get_entity_overwrites(ent, i), ENT_OVERWRITES_EDGE_ATTR);
	}
      }
      /* attached subgraphs */
      if (const_entities && (get_entity_variability(ent) != variability_uninitialized)) {
	if (is_atomic_entity(ent)) {
	  value = get_atomic_ent_value(ent);
	  if (value) {
            print_ent_node_edge(F,ent, value, ENT_VALUE_EDGE_ATTR, i);
	    /* DDMN(value);  $$$ */
	    dump_const_expression(value);
	  }
	}
	if (is_compound_entity(ent)) {
	  for (i = 0; i < get_compound_ent_n_values(ent); i++) {
	    value = get_compound_ent_value(ent, i);
	    if (value) {
              print_ent_node_edge(F,ent,value,ENT_VALUE_EDGE_ATTR,i);
	      dump_const_expression(value);
	      print_ent_ent_edge(F,ent, get_compound_ent_value_member(ent, i), ENT_CORR_EDGE_ATTR, i);
	      /*
		fprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		ENT_CORR_EDGE_ATTR  "}\n", GET_ENTID(ent),
		get_compound_ent_value_member(ent, i), i);
	      */
	    }
	  }
	}
      }
    } break;
  case k_type:
    {
      type *tp = (type *)tore;
      print_type_node(tp);
      /* and now the edges */
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
	{
	  for (i=0; i < get_class_n_supertypes(tp); i++) {
	    print_type_type_edge(F, tp,get_class_supertype(tp, i),TYPE_SUPER_EDGE_ATTR);
	  }

	  for (i=0; i < get_class_n_members(tp); i++) {
	    print_type_ent_edge(F,tp,get_class_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
	  }
	} break;
      case tpo_struct:
	{
	  for (i=0; i < get_struct_n_members(tp); i++) {
	    print_type_ent_edge(F,tp,get_struct_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
	  }
	} break;
      case tpo_method:
	{
	  for (i = 0; i < get_method_n_params(tp); i++)
	  {
             print_type_type_edge(F,tp,get_method_param_type(tp, i),METH_PAR_EDGE_ATTR,i);
	  }
	  for (i = 0; i < get_method_n_ress(tp); i++)
	  {
             print_type_type_edge(F,tp,get_method_res_type(tp, i),METH_RES_EDGE_ATTR,i);
	  }
	} break;
      case tpo_union:
	{
	  for (i = 0; i < get_union_n_members(tp); i++)
	  {
		  print_type_ent_edge(F,tp,get_union_member(tp, i),UNION_EDGE_ATTR);
	  }
	} break;
      case tpo_array:
	{
		  print_type_type_edge(F,tp,get_array_element_type(tp),ARR_ELT_TYPE_EDGE_ATTR);
		  print_type_ent_edge(F,tp,get_array_element_entity(tp),ARR_ENT_EDGE_ATTR);
		  for (i = 0; i < get_array_n_dimensions(tp); i++) {
		    ir_node *upper = get_array_upper_bound(tp, i);
		    ir_node *lower = get_array_lower_bound(tp, i);
		    print_node_type_edge(F,upper, tp, "label: \"upper %d\"", get_array_order(tp, i));
		    print_node_type_edge(F,lower, tp, "label: \"lower %d\"", get_array_order(tp, i));
		    dump_const_expression(upper);
		    dump_const_expression(lower);
		  }

	} break;
      case tpo_enumeration:
	{
	} break;
      case tpo_pointer:
	{
		  print_type_type_edge(F,tp,get_pointer_points_to_type(tp), PTR_PTS_TO_EDGE_ATTR);
	} break;
      case tpo_primitive:
	{
	} break;
      default: break;
      } /* switch type */
    }
    break; /* case k_type */
  default:
    {
      printf(" *** irdump,  dump_type_info(l.%i), faulty type.\n", __LINE__);
    } break;
  } /* switch kind_or_entity */
}

/** For dumping class hierarchies.
 * Dumps a class type node and a superclass edge.
 * If env != null dumps entities of classes and overwrites edges.
 */
static void
dump_class_hierarchy_node (type_or_ent *tore, void *env) {
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */
  switch (get_kind(tore)) {
  case k_entity: {
    entity *ent = (entity *)tore;
    if (get_entity_owner(ent) == get_glob_type()) break;
    if ((env) && is_class_type(get_entity_owner(ent))) {
      /* The node */
      dump_entity_node(ent);
      /* The edges */
      print_type_ent_edge(F,get_entity_owner(ent),ent,TYPE_MEMBER_EDGE_ATTR);
      for(i = 0; i < get_entity_n_overwrites(ent); i++)
      {
      print_ent_ent_edge(F,get_entity_overwrites(ent, i),ent, ENT_OVERWRITES_EDGE_ATTR);
      }
    }
  } break; /* case k_entity */
  case k_type:
    {
      type *tp = (type *)tore;
      if (tp == get_glob_type()) break;
      switch (get_type_tpop_code(tp)) {
        case tpo_class: {
	  print_type_node(tp);
	  /* and now the edges */
	  for (i=0; i < get_class_n_supertypes(tp); i++)
	  {
		  print_type_type_edge(F,tp,get_class_supertype(tp, i),TYPE_SUPER_EDGE_ATTR);
	  }
        } break;
        default: break;
      } /* switch type */
    }
    break; /* case k_type */
  default:
    {
      printf(" *** irdump,  dump_class_hierarchy_node(l.%i), faulty type.\n", __LINE__);
    } break;
  } /* switch kind_or_entity */
}

/*******************************************************************/
/* dump analysis information that is expressed in graph terms.     */
/*******************************************************************/

/* dump out edges */
static void
dump_out_edge (ir_node *n, void* env) {
  int i;
  for (i = 0; i < get_irn_n_outs(n); i++) {
    assert(get_irn_out(n, i));
    fprintf (F, "edge: {sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"");
    PRINT_NODEID(get_irn_out(n, i));
    fprintf (F, "\" color: red linestyle: dashed");
    fprintf (F, "}\n");
  }
}

static INLINE void
dump_loop_node_edge (ir_loop *loop, int i) {
  assert(loop);
  fprintf (F, "edge: {sourcename: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" targetname: \"");
  PRINT_NODEID(get_loop_node(loop, i));
  fprintf (F, "\" color: green");
  fprintf (F, "}\n");
}

static INLINE void
dump_loop_son_edge (ir_loop *loop, int i) {
  assert(loop);
  fprintf (F, "edge: {sourcename: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" targetname: \"");
  PRINT_LOOPID(get_loop_son(loop, i));
  fprintf (F, "\" color: darkgreen label: \"%d\"}\n",
	   get_loop_element_nr(loop, get_loop_son(loop, i)));
}

static
void dump_loops (ir_loop *loop) {
  int i;
  /* dump this loop node */
  fprintf (F, "node: {title: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" label: \"loop %d, %d sons, %d nodes\" }\n",
	   get_loop_depth(loop), get_loop_n_sons(loop), get_loop_n_nodes(loop));
  /* dump edges to nodes in loop -- only if it is a real loop */
  if (get_loop_depth(loop) != 0) {
    for (i = 0; i < get_loop_n_nodes(loop); i++) {
      dump_loop_node_edge(loop, i);
    }
  }
  for (i = 0; i < get_loop_n_sons(loop); i++) {
    dump_loops(get_loop_son(loop, i));
    dump_loop_son_edge(loop, i);
  }
}

static INLINE
void dump_loop_info(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_irg_loop(irg)) dump_loops(get_irg_loop(irg));

  current_ir_graph = rem;
}


/************************************************************************/
/* open and close vcg file                                              */
/************************************************************************/

static INLINE void
dump_vcg_header(const char *name, const char *orientation) {
  char *label;

  if (edge_label) {
    label = "yes";
  } else {
    label = "no";
  }

  if (!orientation) orientation = "bottom_to_top";

  /* print header */
  fprintf (F,
	   "graph: { title: \"ir graph of %s\"\n"
	   "display_edge_labels: %s\n"
	   "layoutalgorithm: mindepth\n"
	   "manhattan_edges: yes\n"
	   "port_sharing: no\n"
	   "orientation: %s\n"
	   "classname 1: \"Data\"\n"
	   "classname 2: \"Block\"\n"
	   "classname 3: \"Entity type\"\n"
	   "classname 4: \"Entity owner\"\n"
	   "classname 5: \"Method Param\"\n"
	   "classname 6: \"Method Res\"\n"
	   "classname 7: \"Super\"\n"
	   "classname 8: \"Union\"\n"
	   "classname 9: \"Points-to\"\n"
	   "classname 10: \"Array Element Type\"\n"
	   "classname 11: \"Overwrites\"\n"
	   "classname 12: \"Member\"\n",
	   name, label, orientation);

  fprintf (F, "\n");		/* a separator */
}

static void vcg_open (ir_graph *irg, char * suffix1, char *suffix2) {
  const char *nm = get_irg_dump_name(irg);
  int len = strlen(nm);
  char *fname;  /* filename to put the vcg information in */

  if (!suffix1) suffix1 = "";
  if (!suffix2) suffix2 = "";

  /** open file for vcg graph */
  fname = malloc (len + strlen(suffix1) + strlen(suffix2) + 5);
  strncpy (fname, nm, len);      /* copy the filename */
  fname[len] = '\0';
  strcat (fname, suffix1);  /* append file suffix */
  strcat (fname, suffix2);  /* append file suffix */
  strcat (fname, ".vcg");   /* append the .vcg suffix */
  F = fopen (fname, "w");   /* open file for writing */
  if (!F) {
    panic ("cannot open %s for writing (%m)", fname);  /* not reached */
  }
  free(fname);
}

static void vcg_open_name (const char *name, char *suffix) {
  char *fname;  /* filename to put the vcg information in */

  if (!suffix) suffix = "";

  /** open file for vcg graph */
  fname = malloc (strlen(name) + 5 + strlen(suffix));
  strcpy (fname, name);    /* copy the filename */
  strcat (fname, suffix);
  strcat (fname, ".vcg");  /* append the .vcg suffix */
  F = fopen (fname, "w");  /* open file for writing */
  if (!F) {
    panic ("cannot open %s for writing (%m)", fname);  /* not reached */
  }
  free(fname);
}

static INLINE void dump_vcg_footer (void) {
  fprintf (F, "}\n");
}

static void
vcg_close (void) {
  dump_vcg_footer();    /* print footer */
  fclose (F);           /* close vcg file */
}

/************************************************************************/
/************************************************************************/
/* Routines that dump all or parts of the firm representation to a file */
/************************************************************************/
/************************************************************************/

/************************************************************************/
/* Dump ir graphs, differnt formats and additional information.         */
/************************************************************************/

/** Routine to dump a graph, blocks as conventional nodes.
 */
void
dump_ir_graph (ir_graph *irg)
{
  ir_graph *rem;
  char *suffix;
  rem = current_ir_graph;
  current_ir_graph = irg;

  if (interprocedural_view) suffix = "-pure-ip";
  else                      suffix = "-pure";
  vcg_open (irg, dump_file_suffix, suffix);
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  /* walk over the graph */
  /* dump_whole_node must be called in post visiting predecessors */
  irg_walk(get_irg_end(irg), NULL, dump_whole_node, NULL);

  /* dump the out edges in a separate walk */
  if ((dump_out_edge_flag) && (get_irg_outs_state(irg) != no_outs)) {
    irg_out_walk(get_irg_start(irg), dump_out_edge, NULL, NULL);
  }

  vcg_close();

  current_ir_graph = rem;
}


void
dump_ir_block_graph (ir_graph *irg)
{
  int i;
  char *suffix;

  if (interprocedural_view) suffix = "-ip";
  else                      suffix = "";
  vcg_open (irg, dump_file_suffix, suffix);
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  construct_block_lists(irg);

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node **arr = ird_get_irg_link(get_irp_irg(i));
    if (arr) {
      dump_graph(get_irp_irg(i));
      DEL_ARR_F(arr);
    }
  }

  vcg_close();
}

/** dumps a graph with type information
 */
void
dump_ir_graph_w_types (ir_graph *irg)
{
  ir_graph *rem = current_ir_graph;
  char *suffix;
  current_ir_graph = irg;

  if (interprocedural_view) suffix = "-pure-wtypes-ip";
  else                      suffix = "-pure-wtypes";
  vcg_open (irg, dump_file_suffix, suffix);
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  /* dump common ir graph */
  irg_walk(get_irg_end(irg), NULL, dump_whole_node, NULL);
  /* dump type info */
  type_walk_irg(irg, dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());
  /* dump edges from graph to type info */
  irg_walk(get_irg_end(irg), dump_node2type_edges, NULL, NULL);

  vcg_close();
  current_ir_graph = rem;
}

void
dump_ir_block_graph_w_types (ir_graph *irg)
{
  int i;
  char *suffix;
  ir_graph *rem = current_ir_graph;

  if (interprocedural_view) suffix = "-wtypes-ip";
  else                      suffix = "-wtypes";
  vcg_open (irg, dump_file_suffix, suffix);
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  /* dump common blocked ir graph */
  construct_block_lists(irg);

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node **arr = ird_get_irg_link(get_irp_irg(i));
    if (arr) {
      dump_graph(get_irp_irg(i));
      DEL_ARR_F(arr);
    }
  }

  /* dump type info */
  current_ir_graph = irg;
  type_walk_irg(irg, dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());

  /* dump edges from graph to type info */
  irg_walk(get_irg_end(irg), dump_node2type_edges, NULL, NULL);

  current_ir_graph = rem;
  vcg_close();
}

/***********************************************************************/
/* The following routines dump a control flow graph.                   */
/***********************************************************************/

static void
dump_block_to_cfg (ir_node *block, void *env) {
  int i;
  ir_node *pred;

  if (is_Block(block)) {
    /* This is a block. Dump a node for the block. */
    fprintf (F, "node: {title: \""); PRINT_NODEID(block);
    fprintf (F, "\" label: \"%s ", get_op_name(get_irn_op(block)));
    PRINT_NODEID(block);
    fprintf (F, "\" ");
    if (dump_dominator_information_flag)
      fprintf(F, "info1:dom depth %d", get_Block_dom_depth(block));
    fprintf (F, "}\n");
    /* Dump the edges */
    for ( i = 0; i < get_Block_n_cfgpreds(block); i++)
      if (get_irn_op(skip_Proj(get_Block_cfgpred(block, i))) != op_Bad) {
	pred = get_nodes_block(skip_Proj(get_Block_cfgpred(block, i)));
	fprintf (F, "edge: { sourcename: \"");
	PRINT_NODEID(block);
	fprintf (F, "\" targetname: \"");
	PRINT_NODEID(pred);
	fprintf (F, "\"}\n");
      }

    /* Dump dominator edge */
    if (dump_dominator_information_flag && get_Block_idom(block)) {
      pred = get_Block_idom(block);
      fprintf (F, "edge: { sourcename: \"");
      PRINT_NODEID(block);
      fprintf (F, "\" targetname: \"");
      PRINT_NODEID(pred);
      fprintf (F, "\" " DOMINATOR_EDGE_ATTR "}\n");
    }
  }
}

void
dump_cfg (ir_graph *irg)
{
  ir_graph *rem = current_ir_graph;
  int ddif = dump_dominator_information_flag;
  int ipv = interprocedural_view;
  current_ir_graph = irg;

  vcg_open (irg, dump_file_suffix, "-cfg");
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  if (interprocedural_view) {
    printf("Warning: dumping cfg not in interprocedural view!\n");
    interprocedural_view = 0;
  }

  if (get_irg_dom_state(irg) != dom_consistent)
    dump_dominator_information_flag = 0;

  /* walk over the blocks in the graph */
  irg_block_walk(get_irg_end(irg), dump_block_to_cfg, NULL, NULL);
  dump_node (get_irg_bad(irg));

  dump_dominator_information_flag = ddif;
  interprocedural_view = ipv;
  vcg_close();
  current_ir_graph = rem;
}



/* Dump all irgs in interprocedural view to a single file. */
void dump_all_cg_block_graph(void) {
  int i;
  int rem_view = interprocedural_view;
  interprocedural_view = 1;

  vcg_open_name ("All_graphs", dump_file_suffix);
  dump_vcg_header("All_graphs", NULL);

  /* collect nodes in all irgs reachable in call graph*/
  for (i = 0; i < get_irp_n_irgs(); i++)
    ird_set_irg_link(get_irp_irg(i), NULL);

  cg_walk(clear_link, collect_node, NULL);

  /* dump all graphs */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    assert(ird_get_irg_link(current_ir_graph));
    dump_graph(current_ir_graph);
    DEL_ARR_F(ird_get_irg_link(current_ir_graph));
  }

  vcg_close();
  interprocedural_view = rem_view;
}

/***********************************************************************/
/* the following routines dumps type information without any ir nodes. */
/***********************************************************************/

void
dump_type_graph (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, dump_file_suffix, "-type");
  dump_vcg_header(get_irg_dump_name(irg), NULL);

  /* walk over the blocks in the graph */
  type_walk_irg(irg, dump_type_info, NULL, NULL);
  /* The walker for the const code can be called several times for the
     same (sub) experssion.  So that no nodes are dumped several times
     we decrease the visited flag of the corresponding graph after each
     walk.  So now increase it finally. */
  inc_irg_visited(get_const_code_irg());

  vcg_close();
  current_ir_graph = rem;
}

void
dump_all_types (void)
{
  vcg_open_name ("All_types", dump_file_suffix);
  dump_vcg_header("All_types", NULL);
  type_walk(dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());
  vcg_close();
}

void
dump_class_hierarchy (bool entities)
{
  vcg_open_name ("class_hierarchy", dump_file_suffix);
  dump_vcg_header("class_hierarchy", NULL);
  if (entities)
    type_walk(dump_class_hierarchy_node, NULL, (void *)1);
  else
    type_walk(dump_class_hierarchy_node, NULL, NULL);
  vcg_close();
}

/***********************************************************************/
/* dumps all graphs with the graph-dumper passed. Possible dumpers:    */
/*  dump_ir_graph                                                      */
/*  dump_ir_block_graph                                                */
/*  dump_cfg                                                           */
/*  dump_type_graph                                                    */
/*  dump_ir_graph_w_types                                              */
/***********************************************************************/

void dump_all_ir_graphs (dump_graph_func *dmp_grph) {
  int i;
  for (i=0; i < get_irp_n_irgs(); i++) {
    dmp_grph(get_irp_irg(i));
  }
}


/**********************************************************************************
 * Dumps a stand alone loop graph with firm nodes which belong to one loop nodes  *
 * packed together in one subgraph                                                *
 **********************************************************************************/



void dump_loops_standalone (ir_loop *loop) {
  int i, loop_node_started = 0, son_number = 0, chunk_nr = 0;
  loop_element le;

  /* Start a new loop node */
  fprintf (F, "node: {title: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" color: yellow label: \"loop %d, %d sons, %d nodes\" }\n",
	   get_loop_depth(loop), get_loop_n_sons(loop), get_loop_n_nodes(loop));

  for(i = 0; i < get_loop_n_elements(loop); i++)
    {
      le = get_loop_element(loop, i);

      ir_loop *son = le.son;
      if (get_kind(son) == k_ir_loop)
	{
	  /* We are a loop son -> Recurse */

	  if(loop_node_started) /* Close the "firm-nodes" node first if we started one. */
	    {
	      fprintf(F, "\" }");
	      loop_node_started = 0;
	    }
	  dump_loops_standalone(son);
	  dump_loop_son_edge(loop, son_number++);
	}
      else
	{
	  /* We are a loop node -> Collect firm nodes */

	  ir_node *n = le.node;

	  if (!loop_node_started)
	    {
	      /* Start a new node which contains all firm nodes of the current loop */

	      fprintf (F, "edge: {sourcename: \"");
	      PRINT_LOOPID(loop);
	      fprintf (F, "\" targetname: \"");
	      PRINT_LOOPID(loop);
	      fprintf (F, "-%d-nodes\" label:\"%d ...\"}\n", chunk_nr, i);

	      fprintf (F, "node: { title: \"");
	      PRINT_LOOPID(loop);
	      fprintf (F, "-%d-nodes\" color: lightyellow label: \"", chunk_nr++);
	      loop_node_started = 1;
	    }
	  else
	    fprintf(F, "\n");

	  dump_node_opcode(n);
	  dump_node_mode (n);
	  dump_node_typeinfo(n);
	  fprintf (F, " ");
	  dump_node_nodeattr(n);
	  fprintf (F, " %ld", get_irn_node_nr(n));
	}
    }

  if(loop_node_started)
    {
      fprintf(F, "\" }");
      loop_node_started = 0;
    }
}

void dump_loop_tree(ir_graph *irg, char *suffix)
{
  ir_graph *rem = current_ir_graph;
  int el_rem = edge_label;
  edge_label = 1;

  current_ir_graph = irg;

  vcg_open(irg, suffix, "-looptree");
  dump_vcg_header(get_irg_dump_name(irg), "top_to_bottom");

  if (get_irg_loop(irg)) dump_loops_standalone(get_irg_loop(irg));

  vcg_close();

  edge_label = el_rem;
  current_ir_graph = rem;
}
