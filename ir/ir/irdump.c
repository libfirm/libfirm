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


#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "firm_common_t.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"
#include "irop_t.h"

#include "irdump.h"

#include "irgwalk.h"
#include "typewalk.h"
#include "tv_t.h"
#include "type_or_entity.h"
#include "irouts.h"
#include "irdom.h"
#include "irloop.h"
#include "callgraph.h"

#include "irvrfy.h"

#include "panic.h"
#include "array.h"
#include "pmap.h"
#include "eset.h"

#if DO_HEAPANALYSIS
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
#define DEFAULT_ENUM_ITEM_ATTRIBUTE " "

/* Attributes of edges between Firm nodes */
#define INTRA_DATA_EDGE_ATTR "class:1  priority:50"
#define INTER_DATA_EDGE_ATTR "class:16 priority:10"
#define BLOCK_EDGE_ATTR      "class:2  priority:50 linestyle:dotted"
#define CF_EDGE_ATTR         "class:13 priority:60 color:red"
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

#if DEBUG_libfirm && NODEID_AS_LABEL
#define PRINT_NODEID(X) fprintf(F, "n%ld", get_irn_node_nr(X))
#define PRINT_TYPEID(X) fprintf(F, "\"t%ld\"", get_type_nr(X))
#define PRINT_ENTID(X)  fprintf(F, "e%ld", get_entity_nr(X))
#define PRINT_IRGID(X)  fprintf(F, "g%ld", get_irg_graph_nr(X))
#define PRINT_CONSTID(X,Y) fprintf(F, "\"n%ldn%ld\"", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_LOOPID(X) fprintf(F, "l%d", get_loop_loop_nr(X))
#define PRINT_ITEMID(X,Y)  fprintf(F, "i%ldT%d", get_type_nr(X), (Y))

#else
#define PRINT_NODEID(X) fprintf(F, "n%p", (void *)(X))
#define PRINT_TYPEID(X) fprintf(F, "\"t%p\"", (void *)(X))
#define PRINT_ENTID(X)  fprintf(F, "e%p", (void *)(X))
#define PRINT_IRGID(X)  fprintf(F, "g%p",(void *)(X))
#define PRINT_CONSTID(X,Y) fprintf(F, "\"n%pn%p\"", (void*)(X), (void*)(Y))
#define PRINT_LOOPID(X) fprintf(F, "l%p", (void *)(X))
#define PRINT_ITEMID(X,Y)  fprintf(F, "i%pT%d", (void *) (X), (Y))
#endif


/* basis for a color range for vcg */
static int n_colors   = 0;
static int base_color = 0;

static const char *get_mode_name_ex(ir_mode *mode, int *bad)
{
  if (is_mode(mode))
    return get_mode_name(mode);
  *bad |= 1;
  return "<ERROR>";
}

static const char *get_type_name_ex(type *tp, int *bad)
{
  if (is_type(tp))
    return get_type_name(tp);
  *bad |= 1;
  return "<ERROR>";
}

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

static void print_ent_ent_edge(FILE *F, entity *E, entity *T, int backedge, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  if (backedge)
    fprintf(F, "backedge: { sourcename: \"");
   else
    fprintf(F, "edge: { sourcename: \"");
  PRINT_ENTID(E);
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

static void print_enum_item_edge(FILE *F, type *E, int item, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  fprintf(F, "edge: { sourcename: "); PRINT_TYPEID(E);
  fprintf(F, " targetname: \""); PRINT_ITEMID(E, item); fprintf(F, "\" ");
  vfprintf(F, fmt, ap);
  fprintf(F,"}\n");
  va_end(ap);
}

/*******************************************************************/
/* global and ahead declarations                                   */
/*******************************************************************/

static void dump_whole_node(ir_node *n, void *env);
static INLINE void dump_loop_nodes_into_graph(FILE *F, ir_graph *irg);

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
  return ((get_op_pinned(get_irn_op(n)) == op_pin_state_floats) &&
      (get_irg_pinned(current_ir_graph) == op_pin_state_floats));
}

static const char *get_ent_dump_name(entity *ent) {
  if (! ent)
    return "<NULL entity>";
  /* Don't use get_entity_ld_ident (ent) as it computes the mangled name! */
  if (ent->ld_name) return get_id_str(ent->ld_name);
  return get_id_str(ent->name);
}

static const char *get_irg_dump_name(ir_graph *irg) {
  /* Don't use get_entity_ld_ident (ent) as it computes the mangled name! */
  entity *ent = get_irg_entity(irg);
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
 * Collects all blocks, nodes not op_pin_state_pinned,
 * Bad and Unknown into a flexible array in link field of
 * irg they belong to.  Sets the irg link field to NULL in all
 * graphs not visited.
 * Free the list with DEL_ARR_F.  */
static ir_node ** construct_block_lists(ir_graph *irg) {
  int i, rem_view = interprocedural_view;
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  for (i = 0; i < get_irp_n_irgs(); i++)
    ird_set_irg_link(get_irp_irg(i), NULL);

  irg_walk_graph(current_ir_graph, clear_link, collect_node, current_ir_graph);

  /* Collect also EndReg and EndExcept. We do not want to change the walker. */
  interprocedural_view = 0;
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);
  irg_walk(get_irg_end_reg(current_ir_graph), clear_link, collect_node, current_ir_graph);
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph)-1);
  irg_walk(get_irg_end_except(current_ir_graph), clear_link, collect_node, current_ir_graph);
  interprocedural_view = rem_view;

  current_ir_graph = rem;
  return ird_get_irg_link(irg);
}

/*******************************************************************/
/* flags to steer output                                           */
/*******************************************************************/

const char *dump_file_filter = "";

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

char* overrule_nodecolor = NULL;

INLINE bool get_opt_dump_const_local(void) {
  if (!dump_out_edge_flag && !dump_loop_information_flag)
    return dump_const_local;
  else
    return false;
}

void only_dump_method_with_name(ident *name) {
  dump_file_filter = get_id_str(name);
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

/*-----------------------------------------------------------------*/
/* Routines to dump information about a single ir node.            */
/*-----------------------------------------------------------------*/

INLINE int
dump_node_opcode(FILE *F, ir_node *n)
{
  int bad = 0;

  switch(get_irn_opcode(n)) {

  case iro_Const: {
    int res;
    char buf[1024];
    res = tarval_snprintf(buf, sizeof(buf), get_Const_tarval(n));
    assert(res < sizeof(buf) && "buffer to small for tarval_snprintf");
    fprintf(F, buf);
  } break;

  case iro_SymConst: {
    if (get_SymConst_kind(n) == symconst_addr_name) {
      /* don't use get_SymConst_ptr_info as it mangles the name. */
      fprintf (F, "SymC %s", get_id_str(get_SymConst_name(n)));
    } else if (get_SymConst_kind(n) == symconst_addr_ent) {
      assert(get_SymConst_entity(n));
      assert(is_entity(get_SymConst_entity(n)));
      fprintf (F, "SymC &%s", get_entity_name(get_SymConst_entity(n)));
    } else {
      assert(get_kind(get_SymConst_type(n)) == k_type);
      assert(get_type_ident(get_SymConst_type(n)));
      fprintf (F, "SymC %s ", get_type_name_ex(get_SymConst_type(n), &bad));
      if (get_SymConst_kind(n) == symconst_type_tag)
        fprintf (F, "tag");
      else
        fprintf (F, "size");
    }
  } break;

  case iro_Filter: {
    if (!interprocedural_view) fprintf(F, "Proj'");
    else                       goto default_case;
  } break;

  case iro_Proj: {
    ir_node *pred = get_Proj_pred(n);

    if (get_irn_opcode(pred) == iro_Cond
        && get_Proj_proj(n) == get_Cond_defaultProj(pred)
        && get_irn_mode(get_Cond_selector(pred)) != mode_b)
      fprintf (F, "defProj");
/*
 *   else if (get_irn_opcode(pred) == iro_Proj && get_irn_opcode(get_Proj_pred(pred)) == iro_Start)
 *     fprintf (F, "Arg");
 */
    else
      goto default_case;
  } break;
  case iro_Start:
  case iro_End:
  case iro_EndExcept:
  case iro_EndReg: {
    if (interprocedural_view) {
      fprintf(F, "%s %s", get_irn_opname(n), get_ent_dump_name(get_irg_entity(get_irn_irg(n))));
      break;
    } else
      goto default_case;
  }
  case iro_CallBegin: {
    ir_node *addr = get_CallBegin_ptr(n);
    entity *ent = NULL;
    if (get_irn_op(addr) == op_Sel)
      ent = get_Sel_entity(addr);
    else if ((get_irn_op(addr) == op_SymConst) && (get_SymConst_kind(addr) == symconst_addr_ent))
      ent = get_SymConst_entity(addr);
    fprintf (F, "%s", get_irn_opname(n));
    if (ent) fprintf (F, " %s", get_entity_name(ent));
    break;
  }
  case iro_Load:
    fprintf (F, "%s[%s]", get_irn_opname(n), get_mode_name_ex(get_Load_mode(n), &bad));
    break;

default_case:
  default: {
    fprintf (F, "%s", get_irn_opname(n));
  }

  }  /* end switch */
  return bad;
}

static INLINE int
dump_node_mode(FILE *F, ir_node *n)
{
  int bad = 0;

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
    fprintf(F, "%s", get_mode_name_ex(get_irn_mode(n), &bad));
    break;
  default:
    ;
  }
  return bad;
}

static int dump_node_typeinfo(FILE *F, ir_node *n) {
  int bad = 0;

  if (opt_dump_analysed_type_info) {
    if (get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_consistent  ||
        get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_inconsistent) {
      type *tp = get_irn_type(n);
      if (tp != none_type)
        fprintf(F, " [%s]", get_type_name_ex(tp, &bad));
      else
        fprintf(F, " []");
    }
  }
  return bad;
}

static INLINE int
dump_node_nodeattr(FILE *F, ir_node *n)
{
  int bad = 0;

  switch (get_irn_opcode(n)) {
  case iro_Start:
    if (false && interprocedural_view) {
      fprintf (F, "%s", get_ent_dump_name(get_irg_entity(current_ir_graph)));
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
    fprintf (F, "(%s)", get_type_name_ex(get_Cast_type(n), &bad));
    } break;
  case iro_Confirm: {
    fprintf (F, "%s", get_pnc_string(get_Confirm_cmp(n)));
    } break;

  default:
    ;
  } /* end switch */

  return bad;
}

static INLINE void dump_node_vcgattr(FILE *F, ir_node *n, int bad)
{
  if (bad) {
    fprintf(F, "color: red");
    return;
  }
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

  if (overrule_nodecolor) fprintf(F, " color: %s", overrule_nodecolor);
}

static INLINE int dump_node_info(FILE *F, ir_node *n)
{
  int i, bad = 0;
  char comma;
  ir_graph *irg;

  fprintf (F, " info1: \"");
  if (opt_dump_pointer_values_to_info)
    fprintf (F, "addr:    %p \n", (void *)n);
  fprintf (F, "mode:    %s\n", get_mode_name(get_irn_mode(n)));
  fprintf (F, "visited: %ld \n", get_irn_visited(n));
  irg = get_irn_irg(n);
  if (irg != get_const_code_irg())
    fprintf (F, "irg:     %s\n", get_ent_dump_name(get_irg_entity(irg)));

  if (get_op_pinned(get_irn_op(n)) == op_pin_state_floats &&
      get_irg_pinned(get_irn_irg(n)) == op_pin_state_floats) {
    fprintf(F, "node was pinned in ");
    dump_node_opcode(F, get_nodes_block(n));
    fprintf(F, " %ld\n", get_irn_node_nr(get_nodes_block(n)));
  }

#if 0
  /* show all predecessor nodes */
  fprintf(F, "pred nodes: \n");
  if (!is_Block(n)) {
    fprintf(F, "  -1: ");
    dump_node_opcode(F, get_nodes_block(n));
    fprintf(F, " %ld\n", get_irn_node_nr(get_nodes_block(n)));
  }
  for ( i = 0; i < get_irn_arity(n); ++i) {
    fprintf(F, "   %d: ", i);
    dump_node_opcode(F, get_irn_n(n, i));
    fprintf(F, " %ld\n", get_irn_node_nr(get_irn_n(n, i)));
  }
#endif

  fprintf(F, "arity: %d", get_irn_arity(n));
  if ((get_irn_op(n) == op_Block) ||
      (get_irn_op(n) == op_Phi) ||
      ((get_irn_op(n) == op_Filter) && interprocedural_view)) {
    fprintf(F, " backedges:");
    comma = ' ';
    for (i = 0; i < get_irn_arity(n); i++)
      if (is_backedge(n, i)) { fprintf(F, "%c %d", comma, i); comma = ','; }
  }
  fprintf(F, "\n");

  /* Loop node   Someone else please tell me what's wrong ...
  if (get_irn_loop(n)) {
    ir_loop *loop = get_irn_loop(n);
    assert(loop);
    fprintf(F, " in loop %d with depth %d\n",
        get_loop_loop_nr(loop), get_loop_depth(loop));
  }
  */

  /* Source types */
  switch (get_irn_opcode(n)) {
  case iro_Start: {
    type *tp = get_entity_type(get_irg_entity(get_irn_irg(n)));
    fprintf(F, "start of method of type %s \n", get_type_name_ex(tp, &bad));
    for (i = 0; i < get_method_n_params(tp); ++i)
      fprintf(F, "  param %d type: %s \n", i, get_type_name_ex(get_method_param_type(tp, i), &bad));
  } break;
  case iro_Alloc: {
    fprintf(F, "allocating entity of type %s \n", get_type_name_ex(get_Alloc_type(n), &bad));
  } break;
  case iro_Free: {
    fprintf(F, "freeing entity of type %s \n", get_type_name_ex(get_Free_type(n), &bad));
  } break;
  case iro_Sel: {
    entity *ent = get_Sel_entity(n);

    if (ent) {
      fprintf(F, "Selecting entity of type %s\n", get_type_name_ex(get_entity_type(ent), &bad));
      fprintf(F, "  from entity of type %s\n", get_type_name_ex(get_entity_owner(ent), &bad));
    }
    else {
      fprintf(F, "<NULL entity>\n");
      bad = 1;
    }
  } break;
  case iro_Call: {
    type *tp = get_Call_type(n);
    fprintf(F, "calling method of type %s \n", get_type_name_ex(tp, &bad));
    for (i = 0; i < get_method_n_params(tp); ++i)
      fprintf(F, "  param %d type: %s \n", i, get_type_name_ex(get_method_param_type(tp, i), &bad));
    for (i = 0; i < get_method_n_ress(tp); ++i)
      fprintf(F, "  resul %d type: %s \n", i, get_type_name_ex(get_method_res_type(tp, i), &bad));
    if (0 && Call_has_callees(n)) {
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
      type *tp = get_entity_type(get_irg_entity(get_irn_irg(n)));
      fprintf(F, "return in method of type %s \n", get_type_name_ex(tp, &bad));
      for (i = 0; i < get_method_n_ress(tp); ++i)
    fprintf(F, "  res %d type: %s \n", i, get_type_name_ex(get_method_res_type(tp, i), &bad));
    }
    } break;
  case iro_Const: {
    type *tp = get_Const_type(n);
    assert(tp != none_type);
    fprintf(F, "Const of type %s \n", get_type_name_ex(get_Const_type(n), &bad));
  } break;
  case iro_SymConst: {
    switch(get_SymConst_kind(n)) {
    case symconst_addr_name:
      fprintf(F, "kind addr_name\n");
      break;
    case symconst_addr_ent:
      fprintf(F, "kind addr_ent\n");
      break;
    case symconst_type_tag:
      fprintf(F, "kind type_tag\n");
      break;
    case symconst_size:
      fprintf(F, "kind size\n");
      break;
    }
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
  case iro_Load:
    fprintf(F, "volatility: %s\n", get_volatility_name(get_Load_volatility(n)));
    break;
  case iro_Store:
    fprintf(F, "volatility: %s\n", get_volatility_name(get_Store_volatility(n)));
    break;

  default: ;
  }

  if (get_irg_typeinfo_state(get_irn_irg(n)) == irg_typeinfo_consistent  ||
      get_irg_typeinfo_state(get_irn_irg(n)) == irg_typeinfo_inconsistent  )
    if (get_irn_type(n) != none_type)
      fprintf (F, "\nAnalysed type: %s", get_type_name_ex(get_irn_type(n), &bad));

  fprintf (F, "\"");

  return bad;
}


static INLINE
bool is_constlike_node(ir_node *n) {
  ir_op *op = get_irn_op(n);
  return (op == op_Const || op == op_Bad || op == op_SymConst || op == op_Unknown);
}


/* outputs the predecessors of n, that are constants, local.  I.e.,
   generates a copy of the constant predecessors for each node called with. */
static void dump_const_node_local(FILE *F, ir_node *n) {
  int i;
  if (!get_opt_dump_const_local()) return;

  /* Use visited flag to avoid outputting nodes twice.
     initialize it first. */
  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con)) {
      set_irn_visited(con, get_irg_visited(current_ir_graph) - 1);
    }
  }

  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con) && irn_not_visited(con)) {
      int bad = 0;

      mark_irn_visited(con);
      /* Generate a new name for the node by appending the names of
         n and const. */
      fprintf(F, "node: {title: "); PRINT_CONSTID(n, con);
      fprintf(F, " label: \"");
      bad |= dump_node_opcode(F, con);
      bad |= dump_node_mode(F, con);
      bad |= dump_node_typeinfo(F, con);
      fprintf (F, " ");
      bad |= dump_node_nodeattr(F, con);
      fprintf(F, " %ld", get_irn_node_nr(con));
      fprintf(F, "\" ");
      bad |= dump_node_info(F, con);
      dump_node_vcgattr(F, con, bad);
      fprintf(F, "}\n");
    }
  }
}

static void INLINE print_node_error(FILE *F, const char *p)
{
  if (! p)
    return;

  fprintf (F, " info2: \"%s\"", p);
}

static void dump_node(FILE *F, ir_node *n)
{
  int bad = 0;
  const char *p;

  if (get_opt_dump_const_local() && is_constlike_node(n)) return;
  /* dump this node */
  fprintf(F, "node: {title: \""); PRINT_NODEID(n); fprintf(F, "\" label: \"");

  bad = ! irn_vrfy_irg_dump(n, current_ir_graph, &p);
  bad |= dump_node_opcode(F, n);
  bad |= dump_node_mode(F, n);
  bad |= dump_node_typeinfo(F, n);
  fprintf(F, " ");
  bad |= dump_node_nodeattr(F, n);
  fprintf(F, " %ld", get_irn_node_nr(n));
  fprintf(F, "\" ");
  bad |= dump_node_info(F, n);
  print_node_error(F, p);
  dump_node_vcgattr(F, n, bad);
  fprintf(F, "}\n");
  dump_const_node_local(F, n);
#if DO_HEAPANALYSIS
  dump_chi_term(F, n);
  dump_state(F, n);
#endif
}

/* dump the edge to the block this node belongs to */
static void
dump_ir_block_edge(FILE *F, ir_node *n)  {
  if (get_opt_dump_const_local() && is_constlike_node(n)) return;
  if (is_no_Block(n)) {
    ir_node *block = get_nodes_block(n);

    fprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: ");
    fprintf(F, "\""); PRINT_NODEID(block); fprintf(F, "\"");
    fprintf (F, " "   BLOCK_EDGE_ATTR "}\n");
  }
}

static void
print_data_edge_vcgattr(FILE *F, ir_node *from, int to) {
  if (get_nodes_block(from) == get_nodes_block(get_irn_n(from, to)))
    fprintf (F, INTRA_DATA_EDGE_ATTR);
  else
    fprintf (F, INTER_DATA_EDGE_ATTR);
}

static void
print_mem_edge_vcgattr(FILE *F, ir_node *from, int to) {
  if (get_nodes_block(from) == get_nodes_block(get_irn_n(from, to)))
    fprintf (F, INTRA_MEM_EDGE_ATTR);
  else
    fprintf (F, INTER_MEM_EDGE_ATTR);
}

static void
print_edge_vcgattr(FILE *F, ir_node *from, int to) {
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
    fprintf (F, INTER_MEM_EDGE_ATTR);
    }
    break;
  case iro_EndReg:
  case iro_EndExcept:
  case iro_Jmp:
  case iro_Break:
  case iro_Cond:
    print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Return:
  case iro_Raise:
    if (to == 0)
      print_mem_edge_vcgattr(F, from, to);
    else
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Const:
  case iro_SymConst:
    print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Sel:
  case iro_Call:
    if (to == 0)
      print_mem_edge_vcgattr(F, from, to);
    else
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_CallBegin:
  case iro_Add:
  case iro_Sub:
  case iro_Minus:
  case iro_Mul:
    print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Quot:
  case iro_DivMod:
  case iro_Div:
  case iro_Mod:
    if (to == 0)
      print_mem_edge_vcgattr(F, from, to);
    else
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Abs:
  case iro_And:
  case iro_Or:
  case iro_Eor:
  case iro_Shl:
  case iro_Shr:
  case iro_Shrs:
  case iro_Rot:
  case iro_Cmp:
  case iro_Conv:
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Phi:
    if (get_irn_modecode(from) == irm_M)
      fprintf (F, INTER_MEM_EDGE_ATTR);
    else
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Load:
  case iro_Store:
  case iro_Alloc:
  case iro_Free:
    if (to == 0)
      print_mem_edge_vcgattr(F, from, to);
    else
      print_data_edge_vcgattr(F, from, to);
    break;
  case iro_Sync:
    print_mem_edge_vcgattr(F, from, to);
    break;
  case iro_Tuple:  break;
  case iro_Proj:
  case iro_Filter:
    switch (get_irn_modecode(from)) {
    case irm_X:
      fprintf (F, CF_EDGE_ATTR);
      break;
    case irm_M:
      fprintf (F, INTER_MEM_EDGE_ATTR);
      break;
    default:
      print_data_edge_vcgattr(F, from, to);
      break;
    }
    break;
  case iro_Bad:     break;
  case iro_Unknown: break;
  case iro_Id:
    switch (get_irn_modecode(from)) {
    case irm_M:
      fprintf (F, INTRA_MEM_EDGE_ATTR);
      break;
    case irm_X:
      fprintf (F, CF_EDGE_ATTR);
      break;
    default:
      print_data_edge_vcgattr(F, from, to);
      break;
    } break;
  default:
    ;
  }
}

/* dump edges to our inputs */
static void
dump_ir_data_edges(FILE *F, ir_node *n)  {
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
    print_edge_vcgattr(F, n, i);
    fprintf (F, "}\n");
  }
}

/** Dumps a node and its edges but not the block edge
 */
static INLINE void
dump_node_wo_blockedge (ir_node *n, void *env) {
  FILE *F = env;
  dump_node(F, n);
  dump_ir_data_edges(F, n);
}

/** Dumps a node and its edges.
 */
static void
dump_whole_node (ir_node *n, void *env) {
  FILE *F = env;
  dump_node_wo_blockedge(n, env);
  if (!node_floats(n)) dump_ir_block_edge(F, n);
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
static void dump_const_expression(FILE *F, ir_node *value) {
  ir_graph *rem = current_ir_graph;
  int rem_dump_const_local = dump_const_local;
  dump_const_local = 0;
  current_ir_graph = get_const_code_irg();
  irg_walk(value, dump_const_node, NULL, F);
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
dump_whole_block(FILE *F, ir_node *block) {
  ir_node *node;
  assert(is_Block(block));

  fprintf(F, "graph: { title: \"");
  PRINT_NODEID(block);
  fprintf(F, "\"  label: \"");
  dump_node_opcode(F, block);
  fprintf (F, " %ld", get_irn_node_nr(block));
#if DO_HEAPANALYSIS
  if (get_opt_dump_abstvals())
    fprintf (F, " seqno: %d", (int)get_Block_seqno(block));
#endif
  fprintf(F, "\" status:clustered color:%s \n",
       get_Block_matured(block) ? "yellow" : "red");

  /* dump the blocks edges */
  dump_ir_data_edges(F, block);

  /* dump the nodes that go into the block */
  for (node = ird_get_irn_link(block); node; node = ird_get_irn_link(node)) {
    dump_node(F, node);
    dump_ir_data_edges(F, node);
  }

  /* Close the vcg information for the block */
  fprintf(F, "}\n");
  dump_const_node_local(F, block);
#if DO_HEAPANALYSIS
  dump_chi_term(F, block);
#endif
  fprintf(F, "\n");
}

/** dumps a graph block-wise. Expects all blockless nodes in arr in irgs link.
 *  The outermost nodes: blocks and nodes not op_pin_state_pinned, Bad, Unknown. */
static void
dump_block_graph(FILE *F, ir_graph *irg) {
  int i;
  ir_graph *rem = current_ir_graph;
  ir_node **arr = ird_get_irg_link(irg);
  current_ir_graph = irg;

  for (i = ARR_LEN(arr) - 1; i >= 0; --i) {
    ir_node * node = arr[i];
    if (is_Block(node)) {
      /* Dumps the block and all the nodes in the block, which are to
         be found in Block->link. */
      dump_whole_block(F, node);
    } else {
      /* Nodes that are not in a Block. */
      dump_node(F, node);
      dump_ir_data_edges(F, node);
    }
  }

  if (dump_loop_information_flag && (get_irg_loopinfo_state(irg) & loopinfo_valid))
    dump_loop_nodes_into_graph(F, irg);

  current_ir_graph = rem;
}

/** Dumps an irg as a graph.
 *  If interprocedural view edges can point to nodes out of this graph.
 */
static void dump_graph(FILE *F, ir_graph *irg) {

  fprintf(F, "graph: { title: \"");
  PRINT_IRGID(irg);
  fprintf(F, "\" label: \"%s\" status:clustered color:white \n",
      get_ent_dump_name(get_irg_entity(irg)));

  dump_block_graph(F, irg);

  /* Close the vcg information for the irg */
  fprintf(F, "}\n\n");
}

/*******************************************************************/
/* Basic type and entity nodes and edges.                          */
/*******************************************************************/

/* dumps the edges between nodes and their type or entity attributes. */
static void dump_node2type_edges(ir_node *n, void *env)
{
  FILE *F = env;
  assert(n);

  switch (get_irn_opcode(n)) {
  case iro_Const :
    /* @@@ some consts have an entity */
    break;
  case iro_SymConst:
    if (   (get_SymConst_kind(n) ==symconst_type_tag)
       || (get_SymConst_kind(n) ==symconst_size))
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


static int print_type_info(FILE *F, type *tp) {
  int bad = 0;

  if (get_type_state(tp) == layout_undefined) {
    fprintf(F, "state: layout_undefined\n");
  } else {
    fprintf(F, "state: layout_fixed,\n");
  }
  if (get_type_mode(tp))
    fprintf(F, "mode: %s,\n", get_mode_name_ex(get_type_mode(tp), &bad));
  fprintf(F, "size: %db,\n", get_type_size_bits(tp));

  return bad;
}

static void print_typespecific_info(FILE *F, type *tp) {
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
      fprintf(F, "variadicity: %s\n", get_variadicity_name(get_method_variadicity(tp)));
      fprintf(F, "params: %d\n", get_method_n_params(tp));
      fprintf(F, "results: %d\n", get_method_n_ress(tp));
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


static void print_typespecific_vcgattr(FILE *F, type *tp) {
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

static int print_type_node(FILE *F, type *tp)
{
  int bad = 0;

  fprintf (F, "node: {title: ");
  PRINT_TYPEID(tp);
  fprintf (F, " label: \"%s %s\"", get_type_tpop_name(tp), get_type_name_ex(tp, &bad));
  fprintf (F, " info1: \"");
  bad |= print_type_info(F, tp);
  print_typespecific_info(F, tp);
  fprintf (F, "\"");
  print_typespecific_vcgattr(F, tp);
  fprintf (F, "}\n");

  return bad;
}

#define X(a)    case a: fprintf(F, #a); break
void dump_entity_node(FILE *F, entity *ent, int color)
{
  fprintf (F, "node: {title: \"");
  PRINT_ENTID(ent); fprintf(F, "\"");
  fprintf (F, DEFAULT_TYPE_ATTRIBUTE);
  fprintf (F, "label: ");
  fprintf (F, "\"ent %s\" ", get_ent_dump_name(ent));
  if (color)
    fprintf(F, "color: %d", color);
  else
    fprintf (F, ENTITY_NODE_ATTR);
  fprintf (F, "\n info1: \"");

  dump_entity_to_file(F, ent, dump_verbosity_entattrs | dump_verbosity_entconsts);

  fprintf(F, "\"\n}\n");
}
#undef X

static void dump_enum_item(FILE *F, type *tp, int pos)
{
  char buf[1024];
  ident *id  = get_enumeration_nameid(tp, pos);
  tarval *tv = get_enumeration_enum(tp, pos);

  tarval_snprintf(buf, sizeof(buf), tv);
  fprintf (F, "node: {title: \"");
  PRINT_ITEMID(tp, pos); fprintf(F, "\"");
  fprintf (F, DEFAULT_ENUM_ITEM_ATTRIBUTE);
  fprintf (F, "label: ");
  fprintf (F, "\"enum item %s\" " ENUM_ITEM_NODE_ATTR, get_id_str(id));
  fprintf (F, "\n info1: \"value: %s\"}\n", buf);
}

/* dumps a type or entity and it's edges. */
static void
dump_type_info(type_or_ent *tore, void *env) {
  FILE *F = env;
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */

  switch (get_kind(tore)) {
  case k_entity:
    {
      entity *ent = (entity *)tore;
      ir_node *value;
      /* The node */
      dump_entity_node(F, ent, 0);
      /* The Edges */
      /* skip this to reduce graph.  Member edge of type is parallel to this edge. *
      fprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_OWN_EDGE_ATTR "}\n", ent, get_entity_owner(ent));*/
      print_ent_type_edge(F,ent, get_entity_type(ent), ENT_TYPE_EDGE_ATTR);
      if(is_class_type(get_entity_owner(ent))) {
        for(i = 0; i < get_entity_n_overwrites(ent); i++)
          print_ent_ent_edge(F,ent, get_entity_overwrites(ent, i), 0, ENT_OVERWRITES_EDGE_ATTR);
      }
      /* attached subgraphs */
      if (const_entities && (get_entity_variability(ent) != variability_uninitialized)) {
        if (is_atomic_entity(ent)) {
          value = get_atomic_ent_value(ent);
          if (value) {
            print_ent_node_edge(F, ent, value, ENT_VALUE_EDGE_ATTR, i);
            /* DDMN(value);  $$$ */
            dump_const_expression(F, value);
          }
        }
        if (is_compound_entity(ent)) {
          for (i = 0; i < get_compound_ent_n_values(ent); i++) {
            value = get_compound_ent_value(ent, i);
            if (value) {
              print_ent_node_edge(F, ent, value, ENT_VALUE_EDGE_ATTR, i);
              dump_const_expression(F, value);
              print_ent_ent_edge(F, ent, get_compound_ent_value_member(ent, i), 0, ENT_CORR_EDGE_ATTR, i);
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
      print_type_node(F, tp);
      /* and now the edges */
      switch (get_type_tpop_code(tp)) {
      case tpo_class:
        {
          for (i=0; i < get_class_n_supertypes(tp); i++)
            print_type_type_edge(F, tp,get_class_supertype(tp, i),TYPE_SUPER_EDGE_ATTR);
          for (i=0; i < get_class_n_members(tp); i++)
            print_type_ent_edge(F,tp,get_class_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
        } break;
      case tpo_struct:
        {
          for (i=0; i < get_struct_n_members(tp); i++)
            print_type_ent_edge(F,tp,get_struct_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
        } break;
      case tpo_method:
        {
          for (i = 0; i < get_method_n_params(tp); i++)
            print_type_type_edge(F,tp,get_method_param_type(tp, i),METH_PAR_EDGE_ATTR,i);
          for (i = 0; i < get_method_n_ress(tp); i++)
            print_type_type_edge(F,tp,get_method_res_type(tp, i),METH_RES_EDGE_ATTR,i);
        } break;
      case tpo_union:
        {
          for (i = 0; i < get_union_n_members(tp); i++)
            print_type_ent_edge(F,tp,get_union_member(tp, i),UNION_EDGE_ATTR);
        } break;
      case tpo_array:
        {
          print_type_type_edge(F,tp,get_array_element_type(tp),ARR_ELT_TYPE_EDGE_ATTR);
          print_type_ent_edge(F,tp,get_array_element_entity(tp),ARR_ENT_EDGE_ATTR);
          for (i = 0; i < get_array_n_dimensions(tp); i++) {
            ir_node *upper = get_array_upper_bound(tp, i);
            ir_node *lower = get_array_lower_bound(tp, i);
            print_node_type_edge(F, upper, tp, "label: \"upper %d\"", get_array_order(tp, i));
            print_node_type_edge(F, lower, tp, "label: \"lower %d\"", get_array_order(tp, i));
            dump_const_expression(F, upper);
            dump_const_expression(F, lower);
          }

        } break;
      case tpo_enumeration:
        {
          for (i = 0; i < get_enumeration_n_enums(tp); ++i) {
            dump_enum_item(F, tp, i);
            print_enum_item_edge(F, tp, i, "label: \"item %d\"", i);
          }
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

typedef struct _h_env {
  int dump_ent;
  FILE *f;
} h_env_t;

/** For dumping class hierarchies.
 * Dumps a class type node and a superclass edge.
 * If env->dump_ent dumps entities of classes and overwrites edges.
 */
static void
dump_class_hierarchy_node (type_or_ent *tore, void *ctx) {
  h_env_t *env = ctx;
  FILE *F = env->f;
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */
  switch (get_kind(tore)) {
  case k_entity: {
    entity *ent = (entity *)tore;
    if (get_entity_owner(ent) == get_glob_type()) break;
    if (!is_method_type(get_entity_type(ent))) break;  /* GL */
    if (env->dump_ent && is_class_type(get_entity_owner(ent))) {
      /* The node */
      dump_entity_node(F, ent, 0);
      /* The edges */
      print_type_ent_edge(F,get_entity_owner(ent),ent,TYPE_MEMBER_EDGE_ATTR);
      for(i = 0; i < get_entity_n_overwrites(ent); i++)
        print_ent_ent_edge(F, get_entity_overwrites(ent, i), ent, 0, ENT_OVERWRITES_EDGE_ATTR);
    }
  } break; /* case k_entity */
  case k_type:
    {
      type *tp = (type *)tore;
      if (tp == get_glob_type()) break;
      switch (get_type_tpop_code(tp)) {
        case tpo_class: {
          print_type_node(F, tp);
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
dump_out_edge(ir_node *n, void *env) {
  FILE *F = env;
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
dump_loop_label(FILE *F, ir_loop *loop) {
  fprintf (F, "loop %d, %d sons, %d nodes",
       get_loop_depth(loop), get_loop_n_sons(loop), get_loop_n_nodes(loop));
}

static INLINE void dump_loop_info(FILE *F, ir_loop *loop) {
  fprintf (F, " info1: \"");
  fprintf (F, " loop nr: %d", get_loop_loop_nr(loop));
#if DEBUG_libfirm   /* GL @@@ debug analyses */
  fprintf (F, "\n The loop was analyzed %d times.", (int)get_loop_link(loop));
#endif
  fprintf (F, "\"");
}

static INLINE void
dump_loop_node(FILE *F, ir_loop *loop) {
  fprintf (F, "node: {title: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" label: \"");
  dump_loop_label(F, loop);
  fprintf (F, "\" ");
  dump_loop_info(F, loop);
  fprintf (F, "}\n");

}

static INLINE void
dump_loop_node_edge(FILE *F, ir_loop *loop, int i) {
  assert(loop);
  fprintf (F, "edge: {sourcename: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" targetname: \"");
  PRINT_NODEID(get_loop_node(loop, i));
  fprintf (F, "\" color: green");
  fprintf (F, "}\n");
}

static INLINE void
dump_loop_son_edge(FILE *F, ir_loop *loop, int i) {
  assert(loop);
  fprintf (F, "edge: {sourcename: \"");
  PRINT_LOOPID(loop);
  fprintf (F, "\" targetname: \"");
  PRINT_LOOPID(get_loop_son(loop, i));
  fprintf (F, "\" color: darkgreen label: \"%d\"}\n",
       get_loop_element_pos(loop, get_loop_son(loop, i)));
}

static
void dump_loops(FILE *F, ir_loop *loop) {
  int i;
  /* dump this loop node */
  dump_loop_node(F, loop);

  /* dump edges to nodes in loop -- only if it is a real loop */
  if (get_loop_depth(loop) != 0) {
    for (i = 0; i < get_loop_n_nodes(loop); i++) {
      dump_loop_node_edge(F, loop, i);
    }
  }
  for (i = 0; i < get_loop_n_sons(loop); i++) {
    dump_loops(F, get_loop_son(loop, i));
    dump_loop_son_edge(F, loop, i);
  }
}

static INLINE
void dump_loop_nodes_into_graph(FILE *F, ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_irg_loop(irg)) dump_loops(F, get_irg_loop(irg));

  current_ir_graph = rem;
}


/**
 * dumps the VCG header
 */
static INLINE void
dump_vcg_header(FILE *F, const char *name, const char *orientation) {
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
       "classname 1:  \"intrablock Data\"\n"
       "classname 16: \"interblock Data\"\n"
       "classname 2:  \"Block\"\n"
       "classname 13: \"Control Flow\"\n"
       "classname 14: \"intrablock Memory\"\n"
       "classname 17: \"interblock Memory\"\n"
       "classname 15: \"Dominators\"\n"
       "classname 3:  \"Entity type\"\n"
       "classname 4:  \"Entity owner\"\n"
       "classname 5:  \"Method Param\"\n"
       "classname 6:  \"Method Res\"\n"
       "classname 7:  \"Super\"\n"
       "classname 8:  \"Union\"\n"
       "classname 9:  \"Points-to\"\n"
       "classname 10: \"Array Element Type\"\n"
       "classname 11: \"Overwrites\"\n"
       "classname 12: \"Member\"\n"
       "infoname 1: \"Attribute\"\n"
       "infoname 2: \"Verification errors\"\n",
       name, label, orientation);

  /* don't use all, the range is too whith/black. */
  n_colors   = 18;
  base_color = 105;
  fprintf (F,
       "colorentry 100:    0   0    0\n"
       "colorentry 101:   20   0    0\n"
       "colorentry 102:   40   0    0\n"
       "colorentry 103:   60   0    0\n"
       "colorentry 104:   80   0    0\n"
       "colorentry 105:  100   0    0\n"
       "colorentry 106:  120   0    0\n"
       "colorentry 107:  140   0    0\n"
       "colorentry 108:  150   0    0\n"
       "colorentry 109:  180   0    0\n"
       "colorentry 110:  200   0    0\n"
       "colorentry 111:  220   0    0\n"
       "colorentry 112:  240   0    0\n"
       "colorentry 113:  255   0    0\n"
       "colorentry 113:  255  20   20\n"
       "colorentry 114:  255  40   40\n"
       "colorentry 115:  255  60   60\n"
       "colorentry 116:  255  80   80\n"
       "colorentry 117:  255 100  100\n"
       "colorentry 118:  255 120  120\n"
       "colorentry 119:  255 140  140\n"
       "colorentry 120:  255 150  150\n"
       "colorentry 121:  255 180  180\n"
       "colorentry 122:  255 200  200\n"
       "colorentry 123:  255 220  220\n"
       "colorentry 124:  255 240  240\n"
       "colorentry 125:  255 250  250\n"
	   );

  fprintf (F, "\n");        /* a separator */
}

/**
 * open a vcg file
 *
 * @param irg     The graph to be dumped
 * @param suffix1 first filename suffix
 * @param suffix2 second filename suffix
 */
static FILE *vcg_open (ir_graph *irg, const char * suffix1, const char *suffix2) {
  FILE *F;
  const char *nm = get_irg_dump_name(irg);
  int len = strlen(nm), i, j;
  char *fname;  /* filename to put the vcg information in */

  if (!suffix1) suffix1 = "";
  if (!suffix2) suffix2 = "";

  /* open file for vcg graph */
  fname = malloc (len * 2 + strlen(suffix1) + strlen(suffix2) + 5);

  /* strncpy (fname, nm, len); */     /* copy the filename */
  j = 0;
  for (i = 0; i < len; ++i) {  /* replase '/' in the name: escape by @. */
    if (nm[i] == '/') {
      fname[j] = '@'; j++; fname[j] = '1'; j++;
    } else if (nm[i] == '@') {
      fname[j] = '@'; j++; fname[j] = '2'; j++;
    } else {
      fname[j] = nm[i]; j++;
    }
  }
  fname[j] = '\0';
  strcat (fname, suffix1);  /* append file suffix */
  strcat (fname, suffix2);  /* append file suffix */
  strcat (fname, ".vcg");   /* append the .vcg suffix */
  F = fopen (fname, "w");   /* open file for writing */
  if (!F) {
    panic("cannot open %s for writing (%m)", fname);  /* not reached */
  }
  free(fname);

  return F;
}

/**
 * open a vcg file
 *
 * @param irg     The graph to be dumped
 * @param suffix  filename suffix
 */
static FILE *vcg_open_name (const char *name, const char *suffix) {
  FILE *F;
  char *fname;  /* filename to put the vcg information in */
  int i, j, len = strlen(name);

  if (!suffix) suffix = "";

  /** open file for vcg graph */
  fname = malloc (len * 2 + 5 + strlen(suffix));
  /* strcpy (fname, name);*/    /* copy the filename */
  j = 0;
  for (i = 0; i < len; ++i) {  /* replase '/' in the name: escape by @. */
    if (name[i] == '/') {
      fname[j] = '@'; j++; fname[j] = '1'; j++;
    } else if (name[i] == '@') {
      fname[j] = '@'; j++; fname[j] = '2'; j++;
    } else {
      fname[j] = name[i]; j++;
    }
  }
  fname[j] = '\0';
  strcat (fname, suffix);
  strcat (fname, ".vcg");  /* append the .vcg suffix */
  F = fopen (fname, "w");  /* open file for writing */
  if (!F) {
    panic ("cannot open %s for writing (%m)", fname);  /* not reached */
  }
  free(fname);

  return F;
}

/**
 * Dumps the vcg file footer
 */
static INLINE void dump_vcg_footer (FILE *F) {
  fprintf (F, "}\n");
}

/**
 * close the vcg file
 */
static void
vcg_close (FILE *F) {
  dump_vcg_footer(F);    /* print footer */
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
dump_ir_graph (ir_graph *irg, const char *suffix )
{
  FILE *f;
  ir_graph *rem;
  char *suffix1;
  rem = current_ir_graph;

  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0) return;
  current_ir_graph = irg;
  if (interprocedural_view) suffix1 = "-pure-ip";
  else                      suffix1 = "-pure";
  f = vcg_open(irg, suffix, suffix1);
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  /* walk over the graph */
  /* dump_whole_node must be called in post visiting predecessors */
  irg_walk(get_irg_end(irg), NULL, dump_whole_node, f);

  /* dump the out edges in a separate walk */
  if ((dump_out_edge_flag) && (get_irg_outs_state(irg) != outs_none)) {
    irg_out_walk(get_irg_start(irg), dump_out_edge, NULL, NULL);
  }

  vcg_close(f);

  current_ir_graph = rem;
}


void
dump_ir_block_graph (ir_graph *irg, const char *suffix)
{
  FILE *f;
  int i;
  char *suffix1;

  if(strncmp(get_entity_name(get_irg_entity(irg)),dump_file_filter,strlen(dump_file_filter))!=0) return;
  if (interprocedural_view) suffix1 = "-ip";
  else                      suffix1 = "";
  f = vcg_open(irg, suffix, suffix1);
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  construct_block_lists(irg);

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node **arr = ird_get_irg_link(get_irp_irg(i));
    if (arr) {
      dump_graph(f, get_irp_irg(i));
      DEL_ARR_F(arr);
    }
  }

  vcg_close(f);
}

/** dumps a graph with type information
 */
void
dump_ir_graph_w_types (ir_graph *irg, const char *suffix)
{
  FILE *f;
  ir_graph *rem = current_ir_graph;
  char *suffix1;

  /* if a filter is set, dump only the irg's that match the filter */
  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0)
    return;

  current_ir_graph = irg;

  if (interprocedural_view) suffix1 = "-pure-wtypes-ip";
  else                      suffix1 = "-pure-wtypes";
  f = vcg_open(irg,suffix, suffix1);
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  /* dump common ir graph */
  irg_walk(get_irg_end(irg), NULL, dump_whole_node, f);
  /* dump type info */
  type_walk_irg(irg, dump_type_info, NULL, f);
  inc_irg_visited(get_const_code_irg());
  /* dump edges from graph to type info */
  irg_walk(get_irg_end(irg), dump_node2type_edges, NULL, f);

  vcg_close(f);
  current_ir_graph = rem;
}

void
dump_ir_block_graph_w_types (ir_graph *irg, const char *suffix)
{
  FILE *f;
  int i;
  char *suffix1;
  ir_graph *rem = current_ir_graph;

  /* if a filter is set, dump only the irg's that match the filter */
  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0)
    return;

  if (interprocedural_view) suffix1 = "-wtypes-ip";
  else                      suffix1 = "-wtypes";
  f = vcg_open(irg, suffix, suffix1);
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  /* dump common blocked ir graph */
  construct_block_lists(irg);

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node **arr = ird_get_irg_link(get_irp_irg(i));
    if (arr) {
      dump_graph(f, get_irp_irg(i));
      DEL_ARR_F(arr);
    }
  }

  /* dump type info */
  current_ir_graph = irg;
  type_walk_irg(irg, dump_type_info, NULL, f);
  inc_irg_visited(get_const_code_irg());

  /* dump edges from graph to type info */
  irg_walk(get_irg_end(irg), dump_node2type_edges, NULL, f);

  current_ir_graph = rem;
  vcg_close(f);
}

/*---------------------------------------------------------------------*/
/* The following routines dump a control flow graph.                   */
/*---------------------------------------------------------------------*/

static void
dump_block_to_cfg(ir_node *block, void *env) {
  FILE *F = env;
  int i, fl;
  ir_node *pred;

  if (is_Block(block)) {
    /* This is a block. Dump a node for the block. */
    fprintf (F, "node: {title: \""); PRINT_NODEID(block);
    fprintf (F, "\" label: \"");
    if (block == get_irg_start_block(get_irn_irg(block)))
      fprintf(F, "Start ");
    if (block == get_irg_end_block(get_irn_irg(block)))
      fprintf(F, "End ");

    fprintf (F, "%s ", get_op_name(get_irn_op(block)));
    PRINT_NODEID(block);
    fprintf (F, "\" ");
    fprintf(F, "info1:\"");
    if (dump_dominator_information_flag)
      fprintf(F, "dom depth %d\n", get_Block_dom_depth(block));

    /* show arity and possible Bad predecessors of the block */
    fprintf(F, "arity: %d\n", get_Block_n_cfgpreds(block));
    for (fl = i = 0; i < get_Block_n_cfgpreds(block); ++i) {
      ir_node *pred = get_Block_cfgpred(block, i);
      if (is_Bad(pred)) {
	if (! fl)
	  fprintf(F, "Bad pred at pos: ");
	fprintf(F, "%d ", i);
	fl = 1;
      }
    }
    if (fl)
      fprintf(F, "\n");

    fprintf (F, "\"");  /* closing quote of info */

    if ((block == get_irg_start_block(get_irn_irg(block))) ||
	(block == get_irg_end_block(get_irn_irg(block)))     )
      fprintf(F, " color:blue ");
    else if (fl)
      fprintf(F, " color:yellow ");

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
dump_cfg (ir_graph *irg, const char *suffix)
{
  FILE *f;
  ir_graph *rem = current_ir_graph;
  int ddif = dump_dominator_information_flag;
  int ipv = interprocedural_view;

  /* if a filter is set, dump only the irg's that match the filter */
  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0)
    return;

  current_ir_graph = irg;

  f = vcg_open(irg, suffix, "-cfg");
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  if (interprocedural_view) {
    printf("Warning: dumping cfg not in interprocedural view!\n");
    interprocedural_view = 0;
  }

  if (get_irg_dom_state(irg) != dom_consistent)
    dump_dominator_information_flag = 0;

  /* walk over the blocks in the graph */
  irg_block_walk(get_irg_end(irg), dump_block_to_cfg, NULL, f);
  dump_node(f, get_irg_bad(irg));

  dump_dominator_information_flag = ddif;
  interprocedural_view = ipv;
  vcg_close(f);
  current_ir_graph = rem;
}

static int weight_overall(int rec, int loop) {
  return 2*rec + loop;
}

static int compute_color (int my, int max) {
  int color;
  if (!max) {
    color = 0;
  } else {
    /* if small, scale to the full color range. */
    if (max < n_colors)
      my = my * (n_colors/max);

    int step = 1 + (max / n_colors);

    color = my/step;
  }
  return base_color + n_colors - color;
}

static int get_entity_color(entity *ent) {
  assert(get_entity_irg(ent));
  ir_graph *irg = get_entity_irg(ent);

  int rec_depth     = get_irg_recursion_depth(irg);
  int loop_depth    = get_irg_loop_depth(irg);
  int overall_depth = weight_overall(rec_depth, loop_depth);

  int max_rec_depth     = irp->max_callgraph_recursion_depth;
  int max_loop_depth    = irp->max_callgraph_loop_depth;
  int max_overall_depth = weight_overall(max_rec_depth, max_loop_depth);

  int my_rec_color     = compute_color(rec_depth, max_rec_depth);
  int my_loop_color    = compute_color(loop_depth, max_loop_depth);
  int my_overall_color = compute_color(overall_depth, max_overall_depth);;

  return my_overall_color;
}

void dump_callgraph(const char *suffix) {
  FILE *F;
  int i, n_irgs = get_irp_n_irgs();
  int rem = edge_label;
  edge_label = 1;
  //ident *prefix = new_id_from_str("java/");

  F = vcg_open_name("Callgraph", suffix);
  dump_vcg_header(F, "Callgraph", NULL);

  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    entity *ent = get_irg_entity(irg);
    int j, n_callees = get_irg_n_callees(irg);

    /* Do not dump runtime system. */
    //if (id_is_prefix(prefix, get_entity_ld_ident(ent))) continue;

    dump_entity_node(F, ent, get_entity_color(ent));
    for (j = 0; j < n_callees; ++j) {
      entity *c = get_irg_entity(get_irg_callee(irg, j));
      //if (id_is_prefix(prefix, get_entity_ld_ident(c))) continue;
      int be = is_irg_callee_backedge(irg, j);
      char *attr;
      attr = (be) ?
        "label:\"recursion %d\" color: %d" :
        "label:\"calls %d\" color: %d";
      print_ent_ent_edge(F, ent, c, be, attr, get_irg_callee_loop_depth(irg, j), get_entity_color(ent));
    }
  }

  edge_label = rem;
  vcg_close(F);
}

/* Dump all irgs in interprocedural view to a single file. */
void dump_all_cg_block_graph(const char *suffix) {
  FILE *f;
  int i;
  int rem_view = interprocedural_view;
  interprocedural_view = 1;

  f = vcg_open_name("All_graphs", suffix);
  dump_vcg_header(f, "All_graphs", NULL);

  /* collect nodes in all irgs reachable in call graph*/
  for (i = 0; i < get_irp_n_irgs(); i++)
    ird_set_irg_link(get_irp_irg(i), NULL);

  cg_walk(clear_link, collect_node, NULL);

  /* dump all graphs */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    assert(ird_get_irg_link(current_ir_graph));
    dump_graph(f, current_ir_graph);
    DEL_ARR_F(ird_get_irg_link(current_ir_graph));
  }

  vcg_close(f);
  interprocedural_view = rem_view;
}

/***********************************************************************/
/* the following routines dumps type information without any ir nodes. */
/***********************************************************************/

void
dump_type_graph (ir_graph *irg, const char *suffix)
{
  FILE *f;
  ir_graph *rem;
  rem = current_ir_graph;

  /* if a filter is set, dump only the irg's that match the filter */
  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0) return;

  current_ir_graph = irg;

  f = vcg_open(irg, suffix, "-type");
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  /* walk over the blocks in the graph */
  type_walk_irg(irg, dump_type_info, NULL, f);
  /* The walker for the const code can be called several times for the
     same (sub) experssion.  So that no nodes are dumped several times
     we decrease the visited flag of the corresponding graph after each
     walk.  So now increase it finally. */
  inc_irg_visited(get_const_code_irg());

  vcg_close(f);
  current_ir_graph = rem;
}

void
dump_all_types (const char *suffix)
{
  FILE *f = vcg_open_name("All_types", suffix);
  dump_vcg_header(f, "All_types", NULL);
  type_walk(dump_type_info, NULL, f);
  inc_irg_visited(get_const_code_irg());
  vcg_close(f);
}

void
dump_class_hierarchy (bool entities, const char *suffix)
{
  FILE *f = vcg_open_name("class_hierarchy", suffix);
  h_env_t env;

  env.f = f;
  dump_vcg_header(f, "class_hierarchy", NULL);
  if (entities)
    env.dump_ent = 1;
  else
    env.dump_ent = 0;
  type_walk(dump_class_hierarchy_node, NULL, &env);
  vcg_close(f);
}

/***********************************************************************/
/* dumps all graphs with the graph-dumper passed. Possible dumpers:    */
/*  dump_ir_graph                                                      */
/*  dump_ir_block_graph                                                */
/*  dump_cfg                                                           */
/*  dump_type_graph                                                    */
/*  dump_ir_graph_w_types                                              */
/***********************************************************************/

void dump_all_ir_graphs(dump_graph_func *dmp_grph, const char *suffix) {
  int i;
  for (i=0; i < get_irp_n_irgs(); i++) {
    dmp_grph(get_irp_irg(i), suffix);
  }
}


/**********************************************************************************
 * Dumps a stand alone loop graph with firm nodes which belong to one loop nodes  *
 * packed together in one subgraph/box                                            *
 **********************************************************************************/

void dump_loops_standalone(FILE *F, ir_loop *loop) {
  int i = 0, loop_node_started = 0, son_number = 0, first = 0;
  loop_element le;
  ir_loop *son = NULL;

  /* Dump a new loop node. */
  dump_loop_node(F, loop);

  /* Dump the loop elements. */

  for(i = 0; i < get_loop_n_elements(loop); i++) {
    le = get_loop_element(loop, i);
    son = le.son;
    if (get_kind(son) == k_ir_loop) {

      /* We are a loop son -> Recurse */

      if(loop_node_started) { /* Close the "firm-nodes" node first if we started one. */
        fprintf(F, "\" }\n");
        fprintf (F, "edge: {sourcename: \"");
        PRINT_LOOPID(loop);
        fprintf (F, "\" targetname: \"");
        PRINT_LOOPID(loop);
        fprintf (F, "-%d-nodes\" label:\"%d...%d\"}\n", first, first, i-1);
        loop_node_started = 0;
      }
      dump_loop_son_edge(F, loop, son_number++);
      dump_loops_standalone(F, son);
    } else if (get_kind(son) == k_ir_node) {
        /* We are a loop node -> Collect firm nodes */

      ir_node *n = le.node;
      int bad = 0;

      if (!loop_node_started) {
	/* Start a new node which contains all firm nodes of the current loop */
	fprintf (F, "node: { title: \"");
	PRINT_LOOPID(loop);
	fprintf (F, "-%d-nodes\" color: lightyellow label: \"", i);
	loop_node_started = 1;
	first = i;
      }
      else
	fprintf(F, "\n");

      bad |= dump_node_opcode(F, n);
      bad |= dump_node_mode(F, n);
      bad |= dump_node_typeinfo(F, n);
      fprintf (F, " ");
      bad |= dump_node_nodeattr(F, n);
      fprintf (F, " %ld", get_irn_node_nr(n));
      if (is_Block(n)) fprintf (F, "\t ->%d", (int)get_irn_link(n));
    } else { /* for callgraph loop tree */
      assert(get_kind(son) == k_ir_graph);
      /* We are a loop node -> Collect firm graphs */
      ir_graph *n = (ir_graph *)le.node;
      if (!loop_node_started) {
	/* Start a new node which contains all firm nodes of the current loop */
	fprintf (F, "node: { title: \"");
	PRINT_LOOPID(loop);
	fprintf (F, "-%d-nodes\" color: lightyellow label: \"", i);
	loop_node_started = 1;
	first = i;
      }
      else
	fprintf(F, "\n");
      fprintf (F, " %s", get_irg_dump_name(n));
      /* fprintf (F, " %s (depth %d)", get_irg_dump_name(n), n->callgraph_weighted_loop_depth); */
    }
  }

  if (loop_node_started) {
    fprintf(F, "\" }\n");
    fprintf (F, "edge: {sourcename: \"");
    PRINT_LOOPID(loop);
    fprintf (F, "\" targetname: \"");
    PRINT_LOOPID(loop);
    fprintf (F, "-%d-nodes\" label:\"%d...%d\"}\n", first, first, i-1);
    loop_node_started = 0;
  }
}

void dump_loop_tree(ir_graph *irg, const char *suffix)
{
  FILE *f;
  ir_graph *rem = current_ir_graph;
  int el_rem = edge_label;
  edge_label = 1;

  /* if a filter is set, dump only the irg's that match the filter */
  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0)
    return;

  current_ir_graph = irg;

  f = vcg_open(irg, suffix, "-looptree");
  dump_vcg_header(f, get_irg_dump_name(irg), "top_to_bottom");

  if (get_irg_loop(irg)) dump_loops_standalone(f, get_irg_loop(irg));

  vcg_close(f);

  edge_label = el_rem;
  current_ir_graph = rem;
}

void dump_callgraph_loop_tree(const char *suffix) {
  FILE *F;
  F = vcg_open_name("Callgraph_looptree", suffix);
  dump_vcg_header(F, "callgraph looptree", "top_to_bottom");
  dump_loops_standalone(F, irp->outermost_cg_loop);
  vcg_close(F);
}


/*******************************************************************************/
/* Dumps the firm nodes in the loop tree to a graph along with the loop nodes. */
/*******************************************************************************/

void collect_nodeloop(FILE *F, ir_loop *loop, eset *loopnodes) {
  int i, son_number = 0, node_number = 0;

  if (dump_loop_information_flag) dump_loop_node(F, loop);

  for (i = 0; i < get_loop_n_elements(loop); i++) {
    loop_element le = get_loop_element(loop, i);
    if (*(le.kind) == k_ir_loop) {
      if (dump_loop_information_flag) dump_loop_son_edge(F, loop, son_number++);
      /* Recur */
      collect_nodeloop(F, le.son, loopnodes);
    } else {
      if (dump_loop_information_flag) dump_loop_node_edge(F, loop, node_number++);
      eset_insert(loopnodes, le.node);
    }
  }
}

void collect_nodeloop_external_nodes(ir_loop *loop, eset *loopnodes, eset *extnodes) {
  int i, j, start;

  for(i = 0; i < get_loop_n_elements(loop); i++) {
    loop_element le = get_loop_element(loop, i);
    if (*(le.kind) == k_ir_loop) {
      /* Recur */
      collect_nodeloop_external_nodes(le.son, loopnodes, extnodes);
    } else {
      if (is_Block(le.node)) start = 0; else start = -1;
      for (j = start; j < get_irn_arity(le.node); j++) {
        ir_node *pred = get_irn_n(le.node, j);
        if (!eset_contains(loopnodes, pred)) {
          eset_insert(extnodes, pred);
          if (!is_Block(pred)) {
            pred = get_nodes_block(pred);
            if (!eset_contains(loopnodes, pred)) eset_insert(extnodes, pred);
          }
        }
      }
    }
  }
}

void dump_loop(ir_loop *l, const char *suffix) {
  FILE *F;
  char name[50];
  eset *loopnodes = eset_create();
  eset *extnodes = eset_create();
  ir_node *n, *b;

  snprintf(name, sizeof(name), "loop_%d", get_loop_loop_nr(l));
  F = vcg_open_name (name, suffix);
  dump_vcg_header(F, name, NULL);

  /* collect all nodes to dump */
  collect_nodeloop(F, l, loopnodes);
  collect_nodeloop_external_nodes(l, loopnodes, extnodes);

  /* build block lists */
  for (n = eset_first(loopnodes); n != NULL; n = eset_next(loopnodes))
    set_irn_link(n, NULL);
  for (n = eset_first(extnodes); n != NULL; n = eset_next(extnodes))
    set_irn_link(n, NULL);
  for (n = eset_first(loopnodes); n != NULL; n = eset_next(loopnodes))
    if (!is_Block(n)) {
      b = get_nodes_block(n);
      set_irn_link(n, get_irn_link(b));
      set_irn_link(b, n);
    }
  for (n = eset_first(extnodes); n != NULL; n = eset_next(extnodes))
    if (!is_Block(n)) {
      b = get_nodes_block(n);
      set_irn_link(n, get_irn_link(b));
      set_irn_link(b, n);
    }

  for (b = eset_first(loopnodes); b != NULL; b = eset_next(loopnodes))
    if (is_Block(b)) {
      fprintf(F, "graph: { title: \"");
      PRINT_NODEID(b);
      fprintf(F, "\"  label: \"");
      dump_node_opcode(F, b);
      fprintf (F, " %ld", get_irn_node_nr(b));
      fprintf(F, "\" status:clustered color:yellow\n");

      /* dump the blocks edges */
      dump_ir_data_edges(F, b);

      /* dump the nodes that go into the block */
      for (n = get_irn_link(b); n; n = get_irn_link(n)) {
        if (eset_contains(extnodes, n)) overrule_nodecolor = "lightblue";
        dump_node(F, n);
        overrule_nodecolor = NULL;
        if (!eset_contains(extnodes, n)) dump_ir_data_edges(F, n);
      }

      /* Close the vcg information for the block */
      fprintf(F, "}\n");
      dump_const_node_local(F, b);
      fprintf(F, "\n");
    }
  for (b = eset_first(extnodes); b != NULL; b = eset_next(extnodes))
    if (is_Block(b)) {
      fprintf(F, "graph: { title: \"");
      PRINT_NODEID(b);
      fprintf(F, "\"  label: \"");
      dump_node_opcode(F, b);
      fprintf (F, " %ld", get_irn_node_nr(b));
      fprintf(F, "\" status:clustered color:lightblue\n");

      /* dump the nodes that go into the block */
      for (n = get_irn_link(b); n; n = get_irn_link(n)) {
        if (!eset_contains(loopnodes, n)) overrule_nodecolor = "lightblue";
        dump_node(F, n);
        overrule_nodecolor = NULL;
        if (eset_contains(loopnodes, n)) dump_ir_data_edges(F, n);
      }

      /* Close the vcg information for the block */
      fprintf(F, "}\n");
      dump_const_node_local(F, b);
      fprintf(F, "\n");
    }

  eset_destroy(loopnodes);
  eset_destroy(extnodes);
  vcg_close(F);
}
