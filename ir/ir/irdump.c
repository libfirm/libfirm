/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irdump.h: dumping of an intermediate representation graph
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irprog.h"
# include "irdump.h"
# include "panic.h"
# include <string.h>
# include "entity_t.h"
# include <stdlib.h>
# include "array.h"
# include "irop_t.h"
# include "tv.h"
# include "type_or_entity.h"
# include "irgwalk.h"
# include "typewalk.h"
# include "irouts.h"
# include "irdom.h"
# include "firm_common_t.h"
# include  "irloop.h"

# include "exc.h"

# include "pmap.h"

/* Attributes of nodes */
#define DEFAULT_NODE_ATTR ""
#define DEFAULT_TYPE_ATTRIBUTE ""

/* Attributes of edges between Firm nodes */
#define BLOCK_EDGE_ATTR "class: 2 priority: 2 linestyle: dotted"
#define CF_EDGE_ATTR    "color: red"
#define MEM_EDGE_ATTR   "color: blue"
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
#define TYPE_SUPER_EDGE_ATTR "class: 7 label: \"supertype\" color: blue"
#define UNION_EDGE_ATTR      "class: 8 label: \"component\" color: blue"
#define PTR_PTS_TO_EDGE_ATTR "class: 9 label: \"points to\" color:green"
#define ARR_ELT_TYPE_EDGE_ATTR "class: 10 label: \"arr elt tp\" color:green"
#define ARR_ENT_EDGE_ATTR    "class: 10 label: \"arr ent\" color: green"
#define ENT_OVERWRITES_EDGE_ATTR "class: 11 label: \"overwrites\" color:red"
#define ENT_VALUE_EDGE_ATTR "label: \"value "
#define ENT_CORR_EDGE_ATTR "label: \"value %d corresponds to \" "
#define TYPE_MEMBER_EDGE_ATTR "class: 12 label: \"member\" color:blue"


#if DEBUG_libfirm && NODEID_AS_LABEL
#define PRINT_NODEID(X) fprintf(F, "%ld", get_irn_node_nr(X))
#else
#define PRINT_NODEID(X) fprintf(F, "%p", X)
#endif

/* A suffix to manipulate the file name. */
char *dump_file_suffix = NULL;

/* file to dump to */
static FILE *F;

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
int dump_const_local = 0;
static INLINE bool dump_const_local_set() {
  if (!dump_out_edge_flag && !dump_loop_information_flag)
    return dump_const_local;
  else
    return false;
}

/* A global variable to record output of the Bad node. */
int Bad_dumped;

void dump_ir_blocks_nodes (ir_node *n, void *env);
void dump_whole_node (ir_node *n, void* env);

/*******************************************************************/
/* routines to dump information about a single node                */
/*******************************************************************/



INLINE void
dump_node_opcode (ir_node *n)
{
  assert(n && n->op);

  /* Const */
  if (n->op->code == iro_Const) {
    xfprintf (F, "%v", n->attr.con);

  /* SymConst */
  } else if (n->op->code == iro_SymConst) {
    if (get_SymConst_kind(n) == linkage_ptr_info) {
      /* don't use get_SymConst_ptr_info as it mangles the name. */
      xfprintf (F, "SymC %I", n->attr.i.tori.ptrinfo);
    } else {
      assert(get_kind(get_SymConst_type(n)) == k_type);
      assert(get_type_ident(get_SymConst_type(n)));
      xfprintf (F, "SymC %I ", get_type_ident(get_SymConst_type(n)));
      if (get_SymConst_kind == type_tag)
	xfprintf (F, "tag");
      else
	xfprintf (F, "size");
    }

  /* Filter */
  } else if (n->op->code == iro_Filter && !interprocedural_view) {
    fprintf(F, "Proj'");

  /* all others */
  } else {
    xfprintf (F, "%I", get_irn_opident(n));
  }
}

INLINE void
dump_node_mode (ir_node *n)
{
  switch (n->op->code) {
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
    xfprintf (F, "%I", get_mode_ident(n->mode));
    break;
  default:
  }
}

void dump_node_loop_info(ir_node *n) {
  //  if (get_irn_loop(n))
  //  xfprintf(F, "\n in loop %d", get_loop_depth(get_irn_loop(n)));
}

INLINE void
dump_node_nodeattr (ir_node *n)
{
  switch (n->op->code) {
  case iro_Start:
    if (false && interprocedural_view) {
      xfprintf (F, "%I", get_entity_ident(get_irg_ent(current_ir_graph)));
    }
    break;
  case iro_Proj:
    if (n->in[1]->op->code == iro_Cmp) {
      xfprintf (F, "%s", get_pnc_string(n->attr.proj));
    } else {
      xfprintf (F, "%ld", n->attr.proj);
    }
    break;
  case iro_Filter:
    xfprintf (F, "%ld", n->attr.filter.proj);
    break;
  case iro_Sel: {
    assert(get_kind(get_Sel_entity(n)) == k_entity);
    xfprintf (F, "%I", get_entity_ident(get_Sel_entity(n)));
    } break;
  default:
  } /* end switch */
}

INLINE void
dump_node_vcgattr (ir_node *n)
{
  switch (n->op->code) {
  case iro_Start:
  case iro_EndReg:
    /* fall through */
  case iro_EndExcept:
    /* fall through */
  case iro_End:
    xfprintf (F, "color: blue");
    break;
  case iro_Block:
    xfprintf (F, "color: lightyellow");
    break;
  case iro_Phi:
    xfprintf (F, "color: green");
    break;
  case iro_Const:
  case iro_Proj:
  case iro_Filter:
  case iro_Tuple:
    xfprintf (F, "color: yellow");
    break;
  default:
    xfprintf (F, DEFAULT_NODE_ATTR);
  }
}

bool pred_in_wrong_graph(ir_node *n, int pos, pmap *irgmap) {
  ir_node *block = (is_Block(n)) ? n : get_nodes_Block(n);

  if (irgmap &&
      ((get_irn_op(n) == op_Filter) || (get_irn_op(n) == op_Block))) {
    ir_node *pred = skip_Proj(get_Block_cfgpred(block, pos));
    if (is_ip_cfop(pred)) {
      ir_graph *irg = get_ip_cfop_irg(pred);
      if (pmap_find(irgmap, irg) == NULL) return true;
    }
  }

  return false;
}


static INLINE
bool is_constlike_node(ir_node *n) {
  ir_op *op = get_irn_op(n);
  return (op == op_Const || op == op_Bad || op == op_SymConst);
}


void dump_const_node_local(ir_node *n, pmap *irgmap) {
  int i;
  if (!dump_const_local_set()) return;
  /* Use visited flag to avoid outputting nodes twice.
     initialize it first. */
  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con)) {
      if (pred_in_wrong_graph(n, i, irgmap)) continue; /* pred not dumped */
      set_irn_visited(con, get_irg_visited(current_ir_graph)-1);
    }
  }
  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node *con = get_irn_n(n, i);
    if (is_constlike_node(con) && irn_not_visited(con)) {
      if (pred_in_wrong_graph(n, i, irgmap)) continue; /* pred not dumped */
      mark_irn_visited(con);
      /* Generate a new name for the node by appending the names of
	 n and const. */
      xfprintf (F, "node: {title: \""); PRINT_NODEID(n); PRINT_NODEID(con);
      fprintf(F, "\" label: \"");
      dump_node_opcode(con);
      dump_node_mode (con);
      xfprintf (F, " ");
      dump_node_nodeattr(con);
#ifdef DEBUG_libfirm
      xfprintf (F, " %ld", get_irn_node_nr(con));
#endif
      xfprintf (F, "\" ");
      dump_node_vcgattr(con);
      xfprintf (F, "}\n");
    }
  }
}

void
dump_node (ir_node *n, pmap * map) {
  if (dump_const_local_set() && is_constlike_node(n)) return;

  /* dump this node */
  xfprintf (F, "node: {title: \""); PRINT_NODEID(n); fprintf(F, "\" label: \"");

  dump_node_opcode(n);
  dump_node_mode (n);
  xfprintf (F, " ");
  dump_node_nodeattr(n);
#ifdef DEBUG_libfirm
  xfprintf (F, " %ld", get_irn_node_nr(n));
#endif
  xfprintf (F, "\" ");
  dump_node_vcgattr(n);
  xfprintf (F, "}\n");
  dump_const_node_local(n, map);
}

void
dump_ir_node (ir_node *n)
{
  /* dump this node */
  fprintf (F, "node: {title: \""); PRINT_NODEID(n); fprintf(F, "\" label: ");

  switch (n->op->code) {  /* node label */
  case iro_Start:
    xfprintf (F, "\"%I\" color: blue ", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
     break;
  case iro_EndReg:
    /* fall through */
  case iro_EndExcept:
    /* fall through */
  case iro_End:
    xfprintf (F, "\"%I\" color: blue ", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Block:
    xfprintf (F, "\"%I\" color: lightyellow ", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Phi:
    xfprintf (F, "\"%I%I\" color: green", get_irn_opident(n), get_irn_modeident(n));
    if (get_irn_modecode(n) == irm_M)
      xfprintf (F, DEFAULT_NODE_ATTR " color: green");
    else
      xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Const:
    xfprintf (F, "\"%v%I\" color: yellow ", n->attr.con, get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Id:
    xfprintf (F, "\"%I%I\" ", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Proj:
    if (n->in[1]->op->code == iro_Cmp) {
      xfprintf (F, "\"%I%I %s\" color: yellow", get_irn_opident(n), get_irn_modeident(n),
                get_pnc_string(n->attr.proj));
    } else {
      xfprintf (F, "\"%I%I %ld\"", get_irn_opident(n), get_irn_modeident(n), n->attr.proj);
    }
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Filter:
    xfprintf (F, "\"%I%I %ld\"", get_irn_opident(n), get_irn_modeident(n), n->attr.filter.proj);
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Conv:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Tuple:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Add:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Sub:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Mul:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Quot:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_DivMod:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Div:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Mod:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_And:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Or:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Eor:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Shl:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Shr:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Abs:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Cmp:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Jmp:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Break:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Cond:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Call:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_CallBegin:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Return:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Raise:
    xfprintf (F, "\"%I%I\"", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Load:
  case iro_Store:
    xfprintf (F, "\"%R\"", n);
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Alloc:
    xfprintf (F, "\"%I\" ", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Sel:
    assert(get_kind(get_Sel_entity(n)) == k_entity);
    xfprintf (F, "\"%I ", get_irn_opident(n));
    xfprintf (F, "%I", get_entity_ident(get_Sel_entity(n)));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_SymConst:
    assert(get_kind(get_SymConst_type(n)) == k_type);
    assert(get_type_ident(get_SymConst_type(n)));
    xfprintf (F, "\"%s ", get_type_name(get_SymConst_type(n)));
    switch (n->attr.i.num){
    case type_tag:
      xfprintf (F, "tag\" ");
      break;
    case size:
      xfprintf (F, "size\" ");
      break;
    default:
      assert(0);
      break;
    }
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Sync:
    xfprintf (F, "\"%I\" ", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR " color: green");
    break;
  case iro_Bad:
    xfprintf (F, "\"%I%I\" ", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Unknown:
    xfprintf (F, "\"%I%I\" ", get_irn_opident(n), get_irn_modeident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  default:
    xfprintf (F, "\"%I%I\" ", get_irn_opident(n), get_irn_modeident(n));
  }
  xfprintf (F, "}\n");		/* footer */
}


/* dump the edge to the block this node belongs to */
void
dump_ir_block_edge(ir_node *n)  {
  if (dump_const_local_set() && is_constlike_node(n)) return;
  if (is_no_Block(n)) {
    xfprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    xfprintf (F, "\" targetname: \"");
    PRINT_NODEID(get_nodes_Block(n));
    xfprintf (F, "\" "	BLOCK_EDGE_ATTR "}\n");
  }
}

void print_edge_vcgattr(ir_node *from, int to) {
  assert(from);

  if (is_backedge(from, to)) xfprintf (F, BACK_EDGE_ATTR);

  switch (get_irn_opcode(from)) {
  case iro_Block:
    xfprintf (F, CF_EDGE_ATTR);
    break;
  case iro_Start:   break;
  case iro_End:
    if (to >= 0) {
      if (get_irn_mode(get_End_keepalive(from, to)) == mode_BB)
	xfprintf (F, CF_EDGE_ATTR);
      if (get_irn_mode(get_End_keepalive(from, to)) == mode_X)
	xfprintf (F, MEM_EDGE_ATTR);
    }
    break;
  case iro_EndReg: break;
  case iro_EndExcept: break;
  case iro_Jmp:     break;
  case iro_Break:   break;
  case iro_Cond:    break;
  case iro_Return:
  case iro_Raise:
    if (to == 0) xfprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Const:   break;
  case iro_SymConst:break;
  case iro_Sel:
  case iro_Call:
    if (to == 0) xfprintf (F, MEM_EDGE_ATTR);
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
    if (to == 0) xfprintf (F, MEM_EDGE_ATTR);
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
    if (get_irn_modecode(from) == irm_M) xfprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Load:
  case iro_Store:
  case iro_Alloc:
  case iro_Free:
    if (to == 0) xfprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Sync:
    xfprintf (F, MEM_EDGE_ATTR);
    break;
  case iro_Tuple:  break;
  case iro_Proj:
  case iro_Filter:
    switch (get_irn_modecode(from)) {
    case irm_X:
      xfprintf (F, CF_EDGE_ATTR);
      break;
    case irm_M:
      xfprintf (F, MEM_EDGE_ATTR);
      break;
    default: break;
    }
    break;
  case iro_Bad:    break;
  case iro_Unknown: break;
  case iro_Id:     break;
  default:
  }
}

/* dump edges to our inputs */
void
dump_ir_data_edges(ir_node *n)  {
  int i, visited = get_irn_visited(n);

  if ((get_irn_op(n) == op_End) && (!dump_keepalive))
    return;

  for (i = 0; i < get_irn_arity(n); i++) {
    ir_node * pred = get_irn_n(n, i);
    assert(pred);
    if ((interprocedural_view && get_irn_visited(pred) < visited))
      continue; /* pred not dumped */
    if (is_backedge(n, i))
      fprintf (F, "backedge: {sourcename: \"");
    else
      fprintf (F, "edge: {sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"");
    if ((dump_const_local_set()) && is_constlike_node(pred))
      PRINT_NODEID(n);
    PRINT_NODEID(pred);
    fprintf (F, "\"");
    fprintf (F, " label: \"%d\" ", i);
    print_edge_vcgattr(n, i);
    fprintf (F, "}\n");
  }
}

/* dump out edges */
void
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
  fprintf (F, "edge: {sourcename: \"%p\" targetname: \"", loop);
  PRINT_NODEID(get_loop_node(loop, i));
  fprintf (F, "\" color: green");
  fprintf (F, "}\n");
}

static
void dump_loops (ir_loop *loop) {
  int i;
  /* dump this loop node */
  xfprintf (F, "node: {title: \"%p\" label: \"loop %d, %d sons, %d nodes\" }\n",
	    loop, get_loop_depth(loop), get_loop_n_sons(loop), get_loop_n_nodes(loop));
  /* dump edges to nodes in loop -- only if it is a real loop */
  if (get_loop_depth(loop) != 0) {
    for (i = 0; i < get_loop_n_nodes(loop); i++) {
      dump_loop_node_edge(loop, i);
    }
  }
  for (i = 0; i < get_loop_n_sons(loop); i++) {
    dump_loops(get_loop_son(loop, i));
  }
}

static INLINE
void dump_loop_info(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_irg_loop(irg))
    dump_loops(get_irg_loop(irg));

  current_ir_graph = rem;
}


/* dumps the edges between nodes and their type or entity attributes. */
void dump_node2type_edges (ir_node *n, void *env)
{
  assert(n);

  switch (get_irn_opcode(n)) {
  case iro_Const :
    /* @@@ some consts have an entity */
    break;
  case iro_SymConst:
    if (   (get_SymConst_kind(n) == type_tag)
	   || (get_SymConst_kind(n) == size)) {
      xfprintf (F, "edge: { sourcename: \"");
      PRINT_NODEID(n);
      fprintf (F, "\" targetname: \"%p\" "
	       NODE2TYPE_EDGE_ATTR "}\n", get_SymConst_type(n));
    }
    break;
  case iro_Sel: {
    xfprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"%p\" "
	     NODE2TYPE_EDGE_ATTR "}\n", get_Sel_entity(n));
    } break;
  case iro_Call: {
    xfprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"%p\" "
	     NODE2TYPE_EDGE_ATTR "}\n", get_Call_type(n));
    } break;
  case iro_Alloc: {
    xfprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"%p\" "
	     NODE2TYPE_EDGE_ATTR "}\n", get_Alloc_type(n));
    } break;
  case iro_Free: {
    xfprintf (F, "edge: { sourcename: \"");
    PRINT_NODEID(n);
    fprintf (F, "\" targetname: \"%p\" "
	     NODE2TYPE_EDGE_ATTR "}\n", get_Free_type(n));
    } break;
  default:
    break;
  }
}


void dump_const_expression(ir_node *value) {
  ir_graph *rem = current_ir_graph;
  int rem_dump_const_local = dump_const_local;
  dump_const_local = 0;
  current_ir_graph = get_const_code_irg();
  irg_walk(value, dump_ir_blocks_nodes, NULL, get_nodes_Block(value));
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph) -1);
  current_ir_graph = rem;
  dump_const_local = rem_dump_const_local;
}


void print_type_info(type *tp) {
  if (get_type_state(tp) == layout_undefined) {
    xfprintf(F, "state: layout_undefined\n");
  } else {
    xfprintf(F, "state: layout_fixed,\n");
  }
  if (get_type_mode(tp))
    xfprintf(F, "mode: %s,\n", get_mode_name(get_type_mode(tp)));
  xfprintf(F, "size: %dB,\n", get_type_size(tp));
}


void print_typespecific_info(type *tp) {
  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    {
      if(existent == get_class_peculiarity(tp))
	xfprintf (F, " " TYPE_CLASS_NODE_ATTR);
      else
	xfprintf (F, " " TYPE_DESCRIPTION_NODE_ATTR);
    } break;
  case tpo_struct:
    {
      xfprintf (F, " " TYPE_METH_NODE_ATTR);
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

void print_type_node(type *tp) {
  xfprintf (F, "node: {title: \"%p\" ", tp);
  xfprintf (F, "label: \"%I %I\"", get_type_tpop_nameid(tp), get_type_ident(tp));
  xfprintf (F, "info1: \"");
  print_type_info(tp);
  xfprintf (F, "\"");
  print_typespecific_info(tp);
  xfprintf (F, "}\n");
}

/* dumps a type or entity and it's edges. */
void
dump_type_info (type_or_ent *tore, void *env) {
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */

  switch (get_kind(tore)) {
  case k_entity:
    {
      entity *ent = (entity *)tore;
      ir_node *value;
      /* The node */
      xfprintf (F, "node: {title: \"%p\" ", tore);
      xfprintf (F, DEFAULT_TYPE_ATTRIBUTE);
      xfprintf (F, "label: ");
      xfprintf (F, "\"ent %I\" " ENTITY_NODE_ATTR , get_entity_ident(ent));
      switch (get_entity_allocation(ent)) {
        case dynamic_allocated:   fprintf (F, " info1:\"dynamic allocated\n");   break;
        case automatic_allocated: fprintf (F, " info1:\"automatic allocated\n"); break;
        case static_allocated:    fprintf (F, " info1:\"static allocated\n");    break;
      }
      switch (get_entity_visibility(ent)) {
	case local:              fprintf (F, "local\n");             break;
	case external_visible:   fprintf (F, "external_visible\n");  break;
	case external_allocated: fprintf (F, "external_allocate\n"); break;
      }
      switch (get_entity_variability(ent)) {
	case uninitialized: fprintf (F, "uninitialized\n");break;
	case initialized:   fprintf (F, "initialized\n");  break;
	case part_constant: fprintf (F, "part_constant\n");break;
	case constant:      fprintf (F, "constant\n");     break;
      }
      switch (get_entity_volatility(ent)) {
	case non_volatile: fprintf (F, "non_volatile\n"); break;
	case is_volatile:  fprintf (F, "is_volatile\n");  break;
      }
      switch (get_entity_peculiarity(ent)) {
	case description: fprintf (F, "description\n"); break;
        case inherited:   fprintf (F, "inherited\n"); break;
	case existent:    fprintf (F, "existent\n");    break;
      }
      if (is_method_type(get_entity_type(ent)))
	xfprintf (F, "\n irg = %p ", get_entity_irg(ent));
      xfprintf(F, "\"}\n");
      /* The Edges */
      /* skip this to reduce graph.  Member edge of type is parallel to this edge. *
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_OWN_EDGE_ATTR "}\n", ent, get_entity_owner(ent));*/
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_TYPE_EDGE_ATTR "}\n", ent, get_entity_type(ent));
      if(is_class_type(get_entity_owner(ent))) {
	for(i = 0; i < get_entity_n_overwrites(ent); i++)
	  xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		    ENT_OVERWRITES_EDGE_ATTR "}\n",
		    ent, get_entity_overwrites(ent, i));
      }
      /* attached subgraphs */
      if (const_entities && (get_entity_variability(ent) != uninitialized)) {
	if (is_atomic_entity(ent)) {
	  value = get_atomic_ent_value(ent);
	  xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"", ent);
	  PRINT_NODEID(value);
	  fprintf(F, "\" " ENT_VALUE_EDGE_ATTR "\"}\n");
	  dump_const_expression(value);
	}
	if (is_compound_entity(ent)) {
	  for (i = 0; i < get_compound_ent_n_values(ent); i++) {
	    value = get_compound_ent_value(ent, i);
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"", ent);
	    PRINT_NODEID(value);
	    fprintf(F, "\" " ENT_VALUE_EDGE_ATTR " %d \"}\n", i);
	    dump_const_expression(value);
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      ENT_CORR_EDGE_ATTR  "}\n", ent,
		      get_compound_ent_value_member(ent, i), i);
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
	  for (i=0; i < get_class_n_supertypes(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      TYPE_SUPER_EDGE_ATTR "}\n",
		      tp, get_class_supertype(tp, i));
	  for (i=0; i < get_class_n_members(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      TYPE_MEMBER_EDGE_ATTR "}\n",
		      tp, get_class_member(tp, i));
	} break;
      case tpo_struct:
	{
	  for (i=0; i < get_struct_n_members(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      TYPE_MEMBER_EDGE_ATTR "}\n",
		      tp, get_struct_member(tp, i));
	} break;
      case tpo_method:
	{
	  for (i = 0; i < get_method_n_params(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      METH_PAR_EDGE_ATTR "}\n",
		      tp, get_method_param_type(tp, i), i);
	  for (i = 0; i < get_method_n_ress(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      METH_RES_EDGE_ATTR "}\n",
		      tp, get_method_res_type(tp, i), i);
	} break;
      case tpo_union:
	{
	  for (i = 0; i < get_union_n_members(tp); i++)
	    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		      "label: \"\"f" UNION_EDGE_ATTR "}\n",
		      tp, get_union_member(tp, i));
	} break;
      case tpo_array:
	{
	  xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		    ARR_ELT_TYPE_EDGE_ATTR "}\n", tp, get_array_element_type(tp), i);
	  xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		    ARR_ENT_EDGE_ATTR "}\n", tp, get_array_element_entity(tp), i);
	} break;
      case tpo_enumeration:
	{
	} break;
      case tpo_pointer:
	{
	  xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		    PTR_PTS_TO_EDGE_ATTR "}\n", tp,
		    get_pointer_points_to_type(tp), i);
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
      printf(" *** irdump,  %s(l.%i), faulty type.\n", __FUNCTION__, __LINE__);
    } break;
  } /* switch kind_or_entity */
}

/************************************************************************/
/* open and close vcg file                                              */
/************************************************************************/

void vcg_open (ir_graph *irg, char *suffix) {
  char *fname;  /* filename to put the vcg information in */
  const char *cp;
  ident *id;
  int len;
  char label[4];
  entity *ent;

  /** open file for vcg graph */
  ent = get_irg_ent(irg);
  id    = ent->ld_name ? ent->ld_name : ent->name;
  /* Don't use get_entity_ld_ident (ent) as it computes the mangled name! */
  len   = id_to_strlen (id);
  cp    = id_to_str (id);
  if (dump_file_suffix)
    fname = malloc (len + 5 + strlen(suffix) + strlen(dump_file_suffix));
  else
    fname = malloc (len + 5 + strlen(suffix));
  strncpy (fname, cp, len);      /* copy the filename */
  fname[len] = '\0';
  if (dump_file_suffix) strcat (fname, dump_file_suffix);  /* append file suffix */
  strcat (fname, suffix);  /* append file suffix */
  strcat (fname, ".vcg");   /* append the .vcg suffix */
  F = fopen (fname, "w");   /* open file for writing */
  if (!F) {
    panic ("cannot open %s for writing (%m)", fname);  /* not reached */
  }

  if (edge_label) {
    strcpy(label, "yes");
  } else {
    strcpy (label, "no");
  }

  /* print header */
  xfprintf (F,
	    "graph: { title: \"ir graph of %s\"\n"
	    "display_edge_labels: %s\n"
	    "layoutalgorithm: mindepth\n"
	    "manhattan_edges: yes\n"
	    "port_sharing: no\n"
	    "orientation: bottom_to_top\n"
	    "classname 1: \"Data\"\n"
	    "classname 2: \"Block\"\n"
	    "classname 3: \"Entity type\""
	    "classname 4: \"Entity owner\""
	    "classname 5: \"Method Param\""
	    "classname 6: \"Method Res\""
	    "classname 7: \"Super\""
	    "classname 8: \"Union\""
	    "classname 9: \"Points-to\""
	    "classname 10: \"Array Element Type\""
	    "classname 11: \"Overwrites\""
	    "classname 12: \"Member\""
	    , cp, label);

  xfprintf (F, "\n");		/* a separator */
}

void vcg_open_name (const char *name) {
  char *fname;  /* filename to put the vcg information in */
  int len;
  char label[4];

  /** open file for vcg graph */
  len   = strlen(name);
  fname = malloc (len + 5);
  if (dump_file_suffix)
    fname = malloc (len + 5 + strlen(dump_file_suffix));
  else
    fname = malloc (len + 5);
  strcpy (fname, name);    /* copy the filename */
  if (dump_file_suffix) strcat (fname, dump_file_suffix);
  strcat (fname, ".vcg");  /* append the .vcg suffix */
  F = fopen (fname, "w");  /* open file for writing */
  if (!F) {
    panic ("cannot open %s for writing (%m)", fname);  /* not reached */
  }

  if (edge_label) {
    strcpy(label, "yes");
  } else {
    strcpy (label, "no");
  }

  /* print header */
  xfprintf (F,
	    "graph: { title: \"ir graph of %s\"\n"
	    "display_edge_labels: %s\n"
	    "layoutalgorithm: mindepth\n"
	    "manhattan_edges: yes\n"
	    "port_sharing: no\n"
	    "orientation: bottom_to_top\n"
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
	    "classname 12: \"Member\"\n"
	    , name, label);

  xfprintf (F, "\n");		/* a separator */
}

void
vcg_close () {
  xfprintf (F, "}\n");  /* print footer */
  fclose (F);           /* close vcg file */
}

/************************************************************************/
/* routines to dump a graph, blocks as conventional nodes.              */
/************************************************************************/

int node_floats(ir_node *n) {
  return ((get_op_pinned(get_irn_op(n)) == floats) &&
	  (get_irg_pinned(current_ir_graph) == floats));
}

void
dump_whole_node (ir_node *n, void* env) {
  dump_node(n, NULL);
  if (!node_floats(n)) dump_ir_block_edge(n);
  dump_ir_data_edges(n);
}

void
dump_ir_graph (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "");

  /* walk over the graph */
  /* dump_whole_node must be called in post visiting predecessors */
  irg_walk(irg->end, NULL, dump_whole_node, NULL);

  /* dump the out edges in a separate walk */
  if ((dump_out_edge_flag) && (get_irg_outs_state(irg) != no_outs)) {
    irg_out_walk(irg->start, dump_out_edge, NULL, NULL);
  }

  vcg_close();

  current_ir_graph = rem;
}

/***********************************************************************/
/* the following routines dump the nodes as attached to the blocks.    */
/***********************************************************************/

void
dump_ir_blocks_nodes (ir_node *n, void *env) {
  ir_node *block = (ir_node *)env;

  if (is_no_Block(n) && get_nodes_Block(n) == block && !node_floats(n)) {
    dump_node(n, NULL);
    dump_ir_data_edges(n);
  }
  if (get_irn_op(n) == op_Bad)
    Bad_dumped = 1;
}

void
dump_ir_block (ir_node *block, void *env) {
  ir_graph *irg = (ir_graph *)env;

  if (get_irn_opcode(block) == iro_Block) {

    /* This is a block. So dump the vcg information to make a block. */
    xfprintf(F, "graph: { title: \"");
	PRINT_NODEID(block);
	fprintf(F, "\"  label: \"");
#ifdef DEBUG_libfirm
    xfprintf (F, "%ld", get_irn_node_nr(block));
#else
    xfprintf (F, "%I", block->op->name);
#endif
    if (exc_normal != get_Block_exc (block))
      fprintf (F, " (%s)", exc_to_string (get_Block_exc (block)));

    xfprintf(F, "\" status:clustered color:%s \n",
	     get_Block_matured (block) ? "yellow" : "red");
    /* dump the blocks edges */
    dump_ir_data_edges(block);

    /* dump the nodes that go into the block */
    irg_walk(irg->end, dump_ir_blocks_nodes, NULL, block);

    /* Close the vcg information for the block */
    xfprintf(F, "}\n\n");
    dump_const_node_local(block, NULL);
  }
}


void
dump_blockless_nodes (ir_node *n, void *env) {
  if (is_no_Block(n) && get_irn_op(get_nodes_Block(n)) == op_Bad) {
    dump_node(n, NULL);
    dump_ir_data_edges(n);
    dump_ir_block_edge(n);
    if (get_irn_op(n) == op_Bad) Bad_dumped = 1;
    return;
  }
  if (node_floats(n)) {
    dump_node(n, NULL);
    dump_ir_data_edges(n);
    if (get_irn_op(n) == op_Bad) Bad_dumped = 1;
  }
}

void dump_ir_block_graph_2  (ir_graph *irg)
{
  Bad_dumped = 0;
  /* walk over the blocks in the graph */
  irg_block_walk(irg->end, dump_ir_block, NULL, irg);

  /* dump all nodes that are not in a Block */
  irg_walk(irg->end, dump_blockless_nodes, NULL, NULL);

  /* dump the Bad node */
  if (!Bad_dumped)
    dump_node(get_irg_bad(irg), NULL);
}

void
dump_ir_block_graph (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "");

  dump_ir_block_graph_2 (irg);

  if (dump_loop_information_flag) dump_loop_info(irg);

  vcg_close();
  current_ir_graph = rem;
}


/***********************************************************************/
/* the following routines dump a control flow graph                    */
/***********************************************************************/


void
dump_block_to_cfg (ir_node *block, void *env) {
  int i;
  ir_node *pred;

  if (get_irn_opcode(block) == iro_Block) {
    /* This is a block. Dump a node for the block. */
    xfprintf (F, "node: {title:\""); PRINT_NODEID(block);
    xfprintf (F, "\" label: \"%I ", block->op->name); PRINT_NODEID(block);

	if (exc_normal != get_Block_exc (block))
	  xfprintf (F, " (%s)", exc_to_string (get_Block_exc (block)));

    xfprintf (F, "\" ");
    if (dump_dominator_information_flag)
      xfprintf(F, "info1:\"dom depth %d\"", get_Block_dom_depth(block));
    xfprintf (F, "}\n");
    /* Dump the edges */
    for ( i = 0; i < get_Block_n_cfgpreds(block); i++)
      if (get_irn_op(skip_Proj(get_Block_cfgpred(block, i))) != op_Bad) {
	pred = get_nodes_Block(skip_Proj(get_Block_cfgpred(block, i)));
	xfprintf (F, "edge: { sourcename: \"");
	PRINT_NODEID(block);
	fprintf (F, "\" targetname: \"");
	PRINT_NODEID(pred);
	fprintf (F, "\" }\n");
      }

    /* Dump dominator edge */
    if (dump_dominator_information_flag && get_Block_idom(block)) {
      pred = get_Block_idom(block);
      xfprintf (F, "edge: { sourcename: \"");
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
  current_ir_graph = irg;
  vcg_open (irg, "-cfg");

  if (get_irg_dom_state(irg) != dom_consistent)
    dump_dominator_information_flag = 0;

  /* walk over the blocks in the graph */
  irg_block_walk(irg->end, dump_block_to_cfg, NULL, NULL);
  dump_ir_node (irg->bad);

  dump_dominator_information_flag = ddif;
  vcg_close();
  current_ir_graph = rem;
}


/***********************************************************************/
/* the following routine dumps all type information reachable from an  */
/* irg                                                                 */
/***********************************************************************/


void
dump_type_graph (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "-type");

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

/***********************************************************************/
/* the following routine dumps all type information                    */
/***********************************************************************/


void
dump_all_types (void)
{
  vcg_open_name ("All_types");
  type_walk(dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());
  vcg_close();
}

/***********************************************************************/
/* dumps a graph with type information                                 */
/***********************************************************************/


void
dump_ir_graph_w_types (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "-all");

  /* dump common ir graph */
  irg_walk(irg->end, dump_whole_node, NULL, NULL);
  /* dump type info */
  type_walk_irg(irg, dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());
  /* dump edges from graph to type info */
  irg_walk(irg->end, dump_node2type_edges, NULL, NULL);

  vcg_close();
  current_ir_graph = rem;
}

void
dump_ir_block_graph_w_types (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "-all");

  /* dump common blocked ir graph */
  dump_ir_block_graph_2(irg);
  /* dump type info */
  type_walk_irg(irg, dump_type_info, NULL, NULL);
  inc_irg_visited(get_const_code_irg());
  /* dump edges from graph to type info */
  irg_walk(irg->end, dump_node2type_edges, NULL, NULL);

  vcg_close();
  current_ir_graph = rem;
}

/***********************************************************************/
/* dumps all graphs with the graph-dumper passed. Possible dumpers:    */
/*  dump_ir_graph                                                      */
/*  dump_ir_block_graph                                                */
/*  dump_cfg                                                           */
/*  dump_type_graph                                                    */
/*  dump_ir_graph_w_types                                              */
/***********************************************************************/
void dump_all_ir_graphs (void dump_graph(ir_graph*)) {
  int i;
  for (i=0; i < get_irp_n_irgs(); i++) {
    dump_graph(get_irp_irg(i));
  }
}


/* To turn off display of edge labels.  Edge labels offen cause xvcg to
   abort with a segmentation fault. */
void turn_off_edge_labels() {
  edge_label = 0;
}


void dump_consts_local(bool b) {
  dump_const_local = b;
}

void turn_off_constant_entity_values() {
  const_entities = 0;
}

void dump_keepalive_edges(bool b) {
  dump_keepalive = b;
}

void dump_out_edges() {
  dump_out_edge_flag = 1;
}

void dump_dominator_information() {
  dump_dominator_information_flag = 1;
}

void dump_loop_information() {
  dump_loop_information_flag = 1;
}

void dont_dump_loop_information() {
  dump_loop_information_flag = 0;
}

static void clear_link(ir_node * node, void * env) {
  set_irn_link(node, NULL);
}

static void collect_blocks_floats_cg(ir_node * node, pmap * map) {
  if (is_Block(node)
      || node_floats(node)
      || get_irn_op(node) == op_Bad
      || get_irn_op(node) == op_Unknown) {
    pmap_entry * entry = pmap_find(map, current_ir_graph);
    if (entry) {
      ARR_APP1(ir_node *, (ir_node **) entry->value, node);
    } else {
      ir_node ** arr = NEW_ARR_F(ir_node *, 1);
      arr[0] = node;
      pmap_insert(map, current_ir_graph, arr);
    }
  } else {
    ir_node * block = get_nodes_Block(node);
    set_irn_link(node, get_irn_link(block));
    set_irn_link(block, node);
  }
}


static void dump_cg_ir_block(ir_node * block, void * env) {
  ir_node *node;
  pmap *irgmap = (pmap *)env;
  assert(is_Block(block));
  xfprintf(F, "graph: { title: \"");
  PRINT_NODEID(block);
  fprintf(F, "\"  label: \"");
#ifdef DEBUG_libfirm
  xfprintf (F, "%ld", get_irn_node_nr(block));
#else
  xfprintf (F, "%I", block->op->name);
#endif
  if (exc_normal != get_Block_exc(block)) {
    fprintf (F, " (%s)", exc_to_string (get_Block_exc(block)));
  }

  xfprintf(F, "\" status:clustered color:%s \n",
	   get_Block_matured(block) ? "yellow" : "red");

  /* dump the blocks edges */
  dump_ir_data_edges(block);

  /* dump the nodes that go into the block */
  for (node = get_irn_link(block); node; node = get_irn_link(node)) {
    dump_node(node, irgmap);
    dump_ir_data_edges(node);
  }

  /* Close the vcg information for the block */
  xfprintf(F, "}\n\n");
}

void d_cg_block_graph(ir_graph *irg, ir_node **arr, pmap *irgmap) {
  int i;

  xfprintf(F, "graph: { title: \"%p\" label: \"%I\" status:clustered color:white \n",
	   irg, get_entity_ident(get_irg_ent(irg)));

  for (i = ARR_LEN(arr) - 1; i >= 0; --i) {
    ir_node * node = arr[i];
    if (is_Block(node)) {
      /* Dumps the block and all the nodes in the block , which are to
	 be found in Block->link. */
      dump_cg_ir_block(node, irgmap);
    } else {
      /* Nodes that are not in a Block. */
      dump_node(node, NULL);
      dump_ir_data_edges(node);
    }
  }
  /* Close the vcg information for the irg */
  xfprintf(F, "}\n\n");
}

/* dump interprocedural graph with surrounding methods */
void dump_cg_block_graph(ir_graph * irg) {
  pmap * map = pmap_create();
  pmap * map2 = pmap_create();
  pmap_entry * entry;

  vcg_open(irg, "");

  irg_walk_graph(irg, clear_link, (irg_walk_func *) collect_blocks_floats_cg, map);
  for (entry = pmap_first(map); entry; entry = pmap_next(map))
    pmap_insert(map2, entry->key, entry->value);
  for (entry = pmap_first(map); entry; entry = pmap_next(map)) {
    d_cg_block_graph(entry->key, entry->value, map2);
    DEL_ARR_F(entry->value);
  }

  pmap_destroy(map);
  pmap_destroy(map2);

  if (dump_loop_information_flag) dump_loop_info(irg);
  vcg_close();
}

static void collect_node(ir_node * node, void *env) {
  if (is_Block(node)
      || node_floats(node)
      || get_irn_op(node) == op_Bad
      || get_irn_op(node) == op_Unknown) {
    ir_node ** arr = (ir_node **) get_irg_link(current_ir_graph);
    ARR_APP1(ir_node *, arr, node);
    set_irg_link(current_ir_graph, arr);    /* arr is an l-value, APP_ARR might change it! */
  } else {
    ir_node * block = get_nodes_Block(node);
    set_irn_link(node, get_irn_link(block));
    set_irn_link(block, node);
  }
}

/* Links all nodes that have the block field set in the link field of
   the block.  Adds all blocks and nodes not associated with a block
   in a array in irg->link. */
static void collect_nodes() {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++)
    set_irg_link(get_irp_irg(i), NEW_ARR_F(ir_node *, 0));
  cg_walk(clear_link, collect_node, NULL);
}

static void dump_graphs() {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    d_cg_block_graph(current_ir_graph, get_irg_link(current_ir_graph), NULL);
  }
}

/* Dump all irgs in interprocedural view to a single file. */
void dump_all_cg_block_graph() {
  int i;
  int rem_view = interprocedural_view;
  interprocedural_view = 1;
  vcg_open_name ("All_graphs");

  collect_nodes();
  dump_graphs();

  if (dump_loop_information_flag)
    for (i = 0; i < get_irp_n_irgs(); i++)
      dump_loop_info(get_irp_irg(i));

  vcg_close();
  interprocedural_view = rem_view;
}

/* dump interprocedural block graph with surrounding methods */
void dump_cg_graph(ir_graph * irg) {
  pmap * map = pmap_create();
  pmap * map2 = pmap_create(); /* We can not iterate in the same map twice! */
  pmap_entry * entry;
  vcg_open(irg, "");

  irg_walk_graph(irg, clear_link, (irg_walk_func *) collect_blocks_floats_cg, map);
  for (entry = pmap_first(map); entry; entry = pmap_next(map))
    pmap_insert(map2, entry->key, entry->value);
  for (entry = pmap_first(map); entry; entry = pmap_next(map)) {
    ir_node ** arr = entry->value;
    int i;
    ident * irg_ident = get_entity_ident(get_irg_ent(entry->key));

    xfprintf(F, "graph: { title: \"%I\" label: \"%I\" status:clustered color:white \n",
	     irg_ident, irg_ident);

    for (i = ARR_LEN(arr) - 1; i >= 0; --i) {
      ir_node * node = arr[i];
      dump_node(node, map2);
      dump_ir_data_edges(node);
      if (is_Block(node)) {
	for (node = get_irn_link(node); node; node = get_irn_link(node)) {
	  dump_node(node, map2);
	  dump_ir_block_edge(node);
	  dump_ir_data_edges(node);
	}
      }
    }

    DEL_ARR_F(arr);

    /* Close the vcg information for the irg */
    xfprintf(F, "}\n\n");
  }

  pmap_destroy(map);
  pmap_destroy(map2);

  vcg_close();
}
