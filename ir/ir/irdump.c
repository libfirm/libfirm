/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*
* irdump.h: dumping of an intermediate representation graph
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <string.h>
# include <stdlib.h>

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


/* Attributes of nodes */
#define PRINT_DEFAULT_NODE_ATTR
#define DEFAULT_NODE_ATTR " "
#define DEFAULT_TYPE_ATTRIBUTE " "

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
#define PRINT_NODEID(X) fprintf(F, "\"n%ld\"", get_irn_node_nr(X))
#define PRINT_TYPEID(X) fprintf(F, "\"t%ld\"", get_type_nr(X))
#define PRINT_ENTID(X)  fprintf(F, "\"e%ld\"", get_entity_nr(X))
#define PRINT_IRGID(X)  fprintf(F, "g%ld", get_irg_graph_nr(X))
#define PRINT_CONSTID(X,Y) fprintf(F, "\"n%ldn%ld\"", get_irn_node_nr(X),get_irn_node_nr(Y))

#else
#define PRINT_NODEID(X) fprintf(F, "\"n%p\"", (void*) X)
#define PRINT_TYPEID(X) fprintf(F, "\"t%p\"", (void *) X)
#define PRINT_ENTID(X)  fprintf(F, "\"e%p\"", (void*) X)
#define PRINT_IRGID(X)  fprintf(F, "g%p",(void*) X)
#define PRINT_CONSTID(X,Y) fprintf(F, "\"%p%p\"", (void*) X, (void*) Y)
#endif

#define PRINT_TYPE_TYPE_EDGE(S,T,...){fprintf (F, "edge: { sourcename: "); PRINT_TYPEID(S); fprintf (F, " targetname: "); PRINT_TYPEID(T); fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_TYPE_ENT_EDGE(S,T,...) {fprintf (F, "edge: { sourcename: "); PRINT_TYPEID(S); fprintf (F, " targetname: "); PRINT_ENTID(T);  fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_ENT_ENT_EDGE(S,T,...)  {fprintf (F, "edge: { sourcename: "); PRINT_ENTID(S);  fprintf (F, " targetname: "); PRINT_ENTID(T);  fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_ENT_TYPE_EDGE(S,T,...) {fprintf (F, "edge: { sourcename: "); PRINT_ENTID(S);  fprintf (F, " targetname: "); PRINT_TYPEID(T); fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_NODE_TYPE_EDGE(S,T,...){fprintf (F, "edge: { sourcename: "); PRINT_NODEID(S); fprintf (F, " targetname: "); PRINT_TYPEID(T); fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_NODE_ENT_EDGE(S,T,...) {fprintf (F, "edge: { sourcename: "); PRINT_NODEID(S); fprintf (F, " targetname: "); PRINT_ENTID(T);  fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }
#define PRINT_ENT_NODE_EDGE(S,T,...) {fprintf (F, "edge: { sourcename: "); PRINT_ENTID(S);  fprintf (F, " targetname: "); PRINT_NODEID(T); fprintf (F, ##__VA_ARGS__); fprintf(F,"}\n"); }


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

INLINE bool get_opt_dump_const_local(void) {
  if (!dump_out_edge_flag && !dump_loop_information_flag)
    return dump_const_local;
  else
    return false;
}

/* A global variable to record output of the Bad node. */
static int Bad_dumped;

static void dump_ir_blocks_nodes (ir_node *n, void *env);
static void dump_whole_node(ir_node *n, void* env);

/*******************************************************************/
/* routines to dump information about a single node                */
/*******************************************************************/



static INLINE void
dump_node_opcode (ir_node *n)
{
  char buf[1024];
  int res;

  /* Const */
  if (get_irn_opcode(n) == iro_Const) {    res = tarval_snprintf(buf, sizeof(buf), get_Const_tarval(n));
    assert(res < sizeof(buf) && "buffer to small for tarval_snprintf");
    fprintf(F, buf);

  /* SymConst */
  } else if (get_irn_opcode(n) == iro_SymConst) {
    if (get_SymConst_kind(n) == linkage_ptr_info) {
      /* don't use get_SymConst_ptr_info as it mangles the name. */
      fprintf (F, "SymC %s", id_to_str(get_SymConst_ptrinfo(n)));
    } else {
      assert(get_kind(get_SymConst_type(n)) == k_type);
      assert(get_type_ident(get_SymConst_type(n)));
      fprintf (F, "SymC %s ", id_to_str(get_type_ident(get_SymConst_type(n))));
      if (get_SymConst_kind(n) == type_tag)
        fprintf (F, "tag");
      else
        fprintf (F, "size");
    }

  /* Filter */
  } else if (get_irn_opcode(n) == iro_Filter && !interprocedural_view) {
    fprintf(F, "Proj'");

  /* all others */
  } else {
    fprintf (F, "%s", id_to_str(get_irn_opident(n)));
  }
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
    fprintf (F, "%s", id_to_str(get_mode_ident(get_irn_mode(n))));
    break;
  default:
    ;
  }
}

static INLINE void
dump_node_nodeattr (ir_node *n)
{
  switch (get_irn_opcode(n)) {
  case iro_Start:
    if (false && interprocedural_view) {
      fprintf (F, "%s", id_to_str(get_entity_ident(get_irg_ent(current_ir_graph))));
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
    assert(get_kind(get_Sel_entity(n)) == k_entity);
    fprintf (F, "%s", id_to_str(get_entity_ident(get_Sel_entity(n))));
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

static bool pred_in_wrong_graph(ir_node *n, int pos, pmap *irgmap) {
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


static void dump_const_node_local(ir_node *n, pmap *irgmap) {
  int i;
  if (!get_opt_dump_const_local()) return;
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
      fprintf (F, "node: {title: "); PRINT_CONSTID(n,con);
      fprintf(F, " label: \"");
      dump_node_opcode(con);
      dump_node_mode (con);
      fprintf (F, " ");
      dump_node_nodeattr(con);
#ifdef DEBUG_libfirm
      fprintf (F, " %ld", get_irn_node_nr(con));
#endif
      fprintf (F, "\" ");
      dump_node_vcgattr(con);
      fprintf (F, "}\n");
    }
  }
}

static void
dump_node (ir_node *n, pmap * map) {
  if (get_opt_dump_const_local() && is_constlike_node(n)) return;

  /* dump this node */
  fprintf (F, "node: {title: "); PRINT_NODEID(n); fprintf(F, " label: \"");

  dump_node_opcode(n);
  dump_node_mode (n);
  fprintf (F, " ");
  dump_node_nodeattr(n);
#ifdef DEBUG_libfirm
  fprintf (F, " %ld", get_irn_node_nr(n));
#endif
  fprintf (F, "\" ");
  dump_node_vcgattr(n);
  fprintf (F, "}\n");
  dump_const_node_local(n, map);
}

/* dump the edge to the block this node belongs to */
static void
dump_ir_block_edge(ir_node *n)  {
  if (get_opt_dump_const_local() && is_constlike_node(n)) return;
  if (is_no_Block(n)) {
    fprintf (F, "edge: { sourcename: ");
    PRINT_NODEID(n);
    fprintf (F, " targetname: ");
    PRINT_NODEID(get_nodes_Block(n));
    fprintf (F, " "	BLOCK_EDGE_ATTR "}\n");
  }
}

static void print_edge_vcgattr(ir_node *from, int to) {
  assert(from);

  if (is_backedge(from, to)) fprintf (F, BACK_EDGE_ATTR);

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
    if (is_backedge(n, i))
      fprintf (F, "backedge: {sourcename: ");
    else
      fprintf (F, "edge: {sourcename: ");
    PRINT_NODEID(n);
    fprintf (F, " targetname: ");
    if ((get_opt_dump_const_local()) && is_constlike_node(pred))
    {
      PRINT_CONSTID(n,pred);
    }
    else
    {
      PRINT_NODEID(pred);
    }
    fprintf (F, " label: \"%d\" ", i);
    print_edge_vcgattr(n, i);
    fprintf (F, "}\n");
  }
}

/* dump out edges */
static void
dump_out_edge (ir_node *n, void* env) {
  int i;
  for (i = 0; i < get_irn_n_outs(n); i++) {
    assert(get_irn_out(n, i));
    fprintf (F, "edge: {sourcename: ");
    PRINT_NODEID(n);
    fprintf (F, " targetname: ");
    PRINT_NODEID(get_irn_out(n, i));
    fprintf (F, " color: red linestyle: dashed");
    fprintf (F, "}\n");
  }
}

static INLINE void
dump_loop_node_edge (ir_loop *loop, int i) {
  assert(loop);
  fprintf (F, "edge: {sourcename: \"%p\" targetname: ", (void*) loop);
  PRINT_NODEID(get_loop_node(loop, i));
  fprintf (F, " color: green");
  fprintf (F, "}\n");
}

static
void dump_loops (ir_loop *loop) {
  int i;
  /* dump this loop node */
  fprintf (F, "node: {title: \"%p\" label: \"loop %d, %d sons, %d nodes\" }\n",
	    (void*)loop, get_loop_depth(loop), get_loop_n_sons(loop), get_loop_n_nodes(loop));
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
	    PRINT_NODE_TYPE_EDGE(n,get_SymConst_type(n),NODE2TYPE_EDGE_ATTR);
    }
    break;
  case iro_Sel: {
	    PRINT_NODE_ENT_EDGE(n,get_Sel_entity(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Call: {
	    PRINT_NODE_TYPE_EDGE(n,get_Call_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Alloc: {
	    PRINT_NODE_TYPE_EDGE(n,get_Alloc_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  case iro_Free: {
	    PRINT_NODE_TYPE_EDGE(n,get_Free_type(n),NODE2TYPE_EDGE_ATTR);
    } break;
  default:
    break;
  }
}


static void dump_const_expression(ir_node *value) {
  ir_graph *rem = current_ir_graph;
  int rem_dump_const_local = dump_const_local;
  dump_const_local = 0;
  current_ir_graph = get_const_code_irg();
  irg_walk(value, dump_ir_blocks_nodes, NULL, get_nodes_Block(value));
  /* Decrease visited flag so that we walk with the same flag for the next
     expresssion.  This guarantees that we don't dump the same node twice,
     as for const expressions cse is performed to save memory. */
  set_irg_visited(current_ir_graph, get_irg_visited(current_ir_graph) -1);
  current_ir_graph = rem;
  dump_const_local = rem_dump_const_local;
}


static void print_type_info(type *tp) {
  if (get_type_state(tp) == layout_undefined) {
    fprintf(F, "state: layout_undefined\n");
  } else {
    fprintf(F, "state: layout_fixed,\n");
  }
  if (get_type_mode(tp))
    fprintf(F, "mode: %s,\n", id_to_str(get_mode_ident(get_type_mode(tp))));
  fprintf(F, "size: %dB,\n", get_type_size(tp));
}


static void print_typespecific_info(type *tp) {
  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    {
      if(existent == get_class_peculiarity(tp))
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

static void print_type_node(type *tp) {
  fprintf (F, "node: {title: ");
  PRINT_TYPEID(tp);
  fprintf (F, " label: \"%s %s\"", id_to_str(get_type_tpop_nameid(tp)), id_to_str(get_type_ident(tp)));
  fprintf (F, " info1: \"");
  print_type_info(tp);
  fprintf (F, "\"");
  print_typespecific_info(tp);
  fprintf (F, "}\n");
}

void dump_entity_node(entity *ent) {
  fprintf (F, "node: {title: ");
  PRINT_ENTID(ent);
  fprintf (F, DEFAULT_TYPE_ATTRIBUTE);
  fprintf (F, "label: ");
  fprintf (F, "\"ent %s\" " ENTITY_NODE_ATTR , id_to_str(get_entity_ident(ent)));
  fprintf (F, "\n info1:\"\nallocation:  ");
  switch (get_entity_allocation(ent)) {
    case dynamic_allocated:   fprintf (F, "dynamic allocated");   break;
    case automatic_allocated: fprintf (F, "automatic allocated"); break;
    case static_allocated:    fprintf (F, "static allocated");    break;
    case parameter_allocated: fprintf (F, "parameter allocated"); break;
  }
  fprintf (F, "\nvisibility:  ");
  switch (get_entity_visibility(ent)) {
    case local:              fprintf (F, "local");             break;
    case external_visible:   fprintf (F, "external_visible");  break;
    case external_allocated: fprintf (F, "external_allocate"); break;
  }
  fprintf (F, "\nvariability: ");
  switch (get_entity_variability(ent)) {
    case uninitialized: fprintf (F, "uninitialized");break;
    case initialized:   fprintf (F, "initialized");  break;
    case part_constant: fprintf (F, "part_constant");break;
    case constant:      fprintf (F, "constant");     break;
  }
  fprintf (F, "\nvolatility:  ");
  switch (get_entity_volatility(ent)) {
    case non_volatile: fprintf (F, "non_volatile"); break;
    case is_volatile:  fprintf (F, "is_volatile");  break;
  }
  fprintf (F, "\npeculiarity: ");
  switch (get_entity_peculiarity(ent)) {
    case description: fprintf (F, "description"); break;
    case inherited:   fprintf (F, "inherited");   break;
    case existent:    fprintf (F, "existent");    break;
  }
  fprintf(F, "\nname:    %s\nld_name: %s",
	  id_to_str(get_entity_ident(ent)),
	  id_to_str(get_entity_ld_ident(ent)));
  fprintf(F, "\noffset:  %d", get_entity_offset(ent));
  if (is_method_type(get_entity_type(ent))) {
    if (get_entity_irg(ent))   /* can be null */
      { fprintf (F, "\nirg = "); PRINT_IRGID(get_entity_irg(ent)); }
    else
      { fprintf (F, "\nirg = NULL"); }
  }
  fprintf(F, "\"\n}\n");
}

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
      PRINT_ENT_TYPE_EDGE(ent, get_entity_type(ent), ENT_TYPE_EDGE_ATTR);
      if(is_class_type(get_entity_owner(ent))) {
	for(i = 0; i < get_entity_n_overwrites(ent); i++){
	  PRINT_ENT_ENT_EDGE(ent, get_entity_overwrites(ent, i), ENT_OVERWRITES_EDGE_ATTR);
	}
      }
      /* attached subgraphs */
      if (const_entities && (get_entity_variability(ent) != uninitialized)) {
	if (is_atomic_entity(ent)) {
	  value = get_atomic_ent_value(ent);
	  if (value) {
            PRINT_ENT_NODE_EDGE(ent, value, ENT_VALUE_EDGE_ATTR, i);
	    /*
	    fprintf (F, "edge: { sourcename: \"%p\" targetname: ", GET_ENTID(ent));
	    PRINT_NODEID(value);
	    fprintf(F, " " ENT_VALUE_EDGE_ATTR "\"}\n");
	    */
	    dump_const_expression(value);
	  }
	}
	if (is_compound_entity(ent)) {
	  for (i = 0; i < get_compound_ent_n_values(ent); i++) {
	    value = get_compound_ent_value(ent, i);
	    if (value) {
              PRINT_ENT_NODE_EDGE(ent,value,ENT_VALUE_EDGE_ATTR,i);
	      dump_const_expression(value);
	      PRINT_ENT_ENT_EDGE(ent, get_compound_ent_value_member(ent, i), ENT_CORR_EDGE_ATTR, i);
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
	    PRINT_TYPE_TYPE_EDGE(tp,get_class_supertype(tp, i),TYPE_SUPER_EDGE_ATTR);
	  }

	  for (i=0; i < get_class_n_members(tp); i++) {
	    PRINT_TYPE_ENT_EDGE(tp,get_class_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
	  }
	} break;
      case tpo_struct:
	{
	  for (i=0; i < get_struct_n_members(tp); i++) {
	    PRINT_TYPE_ENT_EDGE(tp,get_struct_member(tp, i),TYPE_MEMBER_EDGE_ATTR);
	  }
	} break;
      case tpo_method:
	{
	  for (i = 0; i < get_method_n_params(tp); i++)
	  {
		  PRINT_TYPE_TYPE_EDGE(tp,get_method_param_type(tp, i),METH_PAR_EDGE_ATTR,i);
	  }
	  for (i = 0; i < get_method_n_ress(tp); i++)
	  {
		  PRINT_TYPE_TYPE_EDGE(tp,get_method_res_type(tp, i),METH_RES_EDGE_ATTR,i);
	  }
	} break;
      case tpo_union:
	{
	  for (i = 0; i < get_union_n_members(tp); i++)
	  {
		  PRINT_TYPE_ENT_EDGE(tp,get_union_member(tp, i),UNION_EDGE_ATTR);
	  }
	} break;
      case tpo_array:
	{
		  PRINT_TYPE_TYPE_EDGE(tp,get_array_element_type(tp),ARR_ELT_TYPE_EDGE_ATTR);
		  PRINT_TYPE_ENT_EDGE(tp,get_array_element_entity(tp),ARR_ENT_EDGE_ATTR);
	} break;
      case tpo_enumeration:
	{
	} break;
      case tpo_pointer:
	{
		  PRINT_TYPE_TYPE_EDGE(tp,get_pointer_points_to_type(tp), PTR_PTS_TO_EDGE_ATTR);
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

/* dumps a class type node and a superclass edge.
   If env != null dumps entities of classes and overwrites edges. */
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
      PRINT_TYPE_ENT_EDGE(get_entity_owner(ent),ent,TYPE_MEMBER_EDGE_ATTR);
      for(i = 0; i < get_entity_n_overwrites(ent); i++)
      {
      PRINT_ENT_ENT_EDGE(get_entity_overwrites(ent, i),ent, ENT_OVERWRITES_EDGE_ATTR);
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
		  PRINT_TYPE_TYPE_EDGE(tp,get_class_supertype(tp, i),TYPE_SUPER_EDGE_ATTR);
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

/************************************************************************/
/* open and close vcg file                                              */
/************************************************************************/

static void vcg_open (ir_graph *irg, char *suffix) {
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
  fprintf (F,
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

  fprintf (F, "\n");		/* a separator */
}

static void vcg_open_name (const char *name) {
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
  fprintf (F,
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

  fprintf (F, "\n");		/* a separator */
}

static void
vcg_close (void) {
  fprintf (F, "}\n");  /* print footer */
  fclose (F);           /* close vcg file */
}

/************************************************************************/
/* routines to dump a graph, blocks as conventional nodes.              */
/************************************************************************/

static int node_floats(ir_node *n) {
  return ((get_op_pinned(get_irn_op(n)) == floats) &&
	  (get_irg_pinned(current_ir_graph) == floats));
}

static void
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
  irg_walk(get_irg_end(irg), NULL, dump_whole_node, NULL);

  /* dump the out edges in a separate walk */
  if ((dump_out_edge_flag) && (get_irg_outs_state(irg) != no_outs)) {
    irg_out_walk(get_irg_start(irg), dump_out_edge, NULL, NULL);
  }

  vcg_close();

  current_ir_graph = rem;
}

/***********************************************************************/
/* the following routines dump the nodes as attached to the blocks.    */
/***********************************************************************/

static void
dump_ir_blocks_nodes (ir_node *n, void *env) {
  ir_node *block = (ir_node *)env;

  if (is_no_Block(n) && get_nodes_Block(n) == block && !node_floats(n)) {
    dump_node(n, NULL);
    dump_ir_data_edges(n);
  }
  if (get_irn_op(n) == op_Bad)
    Bad_dumped = 1;
}

static void
dump_ir_block (ir_node *block, void *env) {
  ir_graph *irg = (ir_graph *)env;

  if (get_irn_opcode(block) == iro_Block) {

    /* This is a block. So dump the vcg information to make a block. */
    fprintf(F, "graph: { title: ");
	PRINT_NODEID(block);
	fprintf(F, "  label: \"");
#ifdef DEBUG_libfirm
    fprintf (F, "%ld", get_irn_node_nr(block));
#else
    fprintf (F, "%s", get_op_name(get_irn_op(block)));
#endif
    if (exc_normal != get_Block_exc (block))
      fprintf (F, " (%s)", exc_to_string (get_Block_exc (block)));

    fprintf(F, "\" status:clustered color:%s \n",
	     get_Block_matured (block) ? "yellow" : "red");
    /* dump the blocks edges */
    dump_ir_data_edges(block);

    /* dump the nodes that go into the block */
    irg_walk(get_irg_end(irg), dump_ir_blocks_nodes, NULL, block);

    /* Close the vcg information for the block */
    fprintf(F, "}\n\n");
    dump_const_node_local(block, NULL);
  }
}


static void
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

static void dump_ir_block_graph_2  (ir_graph *irg)
{
  Bad_dumped = 0;
  /* walk over the blocks in the graph */
  irg_block_walk(get_irg_end(irg), dump_ir_block, NULL, irg);

  /* dump all nodes that are not in a Block */
  irg_walk(get_irg_end(irg), dump_blockless_nodes, NULL, NULL);

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


static void
dump_block_to_cfg (ir_node *block, void *env) {
  int i;
  ir_node *pred;

  if (get_irn_opcode(block) == iro_Block) {
    /* This is a block. Dump a node for the block. */
    fprintf (F, "node: {title:"); PRINT_NODEID(block);
    fprintf (F, " label: \"%s ", get_op_name(get_irn_op(block)));
#ifdef DEBUG_libfirm
    fprintf (F, "%ld", get_irn_node_nr(block));
#else
    fprintf (F, "%p", (void*) block);
#endif

    if (exc_normal != get_Block_exc (block))
      fprintf (F, " (%s)", exc_to_string (get_Block_exc (block)));

    fprintf (F, "\" ");
    if (dump_dominator_information_flag)
      fprintf(F, "info1:dom depth %d", get_Block_dom_depth(block));
    fprintf (F, "}\n");
    /* Dump the edges */
    for ( i = 0; i < get_Block_n_cfgpreds(block); i++)
      if (get_irn_op(skip_Proj(get_Block_cfgpred(block, i))) != op_Bad) {
	pred = get_nodes_Block(skip_Proj(get_Block_cfgpred(block, i)));
	fprintf (F, "edge: { sourcename: ");
	PRINT_NODEID(block);
	fprintf (F, " targetname: ");
	PRINT_NODEID(pred);
	fprintf (F, " }\n");
      }

    /* Dump dominator edge */
    if (dump_dominator_information_flag && get_Block_idom(block)) {
      pred = get_Block_idom(block);
      fprintf (F, "edge: { sourcename: ");
      PRINT_NODEID(block);
      fprintf (F, " targetname: ");
      PRINT_NODEID(pred);
      fprintf (F, " " DOMINATOR_EDGE_ATTR "}\n");
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
  irg_block_walk(get_irg_end(irg), dump_block_to_cfg, NULL, NULL);
  dump_node (get_irg_bad(irg), NULL);

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

void
dump_class_hierarchy (bool entities)
{
  vcg_open_name ("class_hierarchy");
  if (entities)
    type_walk(dump_class_hierarchy_node, NULL, (void *)1);
  else
    type_walk(dump_class_hierarchy_node, NULL, NULL);
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
  irg_walk(get_irg_end(irg), dump_whole_node, NULL, NULL);
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
  irg_walk(get_irg_end(irg), dump_node2type_edges, NULL, NULL);

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
void dump_all_ir_graphs (dump_graph_func *dump_graph) {
  int i;
  for (i=0; i < get_irp_n_irgs(); i++) {
    dump_graph(get_irp_irg(i));
  }
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

static void clear_link(ir_node * node, void * env) {
  set_irn_link(node, NULL);
}

static void collect_blocks_floats_cg(ir_node * node, pmap * map) {
  if (is_Block(node)
      || node_floats(node)
      || get_irn_op(node) == op_Bad
      || get_irn_op(node) == op_Unknown) {
    pmap_entry * entry = pmap_find(map, current_ir_graph);
    if (entry)
    {
      ir_node ** arr;
      arr = entry->value;
      ARR_APP1(ir_node *, arr , node);
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
  fprintf(F, "graph: { title: ");
  PRINT_NODEID(block);
  fprintf(F, "  label: \"");
  fprintf (F, "%s ", get_op_name(get_irn_op(block)));
#ifdef DEBUG_libfirm
  fprintf (F, "%ld", get_irn_node_nr(block));
#else
  fprintf (F, "%p", (void*) block);
#endif
  if (exc_normal != get_Block_exc(block)) {
    fprintf (F, " (%s)", exc_to_string (get_Block_exc(block)));
  }

  fprintf(F, "\" status:clustered color:%s \n",
	   get_Block_matured(block) ? "yellow" : "red");

  /* dump the blocks edges */
  dump_ir_data_edges(block);

  /* dump the nodes that go into the block */
  for (node = get_irn_link(block); node; node = get_irn_link(node)) {
    dump_node(node, irgmap);
    dump_ir_data_edges(node);
  }

  /* Close the vcg information for the block */
  fprintf(F, "}\n\n");
}

static void d_cg_block_graph(ir_graph *irg, ir_node **arr, pmap *irgmap) {
  int i;

  fprintf(F, "graph: { title: %p label: %s status:clustered color:white \n",
	   (void*) irg, id_to_str(get_entity_ident(get_irg_ent(irg))));

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
  fprintf(F, "}\n\n");
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
static void collect_nodes(void) {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++)
    set_irg_link(get_irp_irg(i), NEW_ARR_F(ir_node *, 0));
  cg_walk(clear_link, collect_node, NULL);
}

static void dump_graphs(void) {
  int i;
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    d_cg_block_graph(current_ir_graph, get_irg_link(current_ir_graph), NULL);
  }
}

/* Dump all irgs in interprocedural view to a single file. */
void dump_all_cg_block_graph(void) {
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

    fprintf(F, "graph: { title: %s label: %s status:clustered color:white \n",
	     id_to_str(irg_ident), id_to_str(irg_ident));

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
    fprintf(F, "}\n\n");
  }

  pmap_destroy(map);
  pmap_destroy(map2);

  vcg_close();
}
