/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irdump.h: dumping of an intermediate representation graph
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irprog.h"
# include "irdump.h"
# include "panic.h"
# include <string.h>
# include "entity.h"
# include <stdlib.h>
# include "array.h"
# include "irop_t.h"
# include "tv.h"
# include "type_or_entity.h"
# include "irgwalk.h"
# include "typewalk.h"

/* Attributes of nodes */
#define DEFAULT_NODE_ATTR ""
#define DEFAULT_TYPE_ATTRIBUTE ""

/* Attributes of edges between Firm nodes */
#define BLOCK_EDGE_ATTR "class: 2 priority: 2 linestyle: dotted"
#define CF_EDGE_ATTR    "color: red"
#define MEM_EDGE_ATTR   "color: blue"

/* Attributes of edges between Firm nodes and type/entity nodes */
#define NODE2TYPE_EDGE_ATTR ""

/* Attributes of edges in type/entity graphs. */
#define TYPE_METH_NODE_ATTR  "color: lightyellow"
#define TYPE_CLASS_NODE_ATTR "color: green"
#define ENTITY_NODE_ATTR     "color: yellow"
#define ENT_TYPE_EDGE_ATTR   "label: \"type\" color: red"
#define ENT_OWN_EDGE_ATTR    "label: \"owner\" color: black"
#define METH_PAR_EDGE_ATTR   "label: \"param %d\" color: green"
#define METH_RES_EDGE_ATTR   "label: \"res %d\" color: green"
#define TYPE_SUPER_EDGE_ATTR "label: \"supertype\" color: blue"
#define PTR_PTS_TO_EDGE_ATTR "label: \"points to\" color:green"
#define ARR_ELT_TYPE_EDGE_ATTR "label: \"arr elt\" color:green"

#define PRINT_NODEID(X) fprintf(F, "%p", X)

/* file to dump to */
static FILE *F;

int edge_label = 1;

/*******************************************************************/
/* routines to dump information about a single node                */
/*******************************************************************/



inline void
dump_node_opcode (ir_node *n)
{
  /* Const */
  if (n->op->code == iro_Const) {
    xfprintf (F, "%v", n->attr.con);
  /* SymConst */
  } else if (n->op->code == iro_SymConst) {
    if (get_SymConst_kind(n) == linkage_ptr_info) {
      xfprintf (F, "%I", get_SymConst_ptrinfo(n));
    } else {
      assert(get_kind(get_SymConst_type(n)) == k_type_class);
      assert(get_class_ident((type_class *)get_SymConst_type(n)));
      xfprintf (F, "%s ", id_to_str(get_class_ident((type_class *)get_SymConst_type(n))));
      if (get_SymConst_kind == type_tag)
	xfprintf (F, "tag");
      else
	xfprintf (F, "size");
    }
  /* all others */
  } else {
    xfprintf (F, "%I", get_irn_opident(n));
  }
}

inline void
dump_node_mode (ir_node *n)
{
  switch (n->op->code) {
  case iro_Phi:
  case iro_Const:
  case iro_Id:
  case iro_Proj:
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

inline void
dump_node_nodeattr (ir_node *n)
{
  switch (n->op->code) {
  case iro_Proj:
    if (n->in[1]->op->code == iro_Cmp) {
      xfprintf (F, "%s", get_pnc_string(n->attr.proj));
    } else {
      xfprintf (F, "%ld", n->attr.proj);
    }
    break;
  case iro_Sel: {
    assert(get_kind(get_Sel_entity(n)) == k_entity);
    xfprintf (F, "%s", id_to_str(get_entity_ident(get_Sel_entity(n))));
    } break;
  default:
  } /* end switch */
}

inline void
dump_node_vcgattr (ir_node *n)
{
  switch (n->op->code) {
  case iro_Start:
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
  case iro_Tuple:
    xfprintf (F, "color: yellow");
    break;
  default:
    xfprintf (F, DEFAULT_NODE_ATTR);
  }
}

void
dump_node (ir_node *n) {

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
  case iro_Cond:
    xfprintf (F, "\"%I\"", get_irn_opident(n));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_Call:
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
    xfprintf (F, "%s", id_to_str(get_entity_ident(get_Sel_entity(n))));
    xfprintf (F, DEFAULT_NODE_ATTR);
    break;
  case iro_SymConst:
    assert(get_kind(get_SymConst_type(n)) == k_type_class);
    assert(get_class_ident((type_class *)get_SymConst_type(n)));
    xfprintf (F, "\"%s ",
	      id_to_str(get_class_ident((type_class *)get_SymConst_type(n))));
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
  default:
    xfprintf (F, "\"%I%I\" ", get_irn_opident(n), get_irn_modeident(n));
  }
  xfprintf (F, "}\n");		/* footer */
}


/* dump the edge to the block this node belongs to */
void
dump_ir_block_edge(ir_node *n)  {
  if (is_no_Block(n))
    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		BLOCK_EDGE_ATTR "}\n", n, get_nodes_Block(n));
}

void print_edge_vcgattr(ir_node *from, int to) {
  assert(from);

  switch (get_irn_opcode(from)) {
  case iro_Block:
    xfprintf (F, CF_EDGE_ATTR);
    break;
  case iro_Start:   break;
  case iro_End:     break;
  case iro_Jmp:     break;
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
  case iro_Id:     break;
  default:
  }
}

/* dump edges to our inputs */
void
dump_ir_data_edges(ir_node *n)  {
  int i;

  for (i = 0; i < get_irn_arity(n); i++) {
    assert(get_irn_n(n, i));
    xfprintf (F, "edge: {sourcename: \"%p\" targetname: \"%p\"",
	      n, get_irn_n(n, i));
    fprintf (F, " label: \"%d\" ", i+1);
    print_edge_vcgattr(n, i);
    fprintf (F, "}\n");
  }
}

/* dumps the edges between nodes and their type or entity attributes. */
void dump_node2type_edges (ir_node *n, void *env)
{
  assert(n);

  switch (get_irn_opcode(n)) {
  case iro_SymConst:
    if (   (get_SymConst_kind(n) == type_tag)
        || (get_SymConst_kind(n) == size))
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		NODE2TYPE_EDGE_ATTR "}\n", n, get_SymConst_type(n));
    break;
  case iro_Sel:
    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
	      NODE2TYPE_EDGE_ATTR "}\n", n, get_Sel_entity(n));
    break;
  case iro_Call:
    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
	      NODE2TYPE_EDGE_ATTR "}\n", n, get_Call_type(n));
    break;
  case iro_Alloc:
    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
	      NODE2TYPE_EDGE_ATTR "}\n", n, get_Alloc_type(n));
    break;
  case iro_Free:
    xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
	      NODE2TYPE_EDGE_ATTR "}\n", n, get_Free_type(n));
    break;
  default:
    break;
  }
}


/* dumps a type or entity and it's edges. */
void
dump_type_info (type_or_ent *tore, void *env) {
  int i = 0;  /* to shutup gcc */

  /* dump this type or entity */
  xfprintf (F, "node: {title: \"%p\" ", tore);
  xfprintf (F, DEFAULT_TYPE_ATTRIBUTE);
  xfprintf (F, "label: ");

  switch (get_kind(tore)) {
  case k_entity:
    {
      entity *ent = (entity *)tore;
      xfprintf (F, "\"ent %I\" " ENTITY_NODE_ATTR "}\n", get_entity_ident(ent));
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_OWN_EDGE_ATTR "}\n", tore, get_entity_owner(ent));
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
                ENT_TYPE_EDGE_ATTR "}\n", tore, get_entity_type(ent));
    } break;
  case k_type_class:
    {
      type_class *type = (type_class *)tore;
      xfprintf (F, "\"class %I\" " TYPE_CLASS_NODE_ATTR "}\n", get_class_ident(type));
      for (i=0; i < get_class_n_supertype(type); i++)
	xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		  TYPE_SUPER_EDGE_ATTR "}\n",
		  type, get_class_supertype(type, i));
    } break;
  case k_type_strct:
    {
      type_strct *type = (type_strct *)tore;
      xfprintf (F, "\"strct %I\"}\n", get_strct_ident(type));
      /* edges !!!??? */
    } break;
  case k_type_method:
    {
      type_method *type = (type_method *)tore;
      xfprintf (F, "\"meth %I\" " TYPE_METH_NODE_ATTR "}\n", get_method_ident(type));
      for (i = 0; i < get_method_arity(type); i++)
	xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		  METH_PAR_EDGE_ATTR "}\n",
		  tore, get_method_param_type(type, i), i);
      for (i = 0; i < get_method_n_res(type); i++)
	xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		  METH_RES_EDGE_ATTR "}\n",
		  tore, get_method_res_type(type, i), i);
    } break;
  case k_type_union:
    {
      type_union *type = (type_union *)tore;
      xfprintf (F, "\"union %I\"}\n", get_union_ident(type));
      /* edges !!!??? */
    } break;
  case k_type_array:
    {
      type_array *type = (type_array *)tore;
      xfprintf (F, "\"array %I\"}\n", get_array_ident(type));
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
	        ARR_ELT_TYPE_EDGE_ATTR "}\n", tore, get_array_element_type(type), i);
      /* edges !!!??? */
    } break;
  case k_type_enumeration:
    {
      type_enumeration *type = (type_enumeration *)tore;
      xfprintf (F, "\"enum %I\"}\n", get_enumeration_ident(type));
    } break;
  case k_type_pointer:
    {
      type_pointer *type = (type_pointer *)tore;
      xfprintf (F, "\"ptr %I\"}\n", get_pointer_ident(type));
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" "
		PTR_PTS_TO_EDGE_ATTR "}\n", tore,
		get_pointer_points_to_type(type), i);
    } break;
  case k_type_primitive:
    {
      type_primitive *type = (type_primitive *)tore;
      xfprintf (F, "\"prim %I, mode %I\"}\n", get_primitive_ident(type),
		get_mode_ident(get_primitive_mode(type)));
    } break;
  default:
    {
      xfprintf (F, "\" faulty type \"}\n");
      printf(" *** irdump,  %s(l.%i), faulty type.\n", __FUNCTION__, __LINE__);
    } break;
  }
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

  /** open file for vcg graph */
  id    = get_entity_ld_name (get_irg_ent(irg));
  len   = id_to_strlen (id);
  cp    = id_to_str (id);

  fname = malloc (len + 5 + strlen(suffix));
  strncpy (fname, cp, len);      /* copy the filename */
  fname[len] = '\0';
  strcat (fname, suffix);  /* append file suffix */

  fname = malloc (len + 5 + strlen(suffix));
  strncpy (fname, cp, len); /* copy the filename */
  fname[len] = '\0';        /* ensure string termination */
  /*strcpy (fname, cp);      * copy the filename *
    this produces wrong, too long strings in conjuction with the
    jocca frontend.  The \0 seems to be missing. */
  strcat (fname, suffix);   /* append file suffix */
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
	    "classname 2: \"Block\"\n", cp, label);

  xfprintf (F, "\n");		/* a separator */
}

void vcg_open_name (const char *name) {
  char *fname;  /* filename to put the vcg information in */
  int len;
  char label[4];

  /** open file for vcg graph */
  len   = strlen(name);
  fname = malloc (len + 5);
  strcpy (fname, name);    /* copy the filename */
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
	    "classname 2: \"Block\"\n", name, label);

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

void
dump_whole_node (ir_node *n, void* env) {
  dump_node(n);
  dump_ir_block_edge(n);
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
  irg_walk(irg->end, dump_whole_node, NULL, NULL);

  vcg_close();

  current_ir_graph = rem;
}

/***********************************************************************/
/* the following routines dump the nodes as attached to the blocks.    */
/***********************************************************************/

void
dump_ir_blocks_nodes (ir_node *n, void *env) {
  ir_node *block = (ir_node *)env;

  if (is_no_Block(n) && get_nodes_Block(n) == block) {
    dump_node(n);
    dump_ir_data_edges(n);
  }
}

void
dump_ir_block (ir_node *block, void *env) {
  ir_graph *irg = (ir_graph *)env;

  if (get_irn_opcode(block) == iro_Block) {

    /* This is a block. So dump the vcg information to make a block. */
    xfprintf(F, "graph: { title: \""); PRINT_NODEID(block); fprintf(F, "\"  label: \"");
#ifdef DEBUG_libfirm
    xfprintf (F, "%ld", get_irn_node_nr(block));
#else
    xfprintf (F, "%I", block->op->name);
#endif
    xfprintf(F, "\" status:clustered color:lightyellow \n");
    /* dump the blocks edges */
    dump_ir_data_edges(block);

    /* dump the nodes that go into the block */
    irg_walk(irg->end, dump_ir_blocks_nodes, NULL, block);

    /* Close the vcg information for the block */
    xfprintf(F, "}\n\n");
  }
}

void
dump_ir_block_graph (ir_graph *irg)
{
  ir_graph *rem;
  rem = current_ir_graph;
  current_ir_graph = irg;

  vcg_open (irg, "");

  /* walk over the blocks in the graph */
  irg_block_walk(irg->end, dump_ir_block, NULL, irg);

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
    xfprintf (F, "node: {title: \"%p\" label: \"%I\"}", block,
	      block->op->name);
    /* Dump the edges */
    for ( i = 0; i < get_Block_n_cfgpreds(block); i++) {
      pred = get_nodes_Block(skip_Proj(get_Block_cfgpred(block, i)));
      xfprintf (F, "edge: { sourcename: \"%p\" targetname: \"%p\" }\n",
                block, pred);
    }
  }
}

void
dump_cfg (ir_graph *irg)
{
  vcg_open (irg, "-cfg");

  /* walk over the blocks in the graph */
  irg_block_walk(irg->end, dump_block_to_cfg, NULL, NULL);

  vcg_close();
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
  /*  irg_block_walk(irg->end, dump_ir_block, NULL, irg); */
  irg_walk(irg->end, dump_whole_node, NULL, NULL);
  /* dump type info */
  type_walk_irg(irg, dump_type_info, NULL, NULL);
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
void turn_of_edge_labels() {
  edge_label = 0;
}
