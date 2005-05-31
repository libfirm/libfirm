/*
 * Project:     libFIRM
 * File name:   ir/ir/irgraph.c
 * Purpose:     Entry point to the representation of procedure code.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

#include <stddef.h>

# include "xmalloc.h"
# include "ircons.h"
# include "irgraph_t.h"
# include "irprog_t.h"
# include "irnode_t.h"
# include "iropt_t.h"
# include "irflag_t.h"
# include "array.h"
# include "irgmod.h"
# include "mangle.h"
# include "irouts.h"
# include "irhooks.h"
# include "irgwalk.h"
# include "iredges_t.h"

/**
 * Indicates, whether additional data can be registered to graphs.
 * If set to 1, this is not possible anymore.
 */
static int forbid_new_data = 0;

/**
 * The amount of additional space for custom data to be allocated upon
 * creating a new graph.
 */
static size_t additional_graph_data_size = 0;

ir_graph *current_ir_graph;
ir_graph *get_current_ir_graph(void) {
  return current_ir_graph;
}
void set_current_ir_graph(ir_graph *graph) {
  current_ir_graph = graph;
}


int __interprocedural_view = false;

int (get_interprocedural_view)(void) {
  return _get_interprocedural_view();
}

void (set_interprocedural_view)(int state) {
  __interprocedural_view = state;

  /* set function vectors for faster access */
  if (state) {
    _get_irn_arity = _get_irn_inter_arity;
    _get_irn_n     = _get_irn_inter_n;
  }
  else {
    _get_irn_arity = _get_irn_intra_arity;
    _get_irn_n     = _get_irn_intra_n;
  }
}

/** contains the suffix for frame type names */
static ident *frame_type_suffix = NULL;

/* initialize the IR graph module */
void init_irgraph(void) {
  frame_type_suffix = new_id_from_str(FRAME_TP_SUFFIX);
	forbid_new_data = 1;
}

/**
 * Allocate a new IR graph.
 * This function respects the registered graph data. The only reason for
 * this function is, that there are two locations, where graphs are
 * allocated (new_r_ir_graph, new_const_code_irg).
 * @return Memory for a new graph.
 */
static ir_graph *alloc_graph(void) {
  size_t size = sizeof(ir_graph) + additional_graph_data_size;
  char *ptr = xmalloc(size);
  memset(ptr, 0, size);

  return (ir_graph *) (ptr + additional_graph_data_size);
}

/**
 * Frees an allocated IR graph
 */
static void free_graph(ir_graph *irg) {
  char *ptr = (char *)irg;
  free(ptr - additional_graph_data_size);
}

#if USE_EXPLICIT_PHI_IN_STACK
/* really defined in ircons.c */
typedef struct Phi_in_stack Phi_in_stack;
Phi_in_stack *new_Phi_in_stack();
void free_Phi_in_stack(Phi_in_stack *s);
#endif

/* Allocates a list of nodes:
    - The start block containing a start node and Proj nodes for it's four
      results (X, M, P, Tuple).
    - The end block containing an end node. This block is not matured after
      new_ir_graph as predecessors need to be added to it.
    - The current block, which is empty and also not matured.
   Further it allocates several datastructures needed for graph construction
   and optimization.
*/
ir_graph *
new_r_ir_graph (entity *ent, int n_loc)
{
  ir_graph *res;
  ir_node *first_block;
  ir_node *projX;

  res = alloc_graph();
  res->kind = k_ir_graph;

  edges_init_graph(res);

  /* inform statistics here, as blocks will be already build on this graph */
  hook_new_graph(res, ent);

  current_ir_graph = res;

  /*-- initialized for each graph. --*/
  if (get_opt_precise_exc_context()) {
    res->n_loc = n_loc + 1 + 1; /* number of local variables that are never
                   dereferenced in this graph plus one for
                   the store plus one for links to fragile
                   operations.  n_loc is not the number of
                   parameters to the procedure!  */
  }
  else {
    res->n_loc = n_loc + 1;  /* number of local variables that are never
                dereferenced in this graph plus one for
                the store. This is not the number of parameters
                to the procedure!  */
  }

  /* descriptions will be allocated on demand */
  res->loc_descriptions = NULL;

  res->visited       = 0; /* visited flag, for the ir walker */
  res->block_visited = 0; /* visited flag, for the 'block'-walker */

#if USE_EXPLICIT_PHI_IN_STACK
  res->Phi_in_stack = new_Phi_in_stack();  /* A stack needed for automatic Phi
                                generation */
#endif
  res->kind = k_ir_graph;
  res->obst = xmalloc (sizeof(*res->obst));
  obstack_init(res->obst);
  res->extbb_obst = NULL;

  res->value_table = new_identities (); /* value table for global value
                       numbering for optimizing use in
                       iropt.c */
  res->outs = NULL;

  res->phase_state      = phase_building;
  res->op_pin_state_pinned = op_pin_state_pinned;
  res->outs_state       = outs_none;
  res->dom_state        = dom_none;
  res->typeinfo_state   = ir_typeinfo_none;
  set_irp_typeinfo_inconsistent();           /* there is a new graph with typeinfo_none. */
  res->loopinfo_state   = loopinfo_none;
  res->class_cast_state = ir_class_casts_transitive;
  res->extblk_state     = ir_extblk_info_none;

  /*-- Type information for the procedure of the graph --*/
  res->ent = ent;
  set_entity_irg(ent, res);

  /*--  a class type so that it can contain "inner" methods as in Pascal. --*/
  res->frame_type = new_type_class(mangle(get_entity_ident(ent), frame_type_suffix));

  /* Remove type from type list.  Must be treated differently than other types. */
  remove_irp_type_from_list(res->frame_type);

  /*-- Nodes needed in every graph --*/
  res->end_block  = new_immBlock();
  res->end        = new_End();
  res->end_reg    = res->end;
  res->end_except = res->end;

  res->start_block = new_immBlock();
  res->start   = new_Start();
  res->bad     = new_ir_node(NULL, res, res->start_block, op_Bad, mode_T, 0, NULL);
  res->no_mem  = new_ir_node(NULL, res, res->start_block, op_NoMem, mode_M, 0, NULL);

  /* Proj results of start node */
  projX            = new_Proj (res->start, mode_X, pn_Start_X_initial_exec);
  res->frame       = new_Proj (res->start, mode_P_mach, pn_Start_P_frame_base);
  res->globals     = new_Proj (res->start, mode_P_mach, pn_Start_P_globals);
  res->initial_mem = new_Proj (res->start, mode_M, pn_Start_M);
  res->args        = new_Proj (res->start, mode_T, pn_Start_T_args);
#ifdef DEBUG_libfirm
  res->graph_nr    = get_irp_new_node_nr();
#endif
  res->proj_args   = NULL;

  set_store(res->initial_mem);

  add_immBlock_pred(res->start_block, projX);
  /*
   * The code generation needs it. leave it in now.
   * Use of this edge is matter of discussion, unresolved. Also possible:
   * add_immBlock_pred(res->start_block, res->start_block), but invalid typed.
   */
  mature_immBlock (res->current_block);

  /*-- Make a block to start with --*/
  first_block = new_immBlock();
  add_immBlock_pred (first_block, projX);

  res->method_execution_frequency = -1;

  return res;
}


ir_graph *
new_ir_graph (entity *ent, int n_loc)
{
  ir_graph *res = new_r_ir_graph (ent, n_loc);
  add_irp_irg(res);          /* remember this graph global. */
  return res;
}

/* Make a rudimentary ir graph for the constant code.
   Must look like a correct irg, spare everything else. */
ir_graph *new_const_code_irg(void) {
  ir_graph *res;
  ir_node *projX;

  res = alloc_graph();

  /* inform statistics here, as blocks will be already build on this graph */
  hook_new_graph(res, NULL);

  current_ir_graph = res;
  res->n_loc = 1;       /* Only the memory. */
  res->visited = 0;     /* visited flag, for the ir walker */
  res->block_visited=0; /* visited flag, for the 'block'-walker */
#if USE_EXPLICIT_PHI_IN_STACK
  res->Phi_in_stack = NULL;
#endif
  res->kind = k_ir_graph;
  res->obst      = xmalloc (sizeof(*res->obst));
  obstack_init (res->obst);
  res->extbb_obst = NULL;

  res->phase_state = phase_building;
  res->op_pin_state_pinned = op_pin_state_pinned;
  res->extblk_state = ir_extblk_info_none;

  res->value_table = new_identities (); /* value table for global value
                       numbering for optimizing use in
                       iropt.c */
  res->ent = NULL;
  res->frame_type  = NULL;

  /* -- The end block -- */
  res->end_block  = new_immBlock ();
  res->end        = new_End ();
  res->end_reg    = res->end;
  res->end_except = res->end;
  mature_immBlock(get_cur_block());  /* mature the end block */

  /* -- The start block -- */
  res->start_block = new_immBlock ();
  res->bad     = new_ir_node (NULL, res, res->start_block, op_Bad, mode_T, 0, NULL);
  res->no_mem  = new_ir_node (NULL, res, res->start_block, op_NoMem, mode_M, 0, NULL);
  res->start   = new_Start ();
  /* Proj results of start node */
  res->initial_mem = new_Proj (res->start, mode_M, pn_Start_M);
  projX            = new_Proj (res->start, mode_X, pn_Start_X_initial_exec);
  add_immBlock_pred (res->start_block, projX);
  mature_immBlock   (res->start_block);  /* mature the start block */

  add_immBlock_pred (new_immBlock (), projX);
  mature_immBlock   (get_cur_block());   /* mature the 'body' block for expressions */

  /* Set the visited flag high enough that the blocks will never be visited. */
  set_irn_visited(get_cur_block(), -1);
  set_Block_block_visited(get_cur_block(), -1);
  set_Block_block_visited(res->start_block, -1);
  set_irn_visited(res->start_block, -1);
  set_irn_visited(res->bad, -1);
  set_irn_visited(res->no_mem, -1);

  res->phase_state = phase_high;

  return res;
}

/* Defined in iropt.c */
void  del_identities (pset *value_table);

/* Frees the passed irgraph.
   Deallocates all nodes in this graph and the ir_graph structure.
   Sets the field irgraph in the corresponding entity to NULL.
   Does not remove the irgraph from the list in irprog (requires
   inefficient search, call remove_irp_irg by hand).
   Does not free types, entities or modes that are used only by this
   graph, nor the entity standing for this graph. */
void free_ir_graph (ir_graph *irg) {
  assert(is_ir_graph(irg));

  hook_free_graph(irg);
  if (irg->outs_state != outs_none) free_outs(irg);
  if (irg->frame_type)  free_type(irg->frame_type);
  if (irg->value_table) del_identities(irg->value_table);
  if (irg->ent) {
    peculiarity pec = get_entity_peculiarity (irg->ent);
    set_entity_peculiarity (irg->ent, peculiarity_description);
    set_entity_irg(irg->ent, NULL);  /* not set in const code irg */
    set_entity_peculiarity (irg->ent, pec);
  }

  free_End(irg->end);
  obstack_free(irg->obst,NULL);
  free(irg->obst);
#if USE_EXPLICIT_PHI_IN_STACK
  free_Phi_in_stack(irg->Phi_in_stack);
#endif
  if (irg->loc_descriptions)
    free(irg->loc_descriptions);
  irg->kind = k_BAD;
  free_graph(irg);
}

/* access routines for all ir_graph attributes:
   templates:
   {attr type} get_irg_{attribute name} (ir_graph *irg);
   void set_irg_{attr name} (ir_graph *irg, {attr type} {attr}); */

int
(is_ir_graph)(const void *thing) {
  return _is_ir_graph(thing);
}

/* Outputs a unique number for this node */

long
get_irg_graph_nr(ir_graph *irg) {
  assert(irg);
#ifdef DEBUG_libfirm
  return irg->graph_nr;
#else
  return (long)irg;
#endif
}

ir_node *
(get_irg_start_block)(const ir_graph *irg) {
  return _get_irg_start_block(irg);
}

void
(set_irg_start_block)(ir_graph *irg, ir_node *node) {
  _set_irg_start_block(irg, node);
}

ir_node *
(get_irg_start)(const ir_graph *irg) {
  return _get_irg_start(irg);
}

void
(set_irg_start)(ir_graph *irg, ir_node *node) {
  _set_irg_start(irg, node);
}

ir_node *
(get_irg_end_block)(const ir_graph *irg) {
  return _get_irg_end_block(irg);
}

void
(set_irg_end_block)(ir_graph *irg, ir_node *node) {
  _set_irg_end_block(irg, node);
}

ir_node *
(get_irg_end)(const ir_graph *irg) {
  return _get_irg_end(irg);
}

void
(set_irg_end)(ir_graph *irg, ir_node *node) {
  _set_irg_end(irg, node);
}

ir_node *
(get_irg_end_reg)(const ir_graph *irg) {
  return _get_irg_end_reg(irg);
}

void     set_irg_end_reg (ir_graph *irg, ir_node *node) {
  assert(get_irn_op(node) == op_EndReg || get_irn_op(node) == op_End);
  irg->end_reg = node;
}

ir_node *
(get_irg_end_except)(const ir_graph *irg) {
  return _get_irg_end_except(irg);
}

void     set_irg_end_except (ir_graph *irg, ir_node *node) {
  assert(get_irn_op(node) == op_EndExcept || get_irn_op(node) == op_End);
  irg->end_except = node;
}

ir_node *
(get_irg_cstore)(const ir_graph *irg) {
  return _get_irg_cstore(irg);
}

void
(set_irg_cstore)(ir_graph *irg, ir_node *node) {
  _set_irg_cstore(irg, node);
}

ir_node *
(get_irg_frame)(const ir_graph *irg) {
  return _get_irg_frame(irg);
}

void
(set_irg_frame)(ir_graph *irg, ir_node *node) {
  _set_irg_frame(irg, node);
}

ir_node *
(get_irg_globals)(const ir_graph *irg) {
  return _get_irg_globals(irg);
}

void
(set_irg_globals)(ir_graph *irg, ir_node *node) {
  _set_irg_globals(irg, node);
}

ir_node *
(get_irg_initial_mem)(const ir_graph *irg) {
  return _get_irg_initial_mem(irg);
}

void
(set_irg_initial_mem)(ir_graph *irg, ir_node *node) {
  _set_irg_initial_mem(irg, node);
}

ir_node *
(get_irg_args)(const ir_graph *irg) {
  return _get_irg_args(irg);
}

void
(set_irg_args)(ir_graph *irg, ir_node *node) {
  _set_irg_args(irg, node);
}

ir_node **
(get_irg_proj_args) (const ir_graph *irg) {
  return _get_irg_proj_args (irg);
}

void
(set_irg_proj_args) (ir_graph *irg, ir_node **nodes) {
  _set_irg_proj_args (irg, nodes);
}

ir_node *
(get_irg_bad)(const ir_graph *irg) {
  return _get_irg_bad(irg);
}

void
(set_irg_bad)(ir_graph *irg, ir_node *node) {
  _set_irg_bad(irg, node);
}

ir_node *
(get_irg_no_mem)(const ir_graph *irg) {
  return _get_irg_no_mem(irg);
}

void
(set_irg_no_mem)(ir_graph *irg, ir_node *node) {
  _set_irg_no_mem(irg, node);
}

ir_node *
(get_irg_current_block)(const ir_graph *irg) {
  return _get_irg_current_block(irg);
}

void
(set_irg_current_block)(ir_graph *irg, ir_node *node) {
  _set_irg_current_block(irg, node);
}

entity *
(get_irg_entity)(const ir_graph *irg) {
  return _get_irg_entity(irg);
}

void
(set_irg_entity)(ir_graph *irg, entity *ent) {
  _set_irg_entity(irg, ent);
}

type *
(get_irg_frame_type)(const ir_graph *irg) {
  return _get_irg_frame_type(irg);
}

void
(set_irg_frame_type)(ir_graph *irg, type *ftp) {
  _set_irg_frame_type(irg, ftp);
}


/* To test for a frame type */
int
is_frame_type(const type *ftp) {
  int i;
  if (is_Class_type(ftp)) {
    for (i = 0; i < get_irp_n_irgs(); i++) {
      const type *frame_tp = get_irg_frame_type(get_irp_irg(i));
      if (ftp == frame_tp) return true;
    }
  }
  return false;
}

int
get_irg_n_locs (ir_graph *irg)
{
  if (get_opt_precise_exc_context())
    return irg->n_loc - 1 - 1;
  else
    return irg->n_loc - 1;
}

void
set_irg_n_loc (ir_graph *irg, int n_loc)
{
  if (get_opt_precise_exc_context())
    irg->n_loc = n_loc + 1 + 1;
  else
    irg->n_loc = n_loc + 1;
}



/* Returns the obstack associated with the graph. */
struct obstack *
(get_irg_obstack)(const ir_graph *irg) {
  return _get_irg_obstack(irg);
}

/*
 * Returns true if the node n is allocated on the storage of graph irg.
 *
 * Implementation is GLIBC specific as is uses the internal _obstack_chunk implementation.
 */
int node_is_in_irgs_storage(ir_graph *irg, ir_node *n)
{
  struct _obstack_chunk *p;

  /*
   * checks weather the ir_node pointer is on the obstack.
   * A more sophisticated check would test the "whole" ir_node
   */
  for (p = irg->obst->chunk; p; p = p->prev) {
    if (((char *)p->contents <= (char *)n) && ((char *)n < (char *)p->limit))
      return 1;
  }

  return 0;
}

irg_phase_state
(get_irg_phase_state)(const ir_graph *irg) {
  return _get_irg_phase_state(irg);
}

void
(set_irg_phase_low)(ir_graph *irg) {
  _set_irg_phase_low(irg);
}

op_pin_state
(get_irg_pinned)(const ir_graph *irg) {
  return _get_irg_pinned(irg);
}

irg_outs_state
(get_irg_outs_state)(const ir_graph *irg) {
  return _get_irg_outs_state(irg);
}

void
(set_irg_outs_inconsistent)(ir_graph *irg) {
  _set_irg_outs_inconsistent(irg);
}

irg_dom_state
(get_irg_dom_state)(const ir_graph *irg) {
  return _get_irg_dom_state(irg);
}

void
(set_irg_dom_inconsistent)(ir_graph *irg) {
  _set_irg_dom_inconsistent(irg);
}

irg_loopinfo_state
(get_irg_loopinfo_state)(const ir_graph *irg) {
  return _get_irg_loopinfo_state(irg);
}

void
(set_irg_loopinfo_state)(ir_graph *irg, irg_loopinfo_state s) {
  _set_irg_loopinfo_state(irg, s);
}

void
(set_irg_loopinfo_inconsistent)(ir_graph *irg) {
  _set_irg_loopinfo_inconsistent(irg);
}

void
(set_irg_pinned)(ir_graph *irg, op_pin_state p) {
  _set_irg_pinned(irg, p);
}

irg_callee_info_state
(get_irg_callee_info_state)(const ir_graph *irg) {
  return _get_irg_callee_info_state(irg);
}

void
(set_irg_callee_info_state)(ir_graph *irg, irg_callee_info_state s) {
  _set_irg_callee_info_state(irg, s);
}

irg_inline_property
(get_irg_inline_property)(const ir_graph *irg) {
  return _get_irg_inline_property(irg);
}

void
(set_irg_inline_property)(ir_graph *irg, irg_inline_property s) {
  _set_irg_inline_property(irg, s);
}

void
(set_irg_link)(ir_graph *irg, void *thing) {
  _set_irg_link(irg, thing);
}

void *
(get_irg_link)(const ir_graph *irg) {
  return _get_irg_link(irg);
}

/** maximum visited flag content of all ir_graph visited fields. */
static unsigned long max_irg_visited = 0;

unsigned long
(get_irg_visited)(const ir_graph *irg) {
  return _get_irg_visited(irg);
}

void
set_irg_visited (ir_graph *irg, unsigned long visited)
{
  irg->visited = visited;
  if (irg->visited > max_irg_visited) {
    max_irg_visited = irg->visited;
  }
}

void
inc_irg_visited (ir_graph *irg)
{
  if (++irg->visited > max_irg_visited) {
    max_irg_visited = irg->visited;
  }
}

unsigned long
get_max_irg_visited(void)
{
  /*
  int i;
  for(i = 0; i < get_irp_n_irgs(); i++)
  assert(max_irg_visited >= get_irg_visited(get_irp_irg(i)));
   */
  return max_irg_visited;
}

void set_max_irg_visited(int val) {
  max_irg_visited = val;
}

unsigned long
inc_max_irg_visited(void)
{
  /*
  int i;
  for(i = 0; i < get_irp_n_irgs(); i++)
  assert(max_irg_visited >= get_irg_visited(get_irp_irg(i)));
  */
  max_irg_visited++;
  return max_irg_visited;
}

unsigned long
(get_irg_block_visited)(const ir_graph *irg) {
  return _get_irg_block_visited(irg);
}

void
(set_irg_block_visited)(ir_graph *irg, unsigned long visited) {
  _set_irg_block_visited(irg, visited);
}

void
(inc_irg_block_visited)(ir_graph *irg) {
  _inc_irg_block_visited(irg);
}


/**
 * walker Start->End: places Proj nodes into the same block
 * as it's predecessors
 *
 * @param n    the node
 * @param env  ignored
 */
static void normalize_proj_walker(ir_node *n, void *env)
{
  if (is_Proj(n)) {
    ir_node *pred  = get_Proj_pred(n);
    ir_node *block = get_nodes_block(pred);

    set_nodes_block(n, block);
  }
}

/* move Proj nodes into the same block as its predecessors */
void normalize_proj_nodes(ir_graph *irg)
{
  irg_walk_graph(irg, NULL, normalize_proj_walker, NULL);
  set_irg_outs_inconsistent(irg);
}

/* set a description for local value n */
void set_irg_loc_description(ir_graph *irg, int n, void *description)
{
  assert(0 <= n && n < irg->n_loc);

  if (! irg->loc_descriptions)
    irg->loc_descriptions = xcalloc(sizeof(*irg->loc_descriptions), irg->n_loc);

  irg->loc_descriptions[n] = description;
}

/* get the description for local value n */
void *get_irg_loc_description(ir_graph *irg, int n)
{
  assert(0 <= n && n < irg->n_loc);
  return irg->loc_descriptions ? irg->loc_descriptions[n] : NULL;
}

/* register additional space in an IR graph */
size_t register_additional_graph_data(size_t size)
{
  assert(!forbid_new_data && "Too late to register additional node data");

  if (forbid_new_data)
    return 0;

  return additional_graph_data_size += size;
}
