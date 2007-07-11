/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Entry point to the representation of procedure code.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif

#include "xmalloc.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irflag_t.h"
#include "array.h"
#include "irgmod.h"
#include "irouts.h"
#include "irhooks.h"
#include "irtools.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "type_t.h"
#include "irmemory.h"

#define INITIAL_IDX_IRN_MAP_SIZE 1024

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


int firm_interprocedural_view = 0;

int (get_interprocedural_view)(void) {
  return _get_interprocedural_view();
}

void (set_interprocedural_view)(int state) {
  firm_interprocedural_view = state;

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
void firm_init_irgraph(void) {
  frame_type_suffix = new_id_from_str(FRAME_TP_SUFFIX);
  forbid_new_data   = 1;
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
new_r_ir_graph (ir_entity *ent, int n_loc)
{
  ir_graph *res;
  ir_node  *first_block;
  ir_node  *end, *start, *start_block, *initial_mem, *projX;

  res = alloc_graph();
  res->kind = k_ir_graph;

  //edges_init_graph_kind(res, EDGE_KIND_NORMAL);
  //edges_init_graph_kind(res, EDGE_KIND_BLOCK);

  /* initialize the idx->node map. */
  res->idx_irn_map = NEW_ARR_F(ir_node *, INITIAL_IDX_IRN_MAP_SIZE);
  memset(res->idx_irn_map, 0, INITIAL_IDX_IRN_MAP_SIZE * sizeof(res->idx_irn_map[0]));

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

  res->last_node_idx = 0;

  res->value_table = new_identities (); /* value table for global value
                       numbering for optimizing use in
                       iropt.c */
  res->outs = NULL;

  res->inline_property       = irg_inline_any;
  res->additional_properties = mtp_property_inherited;  /* inherited from type */

  res->phase_state         = phase_building;
  res->irg_pinned_state    = op_pin_state_pinned;
  res->outs_state          = outs_none;
  res->dom_state           = dom_none;
  res->pdom_state          = dom_none;
  res->typeinfo_state      = ir_typeinfo_none;
  set_irp_typeinfo_inconsistent();           /* there is a new graph with typeinfo_none. */
  res->callee_info_state   = irg_callee_info_none;
  res->loopinfo_state      = loopinfo_none;
  res->class_cast_state    = ir_class_casts_transitive;
  res->extblk_state        = ir_extblk_info_none;
  res->execfreq_state      = exec_freq_none;
  res->fp_model            = fp_model_precise;
  res->adr_taken_state     = ir_address_taken_not_computed;
  res->mem_disambig_opt    = aa_opt_inherited;

  /*-- Type information for the procedure of the graph --*/
  res->ent = ent;
  set_entity_irg(ent, res);

  /*--  a class type so that it can contain "inner" methods as in Pascal. --*/
  res->frame_type = new_type_frame(mangle(get_entity_ident(ent), frame_type_suffix));

  /*-- Nodes needed in every graph --*/
  set_irg_end_block (res, new_immBlock());
  end               = new_End();
  set_irg_end       (res, end);
  set_irg_end_reg   (res, end);
  set_irg_end_except(res, end);

  start_block = new_immBlock();
  set_irg_start_block(res, start_block);
  set_irg_bad        (res, new_ir_node(NULL, res, start_block, op_Bad, mode_T, 0, NULL));
  set_irg_no_mem     (res, new_ir_node(NULL, res, start_block, op_NoMem, mode_M, 0, NULL));
  start = new_Start();
  set_irg_start      (res, start);

  /* Proj results of start node */
  projX                   = new_Proj(start, mode_X, pn_Start_X_initial_exec);
  set_irg_frame           (res, new_Proj(start, mode_P_data, pn_Start_P_frame_base));
  set_irg_globals         (res, new_Proj(start, mode_P_data, pn_Start_P_globals));
  set_irg_tls             (res, new_Proj(start, mode_P_data, pn_Start_P_tls));
  set_irg_args            (res, new_Proj(start, mode_T,      pn_Start_T_args));
  set_irg_value_param_base(res, new_Proj(start, mode_P_data, pn_Start_P_value_arg_base));
  initial_mem             = new_Proj(start, mode_M, pn_Start_M);
  set_irg_initial_mem(res, initial_mem);

  add_immBlock_pred(start_block, projX);
  set_store(initial_mem);

#ifdef DEBUG_libfirm
  res->graph_nr    = get_irp_new_node_nr();
#endif
  res->proj_args   = NULL;

  /*
   * The code generation needs it. leave it in now.
   * Use of this edge is matter of discussion, unresolved. Also possible:
   * add_immBlock_pred(res->start_block, res->start_block), but invalid typed.
   */
  mature_immBlock(res->current_block);

  /*-- Make a block to start with --*/
  first_block = new_immBlock();
  add_immBlock_pred(first_block, projX);

  res->method_execution_frequency = -1.0;
  res->estimated_node_count       = 0;

  return res;
}


ir_graph *
new_ir_graph(ir_entity *ent, int n_loc)
{
  ir_graph *res = new_r_ir_graph(ent, n_loc);
  add_irp_irg(res);          /* remember this graph global. */
  return res;
}

/* Make a rudimentary IR graph for the constant code.
   Must look like a correct irg, spare everything else. */
ir_graph *new_const_code_irg(void) {
  ir_graph *res;
  ir_node  *end, *start_block, *start, *projX;

  res = alloc_graph();

  /* initialize the idx->node map. */
  res->idx_irn_map = NEW_ARR_F(ir_node *, INITIAL_IDX_IRN_MAP_SIZE);
  memset(res->idx_irn_map, 0, INITIAL_IDX_IRN_MAP_SIZE * sizeof(res->idx_irn_map[0]));

  /* inform statistics here, as blocks will be already build on this graph */
  hook_new_graph(res, NULL);

  current_ir_graph = res;
  res->n_loc = 1;         /* Only the memory. */
  res->visited = 0;       /* visited flag, for the ir walker */
  res->block_visited = 0; /* visited flag, for the 'block'-walker */
#if USE_EXPLICIT_PHI_IN_STACK
  res->Phi_in_stack = NULL;
#endif
  res->kind = k_ir_graph;
  res->obst      = xmalloc (sizeof(*res->obst));
  obstack_init (res->obst);
  res->extbb_obst = NULL;

  res->last_node_idx = 0;

  res->phase_state      = phase_building;
  res->irg_pinned_state = op_pin_state_pinned;
  res->extblk_state     = ir_extblk_info_none;
  res->fp_model         = fp_model_precise;

  res->value_table = new_identities (); /* value table for global value
                       numbering for optimizing use in
                       iropt.c */
  res->ent = NULL;
  res->frame_type  = NULL;

  /* -- The end block -- */
  set_irg_end_block (res, new_immBlock());
  end = new_End();
  set_irg_end       (res, end);
  set_irg_end_reg   (res, end);
  set_irg_end_except(res, end);
  mature_immBlock(get_cur_block());  /* mature the end block */

  /* -- The start block -- */
  start_block        = new_immBlock();
  set_irg_start_block(res, start_block);
  set_irg_bad        (res, new_ir_node (NULL, res, start_block, op_Bad, mode_T, 0, NULL));
  set_irg_no_mem     (res, new_ir_node (NULL, res, start_block, op_NoMem, mode_M, 0, NULL));
  start              = new_Start();
  set_irg_start      (res, start);

  /* Proj results of start node */
  set_irg_initial_mem(res, new_Proj(start, mode_M, pn_Start_M));
  projX = new_Proj(start, mode_X, pn_Start_X_initial_exec);
  add_immBlock_pred(start_block, projX);
  mature_immBlock  (start_block);  /* mature the start block */

  add_immBlock_pred(new_immBlock(), projX);
  mature_immBlock  (get_cur_block());   /* mature the 'body' block for expressions */

  /* Set the visited flag high enough that the blocks will never be visited. */
  set_irn_visited(get_cur_block(), -1);
  set_Block_block_visited(get_cur_block(), -1);
  set_Block_block_visited(start_block, -1);
  set_irn_visited(start_block, -1);
  set_irn_visited(get_irg_bad(res), -1);
  set_irn_visited(get_irg_no_mem(res), -1);

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
  if (irg->outs_state != outs_none) free_irg_outs(irg);
  if (irg->frame_type)  free_type(irg->frame_type);
  if (irg->value_table) del_identities(irg->value_table);
  if (irg->ent) {
    ir_peculiarity pec = get_entity_peculiarity (irg->ent);
    set_entity_peculiarity (irg->ent, peculiarity_description);
    set_entity_irg(irg->ent, NULL);  /* not set in const code irg */
    set_entity_peculiarity (irg->ent, pec);
  }

  free_End(get_irg_end(irg));
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
long get_irg_graph_nr(ir_graph *irg) {
  assert(irg);
#ifdef DEBUG_libfirm
  return irg->graph_nr;
#else
  return (long)PTR_TO_INT(irg);
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
  irg->anchors[anchor_end_reg] = node;
}

ir_node *
(get_irg_end_except)(const ir_graph *irg) {
  return _get_irg_end_except(irg);
}

void     set_irg_end_except (ir_graph *irg, ir_node *node) {
  assert(get_irn_op(node) == op_EndExcept || get_irn_op(node) == op_End);
  irg->anchors[anchor_end_except] = node;
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
(get_irg_tls)(const ir_graph *irg) {
  return _get_irg_tls(irg);
}

void
(set_irg_tls)(ir_graph *irg, ir_node *node) {
  _set_irg_tls(irg, node);
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

ir_node *
(get_irg_value_param_base)(const ir_graph *irg) {
  return _get_irg_value_param_base(irg);
}

void
(set_irg_value_param_base)(ir_graph *irg, ir_node *node) {
  _set_irg_value_param_base(irg, node);
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

ir_entity *
(get_irg_entity)(const ir_graph *irg) {
  return _get_irg_entity(irg);
}

void
(set_irg_entity)(ir_graph *irg, ir_entity *ent) {
  _set_irg_entity(irg, ent);
}

ir_type *
(get_irg_frame_type)(ir_graph *irg) {
  return _get_irg_frame_type(irg);
}

void
(set_irg_frame_type)(ir_graph *irg, ir_type *ftp) {
  _set_irg_frame_type(irg, ftp);
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
(set_irg_phase_state)(ir_graph *irg, irg_phase_state state) {
  _set_irg_phase_state(irg, state);
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

irg_extblk_state
(get_irg_extblk_state)(const ir_graph *irg) {
  return _get_irg_extblk_state(irg);
}

void
(set_irg_extblk_inconsistent)(ir_graph *irg) {
  _set_irg_extblk_inconsistent(irg);
}

irg_dom_state
(get_irg_dom_state)(const ir_graph *irg) {
  return _get_irg_dom_state(irg);
}

irg_dom_state
(get_irg_postdom_state)(const ir_graph *irg) {
  return _get_irg_postdom_state(irg);
}

void
(set_irg_doms_inconsistent)(ir_graph *irg) {
  _set_irg_doms_inconsistent(irg);
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

void set_irp_loopinfo_inconsistent(void) {
  int i, n_irgs = get_irp_n_irgs();
  for (i = 0; i < n_irgs; ++i) {
    set_irg_loopinfo_inconsistent(get_irp_irg(i));
  }
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

unsigned
(get_irg_additional_properties)(const ir_graph *irg) {
  return _get_irg_additional_properties(irg);
}

void
(set_irg_additional_properties)(ir_graph *irg, unsigned property_mask) {
  _set_irg_additional_properties(irg, property_mask);
}

void
(set_irg_additional_property)(ir_graph *irg, mtp_additional_property flag) {
  _set_irg_additional_property(irg, flag);
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

/* Return the floating point model of this graph. */
unsigned (get_irg_fp_model)(const ir_graph *irg) {
  return _get_irg_fp_model(irg);
}

/* Sets the floating point model for this graph. */
void set_irg_fp_model(ir_graph *irg, unsigned model) {
  irg->fp_model = model;
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
  (void) env;
  if (is_Proj(n)) {
    ir_node *pred  = get_Proj_pred(n);
    ir_node *block = get_nodes_block(pred);

    set_nodes_block(n, block);
  }
}

/* move Proj nodes into the same block as its predecessors */
void normalize_proj_nodes(ir_graph *irg) {
  irg_walk_graph(irg, NULL, normalize_proj_walker, NULL);
  set_irg_outs_inconsistent(irg);
}

/* set a description for local value n */
void set_irg_loc_description(ir_graph *irg, int n, void *description) {
  assert(0 <= n && n < irg->n_loc);

  if (! irg->loc_descriptions)
    irg->loc_descriptions = xcalloc(sizeof(*irg->loc_descriptions), irg->n_loc);

  irg->loc_descriptions[n] = description;
}

/* get the description for local value n */
void *get_irg_loc_description(ir_graph *irg, int n) {
  assert(0 <= n && n < irg->n_loc);
  return irg->loc_descriptions ? irg->loc_descriptions[n] : NULL;
}

#ifndef NDEBUG
void set_using_block_visited(ir_graph *irg) {
	assert(irg->using_block_visited == 0);
	irg->using_block_visited = 1;
}

void clear_using_block_visited(ir_graph *irg) {
	assert(irg->using_block_visited == 1);
	irg->using_block_visited = 0;
}

int using_block_visited(const ir_graph *irg) {
  	return irg->using_block_visited;
}


void set_using_visited(ir_graph *irg) {
	assert(irg->using_visited == 0);
	irg->using_visited = 1;
}

void clear_using_visited(ir_graph *irg) {
	assert(irg->using_visited == 1);
	irg->using_visited = 0;
}

int using_visited(const ir_graph *irg) {
  	return irg->using_visited;
}


void set_using_irn_link(ir_graph *irg) {
	assert(irg->using_irn_link == 0);
	irg->using_irn_link = 1;
}

void clear_using_irn_link(ir_graph *irg) {
	assert(irg->using_irn_link == 1);
	irg->using_irn_link = 0;
}

int using_irn_link(const ir_graph *irg) {
  	return irg->using_irn_link;
}
#endif

/* Returns a estimated node count of the irg. */
unsigned (get_irg_estimated_node_cnt)(const ir_graph *irg) {
  return _get_irg_estimated_node_cnt(irg);
}

/* Returns the last irn index for this graph. */
unsigned get_irg_last_idx(const ir_graph *irg) {
  return irg->last_node_idx;
}

/* register additional space in an IR graph */
size_t register_additional_graph_data(size_t size)
{
  assert(!forbid_new_data && "Too late to register additional node data");

  if (forbid_new_data)
    return 0;

  return additional_graph_data_size += size;
}
