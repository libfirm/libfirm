/*
 * Project:     libFIRM
 * File name:   ir/ana/field_temperature.c
 * Purpose:     Compute an estimate of field temperature, i.e., field access heuristic.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     21.7.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "field_temperature.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"
#include "irgwalk.h"

#include "array.h"

/* *************************************************************************** */
/* initialize, global variables.                                               */
/* *************************************************************************** */


/* *************************************************************************** */
/*   Access routines for irnodes                                               */
/* *************************************************************************** */

/* The entities that can be accessed by this Sel node. */
int get_Sel_n_accessed_entities(ir_node *sel) {
  return 1;
}

entity *get_Sel_accessed_entity(ir_node *sel, int pos) {
  return get_Sel_entity(sel);
}



/* *************************************************************************** */
/* The heuristic                                                               */
/* *************************************************************************** */

int get_irn_loop_call_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_loop_depth(irg);
}

int get_irn_loop_depth(ir_node *n) {
  return get_loop_depth(get_irn_loop(get_nodes_block(n)));
}

int get_irn_recursion_depth(ir_node *n) {
  ir_graph *irg = get_irn_irg(n);
  return get_irg_recursion_depth(irg);
}


int get_weighted_loop_depth(ir_node *n) {
  int loop_call_depth = get_irn_loop_call_depth(n);
  int loop_depth      = get_irn_loop_depth(n);
  int recursion_depth = get_irn_recursion_depth(n);

  return loop_call_depth + loop_depth + recursion_depth;
}

/* *************************************************************************** */
/* Auxiliary                                                                   */
/* *************************************************************************** */


int is_jack_rts_class(type *t) {
  ident *name = get_type_ident(t);

  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;
  if (id_is_prefix(new_id_from_str("["), name))     return 1;
  if (id_is_prefix(new_id_from_str("gnu/"), name))  return 1;
  if (id_is_prefix(new_id_from_str("java/"), name)) return 1;

  return 0;
}
