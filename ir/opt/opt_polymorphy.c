/*
 * Project:     libFIRM
 * File name:   ir/opt/opt_polymorphy
 * Purpose:     Optimize polymorphic Sel nodes.
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 UniversitÅ‰t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "irprog_t.h"
#include "entity_t.h"
#include "type_t.h"
#include "irop.h"
#include "irnode_t.h"
#include "ircons.h"

#include "iropt_dbg.h"
#include "irflag_t.h"

/** Return dynamic type of ptr.
 *
 * If we can deduct the dynamic type from the firm nodes
 * by a limited test we return the dynamic type.  Else
 * we return unknown_type.
 *
 * If we find a dynamic type this means that the pointer always points
 * to an object of this type during runtime.   We resolved polymorphy.
 */
static type *get_dynamic_type(ir_node *ptr) {
  ptr = skip_Cast(skip_Proj(ptr));
  if (get_irn_op(ptr) == op_Alloc)
    return get_Alloc_type(ptr);
  return firm_unknown_type;
}

/*
 * Transform  Sel(Alloc)[method]
 * to SymC[method]
 */
ir_node *transform_node_Sel(ir_node *node)
{
  entity *ent = get_Sel_entity(node);
  ir_node *new_node;

  if (get_irp_phase_state() == phase_building) return node;

  if (!(get_opt_optimize() && get_opt_dyn_meth_dispatch()))
    return node;

  if (!is_Method_type(get_entity_type(ent)))
    return node;

  /* If the entity is a leave in the inheritance tree,
     we can replace the Sel by a constant. */
  if ((get_irp_phase_state() != phase_building) && (get_entity_n_overwrittenby(ent) == 0)) {
    /* In dead code, we might call a leave entity that is a description.
       Do not turn the Sel to a SymConst. */
    if (get_entity_peculiarity(ent) == peculiarity_description) {
      /* We could remove the Call depending on this Sel. */
      new_node = node;
    } else {
      ir_node *rem_block = get_cur_block();
      set_cur_block(get_nodes_block(node));
      new_node = copy_const_value(get_atomic_ent_value(ent));
      set_cur_block(rem_block);
      DBG_OPT_POLY_ALLOC(node, new_node);
    }

    return new_node;
  }

  /* If we know the dynamic type, we can replace the Sel by a constant. */
  ir_node *ptr = get_Sel_ptr(node);      /* The address we select from. */
  type *dyn_tp = get_dynamic_type(ptr);  /* The runtime type of ptr. */

  if (dyn_tp != firm_unknown_type) {
    entity *called_ent;

    /* We know which method will be called, no dispatch necessary. */
    called_ent = resolve_ent_polymorphy(dyn_tp, ent);
    /* called_ent may not be description: has no Address/Const to Call! */
    assert(get_entity_peculiarity(called_ent) != peculiarity_description);

    ir_node *rem_block = get_cur_block();
    set_cur_block(get_nodes_block(node));
    new_node = copy_const_value(get_atomic_ent_value(called_ent));
    set_cur_block(rem_block);
    DBG_OPT_POLY_ALLOC(node, new_node);

    return new_node;
  }

  return node;
}

/* Transform  Load(Sel(Alloc)[constant static entity])
 *  to Const[constant static entity value].
 *
 *  This function returns a node replacing the Proj(Load)[Value].
 *  If this is actually called in transform_node, we must build
 *  a tuple, or replace the Projs of the load.
 *  Therefore we call this optimization in ldstopt.
 */
ir_node *transform_node_Load(ir_node *n)
{
  if (!(get_opt_optimize() && get_opt_dyn_meth_dispatch()))
    return n;

  ir_node *field_ptr = get_Load_ptr(n);

  if (get_irn_op(field_ptr) != op_Sel) return n;

  entity *ent  = get_Sel_entity(field_ptr);
  ir_node *new_node;

  if ((get_entity_allocation(ent) != allocation_static)    ||
      (get_entity_variability(ent) != variability_constant)  )
    return n;

  /* If the entity is a leave in the inheritance tree,
     we can replace the Sel by a constant. */
  if ((get_irp_phase_state() != phase_building) && (get_entity_n_overwrittenby(ent) == 0)) {
    new_node = copy_const_value(get_atomic_ent_value(ent));
    DBG_OPT_POLY_ALLOC(field_ptr, new_node);

    return new_node;
  }

  /* If we know the dynamic type, we can replace the Sel by a constant. */
  ir_node *ptr = get_Sel_ptr(field_ptr);      /* The address we select from. */
  type *dyn_tp = get_dynamic_type(ptr);  /* The runtime type of ptr. */

  if (dyn_tp != firm_unknown_type) {
    entity *loaded_ent;

    /* We know which method will be called, no dispatch necessary. */
    loaded_ent = resolve_ent_polymorphy(dyn_tp, ent);
    /* called_ent may not be description: has no Address/Const to Call! */
    assert(get_entity_peculiarity(loaded_ent) != peculiarity_description);

    new_node = copy_const_value(get_atomic_ent_value(loaded_ent));
    DBG_OPT_POLY_ALLOC(field_ptr, new_node);

    return new_node;
  }

  return n;
}
