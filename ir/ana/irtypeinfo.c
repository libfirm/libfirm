/**
 *
 * @file irtypeinfo.c
 *
 * Project:     libFIRM
 * File name:   ir/ana/irtypeinfo.c
 * Purpose:     Data structure to hold type information for nodes.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     28.8.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Data structure to hold type information for nodes.
 *
 * This module defines a field "type" of type "type *" for each ir node.
 * It defines a flag for irgraphs to mark whether the type info of the
 * graph is valid.  Further it defines an auxiliary type "initial_type".
 *
 * The module defines a map that contains pairs (irnode, type).  If an irnode
 * is not in the map it is assumed to be initialized, i.e., the initialization
 * requires no compute time.  As firm nodes can not be freed and reallocated
 * pointers for nodes are unique (until a call of dead_node_elimination).
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irtypeinfo.h"

#include <stddef.h>

#include "irgraph_t.h"   /* for setting the state flag. */
#include "irprog.h"
#include "pmap.h"

/* ------------ The map. ---------------------------------------------- */


static pmap *type_node_map = NULL;


/* ------------ Auxiliary type. --------------------------------------- */

/*  This auxiliary type expresses that a field is uninitialized.  The
 *  variable is set by init_irtypeinfo.  The type is freed by
 *  free_irtypeinfo.
 */
type *initial_type = NULL;


/* ------------ Initializing this module. ----------------------------- */

/*  Initializes the type information module.
 *  Generates a type "initial_type" and sets the type of all nodes to this type.
 *  Calling set/get_irn_type is invalid before calling init. Requires memory
 *  in the order of MIN(<calls to set_irn_type>, #irnodes).
 */
void init_irtypeinfo(void) {
  int i;

  if (!initial_type)
    initial_type = new_type_class(new_id_from_str("initial_type"));

  /* We need a new, empty map. */
  if (type_node_map) pmap_destroy(type_node_map);
  type_node_map = pmap_create();

  for (i = 0; i < get_irp_n_irgs(); ++i)
    set_irg_typeinfo_state(get_irp_irg(i), irg_typeinfo_none);

}

void free_irtypeinfo(void) {
  int i;

  if (initial_type) {
    free_type(initial_type);
    initial_type = NULL;
  } else
    assert(0 && "call init_type_info before freeing");


  if (type_node_map) {
    pmap_destroy(type_node_map);
    type_node_map = NULL;
  } else
    assert(0 && "call init_type_info before freeing");

  for (i = 0; i < get_irp_n_irgs(); ++i)
    set_irg_typeinfo_state(get_irp_irg(i), irg_typeinfo_none);
}


/* ------------ Irgraph state handling. ------------------------------- */

void set_irg_typeinfo_state(ir_graph *irg, irg_typeinfo_state s) {
  assert(is_ir_graph(irg));
  irg->typeinfo_state = s;
}

irg_typeinfo_state get_irg_typeinfo_state(ir_graph *irg) {
  assert(is_ir_graph(irg));
  return irg->typeinfo_state;
}

/* ------------ Irnode type information. ------------------------------ */

/* These routines only work properly if the ir_graph is in state
 * irg_typeinfo_consistent or irg_typeinfo_inconsistent.  They
 * assume current_ir_graph set properly.
 */
type *get_irn_type(ir_node *n) {
  type *res = initial_type;
  assert(get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_consistent  ||
	 get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_inconsistent  );

  if (pmap_contains(type_node_map, (void *)n))
    res = (type *) pmap_get(type_node_map, (void *)n);

  return res;
}

void set_irn_type(ir_node *n, type *tp) {
  assert(get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_consistent  ||
  	 get_irg_typeinfo_state(current_ir_graph) == irg_typeinfo_inconsistent  );

  pmap_insert(type_node_map, (void *)n, (void *)tp);
}
