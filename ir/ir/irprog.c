/* Copyright (C) 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** irprog.c: ir representation of a program
*/

# include "irprog.h"
# include "array.h"

#define GLOBAL_TYPE_NAME "GlobalType"

/* initializes ir_prog. Calles the constructor for an ir_prog. */
void init_irprog(void) {
  new_ir_prog ();
}

/* Create a new ir prog. Automatically called by init_firm through init_irprog. */
ir_prog *new_ir_prog (void) {
  ir_prog *res;

  res = (ir_prog *) malloc (sizeof(ir_prog));
  irp = res;

  /* res->obst      = (struct obstack *) xmalloc (sizeof (struct obstack)); */
  res->graphs = NEW_ARR_F (ir_graph *, 1);
  res->types  = NEW_ARR_F (type *, 1);
  res->glob_type = new_type_class(id_from_str (GLOBAL_TYPE_NAME,
					       strlen(GLOBAL_TYPE_NAME)));
  add_irp_type((type *)res->glob_type);

  return res;
}


/** Functions to access the fields of ir_prog **/
type_class *get_glob_type(void) {
  assert(irp);
  return irp->glob_type;
}

/* Adds irg to the list of ir graphs in irp. */
void      add_irp_irg(ir_graph *irg) {
  assert (irg != NULL);
  assert(irp);
  ARR_APP1 (ir_graph *, irp->graphs, irg);
}

/* Adds type to the list of types in irp. */
void      add_irp_type(type *typ) {
  assert (typ != NULL);
  assert(irp);
  ARR_APP1 (type *, irp->types, typ);
}
