/* Copyright (C) 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** irprog.c: ir representation of a program
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irprog_t.h"
# include "array.h"
# include "obst.h"
# include "typegmod.h"

#define GLOBAL_TYPE_NAME "GlobalType"

/* A variable from where everything in the ir can be accessed. */
ir_prog *irp;

/* initializes ir_prog. Calles the constructor for an ir_prog. */
void init_irprog(void) {
  new_ir_prog ();
}

/* Create a new ir prog. Automatically called by init_firm through
   init_irprog. */
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

#ifdef DEBUG_libfirm
  res->max_node_nr = 1;
#endif

  return res;
}

/** Functions to access the fields of ir_prog **/


/* Access the main routine of the compiled program. */
ir_graph *get_irp_main_irg() {
  assert (irp);
  return irp->main_irg;
}

void set_irp_main_irg(ir_graph *main_irg) {
  assert (irp);
  irp->main_irg = main_irg;
}

type *get_glob_type(void) {
  assert(irp);
  return irp->glob_type = skip_tid(irp->glob_type);
}

/* Adds irg to the list of ir graphs in irp. */
void add_irp_irg(ir_graph *irg) {
  assert (irg != NULL);
  assert(irp && irp->graphs);
  ARR_APP1 (ir_graph *, irp->graphs, irg);
}

/* Removes irg from the list or irgs, shrinks the list by one.
   @@@ does not work properly. */
void remove_irp_irg(ir_graph *irg){
  int i;
  assert(irg);
  for (i = 1; i < (ARR_LEN (irp->graphs))-1; i++)
    if (irp->graphs[i+1] == irg) {
      for(i++; i < (ARR_LEN (irp->graphs)) - 1; i++)
	irp->graphs[i] = irp->graphs[i+1];
      ARR_SETLEN(ir_graph*, irp->graphs, (ARR_LEN(irp->graphs)) - 1);
      break;
    }
}

int get_irp_n_irgs() {
  assert (irp && irp->graphs);
  /* Strangely the first element of the array is NULL.  Why??  */
  return (ARR_LEN((irp)->graphs) - 1);
}

ir_graph *get_irp_irg(int pos){
  assert (irp && irp->graphs);
  /* Strangely the first element of the array is NULL.  Why??  */
  return irp->graphs[pos+1];
}

void set_irp_irg(int pos, ir_graph *irg) {
  assert (irp && irg);
  assert (pos < (ARR_LEN((irp)->graphs) - 1));
  /* Strangely the first element of the array is NULL.  Why??  */
  irp->graphs[pos+1] = irg;
}

/* Adds type to the list of types in irp. */
void add_irp_type(type *typ) {
  assert (typ != NULL);
  assert(irp);
  ARR_APP1 (type *, irp->types, typ);
}

int get_irp_n_types (void) {
  assert (irp && irp->types);
  /* Strangely the first element of the array is NULL.  Why??  */
  return (ARR_LEN((irp)->types) - 1);
}

type *get_irp_type(int pos) {
  assert (irp && irp->types);
  /* Strangely the first element of the array is NULL.  Why??  */
  /* Don't set the skip_tid result so that no double entries are generated. */
  return skip_tid(irp->types[pos+1]);
}

void  set_irp_type(int pos, type *typ) {
  assert (irp && typ);
  assert (pos < (ARR_LEN((irp)->types) - 1));
  /* Strangely the first element of the array is NULL.  Why??  */
  irp->types[pos+1] = typ;
}

#ifdef DEBUG_libfirm
int get_irp_new_node_nr() {
  assert(irp);
  irp->max_node_nr = irp->max_node_nr + 1;
  return irp->max_node_nr - 1;
}
#endif
