/*
 * Project:     libFIRM
 * File name:   ir/ir/irprog.c
 * Purpose:     Entry point to the representation of a whole program.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2000
 * CVS-ID:      $Id$
 * Copyright:   (c) 2000-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <string.h>

# include "irprog_t.h"
# include "irgraph_t.h"
# include "array.h"
# include "obst.h"
# include "typegmod.h"

#define GLOBAL_TYPE_NAME "GlobalType"

/* A variable from where everything in the ir can be accessed. */
ir_prog *irp;
ir_prog *get_irp() { return irp; }

/* initializes ir_prog. Calles the constructor for an ir_prog. */
void init_irprog(void) {
  new_ir_prog ();
}

INLINE void remove_irp_type_from_list (type *typ) {
  int i;
  assert(typ);
  for (i = 1; i < (ARR_LEN (irp->types)); i++) {
    if (irp->types[i] == typ) {
      for(; i < (ARR_LEN (irp->types)) - 1; i++) {
	irp->types[i] = irp->types[i+1];
      }
      ARR_SETLEN(type*, irp->types, (ARR_LEN(irp->types)) - 1);
      break;
    }
  }
}

/* Create a new ir prog. Automatically called by init_firm through
   init_irprog. */
ir_prog *new_ir_prog (void) {
  ir_prog *res;

  res = (ir_prog *) malloc (sizeof(ir_prog));
  memset(res, 0, sizeof(res));
  irp = res;
  /* res->obst      = (struct obstack *) xmalloc (sizeof (struct obstack)); */
  res->graphs = NEW_ARR_F (ir_graph *, 1);
  res->types  = NEW_ARR_F (type *, 1);
  res->name   = NULL;

#ifdef DEBUG_libfirm
  res->max_node_nr = 0;
#endif

  res->glob_type = new_type_class(id_from_str (GLOBAL_TYPE_NAME,
					       strlen(GLOBAL_TYPE_NAME)));
  /* Remove type from type list.  Must be treated differently than
     other types. */
  remove_irp_type_from_list(res->glob_type);

  res->const_code_irg = new_const_code_irg();

  res->outs_state = no_outs;
  res->ip_outedges = NULL;

  return res;
}

/* frees all memory used by irp.  Types in type list, irgs in irg
    list and entities in global type must be freed by hand before. */
void     free_ir_prog() {
  free_type(irp->glob_type);
  /* @@@ * free_ir_graph(irp->const_code_irg); * ?? End has no in?? */
  DEL_ARR_F(irp->graphs);
  DEL_ARR_F(irp->types);

  irp->kind = k_BAD;
  irp->const_code_irg = NULL;
}

/*- Functions to access the fields of ir_prog -*/


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

/* Removes irg from the list or irgs, shrinks the list by one. */
void remove_irp_irg(ir_graph *irg){
  int i;
  assert(irg);
  free_ir_graph(irg);
  for (i = 1; i < (ARR_LEN (irp->graphs)); i++) {
    if (irp->graphs[i] == irg) {
      for(; i < (ARR_LEN (irp->graphs)) - 1; i++) {
	irp->graphs[i] = irp->graphs[i+1];
      }
      ARR_SETLEN(ir_graph*, irp->graphs, (ARR_LEN(irp->graphs)) - 1);
      break;
    }
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

void remove_irp_type(type *typ) {
  remove_irp_type_from_list (typ);
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

/*- File name / executable name or the like -*/
void   set_irp_prog_name(ident *name) {
  irp->name = name;
}
ident *get_irp_prog_ident(void) {
  return irp->name;
}
const char  *get_irp_prog_name(void) {
  return get_id_str(irp->name);
}


ir_graph *get_const_code_irg(void)
{
  return irp->const_code_irg;
}

irg_outs_state get_irp_ip_outs_state() {
  return irp->outs_state;
}
void set_irp_ip_outs_inconsistent() {
  irp->outs_state = outs_inconsistent;
}
void      set_irp_ip_outedges(ir_node ** ip_outedges)
{
  irp -> ip_outedges = ip_outedges;
}

ir_node** get_irp_ip_outedges(void)
{
  return(irp -> ip_outedges);
}
