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
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

# include "irprog_t.h"
# include "irgraph_t.h"
# include "pseudo_irg.h"
# include "array.h"
# include "obst.h"
# include "typegmod.h"

#define GLOBAL_TYPE_NAME "GlobalType"
#define INITAL_PROG_NAME "no_name_set"

/* A variable from where everything in the ir can be accessed. */
ir_prog *irp;
ir_prog *get_irp(void) { return irp; }

/* initializes ir_prog. Calles the constructor for an ir_prog. */
void init_irprog(void) {
  new_ir_prog ();
}

void remove_irp_type_from_list (type *typ) {
  int i;
  assert(typ);
#if 0
  for (i = 0; i < (ARR_LEN (irp->types)); i++) {
#else
  for (i = ARR_LEN (irp->types) -1; i >= 0; i--) {
#endif
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

  res = xmalloc (sizeof(*res));
  memset(res, 0, sizeof(*res));
  irp = res;
  /* res->obst      = xmalloc (sizeof(*res->obst)); */
  res->graphs        = NEW_ARR_F (ir_graph *, 0);
  res->pseudo_graphs = NEW_ARR_F (ir_graph *, 0);
  res->types  = NEW_ARR_F (type *, 0);
  res->name   = new_id_from_str(INITAL_PROG_NAME);

#ifdef DEBUG_libfirm
  res->max_node_nr = 0;
#endif

  res->glob_type = new_type_class(new_id_from_str (GLOBAL_TYPE_NAME));
  /* Remove type from type list.  Must be treated differently than
     other types. */
  remove_irp_type_from_list(res->glob_type);

  res->const_code_irg = new_const_code_irg();

  res->outs_state   = outs_none;
  res->ip_outedges  = NULL;
  res->trouts_state = outs_none;

  return res;
}

/* frees all memory used by irp.  Types in type list, irgs in irg
    list and entities in global type must be freed by hand before. */
void     free_ir_prog(void) {
  free_type(irp->glob_type);
  /* @@@ * free_ir_graph(irp->const_code_irg); * ?? End has no in?? */
  DEL_ARR_F(irp->graphs);
  DEL_ARR_F(irp->types);

  irp->kind = k_BAD;
  irp->const_code_irg = NULL;
}

/*- Functions to access the fields of ir_prog -*/


/* Access the main routine of the compiled program. */
ir_graph *get_irp_main_irg(void) {
  assert (irp);
  return irp->main_irg;
}

void set_irp_main_irg(ir_graph *main_irg) {
  assert (irp);
  irp->main_irg = main_irg;
}

type *(get_glob_type)(void) {
  return __get_glob_type();
}

/* Adds irg to the list of ir graphs in irp. */
void add_irp_irg(ir_graph *irg) {
  assert (irg != NULL);
  assert(irp && irp->graphs);
  ARR_APP1 (ir_graph *, irp->graphs, irg);
}

/* Removes irg from the list or irgs, shrinks the list by one. */
void remove_irp_irg_from_list(ir_graph *irg){
  int i, found = false;
  assert(irg);
  for (i = 0; i < (ARR_LEN (irp->graphs)); i++) {
    if (irp->graphs[i] == irg) {
      found = true;
      for(; i < (ARR_LEN (irp->graphs)) - 1; i++) {
        irp->graphs[i] = irp->graphs[i+1];
      }
      ARR_SETLEN(ir_graph*, irp->graphs, (ARR_LEN(irp->graphs)) - 1);
      break;
    }
  }
  if (!found) {
    for (i = 0; i < (ARR_LEN (irp->pseudo_graphs)); i++) {
      if (irp->pseudo_graphs[i] == irg) {
	      for(; i < (ARR_LEN (irp->pseudo_graphs)) - 1; i++) {
	        irp->pseudo_graphs[i] = irp->pseudo_graphs[i+1];
	      }
	      ARR_SETLEN(ir_graph*, irp->pseudo_graphs, (ARR_LEN(irp->pseudo_graphs)) - 1);
	      break;
      }
    }
  }
}

/* Removes irg from the list or irgs, shrinks the list by one. */
void remove_irp_irg(ir_graph *irg){
  assert(irg);
  free_ir_graph(irg);
  remove_irp_irg_from_list(irg);
}

int (get_irp_n_irgs)(void) {
  return __get_irp_n_irgs();
}

ir_graph *(get_irp_irg)(int pos){
  return __get_irp_irg(pos);
}

void set_irp_irg(int pos, ir_graph *irg) {
  assert (irp && irg);
  assert (pos < (ARR_LEN((irp)->graphs)));
  irp->graphs[pos] = irg;
}

/* Gets the number of graphs _and_ pseudo graphs. */
int       get_irp_n_allirgs(void) {
  /* We can not call get_irp_n_irgs, as we end up in a recursion ... */
  return ARR_LEN((irp)->graphs) + get_irp_n_pseudo_irgs();
}

/* Returns the ir graph at position pos of all graphs (including
 pseudo graphs).  Visits first graphs, then pseudo graphs. */
ir_graph *get_irp_allirg(int pos) {
  int n_irgs = ARR_LEN((irp)->graphs);
  assert(0 <= pos);
  if (pos < n_irgs) {
    return (irp)->graphs[pos];
  } else {
    return get_irp_pseudo_irg(pos-n_irgs);
  }
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

int (get_irp_n_types) (void) {
  return __get_irp_n_types();
}

type *(get_irp_type) (int pos) {
  return __get_irp_type(pos);
}

void  set_irp_type(int pos, type *typ) {
  assert (irp && typ);
  assert (pos < (ARR_LEN((irp)->types)));
  irp->types[pos] = typ;
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
int irp_prog_name_is_set(void) {
  return irp->name != new_id_from_str(INITAL_PROG_NAME);
}
ident *get_irp_prog_ident(void) {
  return irp->name;
}
const char  *get_irp_prog_name(void) {
  return get_id_str(irp->name);
}


ir_graph *(get_const_code_irg)(void)
{
  return __get_const_code_irg();
}

irg_phase_state get_irp_phase_state(void) {
  return irp->phase_state;
}
void           set_irp_phase_state(irg_phase_state s) {
  irp->phase_state = s;
}

irg_outs_state get_irp_ip_outs_state() {
  return irp->outs_state;
}

void set_irp_ip_outs_inconsistent() {
  irp->outs_state = outs_inconsistent;
}

void      set_irp_ip_outedges(ir_node ** ip_outedges)
{
  irp->ip_outedges = ip_outedges;
}

ir_node** get_irp_ip_outedges(void)
{
  return irp->ip_outedges;
}


irg_callee_info_state get_irp_callee_info_state(void) {
  return irp->callee_info_state;
}

void set_irp_callee_info_state(irg_callee_info_state s) {
  irp->callee_info_state = s;
}
