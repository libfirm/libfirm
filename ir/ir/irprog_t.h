/*
 * Project:     libFIRM
 * File name:   ir/ir/irprog_t.h
 * Purpose:     Entry point to the representation of a whole program 0-- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2000
 * CVS-ID:      $Id$
 * Copyright:   (c) 2000-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irprog_t.h
 */

# ifndef _IRPROG_T_H_
# define _IRPROG_T_H_

#ifdef HAVE_CONFIG_H
#include "firm_config.h"
#endif

#include "irprog.h"
#include "irgraph.h"
#include "pseudo_irg.h"
#include "ircgcons.h"
#include "firm_common_t.h"
#include "typegmod.h"

#include "callgraph.h"

#include "array.h"

/** ir_prog */
struct ir_prog {
  firm_kind kind;
  ident     *name;                /**< A file name or the like. */
  ir_graph  *main_irg;            /**< entry point to the compiled program
				       @@@ or a list, in case we compile a library or the like? */
  ir_graph **graphs;              /**< all graphs in the ir */
  ir_graph **pseudo_graphs;       /**< all pseudo graphs in the ir. See pseudo_irg.c */
  ir_graph  *const_code_irg;      /**< This ir graph gives the proper environment
				       to allocate nodes the represent values
				       of constant entities. It is not meant as
				       a procedure.  */
  type      *glob_type;           /**< global type.  Must be a class as it can
			  	       have fields and procedures.  */
  type     **types;               /**< all types in the ir */

  /* -- states of and access to generated information -- */

  ip_view_state ip_view;          /**< State of interprocedural view. */

  irg_outs_state outs_state;      /**< Out edges. */
  ir_node **ip_outedges;          /**< Huge Array that contains all out edges
				       in interprocedural view. */

  irg_callee_info_state callee_info_state; /**< Validity of callee information.
				            Contains the lowest value or all irgs.  */


  irp_callgraph_state callgraph_state; /**< State of the callgraph. */
  struct ir_loop *outermost_cg_loop;   /**< For callgraph analysis: entry point
					    to looptree over callgraph. */
  int max_callgraph_loop_depth;
  int max_callgraph_recursion_depth;

#ifdef DEBUG_libfirm
  long max_node_nr;                /**< to generate unique numbers for nodes. */
#endif
};

INLINE void remove_irp_type_from_list (type *typ);

static INLINE type *
__get_glob_type(void) {
  assert(irp);
  return irp->glob_type = skip_tid(irp->glob_type);
}

static INLINE int
__get_irp_n_irgs(void) {
  assert (irp && irp->graphs);
  if (get_visit_pseudo_irgs()) return get_irp_n_allirgs();
  return (ARR_LEN((irp)->graphs));
}

static INLINE ir_graph *
__get_irp_irg(int pos){
  if (get_visit_pseudo_irgs()) return get_irp_allirg(pos);
  assert(0 <= pos && pos <= get_irp_n_irgs());
  return irp->graphs[pos];
}


static INLINE int
__get_irp_n_types (void) {
  assert (irp && irp->types);
  return (ARR_LEN((irp)->types));
}

static INLINE type *
__get_irp_type(int pos) {
  assert (irp && irp->types);
  /* Don't set the skip_tid result so that no double entries are generated. */
  return skip_tid(irp->types[pos]);
}

#ifdef DEBUG_libfirm
/** Returns a new, unique number to number nodes or the like. */
int get_irp_new_node_nr(void);
#endif

static INLINE ir_graph *
__get_const_code_irg(void)
{
  return irp->const_code_irg;
}

void           set_irp_ip_outedges(ir_node ** ip_outedges);
ir_node**      get_irp_ip_outedges(void);

/** initializes ir_prog. Calls the constructor for an ir_prog. */
void init_irprog(void);

#define get_irp_n_irgs()       __get_irp_n_irgs()
#define get_irp_irg(pos)       __get_irp_irg(pos)
#define get_irp_n_types()      __get_irp_n_types()
#define get_irp_type(pos)      __get_irp_type(pos)
#define get_const_code_irg()   __get_const_code_irg()
#define get_glob_type()        __get_glob_type()

#endif /* ifndef _IRPROG_T_H_ */
