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
#include "config.h"
#endif

#include "irprog.h"
#include "firm_common_t.h"


/** ir_prog */
struct ir_prog {
  firm_kind kind;
  ir_graph  *main_irg;            /**< entry point to the compiled program
                     or a list, in case we compile a library or the like? */
  ir_graph **graphs;              /**< all graphs in the ir */
  type      *glob_type;           /**< global type.  Must be a class as it can
				     have fields and procedures.  */
  type     **types;               /**< all types in the ir */
  ir_graph  *const_code_irg;      /**< This ir graph gives the proper environment
				     to allocate nodes the represent values
				     of constant entities. It is not meant as
				     a procedure.  */
  ident     *name;
  /*struct obstack *obst;	   * @@@ Should we place all types and
                                     entities on an obstack, too? */

#ifdef DEBUG_libfirm
  long max_node_nr;                /**< to generate unique numbers for nodes. */
#endif
};

INLINE void remove_irp_type_from_list (type *typ);

#ifdef DEBUG_libfirm
/** Returns a new, unique number to number nodes or the like. */
int get_irp_new_node_nr(void);
#endif

#endif /* ifndef _IRPROG_T_H_ */
