/**
 *
 * @file irtypeinfo.h
 *
 * Project:     libFIRM
 * File name:   ir/ana/irtypeinfo.h
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
 * graph is valid.  Further it defines an auxiliary type "init_type".
 *
 */


# ifndef _IRTYPEINFO_H_
# define _IRTYPEINFO_H_

# include "irgraph.h"
# include "irnode.h"
# include "type.h"

/* ------------ Auxiliary type. --------------------------------------- */

/** An auxiliary type used to express that a field is uninitialized.
 *
 *  This auxiliary type expresses that a field is uninitialized.  The
 *  variable is set by init_irtypeinfo.  The type is freed by
 *  free_irtypeinfo.
 */
extern type *initial_type;



/* ------------ Initializing this module. ----------------------------- */

/** Initializes the type information module.
 *
 *  Initializes the type information module.
 *  Generates a type "init_type" and sets the type of all nodes to this type.
 *  Calling set/get_irn_type is invalid before calling init. Requires memory
 *  in the order of MIN(<calls to set_irn_type>, #irnodes).
 */
void init_irtypeinfo(void);
void free_irtypeinfo(void);

/* ------------ Irgraph state handling. ------------------------------- */

/*
#define irg_typeinfo_none         ir_typeinfo_none
#define irg_typeinfo_consistent   ir_typeinfo_consistent
#define irg_typeinfo_inconsistent ir_typeinfo_inconsistent
#define irg_typeinfo_state        ir_typeinfo_state
*/

typedef enum {
  ir_typeinfo_none,         /**< No typeinfo computed, calls to set/get_irn_type
  				  are invalid. */
  ir_typeinfo_consistent,   /**< Type info valid, calls to set/get_irn_type return
				  the proper type. */
  ir_typeinfo_inconsistent  /**< Type info can be accessed, but it can be invalid
				  because of other transformations. */
} ir_typeinfo_state;

void              set_irg_typeinfo_state(ir_graph *irg, ir_typeinfo_state s);
ir_typeinfo_state get_irg_typeinfo_state(ir_graph *irg);

/** Returns accumulated type information state information.
 *
 * Returns ir_typeinfo_consistent if the type information of all irgs is
 * consistent.  Returns ir_typeinfo_inconsistent if at least one irg has inconsistent
 * or no type information.  Returns ir_typeinfo_none if no irg contains type information.
 */
ir_typeinfo_state get_irp_typeinfo_state(void);
void              set_irp_typeinfo_state(ir_typeinfo_state s);
/** If typeinfo is consistent, sets it to inconsistent. */
void              set_irp_typeinfo_inconsistent(void);

/* ------------ Irnode type information. ------------------------------ */

/** Accessing the type information.
 *
 * These routines only work properly if the ir_graph is in state
 * ir_typeinfo_consistent or ir_typeinfo_inconsistent.  They
 * assume current_ir_graph set properly.
 */
type *get_irn_typeinfo_type(ir_node *n);
void  set_irn_typeinfo_type(ir_node *n, type *tp);

#endif /* _IRTYPEINFO_H_ */
