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

typedef enum {
  irg_typeinfo_none,         /**< No typeinfo computed, calls to set/get_irn_type
  				  are invalid. */
  irg_typeinfo_consistent,   /**< Type info valid, calls to set/get_irn_type return
				  the proper type. */
  irg_typeinfo_inconsistent  /**< Type info can be accessed, but it can be invalid
				  because of other transformations. */
} irg_typeinfo_state;

void               set_irg_typeinfo_state(ir_graph *irg, irg_typeinfo_state s);
irg_typeinfo_state get_irg_typeinfo_state(ir_graph *irg);

/* ------------ Irnode type information. ------------------------------ */

/** Accessing the type information.
 *
 * These routines only work properly if the ir_graph is in state
 * irg_typeinfo_consistent or irg_typeinfo_inconsistent.  They
 * assume current_ir_graph set properly.
 */
type *get_irn_typeinfo_type(ir_node *n);
void  set_irn_typeinfo_type(ir_node *n, type *tp);

/** Return the type associated with the value produced by n
 *  if the node remarks this type as it is the case for
 *  Cast, Const, SymConst and some Proj nodes. */
type *get_irn_type(ir_node *n);

#endif /* _IRTYPEINFO_H_ */
