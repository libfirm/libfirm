/*
 * Project:     libFIRM
 * File name:   ir/ir/ircons_t.h
 * Purpose:     Various irnode constructors.  Automatic construction
 *              of SSA representation.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _IRCONS_T_H_
#define _IRCONS_T_H_

#include "ircons.h"
# include "irgraph_t.h"

/**
 * Initializes the graph construction.
 *
 * @param func  callback that is called if a uninitialized
 *              variable is detected
 *
 * @see uninitialized_local_variable_func_t
 */
void init_cons(uninitialized_local_variable_func_t *func);

/* inline functions */

static INLINE ir_node *
_new_d_Bad(void) {
  return current_ir_graph->anchors[anchor_bad];
}

static INLINE ir_node *
_new_d_NoMem(void) {
  return current_ir_graph->anchors[anchor_no_mem];
}


#define new_d_Bad()               _new_d_Bad()
#define new_d_NoMem()             _new_d_NoMem()

#endif /* _IRCONS_T_H_ */
