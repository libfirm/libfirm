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
 * @param func  @see default_initialize_local_variable_func_t
 */
void init_cons (default_initialize_local_variable_func_t *func);

/* inline functions */

static INLINE ir_node *
__new_d_Bad(void) {
  return current_ir_graph->bad;
}

static INLINE ir_node *
__new_d_NoMem(void) {
  return current_ir_graph->no_mem;
}


#define new_d_Bad()               __new_d_Bad()
#define new_d_NoMem()             __new_d_NoMem()

#endif /* _IRCONS_T_H_ */
