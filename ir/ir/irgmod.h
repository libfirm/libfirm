/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod.h: ir graph modification*/

# include "irgraph.h"
# include "ircons.h"
# include "array.h"
# include "misc.h"

/* ir_node *arg_access (ir_mode *mode, long proj); */


inline ir_node *get_r_value_internal (ir_node *block, int pos, ir_mode *mode);


/** Needed only during ir_graph construction. Should all go into
    ircons.ch **/

/* Read a store.
   Use this function to get the most recent version of the store (type M).
   Internally it does the same as get_value. */
ir_node *get_store (void);

/* write a store */
void set_store (ir_node *store);

/* add a control flow edge */
void add_in_edge (ir_node *block, ir_node *jmp);

/* read a value from the array with the local variables */
ir_node *get_value (int pos, ir_mode *mode);

/* write a value in the array with the local variables */
void set_value (int pos, ir_node *value);

/* fixes the number of predecessors of a block. */
void mature_block (ir_node *block);

/* sets current block */
void switch_block (ir_node *target);


/** always useful, e.g. for optimizations.  **/
/* turns a node into a tuple. The tuples predecessors have to be
   set by hand. */
void turn_into_tuple (ir_node *node, int arity);

/* exchanges two nodes by conserving edges leaving old (i.e., pointers
   pointing to old. */
inline void exchange (ir_node *old, ir_node *new);
