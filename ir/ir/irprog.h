/* Copyright (C) 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Goetz Lindenmaier
*
* irprog.h: ir representation of a program
*
* This file defines a construct that keeps all information about a
* program:
*   - A list of all procedures.
*   - A list of all types.
*   - A global type that can be thought of as a god-class containing all
*     global variables and procedures.  This is not the base class of
*     all classes in a class hierarchy (as, e.g., "object" in java).
*   - (An obstack containing global things, e.g., the above mentioned lists.)
*/

/* $Id$ */

# ifndef _IRPROG_H_
# define _IRPROG_H_

# include "irnode.h"
# include "type.h"

/**
 *
 * NAME  Datastructure that holds central information about a program
 *
 *   Preliminary documentation ;-)
 *
 */

/**
 *
 * NAME  Datastructure that holds central information about a program
 *
 *   Preliminary documentation ;-)
 *
 *  main_irg  The ir graph that is the entry point to the program.
 *            (Anything not reachable from here may be optimized away.
 *            If we want to translate libraries or the like correctly
 *            we must replace this by a list.)
 *  irg       List of all ir graphs in the program.
 *  type      A list containing all types known to the translated program.
 *            Some types can have several entries in this list (as a result of
 *            using exchange_types()).
 *  glob_type The unique global type that is owner of all global entities.
 *
 */
typedef struct ir_prog ir_prog;

/* A variable from where everything in the ir can be accessed. */
extern ir_prog *irp;
ir_prog *get_irp();

/* initializes ir_prog. Calls the constructor for an ir_prog. */
void init_irprog(void);

/* Creates a new ir_prog, returns it and sets irp with it.
   Automatically called by init_firm through init_irprog.  */
ir_prog *new_ir_prog (void);

/* Access the main routine of the compiled program. */
ir_graph *get_irp_main_irg();
void      set_irp_main_irg(ir_graph *main_irg);

/* Adds irg to the list of ir graphs in irp. */
void      add_irp_irg(ir_graph *irg);
/* Removes irg from the list of irgs, deallocates it and
   shrinks the list by one. */
void      remove_irp_irg(ir_graph *irg);
int       get_irp_n_irgs();
ir_graph *get_irp_irg(int pos);
void      set_irp_irg(int pos, ir_graph *irg);

/* Adds type to the list of types in irp. */
void  add_irp_type(type *typ);
/* Removes type from the list of types, deallocates it and
   shrinks the list by one. */
void  remove_irp_type(type *typ);
int   get_irp_n_types();
type *get_irp_type(int pos);
void  set_irp_type(int pos, type *typ);

/** Functions to access the fields of ir_prog **/
type *get_glob_type(void);


/**
 *
 *   Returns an irgraph that only contains constant
 *   expressions for constant entities.
 *   Do not use any access function for this graph, do not generate code
 *   for this graph.  This graph contains only one block.  The constant
 *   expressions may not contain control flow.  See also copy_const_code
 *   in entity.h.
 */
ir_graph *get_const_code_irg();

#endif /* ifndef _IRPROG_H_ */
