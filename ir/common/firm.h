/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer, Goetz Lindenmaier.
*/

/**
 @file firm.h

    Central FIRM header.

    FIRM is a full graph based intermediate representation in SSA Form
    with a novel concept to model side effects.  It allows fast, aggressive
    optimizations.

    This header is the central header of the library implementation of this
    IR.

    The internal representation of a program in firm is separated into five
    different modules:
     - Firm Graphs representing the code of a program. (Subdirectory ir.)
       Firm Graphs are assembled out of several data structures:
       irprog: represents a program.  Allows access to all types and all
         FIRM graphs for procedures and other global things.
       irgraph: represents a procedure.  Allows access to the code of the
         procedure, the actual FIRM graph.
       irnode: A node of a FIRM graph.  Nodes are typed with an opcode and a mode
	 and represent instructions in a program.
       irop: The opcode of FIRM nodes.
       irmode: The mode of FIRM nodes.  Most modes correspond to machine known
         data types (int, float, pointer).
     - Entities representing program known objects. (Subdirectory tr.)
       All variables and procedures are entities.
     - Types describing the type system for the program. (Subdirectory tr.)
     - Target Values representing program known constants. (Subdirectory tv.)
     - Identifiers representing any Strings used in the program. (Subdirectory ident.)

     Further this library supplies functionality to build and optimize FIRM graphs
     and further functionality needed in a compiler.  Finally there is more
     generic functionality to support implementations using firm.  (Code generation,
     further optimizations).

     ircons: Interface to construct firm graphs, implements automatic Phi node
       construction.
     iropt: Optimizations applied to individual nodes.
     irgopt: Optimizations for ir graphs.

     irflag: Flags to direct the functionality.
     common: dynamic typ check for all nodes,  configuration of the library,
     debug:  ???

     irgwalk: walker for ir graphs.
     irvrfy:  verify the correctness of a firm node.
*
*/

/* $Id$ */

# ifndef _FIRM_H_
# define _FIRM_H_

/* The representations */
# include "irprog.h"     /* control flow and data of a program */
# include "type.h"       /* type representation */
# include "entity.h"     /* entity representation */
# include "tv.h"         /* target values */
# include "ident.h"      /* source code identificators */
/* Functionality */
# include "ircons.h"     /* construct ir */
# include "ircgcons.h"   /* construct interprocedural graph */

# include "irflag.h"     /* optimization flags */
# include "irgopt.h"     /* optimize ir */
# include "ircgopt.h"    /* Optimizations based on interprocedural graph */

# include "irdom.h"      /* Dominator analysis */
# include "cgana.h"      /* Analysis to construct interprocedural graph */
                         /* including some optimizations */
# include "irloop.h"     /* loop and backedge analysis */

# include "irgmod.h"     /* Support to modify ir */
# include "irgwalk.h"    /* Support to walk ir */
# include "typewalk.h"   /* Support to walk type information */
# include "typegmod.h"   /* Support to modify type graph */
# include "mangle.h"     /* Support for mangling ident names. */

/* @@@ temporarily for jni builder until preprocessor works.
   Then it should be sufficient to include <file.h> instead
   of firm.h as not all enums are needed in the impelmentation
   files. */
# include "irdump.h"

/**
 * Initialize the firm library.
 */
void init_firm (void);

# endif /* _FIRM_H_ */
