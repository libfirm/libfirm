/*
 * Project:     libFIRM
 * File name:   ir/common/firm.h
 * Purpose:     Central firm header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file firm.h
 *
 *  Central FIRM header.
 *
 *  FIRM is a full graph based intermediate representation in SSA Form
 *  with a novel concept to model side effects.  It allows fast, aggressive
 *  optimizations.
 *
 *  This header is the central header of the library implementation of this
 *  IR.
 *
 *  The internal representation of a program in firm is separated into five
 *  different modules:
 *   - Firm Graphs representing the code of a program. (Subdirectory ir.)
 *     Firm Graphs are assembled out of several data structures:
 *     irprog: represents a program.  Allows access to all types and all
 *       FIRM graphs for procedures and other global things.
 *     irgraph: represents a procedure.  Allows access to the code of the
 *       procedure, the actual FIRM graph.
 *     irnode: A node of a FIRM graph.  Nodes are typed with an opcode and a mode
 *   and represent instructions in a program.
 *     irop: The opcode of FIRM nodes.
 *     irmode: The mode of FIRM nodes.  Most modes correspond to machine known
 *       data types (int, float, pointer).
 *   - Entities representing program known objects. (Subdirectory tr.)
 *     All variables and procedures are entities.
 *   - Types describing the type system for the program. (Subdirectory tr.)
 *   - Target Values representing program known constants. (Subdirectory tv.)
 *   - Identifiers representing any Strings used in the program. (Subdirectory ident.)
 *
 *   Further this library supplies functionality to build and optimize FIRM graphs
 *   and further functionality needed in a compiler.  Finally there is more
 *   generic functionality to support implementations using firm.  (Code generation,
 *   further optimizations).
 */
#ifndef _FIRM_H_
#define _FIRM_H_

#ifdef __cplusplus
extern "C" {
#endif

/* The representations */
#include "firm_common.h"   /* common type tags. */
#include "irprog.h"        /* control flow and data of a program */
#include "type.h"          /* type representation */
#include "entity.h"        /* entity representation */
#include "tv.h"            /* target values */
#include "ident.h"         /* source code identificators */

/* Functionality */
#include "ircons.h"        /* construct ir */
#include "ircgcons.h"      /* construct interprocedural graph */

/* Optimizations */
#include "irflag.h"         /* optimization flags */
#include "irgopt.h"         /* optimize ir */
#include "reassoc.h"        /* optimize ir by reassociation */
#include "ldstopt.h"        /* optimize Load/Store */
#include "cfopt.h"          /* optimize control flow */
#include "tailrec.h"        /* optimize tail-recursion calls */
#include "ircgopt.h"        /* Optimizations based on interprocedural graph */
#include "strength_red.h"   /* Strength reduction */
#include "loop_unrolling.h" /* Do loop unrolling */
#include "ifconv.h"         /* if conversion */
#include "funccall.h"       /* real function call optimization */

/* Analyses */
#include "irouts.h"         /* Graph reversal / out edges. */
#include "irdom.h"          /* Dominator analysis */
#include "cgana.h"          /* Analysis to construct interprocedural graph */
                            /* including some optimizations */
#include "irloop.h"         /* loop and backedge analysis */
#include "callgraph.h"      /* Callgraph construction */
#include "interval_analysis.h"
#include "field_temperature.h"
#include "execution_frequency.h"

/* Support */
#include "irgmod.h"         /* Support to modify ir */
#include "irgwalk.h"        /* Support to walk ir */
#include "typewalk.h"       /* Support to walk type information */
#include "typegmod.h"       /* Support to modify type graph */
#include "type_identify.h"  /* Support for type identification */
#include "mangle.h"         /* Support for mangling ident names. */
#include "tr_inheritance.h" /* Support to handle inheritance. */

#include "irarch.h"        /* architecture dependant optimizations */
//#include "modeconv.h"      /* architecture dependant mode conversion */

#include "firmstat.h"      /* statistics */

#include "irreflect.h"     /* reflection */


/* @@@ temporarily for jni builder until preprocessor works.
   Then it should be sufficient to include <file.h> instead
   of firm.h as not all enums are needed in the implementation
   files. */
#include "irdump.h"
#include "irprintf.h"
#include "irvrfy.h"
#include "trvrfy.h"

#include "irarch.h"

#include "iredges.h"

/* Makros that define the old function names we decided to rename.
   Use for compatibility with old implementations. */
/*#include "old_fctnames.h"*/

/**
 * libFirm initialization parameters.
 */
struct _firm_parameter_t {
  /**
   * The size of this structure. init_firm() will only initialize
   * this amount of data. This allows to add more fields to this structure
   * without breaking compatibility to older source.
   */
  unsigned int size;

  /**
   * Statistic options. If statistic function where enabled, these
   * flags configure it, see enum firmstat_options_t.
   */
  unsigned enable_statistics;

  /**
   * This function is called, whenever a local variable is
   * used before definition.  The function should either insert a default value,
   * or raise a compiler error/warning.
   */
  uninitialized_local_variable_func_t *initialize_local_func;

  /**
   * The interface functions for the type identification module.
   * If not set, the default implementation with compare_strict() and
   * firm_hash_name() will be used.
   */
  type_identify_if_t *ti_if;

  /**
   * The interface for the ident module.
   * If not set, the default libFirm ident module (using hash sets).
   */
  ident_if_t *id_if;

	/**
	 * The factory function for the architecture dependent
	 * optimizations.
	 */

};


typedef struct _firm_parameter_t firm_parameter_t;



/**
 * Initialize the firm library.
 *
 * Initializes the firm library.  Allocates default data structures.
 * Initializes configurable behaviour of the library.
 *
 * @param param    This function is called, whenever a local variable is
 * used before definition.  The function should either insert a default value,
 * or raise a compiler error.
 *
 * The parameter func may be NULL. In that case, the original FIRM behavior
 * is conserved.
 *
 * @see default_initialize_local_variable_func_t
 */
void init_firm(const firm_parameter_t *params);

/**
 * Frees all memory occupied by the firm library.
 */
void free_firm(void);

#ifdef __cplusplus
}
#endif

#endif /* _FIRM_H_ */
