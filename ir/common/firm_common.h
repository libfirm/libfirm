/*
 * Project:     libFIRM
 * File name:   ir/common/firm_common.c
 * Purpose:     Stuff common to all firm modules.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file firm_common.h
 *
 * common firm declarations
 *
 * @author Martin Trapp, Christian Schaefer & Goetz Lindenmaier
 */

# ifndef _FIRM_COMMON_H_
# define _FIRM_COMMON_H_

#ifndef INLINE
#ifdef USE_GCC_INLINE
#define INLINE inline
#else
#define INLINE
#endif
#endif

/** a list of firm kinds
 @@@ not all datatypes are tagged yet. */
typedef enum {
  k_BAD = 0,    /**< an invalid firm node */
  k_entity,     /**< an entity */
  k_type,       /**< a type */
  k_ir_graph,   /**< an ir graph */
  k_ir_node,    /**< an ir node */
  k_ir_mode,    /**< an ir mode */
  k_ir_op,      /**< an ir opcode */
  k_tarval,     /**< a tarval */
  k_ir_loop,    /**< a loop */
  k_ir_compound_graph_path, /**< a compound graph path, see entity.h */
  k_ir_max      /**< maximum value -- illegal for firm nodes. */
} firm_kind;

/**
 * Returns the kind of a thing.
 *
 * @param firm_thing  pointer representing a firm object
 */
firm_kind get_kind(const void *firm_thing);

/** Returns the kind of a thing as a string. */
const char* print_firm_kind(void *firm_thing);

/** Print an identification of a firm thing. */
void firm_identify_thing(void *X);

# endif /*_FIRM_COMMON_H_ */
