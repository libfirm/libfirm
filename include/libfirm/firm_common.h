/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    common firm declarations
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 * @version  $Id$
 */
#ifndef FIRM_COMMON_FIRM_COMMON_H
#define FIRM_COMMON_FIRM_COMMON_H

#include "firm_types.h"

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
   * used before definition. The function should insert a default value,
   * and/or raise a compiler error/warning. Note that returning
   * an Unknown is allowed here.
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
   * The default calling convention.
   */
  unsigned cc_mask;

  /**
   * The debug info that should be used for "builtin" objects.
   */
  dbg_info *builtin_dbg;
};

typedef struct _firm_parameter_t firm_parameter_t;

/**
 * Initialize the firm library.
 *
 * Initializes the firm library.  Allocates default data structures.
 * Initializes configurable behavior of the library.
 *
 * @param params   A structure containing the parameters of the libFirm.
 *
 * The parameter struct may be NULL. In that case, the original FIRM behavior
 * is conserved.
 */
void ir_init(const firm_parameter_t *params);

/**
 * Frees all memory occupied by the firm library.
 */
void ir_finish(void);

/** returns the libFirm major version number */
unsigned ir_get_version_major(void);
/** returns libFirm minor version number */
unsigned ir_get_version_minor(void);
/** returns string describing libFirm revision */
const char *ir_get_version_revision(void);
/** returns string describing libFirm build */
const char *ir_get_version_build(void);



/** a list of firm kinds
 @@@ not all datatypes are tagged yet. */
typedef enum {
	k_BAD = 0,                /**< An invalid firm node. */
	k_entity,                 /**< An entity. */
	k_type,                   /**< A type. */
	k_ir_graph,               /**< An IR graph. */
	k_ir_node,                /**< An IR node. */
	k_ir_mode,                /**< An IR mode. */
	k_ir_op,                  /**< An IR opcode. */
	k_tarval,                 /**< A tarval. */
	k_ir_loop,                /**< A loop. */
	k_ir_compound_graph_path, /**< A compound graph path, see entity.h. */
	k_ir_extblk,              /**< An extended basic block. */
	k_ir_prog,                /**< A program representation (irp). */
	k_ir_region,              /**< A region. */
	k_ir_graph_pass,          /**< An ir_graph pass. */
	k_ir_prog_pass,           /**< An ir_prog pass. */
	k_ir_graph_pass_mgr,      /**< An ir_graph pass manager. */
	k_ir_prog_pass_mgr,       /**< An ir_prog pass manager. */
	k_ir_max                  /**< maximum value -- illegal for firm nodes. */
} firm_kind;

/**
 * Returns the kind of a thing.
 *
 * @param firm_thing  pointer representing a firm object
 */
firm_kind get_kind(const void *firm_thing);

/** Returns the kind of a thing as a string. */
const char *print_firm_kind(void *firm_thing);

/** Print an identification of a firm thing. */
void firm_identify_thing(void *X);

#endif
