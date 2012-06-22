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
 */
#ifndef FIRM_COMMON_FIRM_COMMON_H
#define FIRM_COMMON_FIRM_COMMON_H

#include "firm_types.h"
#include "begin.h"

/**
 * Initializes the firm library.  Allocates default data structures.
 */
FIRM_API void ir_init(void);

/**
 * Frees all memory occupied by the firm library.
 */
FIRM_API void ir_finish(void);

/** returns the libFirm major version number */
FIRM_API unsigned ir_get_version_major(void);
/** returns libFirm minor version number */
FIRM_API unsigned ir_get_version_minor(void);
/** returns string describing libFirm revision */
FIRM_API const char *ir_get_version_revision(void);
/** returns string describing libFirm build */
FIRM_API const char *ir_get_version_build(void);

/**
 * a list of firm kinds
 * Most important datastructures in firm contain a firm_kind field at the
 * beginning so given void* pointer you can usually still guess the kind
 * of thing the pointer points to.
 * This is used in debug helper functions and printers.
 */
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
	k_ir_prog,                /**< A program representation (irp). */
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
FIRM_API firm_kind get_kind(const void *firm_thing);

#include "end.h"

#endif
