/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
