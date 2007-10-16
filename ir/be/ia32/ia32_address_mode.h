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
 * @brief       This file contains functions for matching firm graphs for
 *              nodes that can be used as address mode for x86 instructions
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifndef IA32_ADDRESS_MODE_H
#define IA32_ADDRESS_MODE_H

#include "irtypes.h"

/**
 * The address mode data: Used to construct (memory) address modes.
 */
typedef struct ia32_address_t ia32_address_t;
struct ia32_address_t {
    ir_node   *base;             /**< The base register (if any) */
    ir_node   *index;            /**< The index register (if any). */
	ir_node   *mem;              /**< The memory value (if any). */
    int        offset;           /**< An integer offset. */
    int        scale;            /**< An integer scale. {0,1,2,3} */
    ir_entity *symconst_ent;     /**< A SynConst entity if any. */
    int        use_frame;        /**< Set, if the frame is accessed */
    ir_entity *frame_entity;     /**< The accessed frame entity if any. */
    int        symconst_sign;    /**< The "sign" of the symconst. */
};

/**
 * Create an address mode for a given node.
 */
void ia32_create_address_mode(ia32_address_t *addr, ir_node *node, int force);

/**
 * Mark those nodes of the given graph that cannot be used inside an
 * address mode because there values must be materialized in registers.
 */
void calculate_non_address_mode_nodes(ir_graph *irg);

/**
 * Free the non_address_mode information.
 */
void free_non_address_mode_nodes(void);

#endif
