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
 *              nodes that can be used as addressmode for x86 commands
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifndef IA32_ADDRESS_MODE_H
#define IA32_ADDRESS_MODE_H

#include "irtypes.h"

typedef struct ia32_address_t ia32_address_t;
struct ia32_address_t {
    ir_node   *base;
    ir_node   *index;
	ir_node   *mem;
    int        offset;
    int        scale;
    ir_entity *symconst_ent;
    int        use_frame;
    ir_entity *frame_entity;
    int        symconst_sign;
};

void ia32_create_address_mode(ia32_address_t *addr, ir_node *node, int force);

void calculate_non_address_mode_nodes(ir_graph *irg);

void free_non_address_mode_nodes(void);

#endif
