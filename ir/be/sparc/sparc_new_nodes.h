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
 * @brief   Function prototypes for the assembler ir node constructors.
 * @author  Hannes Rapp, Matthias Braun
 */
#ifndef FIRM_BE_SPARC_SPARC_NEW_NODES_H
#define FIRM_BE_SPARC_SPARC_NEW_NODES_H

#include <stdbool.h>
#include "sparc_nodes_attr.h"

/**
 * Returns the attributes of an sparc node.
 */
sparc_attr_t *get_sparc_attr(ir_node *node);
const sparc_attr_t *get_sparc_attr_const(const ir_node *node);

bool sparc_has_load_store_attr(const ir_node *node);
sparc_load_store_attr_t *get_sparc_load_store_attr(ir_node *node);
const sparc_load_store_attr_t *get_sparc_load_store_attr_const(const ir_node *node);

sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr(ir_node *node);
const sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr_const(const ir_node *node);

sparc_switch_jmp_attr_t *get_sparc_switch_jmp_attr(ir_node *node);
const sparc_switch_jmp_attr_t *get_sparc_switch_jmp_attr_const(const ir_node *node);

sparc_fp_attr_t *get_sparc_fp_attr(ir_node *node);
const sparc_fp_attr_t *get_sparc_fp_attr_const(const ir_node *node);

sparc_fp_conv_attr_t *get_sparc_fp_conv_attr(ir_node *node);
const sparc_fp_conv_attr_t *get_sparc_fp_conv_attr_const(const ir_node *node);

sparc_permi_attr_t *get_sparc_permi_attr(ir_node *node);
const sparc_permi_attr_t *get_sparc_permi_attr_const(const ir_node *node);

sparc_permi23_attr_t *get_sparc_permi23_attr(ir_node *node);
const sparc_permi23_attr_t *get_sparc_permi23_attr_const(const ir_node *node);

/* Include the generated headers */
#include "gen_sparc_new_nodes.h"

#endif
