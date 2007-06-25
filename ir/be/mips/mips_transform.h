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
 * @brief   declarations for code transform (code selection)
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifndef FIRM_BE_MIPS_MIPS_TRANSFORM_H
#define FIRM_BE_MIPS_MIPS_TRANSFORM_H

/**
 * Create Firm assembler for a copyB node.
 *
 * @param blk   the block where to place the code
 * @param node  the copyB node to lower
 *
 * @return the memory from the lowered CopyB
 */
ir_node *gen_code_for_CopyB(ir_node *blk, ir_node *node);

ir_node *mips_create_Immediate(long offset);
ir_node *mips_create_zero(void);

void mips_transform_graph(mips_code_gen_t *cg);
void mips_after_ra_walker(ir_node *node, void *env);

#endif
