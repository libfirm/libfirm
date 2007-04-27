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

#ifndef _ARM_TRANSFORM_H_
#define _ARM_TRANSFORM_H_

void arm_move_consts(ir_node *node, void *env);
void arm_move_symconsts(ir_node *node, void *env);

void arm_register_transformers(void);
void arm_transform_node(ir_node *node, void *env);

#endif /* _ARM_TRANSFORM_H_ */
