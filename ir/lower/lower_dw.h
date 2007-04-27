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
 * @brief   Lower Double word operations, ie 64bit -> 32bit, 32bit -> 16bit etc.
 * @date    8.10.2004
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_LOWER_LOWER_DW_H
#define FIRM_LOWER_LOWER_DW_H

#include "firm_types.h"

/**
 * A callback type for creating an intrinsic entity for a given opcode.
 *
 * @param method   the method type of the emulation function entity
 * @param op       the emulated ir_op
 * @param imode    the input mode of the emulated opcode
 * @param omode    the output mode of the emulated opcode
 * @param context  the context parameter
 */
typedef ir_entity *(create_intrinsic_fkt)(ir_type *method, const ir_op *op,
                                          const ir_mode *imode, const ir_mode *omode,
                                          void *context);

/**
 * The lowering parameter description.
 */
typedef struct _lwrdw_param_t {
	int enable;                   /**< if true lowering is enabled */
	int little_endian;            /**< if true should be lowered for little endian, else big endian */
	ir_mode *high_signed;         /**< the double word signed mode to be lowered, typically Ls */
	ir_mode *high_unsigned;       /**< the double word unsigned mode to be lowered, typically Lu */
	ir_mode *low_signed;          /**< the word signed mode to be used, typically Is */
	ir_mode *low_unsigned;        /**< the word unsigned mode to be used, typically Iu */

	/** callback that creates the intrinsic entity */
	create_intrinsic_fkt *create_intrinsic;
	void *ctx;                    /**< context parameter for the creator function */
} lwrdw_param_t;

/**
 * Lower all double word operations.
 */
void lower_dw_ops(const lwrdw_param_t *param);

/**
 * Default implementation. Context is unused.
 */
ir_entity *def_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                    const ir_mode *imode, const ir_mode *omode,
                                    void *context);

#endif /* FIRM_LOWER_LOWER_DW_H */
