/**
 * @file irlwrdw.h
 * @date 8.10.2004
 * @author Michael Beck
 * @brief Lower Double word operations, ie Mode L -> I
 *
 * $Id$
 */

#ifndef _FIRM_LOWER_DW_H
#define _FIRM_LOWER_DW_H

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

#endif /* _FIRM_LOWER_DW_H */
