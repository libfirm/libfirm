/**
 * Project:     libFIRM
 * File name:   ir/opt/opt_osr.h
 * Purpose:     Operator Strength Reduction,
 *              Keith D. Cooper, L. Taylor Simpson, Christopher A. Vick
 * Author:      Michael Beck
 * Modified by:
 * Created:     12.5.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _OPT_OSR_H_
#define _OPT_OSR_H_

#include "firm_types.h"

enum osr_flags {
	osr_flag_none               = 0,  /**< no additional flags */
	osr_flag_lftr_with_ov_check = 1,  /**< do only linear function test replacement
	                                       if no overflow occurs. */
	/** default setting */
	osr_flag_default = osr_flag_lftr_with_ov_check
} osr_flags;

/**
 * Do the Operator Scalar Replacement optimization and linear
 * function test replacement for loop control.
 *
 * @param irg    the graph which should be optimized
 * @param flags  one of osr_flags
 *
 * The linear function replacement test is controlled by the flags.
 * If the osr_flag_lftr_with_ov_check is set, the replacement is only
 * done if do overflow can occur.
 * Otherwise it is ALWAYS done which might be unsure.
 *
 * For instance:
 *
 * for (i = 0; i < 100; ++i)
 *
 * might be replaced by
 *
 * for (i = 0; i < 400; i += 4)
 *
 * But
 *
 * for (i = 0; i < 0x7FFFFFFF; ++i)
 *
 * will not be replaced by
 *
 * for (i = 0; i < 0xFFFFFFFC; i += 4)
 *
 * because of overflow.
 *
 * Note that i < a + 400 is also not possible with the current implementation
 * although this might be allowed by other compilers...
 */
void opt_osr(ir_graph *irg, unsigned flags);

#endif /* _OPT_OSR_H_ */
