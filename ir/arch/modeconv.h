/*
 * Project:     libFIRM
 * File name:   ir/arch/modeconv.h
 * Purpose:     integer mode conversion
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id:
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file modeconv.h
 *
 * Contains the mode conversion for architectures that did not support lesser
 * integer modes. Converts all Op(mode_l) into Op(mode) operations by adding
 * conversions were needed. These Conv operations must be implemented in the backend
 * as bit-reducing ops.
 *
 * @author Michael Beck
 */
#ifndef _MODECONV_H_
#define _MODECONV_H_

#include "irgraph.h"

/** Mode conversion..
 *
 * Converts all operations with a integer mode lesser than mode into
 * operations of type mode. Adds Conv() operations only were strictly
 * needed.
 */
void arch_mode_conversion(ir_graph *irg, ir_mode *mode);

#endif /* _MODECONV_H_ */
