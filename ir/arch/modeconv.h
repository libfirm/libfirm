/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief     integer mode conversion
 * @author    Michael Beck
 * @version   $Id$
 * @summary
 *  Contains the mode conversion for architectures that did not support lesser
 *  integer modes. Converts all Op(mode_l) into Op(mode) operations by adding
 *  conversions were needed. These Conv operations must be implemented in the
 *  backend as bit-reducing ops.
 */
#ifndef FIRM_ARCH_MODECONV_H
#define FIRM_ARCH_MODECONV_H

#include "irgraph.h"

/** Mode conversion..
 *
 * Converts all operations with a integer mode lesser than mode into
 * operations of type mode. Adds Conv() operations only were strictly
 * needed.
 */
void arch_mode_conversion(ir_graph *irg, ir_mode *mode);

#endif
