/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Lower floating point operations to function calls
 * @author  Sebastian Buchwald
 */
#ifndef FIRM_LOWER_LOWER_SOFTFLOAT_H
#define FIRM_LOWER_LOWER_SOFTFLOAT_H

/**
 * Lowers all floating-point operations.
 *
 * They are replaced by calls into a soft float library.
 */
void lower_floating_point(void);

#endif
