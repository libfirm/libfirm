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
 * @brief      Provides a simple bit set.
 * @author     Florian Liekweg
 * @date       4.3.2002
 * @version    $Id$
 * @note       Not quite complete
 */
#ifndef FIRM_ST_BS_H
#define FIRM_ST_BS_H

/**
 * the type of a bit set
 */
typedef long int bs_t;

/** set bit in a bit set */
# define bs_set(bs, i) (bs) |= (0x00000001 << i)

/** get bit in a bit set */
# define bs_get(bs, i) (bs) &  (0x00000001 << i)

/** logical AND of two bit sets */
# define bs_and(bsa, bsb) (bsa) &= (bsb)

/** logical OR of two bit sets */
# define bs_or(bsa, bsb)  (bsa) |= (bsb)

/** logical XOR of two bit sets */
# define bs_xor(bsa, bsb) (bsa) ^= (bsb)

/** returns TRUE if at least one bit is set */
# define bs_zro(bs) (0x00000000 != bs)

#endif
