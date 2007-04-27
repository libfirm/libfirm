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

/*
 * Project:     libFIRM
 * File name:   ir/st/bs.h
 * Purpose:     Provides a simple bit set.
 * Author:      Florian Liekweg
 * Modified by:
 * Created:     4.3.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 */

/**
   @file bs.h

   Provides a simple bit set.

   Not quite complete
*/


# ifndef _BS_H_
# define _BS_H_

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

# endif /* ndef _BS_H_ */
