/*
 * Project:     libFIRM
 * File name:   ir/st/bs.h
 * Purpose:     Provides a simple bit set.
 * Author:      Florian Liekweg
 * Modified by:
 * Created:     4.3.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
