/* Copyright (c) 2002 by Universität Karlsruhe (TH).  All Rights Reserved */

/**
   NAME
     bs
   PURPOSE
     provide bs_t
   S
     not quite complete
   HISTORY
     liekweg - Feb 27, 2002: Created.
   CVS:
     $Id$
***/

# ifndef _BS_H_
# define _BS_H_

typedef long int bs_t;

# define bs_set(bs, i) (bs) |= (0x00000001 << i)
# define bs_get(bs, i) (bs) &  (0x00000001 << i)

# define bs_and(bsa, bsb) (bsa) &= (bsb)
# define bs_or(bsa, bsb)  (bsa) |= (bsb)
# define bs_xor(bsa, bsb) (bsa) ^= (bsb)

# define bs_zro(bs) (0x00000000 != bs)

# endif /* ndef _BS_H_ */
