/*
 * Project:     libFIRM
 * File name:   ir/common/ident_t.h
 * Purpose:     Hash table to store names -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _IDENT_T_H_
# define _IDENT_T_H_

# include "ident.h"

void id_init (int initial_n_idents);

#define ID_HASH(str, len) \
  (((  ((unsigned char *)(str))[0] * 33 \
     + ((unsigned char *)(str))[(len)>>1]) * 31 \
    + ((unsigned char *)(str))[(len)-1]) * 9 \
   + (len))

# endif /* _IDENT_T_H_ */
