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
# include "set.h"

void init_ident (int initial_n_idents);
void finish_ident (void);

#define ID_HASH(str, len) \
  (((  ((unsigned char *)(str))[0] * 33 \
     + ((unsigned char *)(str))[(len)>>1]) * 31 \
    + ((unsigned char *)(str))[(len)-1]) * 9 \
   + (len))


/* ------------------------ *
 * inline functions         *
 * ------------------------ */
extern set *__id_set;

static INLINE ident *
__id_from_str(const char *str, int len)
{
  assert(len > 0);
  return set_hinsert0(__id_set, str, len, ID_HASH(str, len));
}

static INLINE const char * __get_id_str(ident *id) { return (const char *)id->dptr; }

static INLINE int __get_id_strlen(ident *id) { return id->size; }


#define new_id_from_chars(str, len)    __id_from_str(str, len)
#define get_id_str(id)           __get_id_str(id)
#define get_id_strlen(id)        __get_id_strlen(id)

# endif /* _IDENT_T_H_ */
