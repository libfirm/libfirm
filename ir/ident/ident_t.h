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

/**
 * Initialize the ident module.
 *
 * @param id_if             The ident module interface, if NULL, the default
 *                          libFirm ident module will be used.
 * @param initial_n_idents  Only used in the default libFirm ident module, initial
 *                          number of entries in the hash table.
 */
void init_ident (ident_if_t *id_if, int initial_n_idents);

/**
 * Finishes the ident module, frees all entries.
 */
void finish_ident (void);

/** The hash function of the internal ident module implementation. */
#define ID_HASH(str, len) \
  (((  ((unsigned char *)(str))[0] * 33 \
     + ((unsigned char *)(str))[(len)>>1]) * 31 \
    + ((unsigned char *)(str))[(len)-1]) * 9 \
   + (len))

# endif /* _IDENT_T_H_ */
