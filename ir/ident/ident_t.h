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
 * @brief    Hash table to store names -- private header.
 * @author   Goetz Lindenmaier
 * @version  $Id$
 */
#ifndef FIRM_IDENT_IDENT_T_H
#define FIRM_IDENT_IDENT_T_H

#include "ident.h"

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
#define ID_HASH(type, str, len) \
  (((  ((type *)(str))[0] * 33 \
     + ((type *)(str))[(len)>>1]) * 31 \
    + ((type *)(str))[(len)-1]) * 9 \
   + (len))

#endif
