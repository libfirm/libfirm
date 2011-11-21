/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 */
#ifndef FIRM_IDENT_IDENT_T_H
#define FIRM_IDENT_IDENT_T_H

#include "ident.h"

/**
 * Initialize the ident module.
 */
void init_ident(void);

/**
 * Finishes the ident module, frees all entries.
 */
void finish_ident(void);

/** initializes the name mangling code */
void firm_init_mangle(void);

#endif
