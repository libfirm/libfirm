/*
 * Project:     libFIRM
 * File name:   ir/tr/mangle.h
 * Purpose:     Methods to manipulate names.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file mangle.h
*
* FIRM name mangling -- methods to manipulate names.
*
* @author Martin Trapp, Christian Schaefer
*/

#ifndef _MANGLE_H_
#define _MANGLE_H_

# include "ident.h"
# include "entity.h"
# include "type.h"

/** initializes the name mangling code */
void   init_mangle (void);

/** Computes a definite name for this entity by concatenating
   the name of the owner type and the name of the entity with
   a separating "_". */
ident *mangle_entity (entity *ent);

/** mangle underscore: Returns a new ident that represents first_scnd. */
ident *mangle_u (ident *first, ident* scnd);

/** mangle: Returns a new ident that represents firstscnd. */
ident *mangle (ident *first, ident* scnd);

#endif /* _MANGLE_H_ */
