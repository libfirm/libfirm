/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*
*/

/* $Id$ */

# include "ident.h"
# include "entity.h"
# include "type.h"


void init_mangle (void);

/* Computes a definite name for this entity by concatenating
   the name of the owner type and the name of the entity with
   a separating "_". f*/
ident *mangle_entity (entity *ent);

/* Sorry, I'm not sure what this does... seems to copy the string. */
ident *mangle_type   (type *tp);

/* mangle underscore: Returns a new ident that represents first_scnd. */
ident *mangle_u (ident *first, ident* scnd);


/* mangle: Returns a new ident that represents firstscnd. */
ident *mangle (ident *first, ident* scnd);
