/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

# include "ident.h"
# include "entity.h"
# include "type.h"


void init_mangle (void);

ident *mangle_entity (entity *ent);
ident *mangle_type (type *type);
