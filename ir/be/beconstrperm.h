/**
 * Author:      Daniel Grund
 * Date:		15.12.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 */

#ifndef BECONSTRPERM_H_
#define BECONSTRPERM_H_

#include "bechordal_t.h"

/**
 * Inserts a perm instruction before all instructions having
 * the register requirement "arch_register_req_type_limited"
 */
void be_insert_constr_perms(be_chordal_env_t *cenv);

#endif /*BECONSTRPERM_H_*/
