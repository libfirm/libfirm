
/**
 * @file   bespillilp.h
 * @date   27.07.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BESPILLILP_H
#define _BESPILLILP_H

#include "bearch.h"
#include "be_t.h"

void be_spill_ilp(const be_main_session_env_t *env,
    const arch_register_class_t *cls);

#endif /* _BESPILLILP_H */
