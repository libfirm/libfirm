/**
 * Author:      Daniel Grund
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILL_H_
#define BESPILL_H_

#include "set.h"
#include "pset.h"

#include "be_t.h"
#include "irnode.h"

#include "bearch.h"

typedef struct _spill_env_t spill_env_t;

spill_env_t *be_new_spill_env(const be_main_session_env_t *session,
		const arch_register_class_t *cls);

void be_delete_spill_env(spill_env_t *senv);

void be_add_spill(spill_env_t *senv, ir_node *to_spill, ir_node *before);

void be_add_spill_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos);

void insert_spills_reloads(spill_env_t *senv, pset *mem_phis, pset *reload_set);

#endif /*BESPILL_H_*/
