/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILL_H_
#define BESPILL_H_

#include "set.h"
#include "pset.h"
#include "irnode.h"
#include "debug.h"

#include "be_t.h"

#include "bearch.h"

typedef struct _spill_env_t spill_env_t;
typedef int(*decide_irn_t)(const ir_node*, void*);


spill_env_t *be_new_spill_env(
		firm_dbg_module_t *dbg,
		const be_main_session_env_t *session,
		const arch_register_class_t *cls,
		decide_irn_t is_mem_phi,
		void *data);

void be_delete_spill_env(spill_env_t *senv);

void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before);

void be_add_reload_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos);

void be_insert_spills_reloads(spill_env_t *senv, pset *reload_set);

#endif /*BESPILL_H_*/
