/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILL_H_
#define BESPILL_H_

#include "firm_types.h"
#include "set.h"
#include "pset.h"
#include "debug.h"

#include "bechordal.h"
#include "be_t.h"

#include "bearch.h"

typedef struct _spill_env_t spill_env_t;
typedef int(*decide_irn_t)(const ir_node*, void*);

/**
 * Creates a new spill environment.
 *
 * @param chordal
 * @param is_mem_phi  a function that evaluates a Phi node
 * @param data        context parameter for the is_mem_phi function
 */
spill_env_t *be_new_spill_env(const be_chordal_env_t *chordal, decide_irn_t is_mem_phi, void *data);

/**
 * Deletes a spill environment.
 */
void be_delete_spill_env(spill_env_t *senv);

void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before);

void be_add_reload_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos);

void be_insert_spills_reloads(spill_env_t *senv, pset *reload_set);

/**
 * Computes the spill offsets for all spill nodes in the irg
 */
void be_compute_spill_offsets(be_chordal_env_t *cenv);

/**
 * Sets the debug module of a spill environment.
 */
DEBUG_ONLY(void be_set_spill_env_dbg_module(spill_env_t *env, firm_dbg_module_t *dbg));

#endif /* BESPILL_H_ */
