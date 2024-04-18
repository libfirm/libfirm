/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Contains some useful function for the backend.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_BEUTIL_H
#define FIRM_BE_BEUTIL_H

#include "entity_t.h"
#include "firm_types.h"
#include "irprog_t.h"

#include "be_types.h"

/**
 * Returns an array (an ARR_F) of the programs blocks in reverse postorder
 * (note: caller has to free the memory with DEL_ARR_F after use;
 *  of course you can use ARR_LEN on the array too.)
 */
ir_node **be_get_cfgpostorder(ir_graph *irg);

static inline bool is_tls_entity(const ir_entity *const ent)
{
	return get_entity_owner(ent) == get_tls_type();
}

/**
 * Insert a Perm which permutes all (non-ignore) live values of a given register class
 * before a certain instruction.
 * @param lv        Liveness Information.
 * @param irn       The node to insert the Perm before.
 * @return          The Perm or NULL if nothing was live before @p irn.
 */
ir_node *insert_Perm_before(ir_graph *irg, const arch_register_class_t *cls,
						   ir_node *irn);

/**
 * Removes dead nodes from schedule
 * @param irg  the graph
 */
void be_remove_dead_nodes_from_schedule(ir_graph *irg);

/**
 * Keep the given node alive, if it has no users, by adding a be_Keep.
 *
 * @param node  The node to kept alive.
 */
void be_keep_if_unused(ir_node *node);

#endif
