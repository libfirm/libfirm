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

/**
 * Convenient block getter.
 * Works also, if the given node is a block.
 * @param  irn The node.
 * @return The block of the node, or the node itself, if the node is a
 *         block.
 */
static inline ir_node *get_block(ir_node *irn)
{
	return is_Block(irn) ? irn : get_nodes_block(irn);
}

static inline const ir_node *get_block_const(const ir_node *irn)
{
	return is_Block(irn) ? irn : get_nodes_block(irn);
}

/**
 * Gets the Proj with number pn from irn.
 */
ir_node *be_get_Proj_for_pn(const ir_node *irn, long pn);

/**
 * Returns an array (an ARR_F) of the programs blocks in reverse postorder
 * (note: caller has to free the memory with DEL_ARR_F after use;
 *  of course you can use ARR_LEN on the array too.)
 */
ir_node **be_get_cfgpostorder(ir_graph *irg);

static inline bool is_tls_entity(ir_entity *const ent)
{
	return get_entity_owner(ent) == get_tls_type();
}

#endif
