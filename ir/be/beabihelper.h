/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for handling ABI constraints in the code
 *              selection phase.
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_BEABI_HELPER_H
#define FIRM_BE_BEABI_HELPER_H

#include "firm_types.h"
#include "be_types.h"
#include "bearch.h"

typedef struct be_stackorder_t    be_stackorder_t;

/**
 * Adds a X->Proj->Keep for each output value of X which has no Proj yet
 */
void be_add_missing_keeps(ir_graph *irg);

/**
 * Make sure all outputs of a node are used, add keeps otherwise
 */
void be_add_missing_keeps_node(ir_node *node);

/**
 * In the normal firm representation some nodes like pure calls, builtins
 * have no memory inputs+outputs. However in the backend these sometimes have to
 * access the stack to work and therefore suddenly need to be enqueued into the
 * memory edge again.
 * This API creates a possible order to enqueue them so we can be sure to create
 * a legal dependency graph when transforming them.
 */
be_stackorder_t *be_collect_stacknodes(ir_graph *irg);

/**
 * return node that should produce the predecessor stack node in a block.
 * returns NULL if there's no predecessor in the current block.
 */
ir_node *be_get_stack_pred(const be_stackorder_t *env, const ir_node *node);

/**
 * free memory associated with a stackorder structure
 */
void be_free_stackorder(be_stackorder_t *env);

/**
 * In case where a parameter is transmitted via register but someone takes its
 * address a store to the frame which can be references is necessary.
 * This function can be used as a preprocessing phase before transformation to
 * do this. The assumption is that all parameter_entities which are passed
 * through the stack are already moved to the arg_type and all remaining
 * parameter_entities on the frame type need stores.
 */
void be_add_parameter_entity_stores(ir_graph *irg);

#endif
