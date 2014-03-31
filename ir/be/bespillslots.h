/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Spillslot coalescer.
 * @author      Matthias Braun
 * @date        27.07.2006
 */
#ifndef FIRM_BE_BESPILLSLOTS_H
#define FIRM_BE_BESPILLSLOTS_H

#include <stdbool.h>

typedef struct be_fec_env_t be_fec_env_t;

/**
 * Initializes a new frame entity coalescer environment
 */
be_fec_env_t *be_new_frame_entity_coalescer(ir_graph *irg);

/**
 * Frees a frame entity coalescer environment
 */
void be_free_frame_entity_coalescer(be_fec_env_t *env);

/**
 * forbid coalescing of spillslots.
 */
void be_forbid_coalescing(be_fec_env_t *env);

void be_load_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_type *type);

typedef void (*set_frame_entity_func)(ir_node *node, ir_entity *entity,
                                      const ir_type *final_type);

/**
 * Assigned frame entities to all the nodes added by be_node_needs_frame_entity.
 * Adds memory perms where needed.
 */
void be_assign_entities(be_fec_env_t *env, set_frame_entity_func set_frame,
                        bool alloc_entities_at_begin);

#endif
