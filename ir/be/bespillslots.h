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
 * Adds a node that needs a frame entity and consumes memory (Reload nodes). The
 * memory edges are followed to find memory-phis and the entities that produce
 * the memory values.
 *
 * @param env   The frame entity coalescer environment
 * @param node  The node that uses the frame entity
 * @param mode  The mode of the needed spillslot
 * @param align The alignment of the needed spillslot
 */
void be_node_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_mode *mode, int alignment);

typedef void (*set_frame_entity_func)(ir_node *node, ir_entity *entity);

/**
 * Assigned frame entities to all the nodes added by be_node_needs_frame_entity.
 * Adds memory perms where needed.
 */
void be_assign_entities(be_fec_env_t *env, set_frame_entity_func set_frame,
                        bool alloc_entities_at_begin, bool coalescing_allowed);

#endif
