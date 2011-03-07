/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Spillslot coalescer.
 * @author      Matthias Braun
 * @date        27.07.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BESPILLSLOTS_H
#define FIRM_BE_BESPILLSLOTS_H

#include <stdbool.h>
#include "beirg.h"

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
                        bool alloc_entities_at_begin);

#endif
