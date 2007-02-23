/*
 * Author:      Matthias Braun
 * Date:		27.7.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILLSLOTS_H_
#define BESPILLSLOTS_H_

#include "beirg.h"

typedef struct _be_fec_env_t be_fec_env_t;

/**
 * Initializes a new frame entity coalescer environment
 */
be_fec_env_t *be_new_frame_entity_coalescer(be_irg_t *birg);

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

/**
 * Assigned frame entities to all the nodes added by be_node_needs_frame_entity.
 * Adds memory perms where needed.
 */
void be_assign_entities(be_fec_env_t *env);

//-------------------------------------------------------------------
// Old API
//-------------------------------------------------------------------

/**
 * Assigns frame entities to all spill nodes in the irg.
 * Coalesces spillslots and minimizes the number of memcopies induced by
 * memory-phis.
 */
void be_coalesce_spillslots(be_irg_t *birg);

#endif /* BESPILLSLOTS_H_ */
