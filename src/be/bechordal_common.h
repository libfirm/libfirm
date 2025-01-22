/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Common functions for chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 */

#ifndef BECHORDAL_COMMON_H_
#define BECHORDAL_COMMON_H_

#include "bechordal.h"
#include "beinsn_t.h"

/**
 * Annotate the register pressure to the nodes and compute
 * the liveness intervals.
 * @param block The block to do it for.
 * @param env_ptr The environment.
 */
void create_borders(ir_node *block, void *env_ptr);

/**
 * Insert perm nodes
 * @param env The chordal environment.
 * @param the_insn The current be_insn node.
 * @return The new perm node.
 */
ir_node *pre_process_constraints(be_chordal_env_t *_env, be_insn_t **the_insn);

#endif
