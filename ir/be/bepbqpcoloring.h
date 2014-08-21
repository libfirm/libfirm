/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       PBQP based register allocation.
 * @author      Thomas Bersch
 * @date        27.11.2009
 */
#ifndef BEPBQPALLOC_H_
#define BEPBQPALLOC_H_

/**
 * PBQP based register allocation.
 * @param env The chordal environment.
 */
void be_pbqp_coloring(be_chordal_env_t *env);

#endif
