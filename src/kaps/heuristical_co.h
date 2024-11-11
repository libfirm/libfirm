/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Heuristic PBQP solver for SSA-based register allocation.
 * @date    18.09.2009
 * @author  Thomas Bersch
 */
#ifndef KAPS_HEURISTICAL_H
#define KAPS_HEURISTICAL_H

#include "pbqp_t.h"

#include "deq.h"

void solve_pbqp_heuristical_co(pbqp_t *pbqp, deq_t *rpeo);

#endif
