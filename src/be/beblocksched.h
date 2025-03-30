/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Block schedule calculation.
 * @author      Matthias Braun, Christoph Mallon
 * @date        27.09.2006
 */
#ifndef FIRM_BE_BEBLOCKSCHED_H
#define FIRM_BE_BEBLOCKSCHED_H

#include "firm_types.h"

ir_node **be_create_block_schedule(ir_graph *irg);

#endif
