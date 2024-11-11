/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Access function for backedges -- private header.
 * @author    Goetz Lindenmaier
 * @date      7.2002
 */
#ifndef FIRM_ANA_IRBACKEDGE_T_H
#define FIRM_ANA_IRBACKEDGE_T_H

#include "bitset.h"
#include "firm_types.h"

/**
 * Allocate a new backedge array on the obstack for given size.
 *
 * @param obst   the obstack to allocate the array on
 * @param size   the size of the backedge array
 */
bitset_t *new_backedge_arr(struct obstack *obst, size_t size);

/**
 * Adapts backedges array to new size.
 * Must be called if the in array of an IR node is changed.  Else
 * Segmentation faults might occur.
 */
void fix_backedges(struct obstack *obst, ir_node *n);

#endif
