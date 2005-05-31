/*
 * Project:     libFIRM
 * File name:   ir/opt/reassoc.h
 * Purpose:     Reassociation
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file reassoc.h
 *
 * Reassociation optimization.
 * Uses the "firm.opt.reassoc" debug space
 *
 * @author Michael Beck
 */
#ifndef _REASSOC_H_
#define _REASSOC_H_

#include "irgraph.h"

/** Reassociation.
 *
 * Applies Reassociation rules to integer expressions.
 * Beware: Works only if integer overflow might be ignored, as for C, Java
 * and for address expression.
 * Works only if Constant folding is activated.
 *
 * Uses loop information to detect loop-invariant (ie contant
 * inside the loop) values.
 *
 * See Muchnik 12.3.1 Algebraic Simplification and Reassociation of
 * Addressing Expressions.
 *
 *
 */
void optimize_reassociation(ir_graph *irg);

#endif /* _REASSOC_H_ */
