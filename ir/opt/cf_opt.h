/*
 * Project:     libFIRM
 * File name:   ir/opt/cfopt.h
 * Purpose:     control flow optimizations
 * Author:      Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file cfopt.h
 *
 * Control flow optimization.
 *
 * @author Goetz Lindenmaier
 */
#ifndef _CFOPT_H_
#define _CFOPT_H_

#include "irgraph.h"

/** Control flow optimization.
 *
 * Removes empty blocks doing if simplifications and loop simplifications.
 * A block is empty if it contains only a Jmp node and Phi nodes.
 * Merges single entry single exit blocks with their predecessor
 * and propagates dead control flow by calling equivalent_node.
 * Independent of compiler flag it removes Tuples from cf edges,
 * Bad predecessors form Blocks and Phis, and unnecessary predecessors of End.
 *
 * @bug So far destroys backedge information.
 * @bug Chokes on Id nodes if called in a certain order with other
 *      optimizations.  Call local_optimize_graph() before to remove
 *      Ids.
 */
void optimize_cf(ir_graph *irg);

#endif /* _CFOPT_H_ */
