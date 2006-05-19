/*
 * Project:     libFIRM
 * File name:   ir/ir/iropt.h
 * Purpose:     iropt --- optimizations of an ir node.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file iropt.h
 *
 * Declarations for optimizations of an ir node.
 *
 * @author Martin Trapp, Christian Schaefer
 */
#ifndef _FIRM_IR_IROPT_H_
#define _FIRM_IR_IROPT_H_

#include "firm_types.h"

/** If the expression referenced can be evaluated statically
 *  computed_value returns a tarval representing the result.
 *  Else returns tarval_bad. */
tarval *computed_value(ir_node *n);

/** Applies all optimizations to n that are expressible as a pattern
 *  in Firm, i.e., they need not a walk of the graph.
 *  Returns a better node for n.  Does not free n -- other nodes could
 *  reference n.
 *
 *  An equivalent optimization is applied in the constructors defined in
 *  ircons.ch.  There n is freed if a better node could be found.
 */
ir_node *optimize_in_place(ir_node *n);

#endif /* _FIRM_IR_IROPT_H_ */
