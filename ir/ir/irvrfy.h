/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
* @file irvrfy.h
*
* ir graph verification.
*
* @author Christian Schaefer
*/

/* $Id$ */

# ifndef _IRVRFY_H_
# define _IRVRFY_H_

# include "irnode.h"
# include "irgraph.h"

/**
 * Tests the modes of chechnode and its predecessors.
 * Checknode must be in current_ir_graph.
 *
 * \return
 * 	NON-zero on success
 */
int irn_vrfy(struct ir_node *checknode);

/**
 * Tests the modes of checknode and its predecessors.
 * Checknode must be in given ir_graph.
 *
 * \return
 * 	NON-zero on success
 */
int irn_vrfy_irg(struct ir_node *checknode, ir_graph *irg);


/**
 * Calls irn_vrfy for each node in irg.
 * Graph must be in state "pinned".
 *
 * \return
 * 	NON-zero on success.
 */
int irg_vrfy(ir_graph *irg);


# endif /* _IRVRFY_H_ */
