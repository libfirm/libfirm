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

/** Tests the modes of chechnode and its predecessors.
   Checknode must be in current_ir_graph. */
void irn_vrfy (struct ir_node *checknode);

/** Calls irn_vrfy for each node in irg.
   Graph must be in state "pinned".*/
void irg_vrfy (ir_graph *irg);


# endif /* _IRVRFY_H_ */
