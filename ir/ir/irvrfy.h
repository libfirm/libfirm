/*
 * Project:     libFIRM
 * File name:   ir/ir/irvrfy.h
 * Purpose:     Check irnodes for correctness.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier. Till Riedel
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file irvrfy.h
*
* ir graph verification.
*
* @author Christian Schaefer
*/

# ifndef _IRVRFY_H_
# define _IRVRFY_H_

# include "irnode.h"
# include "irgraph.h"

typedef enum _node_verification_t {
  NODE_VERIFICATION_OFF        = 0,	/**< do not verify nodes at all */
  NODE_VERIFICATION_ON         = 1,	/**< do node verification and assert on error in debug version */
  NODE_VERIFICATION_REPORT     = 2,	/**< do node verification, but report to stderr only */
  NODE_VERIFICATION_ERROR_ONLY = 3	/**< do node verification, but NEVER do assert nor report */
} node_verification_t;

/** Select verification of nodes.
 *
 *  Per default the  verification is in mode NODE_VERIFICATION_ASSERT.
 *  Turn the verification off during development to check partial implementations.
 */
void do_node_verification(node_verification_t mode);

/**
 * Tests the modes of checknode and its predecessors.
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
 * Same as irn_vrfy_irg, but temporary sets verification mode to
 * NODE_VERIFICATION_ERROR_ONLY.
 * \return
 * 	NON-zero on success
 */
int irn_vrfy_irg_dump(struct ir_node *checknode, ir_graph *irg, const char **bad_string);

/**
 * Calls irn_vrfy for each node in irg.
 * Graph must be in state "op_pin_state_pinned".
 *
 * \return
 * 	NON-zero on success.
 */
int irg_vrfy(ir_graph *irg);

/**
 * Possible flags for irg_vrfy_bads().
 */
enum verify_bad_flags_t {
  BAD_CF      = 1,	/**< Bad nodes are allowed as predecessors of Blocks and Phis. */
  BAD_DF      = 2,	/**< Bad nodes are allowed as dataflow predecessors. */
  BAD_BLOCK   = 4,	/**< Bad nodes are allowed as Block input. */
  TUPLE       = 8	/**< Tuple nodes are allowed. */
};

/**
 * Verify occurance of bad nodes in a graph.
 *
 * @param irg    The graph to verify
 * @param flags  combination of verify_bad_flags_t flags describing
 *               which Bads are allowed
 * @returns      a value combined of verify_bad_flags_t indicating the problems found.
 */
int irg_vrfy_bads(ir_graph *irg, int flags);

/**
 *  Enable/disable verification of Load/Store nodes with
 *  its entities. If disabled, Store(SymConst(array)) will be allowed
 *  (C-frontend builds this :-)
 */
void vrfy_enable_entity_tests(int enable);

# endif /* _IRVRFY_H_ */
