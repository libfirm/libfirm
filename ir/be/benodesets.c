#include "benodesets.h"
#include "irnode_t.h"

/*
 * Calculates a hash value for a node.
 *
 * Use its node number
 */
unsigned nodeset_hash(ir_node *n) {
	return (unsigned)get_irn_node_nr(n);
}
