#ifndef FP_VRP_H
#define FP_VRP_H

#include "adt/obst.h"

typedef struct vrp_bitinfo
{
	ir_tarval* z; /* safe zeroes, 0 = bit is zero,       1 = bit maybe is 1 */
	ir_tarval* o; /* safe ones,   0 = bit maybe is zero, 1 = bit is 1 */
} vrp_bitinfo;

/* Get VRP analysis information for node irn */
vrp_bitinfo* get_bitinfo(ir_node const* const irn);

/* Set VRP analysis information for node irn */
int set_bitinfo(ir_node* const irn, ir_tarval* const z, ir_tarval* const o);

/* Compute VRP fixpoint.  The Result is available via links to
 * vrp_bitinfo*, allocated on client_obst. */
void fp_vrp_analyze(ir_graph* const irg, struct obstack *client_obst);

#endif
