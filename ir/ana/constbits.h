#ifndef CONSTBITS_H
#define CONSTBITS_H

#include "adt/obst.h"
#include "tv.h"

typedef struct bitinfo
{
	ir_tarval* z; /* safe zeroes, 0 = bit is zero,       1 = bit maybe is 1 */
	ir_tarval* o; /* safe ones,   0 = bit maybe is zero, 1 = bit is 1 */
} bitinfo;

/* Get analysis information for node irn */
bitinfo* get_bitinfo(ir_node const* const irn);

/* Set analysis information for node irn */
int set_bitinfo(ir_node* const irn, ir_tarval* const z, ir_tarval* const o);

/* Compute value range fixpoint aka which bits of value are constant zero/one.
 * The result is available via links to bitinfo*, allocated on client_obst. */
void constbits_analyze(ir_graph* const irg, struct obstack *client_obst);

#endif
