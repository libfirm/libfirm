#ifndef CONSTBITS_H
#define CONSTBITS_H

#include <stdbool.h>
#include "adt/obst.h"
#include "tv.h"

typedef struct bitinfo
{
	ir_tarval *z; /**< safe zeroes, 0 = bit is zero,       1 = bit maybe is 1 */
	ir_tarval *o; /**< safe ones,   0 = bit maybe is zero, 1 = bit is 1 */
} bitinfo;

/** Get analysis information for node irn */
bitinfo *get_bitinfo(ir_node const *irn);

/** Joins the existing information with the given one. */
bool join_bitinfo(ir_node *irn, ir_tarval *z, ir_tarval *o);

/** Set analysis information for node irn. */
bool set_bitinfo(ir_node *irn, ir_tarval *z, ir_tarval *o);

/**
 * Compute value range fixpoint aka which bits of value are constant zero/one.
 * The result is available via @see get_bitinfo.
 */
void constbits_analyze(ir_graph *irg);

/**
 * Clears the bit information for the given graph.
 */
void constbits_clear(ir_graph *irg);

#endif
