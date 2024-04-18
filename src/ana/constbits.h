#ifndef CONSTBITS_H
#define CONSTBITS_H

#include <stdbool.h>
#include "tv.h"

typedef enum bitinfo_state {
	BITINFO_INVALID,
	BITINFO_VALID,
	BITINFO_IN_FLIGHT,
	BITINFO_UNSTABLE,
} bitinfo_state;

typedef struct bitinfo
{
	ir_tarval    *z; /**< safe zeroes, 0 = bit is zero,       1 = bit maybe is 1 */
	ir_tarval    *o; /**< safe ones,   0 = bit maybe is zero, 1 = bit is 1 */
	bitinfo_state state;
} bitinfo;

/** Get analysis information for node irn */
bitinfo *get_bitinfo(ir_node const *irn);

/**
 * Get analysis information for @p irn, if it is available.
 *
 * This is guaranteed not to change the analysis state.
 */
bitinfo const *try_get_bitinfo(ir_node const *irn);

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
