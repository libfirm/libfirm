/**
 * Contains implementation of some useful functions for ia32 backend.
 * @author Christian Wuerdig
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#include "irnode.h"
#include "iredges.h"

#include "ia32_util.h"

/**
 * Returns the first Proj with given mode connected to irn.
 * @param irn  The irn
 * @param First proj with mode == mode or NULL if none found
 */
ir_node *ia32_get_proj_for_mode(const ir_node *irn, ir_mode *mode) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) == mode_M)
			return src;
	}

	return NULL;
}

/**
 * Returns the first Proj with mode != mode_M connected to irn.
 * @param irn  The irn
 * @param First proj with mode != mode_M or NULL if none found
 */
ir_node *ia32_get_res_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) != mode_M)
			return src;
	}

	return NULL;
}
