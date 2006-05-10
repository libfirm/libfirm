
/**
 * Some convenience macros for node bitmaps.
 * @author Sebastian Hack
 * @date   10.05.2006
 */

#ifndef __FIRM_IRBITSET_H
#define __FIRM_IRBITSET_H

#include "bitset.h"

#define bitset_irg_malloc(irg)                bitset_malloc(get_irg_last_idx(irg))
#define bitset_irg_obstack_alloc(obst, irg)   bitset_obstac_alloc(obst, get_irg_last_idx(irg))
#define bitset_add_irn(bs, irn)               bitset_set((bs), get_irn_idx(irn))
#define bitset_remv_irn(bs, irn)              bitset_clear((bs), get_irn_idx(irn))
#define bitset_contains_irn(bs, irn)          bitset_is_set((bs), get_irn_idx(irn))


/* Internal use. */
#define _bsfe_get_irn(irg, elm) (elm == -1 ? NULL : get_idx_irn((irg), (unsigned) elm))

/**
 * Iterate over a bitset containing node indexes.
 * @param irg The graph the nodes are in.
 * @param bs  The bitset containing the indexes.
 * @param elm A loop variable for the bitset (must be of type bitset_pos_t).
 * @param irn An ir_node * which is set to the current node.
 */
#define bitset_foreach_irn(irg, bs, elm, irn) \
	for(elm = bitset_next_set(bs, 0), irn = _bsfe_get_irn(irg, elm); elm != -1; elm = bitset_next_set(bs, elm + 1), irn = _bsfe_get_irn(irg, elm))


#endif /* __FIRM_IRBITSET_H */
