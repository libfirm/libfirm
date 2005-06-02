
/**
 * IRG modifications for be routines.
 * @date 4.5.2005
 *
 * Copyright (C) 2005 Universitaet Karlsruhe.
 * Released under the GPL.
 */

#ifndef _BEIRGMOD_H
#define _BEIRGMOD_H

typedef struct _dom_front_info_t dom_front_info_t;

dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg);
pset *be_get_dominance_frontier(dom_front_info_t *info, ir_node *block);
void be_free_dominance_frontiers(dom_front_info_t *info);

void be_introduce_copies(dom_front_info_t *info, ir_node *orig,
    int n, ir_node *copy_nodes[]);

#endif
