/**
 * @author Daniel Grund
 * @date 05.01.2005
 */

#ifndef _DOMTREE_H
#define _DOMTREE_H

#include "pmap.h"
#include "irgraph.h"
#include "irnode.h"

typedef struct _domtree_t domtree_t;
typedef struct _dominfo_t dominfo_t;

dominfo_t *domtree_create(ir_graph *irg);
domtree_t *domtree_find(dominfo_t *dom, ir_node *block);
void domtree_free(dominfo_t *dom);

#endif
