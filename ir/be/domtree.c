/**
 * @author Daniel Grund
 * @date 05.01.2005
 */

#include <stdlib.h>

#include "irgwalk.h"
#include "irdom.h"
#include "domtree.h"


struct _domtree_t {
	ir_node *block;
	struct _domtree_t *up, *right, *down;
};

struct _dominfo_t {
	domtree_t *root;
	pmap *b2dom;
};


domtree_t *domtree_find(dominfo_t *dom, ir_node *block) {
	assert(is_Block(block));
	return (domtree_t *)pmap_find(dom->b2dom, block);
}


static void domtree_insert(dominfo_t *dom, ir_node *idom, ir_node *dominated) {
	domtree_t *parent, *child;

	parent = domtree_find(dom, idom);
	if (!parent) {
		parent = malloc(sizeof(domtree_t));
		parent->block = idom;
		parent->up = parent->right = parent->down = NULL;
		pmap_insert(dom->b2dom, idom, parent);
	}

	child = domtree_find(dom, dominated);
	if (!child) {
		child = malloc(sizeof(domtree_t));
		child->block = dominated;
		child->up = child->right = child->down = NULL;
		pmap_insert(dom->b2dom, dominated, child);
	}

	child->right = parent->down;
	child->up = parent;
	parent->down = child;
}


static void block_walker(ir_node *node, void *env) {
	ir_node *idom;
	if (!is_Block(node))
		return;

	idom = get_Block_idom(node);
	domtree_insert((dominfo_t *)env, idom, node);
}


dominfo_t *domtree_create(ir_graph *irg) {
	dominfo_t *res;
	domtree_t *dom;

	res = malloc(sizeof(dominfo_t));
	res->b2dom = pmap_create();

	/* construct domtree */
	compute_doms(irg);
	irg_walk_graph(irg, block_walker, NULL, res);
	free_dom_and_peace(irg);

	/* determine root of dom tree and store in dominfo. */
	dom = pmap_first(res->b2dom)->value;
	while (dom->up)
		dom = dom->up;
	res->root = dom;

	return res;
}


void domtree_free(dominfo_t *dom) {
	pmap_entry *e;

	for (e = pmap_first(dom->b2dom); e; e = pmap_next(dom->b2dom))
		free(e->value);

	pmap_destroy(dom->b2dom);
	free(dom);
}
