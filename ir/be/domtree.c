/**
 * @author Daniel Grund
 * @date 05.01.2005
 */

#include <stdlib.h>

#include "debug.h"
#include "hashptr.h"
#include "set.h"
#include "irprintf_t.h"
#include "irgwalk.h"

#include "irdom.h"
#include "domtree.h"

struct _domtree_t {
	ir_node *block;
	struct _domtree_t *up, *right, *down;
};

struct _dominfo_t {
	domtree_t *root;
	set *b2dom;
};


static firm_dbg_module_t *dbg = NULL;


int set_cmp_domtree(const void *x, const void *y, size_t size) {
	return ((domtree_t *)x)->block != ((domtree_t *)y)->block;
}


domtree_t *domtree_find(dominfo_t *dom, ir_node *block) {
	domtree_t d;
	DBG((dbg, 1, "%n\n", block));
	assert(is_Block(block));
	d.block = block;
	return set_find(dom->b2dom, &d, sizeof(d), HASH_PTR(block));
}


static void domtree_insert(dominfo_t *dom, ir_node *idom, ir_node *dominated) {
	domtree_t *parent, *child;

	DBG((dbg, 1, "%n %n\n", idom, dominated));
	parent = domtree_find(dom, idom);
	if (!parent) {
		domtree_t d;
		d.block = idom;
		d.up = d.right = d.down = NULL;
		set_insert(dom->b2dom, &d, sizeof(d), HASH_PTR(idom));
	}

	child = domtree_find(dom, dominated);
	if (!child) {
		domtree_t d;
		d.block = dominated;
		d.up = d.right = d.down = NULL;
		set_insert(dom->b2dom, &d, sizeof(d), HASH_PTR(dominated));
	}

	/* get them (perhaps) again, cause they were _copied_ */
	parent = domtree_find(dom, idom);
	child = domtree_find(dom, dominated);

	child->right = parent->down;
	child->up = parent;
	parent->down = child;
}


static void block_walker(ir_node *node, void *env) {
	ir_node *idom;
	if (!is_Block(node))
		return;

	idom = get_Block_idom(node);

	if (idom)
		domtree_insert((dominfo_t *)env, idom, node);
}


dominfo_t *domtree_create(ir_graph *irg) {
	dominfo_t *res;
	domtree_t *dom;

	dbg = firm_dbg_register("Domtree");
	firm_dbg_set_mask(dbg, 1);

	DBG((dbg, 1, "\n"));
	res = malloc(sizeof(dominfo_t));
	res->b2dom = new_set(set_cmp_domtree, 64);

	/* construct domtree */
	compute_doms(irg);
	irg_walk_graph(irg, block_walker, NULL, res);
	free_dom_and_peace(irg);

	/* determine root of dom tree and store in dominfo. */
	dom = set_first(res->b2dom);
	set_break(res->b2dom);
	while (dom->up)
		dom = dom->up;
	res->root = dom;

	return res;
}


void domtree_free(dominfo_t *dom) {
	DBG((dbg, 1, "\n"));
	del_set(dom->b2dom);
	free(dom);
}
