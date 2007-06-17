/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file    dfs.c
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @version $Id$
 * @summary
 *
 * Simple depth first search on CFGs.
 */
#include <assert.h>
#include "irtools.h"
#include "irprintf.h"
#include "set.h"
#include "dfs_t.h"

static int cmp_edge(const void *a, const void *b, size_t sz)
{
	const dfs_edge_t *p = a;
	const dfs_edge_t *q = b;
	(void) sz;

	return !(p->src == q->src && p->tgt == q->tgt);
}

static int cmp_node(const void *a, const void *b, size_t sz)
{
	const dfs_node_t *p = a;
	const dfs_node_t *q = b;
	(void) sz;

	return p->node != q->node;
}

#define get_node(dfs, node) _dfs_get_node(dfs, node)

static dfs_edge_t *get_edge(const dfs_t *self, void *src, void *tgt)
{
	unsigned hash = HASH_COMBINE(HASH_PTR(src), HASH_PTR(tgt));
	dfs_edge_t templ;

	templ.src = src;
	templ.tgt = tgt;
	templ.kind = -1;

	return set_insert(self->edges, &templ, sizeof(templ), hash);
}

static void dfs_perform(dfs_t *dfs, void *n, void *anc)
{
	dfs_node_t *node = get_node(dfs, n);
	void **succs, **iter;

	assert(node->visited == 0);

	node->visited     = 1;
	node->node        = n;
	node->ancestor    = anc;
	node->pre_num     = dfs->pre_num++;
	node->max_pre_num = node->pre_num;

	dfs->graph_impl->grow_succs(dfs->graph, n, &dfs->obst);
	obstack_ptr_grow(&dfs->obst, NULL);
	succs = obstack_finish(&dfs->obst);

	for (iter = succs; *iter; ++iter) {
		void *p = *iter;

		/* get the node */
		dfs_node_t *child = get_node(dfs, p);

		/* create the edge object */
		dfs_edge_t *edge = get_edge(dfs, n, p);
		edge->s = node;
		edge->t = child;

		if (!child->visited)
			dfs_perform(dfs, p, node);

		/* get the maximum pre num of the subtree. needed for ancestor determination. */
		node->max_pre_num = MAX(node->max_pre_num, child->max_pre_num);
	}

	node->post_num = dfs->post_num++;
	obstack_free(&dfs->obst, succs);
}

static void classify_edges(dfs_t *dfs)
{
	dfs_edge_t *edge;

	foreach_set (dfs->edges, edge) {
		dfs_node_t *src = edge->s;
		dfs_node_t *tgt = edge->t;

		if (tgt->ancestor == src)
			edge->kind = DFS_EDGE_ANC;
		else if (_dfs_int_is_ancestor(tgt, src))
			edge->kind = DFS_EDGE_BACK;
		else if (_dfs_int_is_ancestor(src, tgt))
			edge->kind = DFS_EDGE_FWD;
		else
			edge->kind = DFS_EDGE_CROSS;
	}
}

dfs_edge_kind_t dfs_get_edge_kind(const dfs_t *dfs, void *a, void *b)
{
	if (!dfs->edges_classified) {
		dfs_t *urg = (dfs_t *) dfs;
		classify_edges(urg);
		urg->edges_classified = 1;
	}
	return get_edge(dfs, a, b)->kind;
}

dfs_t *dfs_new(const absgraph_t *graph_impl, void *graph_self)
{
	dfs_t *res = xmalloc(sizeof(res[0]));
	dfs_node_t *node;

	res->graph_impl = graph_impl;
	res->graph      = graph_self;
	res->nodes      = new_set(cmp_node, 64);
	res->edges      = new_set(cmp_edge, 128);

	res->pre_num  = 0;
	res->post_num = 0;
	res->edges_classified = 0;

	obstack_init(&res->obst);

	dfs_perform(res, graph_impl->get_root(graph_self), NULL);
	classify_edges(res);

	res->pre_order = xmalloc(res->pre_num * sizeof(res->pre_order));
	res->post_order = xmalloc(res->pre_num * sizeof(res->post_order));
	foreach_set (res->nodes, node) {
		res->pre_order[node->pre_num] = node;
		res->post_order[node->post_num] = node;
	}

	return res;
}

void dfs_free(dfs_t *dfs)
{
	del_set(dfs->nodes);
	del_set(dfs->edges);
	xfree(dfs->pre_order);
	xfree(dfs->post_order);
	xfree(dfs);
}

void dfs_dump(const dfs_t *dfs, FILE *file)
{
	dfs_node_t *node;
	dfs_edge_t *edge;

	ir_fprintf(file, "digraph G {\n");
	foreach_set (dfs->nodes, node) {
		ir_fprintf(file, "\tn%d [shape=box,label=\"%+F\\l%d/%d %d\"];\n",
				node->pre_num, node->node, node->pre_num, node->post_num, node->max_pre_num);

	}

	foreach_set (dfs->edges, edge) {
		dfs_node_t *src = edge->s;
		dfs_node_t *tgt = edge->t;
		const char *s, *color;

#define XXX(e)		case DFS_EDGE_ ## e: s = #e; break
		switch (edge->kind) {
			XXX(BACK);
			XXX(FWD);
			XXX(CROSS);
			XXX(ANC);
			default:
			s = "?";
		}
#undef XXX

#define XXX(e)	case DFS_EDGE_ ## e
		switch (edge->kind) {
			XXX(ANC):   color = "black"; break;
			XXX(FWD):   color = "blue"; break;
			XXX(CROSS): color = "red"; break;
			XXX(BACK):  color = "darkviolet"; break;
			default: color = "?";
		}
#undef XXX

		ir_fprintf(file, "\tn%d -> n%d [label=\"%s\",color=\"%s\"];\n", src->pre_num, tgt->pre_num, s, color);
	}

	ir_fprintf(file, "}\n");
}
