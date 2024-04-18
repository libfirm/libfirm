/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @brief
 *
 * Simple depth first search on CFGs.
 */
#include "dfs_t.h"

#define DISABLE_STATEV

#include "irdom_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irprintf.h"
#include "set.h"
#include "statev_t.h"
#include "util.h"
#include <assert.h>
#include <stdlib.h>

static int cmp_edge(const void *a, const void *b, size_t sz)
{
	(void)sz;
	const dfs_edge_t *p = (const dfs_edge_t*) a;
	const dfs_edge_t *q = (const dfs_edge_t*) b;
	return !(p->src == q->src && p->tgt == q->tgt);
}

static int cmp_node(const void *a, const void *b, size_t sz)
{
	(void)sz;
	const dfs_node_t *p = (const dfs_node_t*) a;
	const dfs_node_t *q = (const dfs_node_t*) b;
	return p->node != q->node;
}

#define get_node(dfs, node) _dfs_get_node(dfs, node)

static dfs_edge_t *get_edge(dfs_t const *const self, ir_node const *const src, ir_node const *const tgt)
{
	unsigned hash = hash_combine(hash_ptr(src), hash_ptr(tgt));

	dfs_edge_t templ;
	templ.src = src;
	templ.tgt = tgt;
	templ.kind = (dfs_edge_kind_t) -1;

	return set_insert(dfs_edge_t, self->edges, &templ, sizeof(templ), hash);
}

static void dfs_perform(dfs_t *dfs, ir_node *n, dfs_node_t const *anc, int level)
{
	dfs_node_t *node = get_node(dfs, n);
	assert(node->visited == 0);
	node->visited     = 1;
	node->node        = n;
	node->ancestor    = anc;
	node->pre_num     = dfs->pre_num++;
	node->max_pre_num = node->pre_num;
	node->level       = level;

	foreach_block_succ(n, edge) {
		ir_node *const p = get_edge_src_irn(edge);

		/* get the node */
		dfs_node_t *child = get_node(dfs, p);

		/* create the edge object */
		dfs_edge_t *edge = get_edge(dfs, n, p);
		edge->s = node;
		edge->t = child;

		if (!child->visited)
			dfs_perform(dfs, p, node, level + 1);

		/* get the maximum pre num of the subtree. needed for ancestor determination. */
		node->max_pre_num = MAX(node->max_pre_num, child->max_pre_num);
	}

	node->post_num = dfs->post_num++;
}

static void classify_edges(dfs_t *dfs)
{
	stat_ev_cnt_decl(anc);
	stat_ev_cnt_decl(back);
	stat_ev_cnt_decl(fwd);
	stat_ev_cnt_decl(cross);

	foreach_set (dfs->edges, dfs_edge_t, edge) {
		dfs_node_t *src = edge->s;
		dfs_node_t *tgt = edge->t;

		if (tgt->ancestor == src) {
			stat_ev_cnt_inc(anc);
			edge->kind = DFS_EDGE_ANC;
		} else if (_dfs_int_is_ancestor(tgt, src)) {
			stat_ev_cnt_inc(back);
			edge->kind = DFS_EDGE_BACK;
		} else if (_dfs_int_is_ancestor(src, tgt)) {
			stat_ev_cnt_inc(fwd);
			edge->kind = DFS_EDGE_FWD;
		} else {
			stat_ev_cnt_inc(cross);
			edge->kind = DFS_EDGE_CROSS;
		}
	}

	stat_ev_cnt_done(anc,   "dfs_edge_anc");
	stat_ev_cnt_done(back,  "dfs_edge_back");
	stat_ev_cnt_done(fwd,   "dfs_edge_fwd");
	stat_ev_cnt_done(cross, "dfs_edge_cross");
}

dfs_edge_kind_t dfs_get_edge_kind(dfs_t const *const dfs, ir_node const *const a, ir_node const *const b)
{
	if (!dfs->edges_classified) {
		dfs_t *urg = (dfs_t *) dfs;
		classify_edges(urg);
		urg->edges_classified = true;
	}
	return get_edge(dfs, a, b)->kind;
}

dfs_t *dfs_new(ir_graph *const irg)
{
	dfs_t *res = XMALLOC(dfs_t);
	res->nodes            = new_set(cmp_node, 64);
	res->edges            = new_set(cmp_edge, 128);
	res->pre_num          = 0;
	res->post_num         = 0;
	res->edges_classified = false;

	ir_node *const root = get_irg_start_block(irg);
	dfs_perform(res, root, NULL, 0);

	/* make sure the end node (which might not be accessible) has a number */
	ir_node    *const end  = get_irg_end_block(irg);
	dfs_node_t *const node = get_node(res, end);
	if (!node->visited) {
		node->visited     = 1;
		node->node        = end;
		node->ancestor    = NULL;
		node->pre_num     = res->pre_num++;
		node->post_num    = res->post_num++;
		node->max_pre_num = node->pre_num;
		node->level       = 0;
	}

	classify_edges(res);

	assert(res->pre_num == res->post_num);
	res->pre_order  = XMALLOCN(dfs_node_t*, res->pre_num);
	res->post_order = XMALLOCN(dfs_node_t*, res->post_num);
	foreach_set (res->nodes, dfs_node_t, node) {
		assert(node->pre_num < res->pre_num);
		assert(node->post_num < res->post_num);

		res->pre_order[node->pre_num] = node;
		res->post_order[node->post_num] = node;
	}

	stat_ev_dbl("dfs_n_blocks", res->pre_num);

	return res;
}

void dfs_free(dfs_t *dfs)
{
	del_set(dfs->nodes);
	del_set(dfs->edges);
	free(dfs->pre_order);
	free(dfs->post_order);
	free(dfs);
}

static void dfs_dump_edge(const dfs_edge_t *edge, FILE *file)
{
	const char *s;
#define XXX(e)   case DFS_EDGE_ ## e: s = #e; break
	switch (edge->kind) {
		XXX(FWD);
		XXX(CROSS);
		default:
		s = "";
	}
#undef XXX

	int         weight = edge->kind == DFS_EDGE_BACK ? 1 : 1000;
	const char *style  = edge->kind == DFS_EDGE_BACK ? "dashed" : "solid";

	dfs_node_t *src = edge->s;
	dfs_node_t *tgt = edge->t;
	ir_fprintf(file, "\tn%d -> n%d [label=\"%s\",style=\"%s\",weight=\"%d\"];\n", src->pre_num, tgt->pre_num, s, style, weight);
}

static int node_level_cmp(const void *a, const void *b)
{
	const dfs_node_t *p = *(const dfs_node_t **) a;
	const dfs_node_t *q = *(const dfs_node_t **) b;
	if (p->level == q->level)
		return p->pre_num - q->pre_num;
	return p->level - q->level;
}

void dfs_dump(const dfs_t *dfs, FILE *file)
{
	dfs_node_t **nodes = XMALLOCN(dfs_node_t*, dfs->pre_num);

	ir_fprintf(file, "digraph G {\nranksep=0.5\n");
	int n = 0;
	foreach_set (dfs->nodes, dfs_node_t, node) {
		nodes[n++] = node;
	}

	QSORT(nodes, n, node_level_cmp);

	int i = 0;
	while (i < n) {
		int level = nodes[i]->level;

		ir_fprintf(file, "\t{ rank = same; ");
		for (; i < n && nodes[i]->level == level; ++i)
			ir_fprintf(file, "n%d;", nodes[i]->pre_num);
		ir_fprintf(file, "}\n");
	}

	for (int i = 0; i < n; ++i) {
		dfs_node_t *const node = nodes[i];
		ir_fprintf(file, "\tn%d [label=\"%d\"]\n", node->pre_num, get_Block_dom_tree_pre_num((ir_node*) node->node));
	}

	foreach_set (dfs->edges, dfs_edge_t, edge)
		dfs_dump_edge(edge, file);

	ir_fprintf(file, "}\n");
	free(nodes);
}
