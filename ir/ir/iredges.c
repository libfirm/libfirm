/*
 * Project:     libFIRM
 * File name:   ir/ir/iredges.c
 * Purpose:     Always available outs.
 * Author:      Sebastian Hack
 * Modified by: Michael Beck, Andreas Schoesser
 * Created:     14.1.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * Always available outs.
 * @author Sebastian Hack
 * @date 14.1.2005
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "irnode_t.h"
#include "iropt_t.h"
#include "iredgekinds.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irprintf.h"
#include "irhooks.h"
#include "debug.h"
#include "set.h"
#include "bitset.h"

/**
* A function that allows for setting an edge.
* This abstraction is necessary since different edge kind have
* different methods of setting edges.
*/
typedef void (set_edge_func_t)(ir_node *src, int pos, ir_node *tgt);

typedef int (get_edge_src_arity_func_t)(const ir_node *src);

typedef int (get_edge_src_first_func_t)(const ir_node *src);

typedef ir_node *(get_edge_src_n_func_t)(const ir_node *src, int pos);

/**
* Additional data for an edge kind.
*/
typedef struct {
	const char                *name;
	set_edge_func_t           *set_edge;
	get_edge_src_first_func_t *get_first;
	get_edge_src_arity_func_t *get_arity;
	get_edge_src_n_func_t     *get_n;
} ir_edge_kind_info_t;

static int get_zero(const ir_node *irn)
{
	return 0;
}

static int get_irn_first(const ir_node *irn)
{
	return 0 - !is_Block(irn);
}

static ir_node *get_block_n(const ir_node *irn, int pos)
{
	return is_Block(irn) ? get_Block_cfgpred_block((ir_node *) irn, pos) : 0;
}

static const ir_edge_kind_info_t edge_kind_info[EDGE_KIND_LAST] = {
	{ "normal"     , set_irn_n,   get_irn_first, get_irn_arity,  get_irn_n   },
	{ "block succs", NULL,        get_zero,      get_irn_arity,  get_block_n },
	{ "dependency",  set_irn_dep, get_zero,      get_irn_deps,   get_irn_dep }
};

#define foreach_tgt(irn, i, n, kind) for(i = edge_kind_info[kind].get_first(irn), n = edge_kind_info[kind].get_arity(irn); i < n; ++i)
#define get_n(irn, pos, kind)        (edge_kind_info[kind].get_n(irn, pos))
#define get_kind_str(kind)           (edge_kind_info[kind].name)

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * This flag is set to 1, if the edges get initialized for an irg.
 * Then register additional data is forbidden.
 */
static int edges_used = 0;

static int edges_private_size = 0;
#define EDGE_SIZE (sizeof(ir_edge_t) + edges_private_size)

/**
 * If set to 1, the list heads are checked every time an edge is changed.
 */
static int edges_dbg = 0;

int edges_register_private_data(size_t n)
{
	int res = edges_private_size;

	assert(!edges_used && "you cannot register private edge data, if edges have been initialized");

	edges_private_size += n;
	return res;
}

/**
 * Reset the user's private data at offset 'offset'
 */

void edges_reset_private_data(ir_graph *irg, int offset, size_t size)
{
	irg_edge_info_t *info = _get_irg_edge_info(irg, EDGE_KIND_NORMAL);
	ir_edge_t       *edge;

	foreach_set(info->edges, edge)
	{
		memset(edge + sizeof(*edge) + offset, 0, size);
	}
}

#define TIMES37(x) (((x) << 5) + ((x) << 2) + (x))

#define get_irn_out_list_head(irn) (&get_irn_out_info(irn)->outs)

static int edge_cmp(const void *p1, const void *p2, size_t len)
{
	const ir_edge_t *e1 = p1;
	const ir_edge_t *e2 = p2;
	int             res = e1->src == e2->src && e1->pos == e2->pos;

	return ! res;
}

#define edge_hash(edge) (TIMES37((edge)->pos) + HASH_PTR((edge)->src))

/**
 * Initialize the out information for a graph.
 * @note Dead node elimination can call this on an already initialized graph.
 */
void edges_init_graph_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	if(edges_activated_kind(irg, kind)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg, kind);
		int amount = 2048;

		edges_used = 1;
		if(info->edges) {
			amount = set_count(info->edges);
			del_set(info->edges);
		}
		info->edges = new_set(edge_cmp, amount);
	}
}

/**
 * Get the edge object of an outgoing edge at a node.
 * @param   irg The graph, the node is in.
 * @param   src The node at which the edge originates.
 * @param   pos The position of the edge.
 * @return      The corresponding edge object or NULL,
 *              if no such edge exists.
 */
const ir_edge_t *get_irn_edge_kind(ir_graph *irg, const ir_node *src, int pos, ir_edge_kind_t kind)
{
	if (edges_activated_kind(irg, kind)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg, kind);
		ir_edge_t       key;

		key.src = (ir_node *)src;
		key.pos = pos;
		return set_find(info->edges, &key, EDGE_SIZE, edge_hash(&key));
	}

	return NULL;
}

/**
 * Get the edge object of an outgoing edge at a node.
 * Looks for an edge for all kinds.
 */

const ir_edge_t *get_irn_edge(ir_graph *irg, const ir_node *src, int pos)
{
	const ir_edge_t *edge;
	if((edge = get_irn_edge_kind(irg, src, pos, EDGE_KIND_NORMAL)) == NULL)
		edge = get_irn_edge_kind(irg, src, pos, EDGE_KIND_BLOCK);
	return(edge);
}

/**
 * Change the out count
 */
static INLINE void edge_change_cnt(ir_node *tgt, ir_edge_kind_t kind, int ofs) {
	irn_edge_info_t *info = _get_irn_edge_info(tgt, kind);
	info->out_count += ofs;
}

/**
 * Verify the edge list of a node, ie. ensure it's a loop:
 * head -> e_1 -> ... -> e_n -> head
 */
static INLINE void vrfy_list_head(ir_node *irn, ir_edge_kind_t kind) {
	int                    err       = 0;
	int                    num       = 0;
	pset                   *lh_set   = pset_new_ptr(16);
	const struct list_head *head     = _get_irn_outs_head(irn, kind);
	const struct list_head *pos;

	list_for_each(pos, head) {
		if (pset_find_ptr(lh_set, pos)) {
			const ir_edge_t *edge = list_entry(pos, ir_edge_t, list);

			ir_fprintf(stderr, "EDGE Verifier: edge list broken (self loop not to head) for %+F:\n", irn);
			fprintf(stderr, "- at list entry %d\n", num);
			if (edge->invalid)
				fprintf(stderr, "- edge is invalid\n");
			if (edge->src);
				ir_fprintf(stderr, "- edge %+F(%d)\n", edge->src, edge->pos);
			err = 1;
			break;
		}
		num++;
		pset_insert_ptr(lh_set, pos);
	}

	del_pset(lh_set);

	assert(err == 0);
}

/* The edge from (src, pos) -> old_tgt is redirected to tgt */
void edges_notify_edge_kind(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_edge_kind_t kind, ir_graph *irg)
{
	const char *msg = "";

	assert(edges_activated_kind(irg, kind));

	/*
	 * Only do something, if the old and new target differ.
	 */
	if(tgt != old_tgt) {
		irg_edge_info_t *info = _get_irg_edge_info(irg, kind);
		set *edges            = info->edges;
		ir_edge_t *templ      = alloca(EDGE_SIZE);
		ir_edge_t *edge;

		/* Initialize the edge template to search in the set. */
		memset(templ, 0, EDGE_SIZE);
		templ->src     = src;
		templ->pos     = pos;
		templ->invalid = 0;
		templ->present = 0;
		templ->kind    = kind;
		DEBUG_ONLY(templ->src_nr = get_irn_node_nr(src));

		/*
		 * If the target is NULL, the edge shall be deleted.
		 */
		if (tgt == NULL) {
			/* search the edge in the set. */
			edge = set_find(edges, templ, EDGE_SIZE, edge_hash(templ));

			/* mark the edge invalid if it was found */
			if (edge) {
				msg = "deleting";
				list_del(&edge->list);
				edge->invalid = 1;
				edge->pos = -2;
				edge->src = NULL;
				edge_change_cnt(old_tgt, kind, -1);
			}

			/* If the edge was not found issue a warning on the debug stream */
			else {
				msg = "edge to delete not found!\n";
			}
		} /* if */

		/*
		 * The target is not NULL and the old target differs
		 * from the new target, the edge shall be moved (if the
		 * old target was != NULL) or added (if the old target was
		 * NULL).
		 */
		else {
			struct list_head *head = _get_irn_outs_head(tgt, kind);

			assert(head->next && head->prev &&
					"target list head must have been initialized");

			/* If the old target is not null, the edge is moved. */
			if (old_tgt) {
				edge = set_find(edges, templ, EDGE_SIZE, edge_hash(templ));
				assert(edge && "edge to redirect not found!");
				assert(! edge->invalid && "Invalid edge encountered");

				msg = "redirecting";

				list_move(&edge->list, head);
				edge_change_cnt(old_tgt, kind, -1);
			}

			/* The old target was null, thus, the edge is newly created. */
			else {
				edge = set_insert(edges, templ, EDGE_SIZE, edge_hash(templ));

				assert(! edge->invalid && "Freshly inserted edge is invalid?!?");
				assert(edge->list.next == NULL && edge->list.prev == NULL &&
					"New edge must not have list head initialized");

				msg = "adding";
				list_add(&edge->list, head);
			}

			edge_change_cnt(tgt, kind, +1);
		} /* else */

		/* verify list heads */
		if (edges_dbg) {
			if (tgt)
				vrfy_list_head(tgt, kind);
			if (old_tgt)
				vrfy_list_head(old_tgt, kind);
		}
	}

	/* If the target and the old target are equal, nothing is done. */
	DBG((dbg, LEVEL_5, "announce out edge: %+F %d-> %+F(%+F): %s\n", src, pos, tgt, old_tgt, msg));
}

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_graph *irg)
{
	if(edges_activated_kind(irg, EDGE_KIND_NORMAL)) {
		edges_notify_edge_kind(src, pos, tgt, old_tgt, EDGE_KIND_NORMAL, irg);
	}

	if (edges_activated_kind(irg, EDGE_KIND_BLOCK) && is_Block(src)) {
		/* do not use get_nodes_block() here, it fails when running unpinned */
		ir_node *bl_old = old_tgt ? get_irn_n(skip_Proj(old_tgt), -1) : NULL;
		ir_node *bl_tgt = NULL;

		if (tgt)
			bl_tgt = is_Bad(tgt) ? tgt : get_irn_n(skip_Proj(tgt), -1);

		edges_notify_edge_kind(src, pos, bl_tgt, bl_old, EDGE_KIND_BLOCK, irg);
	}
}


void edges_node_deleted_kind(ir_node *old, ir_edge_kind_t kind, ir_graph *irg)
{
	int i, n;

	if(!edges_activated_kind(irg, kind))
		return;

	DBG((dbg, LEVEL_5, "node deleted (kind: %s): %+F\n", get_kind_str(kind), old));

	foreach_tgt(old, i, n, kind) {
		ir_node *old_tgt = get_n(old, i, kind);
		edges_notify_edge_kind(old, i, NULL, old_tgt, kind, irg);
	}
}

struct build_walker {
	ir_graph       *irg;
	ir_edge_kind_t kind;
	bitset_t       *reachable;
	unsigned       problem_found;
};

/**
 * Post-Walker: notify all edges
 */
static void build_edges_walker(ir_node *irn, void *data) {
	struct build_walker *w = data;
	int                 i, n;

	if (! edges_activated_kind(w->irg, w->kind))
		return;

	foreach_tgt(irn, i, n, w->kind)
		edges_notify_edge_kind(irn, i, get_n(irn, i, w->kind), NULL, w->kind, w->irg);
}

/**
 * Pre-Walker: initializes the list-heads and set the out-count
 * of all nodes to 0.
 */
static void init_lh_walker(ir_node *irn, void *data) {
	struct build_walker *w = data;
	INIT_LIST_HEAD(_get_irn_outs_head(irn, w->kind));
	_get_irn_edge_info(irn, w->kind)->out_count = 0;
}

/**
 * Visitor: initializes the list-heads and set the out-count
 * of all nodes to 0 of nodes that are not seen so far.
 */
static void visitor(ir_node *irn, void *data) {
	if (irn_not_visited(irn)) {
		mark_irn_visited(irn);
		init_lh_walker(irn, data);
	}
}

/*
 * Build the initial edge set.
 * Beware, this is not a simple task because it suffers from two
 * difficulties:
 * - the anchor set allows access to Nodes that may not be reachable from
 *   the End node
 * - the identities add nodes to the "root set" that are not yet reachable
 *   from End. However, after some transformations, the CSE may revival these
 *   nodes
 *
 * These problems can be fixed using different strategies:
 * - Add an age flag to every node. Whenever the edge of a node is older
 *   then the current edge, invalidate the edges of this node.
 *   While this would help for revivaled nodes, it increases memory and runtime.
 * - Delete the identities set.
 *   Solves the revival problem, but may increase the memory consumption, as
 *   nodes cannot be revivaled at all.
 * - Manually iterate over the identities root set. This did not consume more memory
 *   but increase the computation time because the |identities| >= |V|
 *
 * Currently, we use the last option.
 */
void edges_activate_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	struct build_walker w;
	irg_edge_info_t *info = _get_irg_edge_info(irg, kind);

	w.irg  = irg;
	w.kind = kind;

	info->activated = 1;
	edges_init_graph_kind(irg, kind);
	//irg_walk_graph(irg, init_lh_walker, build_edges_walker, &w);
	inc_irg_visited(irg);
	irg_walk_anchors(irg, init_lh_walker, build_edges_walker, &w);
	visit_all_identities(irg, visitor, &w);
}

void edges_deactivate_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	irg_edge_info_t *info = _get_irg_edge_info(irg, kind);

	info->activated = 0;
	if (info->edges) {
		del_set(info->edges);
		info->edges = NULL;
	}
}

int (edges_activated_kind)(const ir_graph *irg, ir_edge_kind_t kind)
{
	return _edges_activated_kind(irg, kind);
}


/**
 * Reroute all use-edges from a node to another.
 * @param from The node whose use-edges shall be withdrawn.
 * @param to   The node to which all the use-edges of @p from shall be
 *             sent to.
 * @param irg  The graph.
 */
void edges_reroute_kind(ir_node *from, ir_node *to, ir_edge_kind_t kind, ir_graph *irg)
{
	set_edge_func_t *set_edge = edge_kind_info[kind].set_edge;

	if(set_edge && edges_activated_kind(irg, kind)) {
		struct list_head *head = _get_irn_outs_head(from, kind);

		DBG((dbg, LEVEL_5, "reroute from %+F to %+F\n", from, to));

		while(head != head->next) {
			ir_edge_t *edge = list_entry(head->next, ir_edge_t, list);
			assert(edge->pos >= -1);
			set_edge(edge->src, edge->pos, to);
		}
	}
}

static void verify_set_presence(ir_node *irn, void *data)
{
	struct build_walker *w     = data;
	set                 *edges = _get_irg_edge_info(w->irg, w->kind)->edges;
	int i, n;

	foreach_tgt(irn, i, n, w->kind) {
		ir_edge_t templ, *e;

		templ.src = irn;
		templ.pos = i;

		e = set_find(edges, &templ, EDGE_SIZE, edge_hash(&templ));
		if(e != NULL)
			e->present = 1;
		else {
			w->problem_found = 1;
			ir_fprintf(stderr, "Edge Verifier: edge %+F,%d (kind: \"%s\") is missing\n", irn, i, get_kind_str(w->kind));
		}
	}
}

static void verify_list_presence(ir_node *irn, void *data)
{
	struct build_walker *w = data;
	const ir_edge_t     *e;

	bitset_set(w->reachable, get_irn_idx(irn));

	/* check list heads */
	vrfy_list_head(irn, w->kind);

	foreach_out_edge_kind(irn, e, w->kind) {
		ir_node *tgt;

		if (w->kind == EDGE_KIND_NORMAL && get_irn_arity(e->src) <= e->pos) {
			w->problem_found = 1;
			ir_fprintf(stderr, "Edge Verifier: edge %+F -> %+F recorded at src position %d, but src has arity %d\n",
				e->src, irn, e->pos, get_irn_arity(e->src));
			continue;
		}

		tgt = get_n(e->src, e->pos, w->kind);

		if (irn != tgt) {
			w->problem_found = 1;
			ir_fprintf(stderr, "Edge Verifier: edge %+F,%d (kind \"%s\") is no out edge of %+F but of %+F\n",
				e->src, e->pos, get_kind_str(w->kind), irn, tgt);
		}
	}
}

int edges_verify_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	struct build_walker w;
	set                 *edges = _get_irg_edge_info(irg, kind)->edges;
	ir_edge_t           *e;

	w.irg           = irg;
	w.kind          = kind;
	w.reachable     = bitset_alloca(get_irg_last_idx(irg));
	w.problem_found = 0;

	/* Clear the present bit in all edges available. */
	for (e = set_first(edges); e; e = set_next(edges))
		e->present = 0;

	irg_walk_graph(irg, verify_set_presence, verify_list_presence, &w);

	/*
	 * Dump all edges which are not invalid and not present.
	 * These edges are superfluous and their presence in the
	 * edge set is wrong.
	 */
	for (e = set_first(edges); e; e = set_next(edges)) {
		if (! e->invalid && ! e->present && bitset_is_set(w.reachable, get_irn_idx(e->src))) {
			w.problem_found = 1;
			ir_fprintf(stderr, "Edge Verifier: edge %+F,%d is superfluous\n", e->src, e->pos);
		}
	}

	return w.problem_found;
}

#define IGNORE_NODE(irn) (is_Bad((irn)) || is_Block((irn)))

/**
 * Clear link field of all nodes.
 */
static void clear_links(ir_node *irn, void *env) {
	struct build_walker *w  = env;
	bitset_t            *bs;

	set_irn_link(irn, NULL);

	if (IGNORE_NODE(irn))
		return;

	bs = bitset_malloc(get_irg_last_idx(w->irg));
	set_irn_link(irn, bs);
}

/**
 * Increases count (stored in link field) for all operands of a node.
 */
static void count_user(ir_node *irn, void *env) {
	int i;
	int first;

	first = get_irn_first(irn);
	for (i = get_irn_arity(irn) - 1; i >= first; --i) {
		ir_node  *op = get_irn_n(irn, i);
		bitset_t *bs = get_irn_link(op);

		if (bs)
			bitset_set(bs, get_irn_idx(irn));
	}
}

/**
 * Verifies if collected count, number of edges in list and stored edge count are in sync.
 */
static void verify_edge_counter(ir_node *irn, void *env) {
	struct build_walker    *w       = env;
	bitset_t               *bs;
	int                    list_cnt;
	int                    ref_cnt;
	int                    edge_cnt;
	const struct list_head *head;
	const struct list_head *pos;

	if (IGNORE_NODE(irn))
		return;

	bs       = get_irn_link(irn);
	list_cnt = 0;
	ref_cnt  = bitset_popcnt(bs);
	edge_cnt = _get_irn_edge_info(irn, EDGE_KIND_NORMAL)->out_count;
	head     = _get_irn_outs_head(irn, EDGE_KIND_NORMAL);

	/* We can iterate safely here, list heads have already been verified. */
	list_for_each(pos, head) {
		ir_edge_t *edge = list_entry(pos, ir_edge_t, list);
		if (! is_Bad(edge->src))
			list_cnt++;
	}

	if (edge_cnt != list_cnt) {
		w->problem_found = 1;
		ir_fprintf(stderr, "Edge Verifier: edge count is %d, but %d edge(s) are recorded in list at %+F\n",
			edge_cnt, list_cnt, irn);
	}

	if (ref_cnt != list_cnt) {
		unsigned long idx;

		w->problem_found = 1;
		ir_fprintf(stderr, "Edge Verifier: %+F reachable by %d node(s), but %d edge(s) recorded in list\n",
			irn, ref_cnt, list_cnt);

		list_for_each(pos, head) {
			ir_edge_t *edge = list_entry(pos, ir_edge_t, list);
			if (! is_Bad(edge->src))
				bitset_flip(bs, get_irn_idx(edge->src));
		}

		if (ref_cnt < list_cnt)
			fprintf(stderr,"               following nodes are recorded in list, but not as user:\n");
		else
			fprintf(stderr,"               following nodes are user, but not recorded in list:\n");

		fprintf(stderr,"              ");
		bitset_foreach(bs, idx) {
			ir_node *src = get_idx_irn(w->irg, idx);
			ir_fprintf(stderr, " %+F", src);
		}
		fprintf(stderr, "\n");
	}

	bitset_free(bs);
}

/**
 * Verifies the out edges of an irg.
 */
int edges_verify(ir_graph *irg) {
	struct build_walker w;
	int    problem_found = 0;

	if (! edges_dbg)
		return 0;

	/* verify normal edges only */
	problem_found  = edges_verify_kind(irg, EDGE_KIND_NORMAL);

	w.irg           = irg;
	w.kind          = EDGE_KIND_NORMAL;
	w.problem_found = 0;

	/* verify counter */
	inc_irg_visited(irg);
	irg_walk_anchors(irg, clear_links, count_user, &w);
	irg_walk_anchors(irg, NULL, verify_edge_counter, &w);

	return problem_found ? 1 : w.problem_found;
}

void init_edges(void)
{
	FIRM_DBG_REGISTER(dbg, DBG_EDGES);
	/* firm_dbg_set_mask(dbg, -1); */
}

void edges_init_dbg(int do_dbg) {
	edges_dbg = do_dbg;
}

void edges_activate(ir_graph *irg)
{
	edges_activate_kind(irg, EDGE_KIND_NORMAL);
	edges_activate_kind(irg, EDGE_KIND_BLOCK);
}

void edges_deactivate(ir_graph *irg)
{
	edges_deactivate_kind(irg, EDGE_KIND_NORMAL);
	edges_deactivate_kind(irg, EDGE_KIND_BLOCK);
}

int edges_assure(ir_graph *irg)
{
	int activated = edges_activated(irg);

	if(!activated)
		edges_activate(irg);

	return activated;
}

void edges_node_deleted(ir_node *irn, ir_graph *irg)
{
	edges_node_deleted_kind(irn, EDGE_KIND_NORMAL, irg);
	edges_node_deleted_kind(irn, EDGE_KIND_BLOCK, irg);
}


const ir_edge_t *(get_irn_out_edge_first_kind)(const ir_node *irn, ir_edge_kind_t kind)
{
	return _get_irn_out_edge_first_kind(irn, kind);
}

const ir_edge_t *(get_irn_out_edge_next)(const ir_node *irn, const ir_edge_t *last)
{
	return _get_irn_out_edge_next(irn, last);
}

ir_node *(get_edge_src_irn)(const ir_edge_t *edge)
{
	return _get_edge_src_irn(edge);
}

int (get_edge_src_pos)(const ir_edge_t *edge)
{
	return _get_edge_src_pos(edge);
}

int (get_irn_n_edges_kind)(const ir_node *irn, ir_edge_kind_t kind)
{
	return _get_irn_n_edges_kind(irn, kind);
}

void dump_all_out_edges(ir_node *irn)
{
	int i;
	for(i = 0; i < EDGE_KIND_LAST; ++i) {
		const ir_edge_t *edge;

		printf("kind \"%s\"\n", get_kind_str(i));
		foreach_out_edge_kind(irn, edge, i) {
			ir_printf("\t%+F(%d)\n", edge->src, edge->pos);
		}
	}
}
