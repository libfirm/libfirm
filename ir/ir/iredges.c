/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @file
 * @brief   Always available outs.
 * @author  Sebastian Hack, Michael Beck, Andreas Schoesser
 * @date    14.1.2005
 * @version $Id$
 * @summary
 *   This are out-edges (also called def-use edges) that are dynamically
 *   updated as the graph changes.
 */
#include "config.h"

#include "irnode_t.h"
#include "iropt_t.h"
#include "iredgekinds.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irprintf.h"
#include "debug.h"
#include "set.h"
#include "bitset.h"

#include "iredgeset.h"
#include "hashptr.h"

#define DO_REHASH
#define SCALAR_RETURN
#define HashSet                   ir_edgeset_t
#define HashSetIterator           ir_edgeset_iterator_t
#define ValueType                 ir_edge_t*
#define NullValue                 NULL
#define DeletedValue              ((ir_edge_t*)-1)
#define Hash(this,key)            (HASH_PTR(key->src) ^ (key->pos * 40013))
#define KeysEqual(this,key1,key2) ((key1->src) == (key2->src) && (key1->pos == key2->pos))
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))

#define hashset_init            ir_edgeset_init
#define hashset_init_size       ir_edgeset_init_size
#define hashset_destroy         ir_edgeset_destroy
#define hashset_insert          ir_edgeset_insert
#define hashset_remove          ir_edgeset_remove
#define hashset_find            ir_edgeset_find
#define hashset_size            ir_edgeset_size
#define hashset_iterator_init   ir_edgeset_iterator_init
#define hashset_iterator_next   ir_edgeset_iterator_next
#define hashset_remove_iterator ir_edgeset_remove_iterator

#include "hashset.c"

/**
 * A function that allows for setting an edge.
 * This abstraction is necessary since different edge kind have
 * different methods of setting edges.
 */
typedef void (set_edge_func_t)(ir_node *src, int pos, ir_node *tgt);

typedef int (get_edge_src_arity_func_t)(const ir_node *src);

typedef ir_node *(get_edge_src_n_func_t)(const ir_node *src, int pos);

/**
 * Additional data for an edge kind.
 */
typedef struct {
	const char                *name;       /**< name of this edge kind */
	set_edge_func_t           *set_edge;   /**< the set_edge function */
	int                       first_idx;   /**< index of the first possible edge */
	get_edge_src_arity_func_t *get_arity;  /**< the get_arity function */
	get_edge_src_n_func_t     *get_n;      /**< the get_n function */
} ir_edge_kind_info_t;

/**
 * Get the predecessor block.
 */
static ir_node *get_block_n(const ir_node *irn, int pos) {
	if (is_Block(irn))
		return get_Block_cfgpred_block(irn, pos);
	/* might be a Bad */
	return NULL;
}

static const ir_edge_kind_info_t edge_kind_info[EDGE_KIND_LAST] = {
	{ "normal"     , set_irn_n,   -1, get_irn_arity,  get_irn_n   },
	{ "block succs", NULL,         0, get_irn_arity,  get_block_n },
	{ "dependency",  set_irn_dep,  0, get_irn_deps,   get_irn_dep }
};

#define foreach_tgt(irn, i, n, kind) for(i = edge_kind_info[kind].first_idx, n = edge_kind_info[kind].get_arity(irn); i < n; ++i)
#define get_n(irn, pos, kind)        (edge_kind_info[kind].get_n(irn, pos))
#define get_kind_str(kind)           (edge_kind_info[kind].name)

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * This flag is set to 1, if the edges get initialized for an irg.
 * Then register additional data is forbidden.
 */
static int edges_used = 0;

/**
 * Summed size of all users private data
 */

static int edges_private_size = 0;
#define EDGE_SIZE (sizeof(ir_edge_t) + edges_private_size)

/**
 * If set to 1, the list heads are checked every time an edge is changed.
 */
static int edges_dbg = 0;

#ifdef DEBUG_libfirm
/* a static variable holding the last number assigned to a new edge */
static long last_edge_num = -1;
#endif

static inline long edge_get_id(const ir_edge_t *e) {
#ifdef DEBUG_libfirm
	return e->edge_nr;
#else /* DEBUG_libfirm */
	return (long)e;
#endif /* DEBUG_libfirm */
}

/**
 * Announce to reserve extra space for each edge to be allocated.
 *
 * @param n: Size of the space to reserve
 *
 * @return Offset at which the private data will begin
 *
 * Several users can reserve extra space for private usage.
 * Each user has to remember his given offset and the size of his private data.
 * To be called before FIRM is initialized.
 */
int edges_register_private_data(size_t n) {
	int res = edges_private_size;

	assert(!edges_used && "you cannot register private edge data, if edges have been initialized");

	edges_private_size += n;
	return res;
}

/**
 * Reset the user's private data at offset 'offset'
 * The user has to remember his offset and the size of his data!
 * Caution: Using wrong values here can destroy other users private data!
 */
void edges_reset_private_data(ir_graph *irg, int offset, size_t size) {
	irg_edge_info_t       *info = _get_irg_edge_info(irg, EDGE_KIND_NORMAL);
	ir_edge_t             *edge;
	ir_edgeset_iterator_t  iter;

	foreach_ir_edgeset(&info->edges, edge, iter) {
		memset(edge + sizeof(*edge) + offset, 0, size);
	}
}

#define TIMES37(x) (((x) << 5) + ((x) << 2) + (x))

#define get_irn_out_list_head(irn) (&get_irn_out_info(irn)->outs)

#define edge_hash(edge) (TIMES37((edge)->pos) + HASH_PTR((edge)->src))

/**
 * Initialize the out information for a graph.
 * @note Dead node elimination can call this on an already initialized graph.
 */
void edges_init_graph_kind(ir_graph *irg, ir_edge_kind_t kind) {
	if (edges_activated_kind(irg, kind)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg, kind);
		size_t amount = irg->estimated_node_count * 2;

		edges_used = 1;
		if(info->allocated) {
			amount = ir_edgeset_size(&info->edges);
			ir_edgeset_destroy(&info->edges);
			obstack_free(&info->edges_obst, NULL);
		}
		obstack_init(&info->edges_obst);
		ir_edgeset_init_size(&info->edges, amount);
		info->allocated = 1;
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

		return ir_edgeset_find(&info->edges, &key);
	}

	return NULL;
}

/**
 * Get the edge object of an outgoing edge at a node.
 * Looks for an edge for all kinds.
 */
const ir_edge_t *get_irn_edge(ir_graph *irg, const ir_node *src, int pos) {
	const ir_edge_t *edge;
	if((edge = get_irn_edge_kind(irg, src, pos, EDGE_KIND_NORMAL)) == NULL)
		edge = get_irn_edge_kind(irg, src, pos, EDGE_KIND_BLOCK);
	return(edge);
}

/**
 * Change the out count
 *
 * @param tgt  the edge target
 * @param kind the kind of the edge
 */
static inline void edge_change_cnt(ir_node *tgt, ir_edge_kind_t kind, int ofs) {
	irn_edge_info_t *info = _get_irn_edge_info(tgt, kind);
	info->out_count += ofs;

#if 0
	assert(info->out_count >= 0);
	if (info->out_count == 0 && kind == EDGE_KIND_NORMAL) {
		/* tgt lost it's last user */
		int i;

		for (i = get_irn_arity(tgt) - 1; i >= -1; --i) {
			ir_node *prev = get_irn_n(tgt, i);

			edges_notify_edge(tgt, i, NULL, prev, current_ir_graph);
		}
		for (i = get_irn_deps(tgt) - 1; i >= 0; --i) {
			ir_node *prev = get_irn_dep(tgt, i);

			edges_notify_edge_kind(tgt, i, NULL, prev, EDGE_KIND_DEP, current_ir_graph);

		}
	}
#endif
}

/**
 * Verify the edge list of a node, ie. ensure it's a loop:
 * head -> e_1 -> ... -> e_n -> head
 */
static inline void vrfy_list_head(ir_node *irn, ir_edge_kind_t kind) {
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
				fprintf(stderr, "- edge(%ld) is invalid\n", edge_get_id(edge));
			if (edge->src)
				ir_fprintf(stderr, "- edge(%ld) %+F(%d)\n", edge_get_id(edge), edge->src, edge->pos);
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
void edges_notify_edge_kind(ir_node *src, int pos, ir_node *tgt,
                            ir_node *old_tgt, ir_edge_kind_t kind,
                            ir_graph *irg)
{
	const char      *msg = "";
	irg_edge_info_t *info;
	ir_edgeset_t    *edges;
	ir_edge_t        templ;
	ir_edge_t       *edge;

	assert(edges_activated_kind(irg, kind));

	/*
	 * Only do something, if the old and new target differ.
	 */
	if (tgt == old_tgt)
		return;

	info  = _get_irg_edge_info(irg, kind);
	edges = &info->edges;

	/* Initialize the edge template to search in the set. */
	templ.src = src;
	templ.pos = pos;

	/*
	 * If the target is NULL, the edge shall be deleted.
	 */
	if (tgt == NULL) {
		/* search the edge in the set. */
		edge = ir_edgeset_find(edges, &templ);

		/* mark the edge invalid if it was found */
		if (edge) {
			msg = "deleting";
			list_del(&edge->list);
			ir_edgeset_remove(edges, edge);
			edge->invalid = 1;
			edge->pos = -2;
			edge->src = NULL;
#ifdef DEBUG_libfirm
			edge->edge_nr = -1;
#endif /* DEBUG_libfirm */
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
			edge = ir_edgeset_find(edges, &templ);
			assert(edge && "edge to redirect not found!");
			assert(! edge->invalid && "Invalid edge encountered");

			msg = "redirecting";

			list_move(&edge->list, head);
			edge_change_cnt(old_tgt, kind, -1);
		}

		/* The old target was null, thus, the edge is newly created. */
		else {
			ir_edge_t *new_edge;
			ir_edge_t *edge
				= obstack_alloc(&info->edges_obst, EDGE_SIZE);
			memset(edge, 0, EDGE_SIZE);
			edge->src = src;
			edge->pos = pos;
			edge->kind = kind;
			DEBUG_ONLY(edge->src_nr = get_irn_node_nr(src));

			new_edge = ir_edgeset_insert(edges, edge);
			if(new_edge != edge) {
				obstack_free(&info->edges_obst, edge);
			}

			assert(! edge->invalid && "Freshly inserted edge is invalid?!?");
			assert(edge->list.next == NULL && edge->list.prev == NULL &&
				"New edge must not have list head initialized");

			msg = "adding";
			list_add(&edge->list, head);
#ifdef DEBUG_libfirm
			edge->edge_nr = ++last_edge_num;
#endif /* DEBUG_libfirm */
		}

		edge_change_cnt(tgt, kind, +1);
	} /* else */

#ifndef DEBUG_libfirm
	/* verify list heads */
	if (edges_dbg) {
		if (tgt)
			vrfy_list_head(tgt, kind);
		if (old_tgt)
			vrfy_list_head(old_tgt, kind);
	}
#endif

	DBG((dbg, LEVEL_5, "announce out edge: %+F %d-> %+F(%+F): %s\n", src, pos, tgt, old_tgt, msg));
}

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_graph *irg)
{
	if (edges_activated_kind(irg, EDGE_KIND_NORMAL)) {
		edges_notify_edge_kind(src, pos, tgt, old_tgt, EDGE_KIND_NORMAL, irg);
	}

	if (edges_activated_kind(irg, EDGE_KIND_BLOCK) && is_Block(src)) {
		if (pos == -1) {
			/* a MacroBlock edge: ignore it here */
		} else {
			ir_node *bl_old = old_tgt ? get_nodes_block(skip_Proj(old_tgt)) : NULL;
			ir_node *bl_tgt = NULL;

			if (tgt)
				bl_tgt = is_Bad(tgt) ? tgt : get_nodes_block(skip_Proj(tgt));

			edges_notify_edge_kind(src, pos, bl_tgt, bl_old, EDGE_KIND_BLOCK, irg);
		}
	}
}

/**
 * Delete all in edges of a given kind from the node old.
 *
 * @param old   the node
 * @param kind  the kind of edges to remove
 * @param irg   the irg of the old node
 */
static void edges_node_deleted_kind(ir_node *old, ir_edge_kind_t kind, ir_graph *irg)
{
	int i, n;

	if (!edges_activated_kind(irg, kind))
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
	struct build_walker   *w = data;
	int                   i, n;
	ir_edge_kind_t        kind = w->kind;
	ir_graph              *irg = w->irg;
	get_edge_src_n_func_t *get_n;

	get_n = edge_kind_info[kind].get_n;
	foreach_tgt(irn, i, n, kind) {
		ir_node *pred = get_n(irn, i, kind);
		edges_notify_edge_kind(irn, i, pred, NULL, kind, irg);
	}
}

/**
 * Pre-Walker: initializes the list-heads and set the out-count
 * of all nodes to 0.
 */
static void init_lh_walker(ir_node *irn, void *data) {
	struct build_walker *w   = data;
	ir_edge_kind_t      kind = w->kind;
	list_head           *head = _get_irn_outs_head(irn, kind);
	INIT_LIST_HEAD(head);
	_get_irn_edge_info(irn, kind)->out_count = 0;
}

/**
 * Pre-Walker: initializes the list-heads and set the out-count
 * of all nodes to 0.
 *
 * Additionally touches DEP nodes, as they might be DEAD.
 * THIS IS UGLY, but I don't find a better way until we
 *
 * a) ensure that dead nodes are not used as input
 * b) it might be sufficient to add those stupid NO_REG nodes
 * to the anchor
 */
static void init_lh_walker_dep(ir_node *irn, void *data) {
	struct build_walker *w   = data;
	ir_edge_kind_t      kind = w->kind;
	list_head           *head = _get_irn_outs_head(irn, kind);
	int                 i;

	INIT_LIST_HEAD(head);
	_get_irn_edge_info(irn, kind)->out_count = 0;

	for (i = get_irn_deps(irn) - 1; i >= 0; --i) {
		ir_node *dep = get_irn_dep(irn, i);

		head = _get_irn_outs_head(dep, kind);

		INIT_LIST_HEAD(head);
		_get_irn_edge_info(dep, kind)->out_count = 0;
	}
}

typedef struct visitor_info_t {
	irg_walk_func *visit;
	void *data;
} visitor_info_t;

/**
 * Visitor: initializes the list-heads and set the out-count
 * of all nodes to 0 of nodes that are not seen so far.
 */
static void visitor(ir_node *irn, void *data) {
	visitor_info_t *info = data;

	if (!irn_visited(irn)) {
		mark_irn_visited(irn);
		info->visit(irn, info->data);
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
	irg_edge_info_t     *info = _get_irg_edge_info(irg, kind);
	visitor_info_t      visit;

	w.irg  = irg;
	w.kind = kind;

	visit.data = &w;

	info->activated = 1;
	edges_init_graph_kind(irg, kind);
	if (kind == EDGE_KIND_DEP) {
		irg_walk_anchors(irg, init_lh_walker_dep, NULL, &w);
		/* Argh: Dep nodes might be dead, so we MUST visit identities first */
		visit.visit = init_lh_walker_dep;
		visit_all_identities(irg, visitor, &visit);
		irg_walk_anchors(irg, NULL, build_edges_walker, &w);
	} else {
		irg_walk_anchors(irg, init_lh_walker, build_edges_walker, &w);
		visit.visit = init_lh_walker;
		visit_all_identities(irg, visitor, &visit);
	}
}

void edges_deactivate_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	irg_edge_info_t *info = _get_irg_edge_info(irg, kind);

	info->activated = 0;
	if (info->allocated) {
		obstack_free(&info->edges_obst, NULL);
		ir_edgeset_destroy(&info->edges);
		info->allocated = 0;
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

		while (head != head->next) {
			ir_edge_t *edge = list_entry(head->next, ir_edge_t, list);
			assert(edge->pos >= -1);
			set_edge(edge->src, edge->pos, to);
		}
	}
}

static void verify_set_presence(ir_node *irn, void *data)
{
	struct build_walker *w     = data;
	ir_edgeset_t        *edges = &_get_irg_edge_info(w->irg, w->kind)->edges;
	int i, n;

	foreach_tgt(irn, i, n, w->kind) {
		ir_edge_t templ, *e;

		templ.src = irn;
		templ.pos = i;

		e = ir_edgeset_find(edges, &templ);
		if(e != NULL) {
			e->present = 1;
		} else {
			w->problem_found = 1;
#if 0
			ir_fprintf(stderr, "Edge Verifier: edge %+F,%d -> %+F (kind: \"%s\") is missing\n",
				irn, i, get_n(irn, i, w->kind), get_kind_str(w->kind));
#endif
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
#if 0
			ir_fprintf(stderr, "Edge Verifier: edge(%ld) %+F -> %+F recorded at src position %d, but src has arity %d\n",
				edge_get_id(e), e->src, irn, e->pos, get_irn_arity(e->src));
#endif
			continue;
		}

		tgt = get_n(e->src, e->pos, w->kind);

		if (irn != tgt) {
			w->problem_found = 1;
#if 0
			ir_fprintf(stderr, "Edge Verifier: edge(%ld) %+F,%d (kind \"%s\") is no out edge of %+F but of %+F\n",
				edge_get_id(e), e->src, e->pos, get_kind_str(w->kind), irn, tgt);
#endif
		}
	}
}

int edges_verify_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	struct build_walker w;
	ir_edgeset_t        *edges = &_get_irg_edge_info(irg, kind)->edges;
	ir_edge_t           *e;
	ir_edgeset_iterator_t  iter;

	w.irg           = irg;
	w.kind          = kind;
	w.reachable     = bitset_alloca(get_irg_last_idx(irg));
	w.problem_found = 0;

	/* Clear the present bit in all edges available. */
	foreach_ir_edgeset(edges, e, iter) {
		e->present = 0;
	}

	irg_walk_graph(irg, verify_set_presence, verify_list_presence, &w);

	/*
	 * Dump all edges which are not invalid and not present.
	 * These edges are superfluous and their presence in the
	 * edge set is wrong.
	 */
	foreach_ir_edgeset(edges, e, iter) {
		if (! e->invalid && ! e->present && bitset_is_set(w.reachable, get_irn_idx(e->src))) {
			w.problem_found = 1;
			ir_fprintf(stderr, "Edge Verifier: edge(%ld) %+F,%d is superfluous\n", edge_get_id(e), e->src, e->pos);
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

	if (IGNORE_NODE(irn)) {
		set_irn_link(irn, NULL);
		return;
	}

	bs = bitset_malloc(get_irg_last_idx(w->irg));
	set_irn_link(irn, bs);
}

/**
 * Increases count (stored in link field) for all operands of a node.
 */
static void count_user(ir_node *irn, void *env) {
	int i;
	int first;
	(void) env;

	first = -1;
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
	struct build_walker    *w = env;
	bitset_t               *bs;
	int                    list_cnt;
	int                    ref_cnt;
	int                    edge_cnt;
	unsigned long          idx;
	const struct list_head *head;
	const struct list_head *pos;

	if (IGNORE_NODE(irn))
		return;

	bs       = get_irn_link(irn);
	list_cnt = 0;
	ref_cnt  = 0;
	edge_cnt = _get_irn_edge_info(irn, EDGE_KIND_NORMAL)->out_count;
	head     = _get_irn_outs_head(irn, EDGE_KIND_NORMAL);

	/* We can iterate safely here, list heads have already been verified. */
	list_for_each(pos, head) {
		++list_cnt;
	}

	/* check all nodes that reference us and count edges that point number
	 * of ins that actually point to us */
	ref_cnt = 0;
	bitset_foreach(bs, idx) {
		int i, arity;
		ir_node *src = get_idx_irn(w->irg, idx);

		arity = get_irn_arity(src);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(src, i);
			if (in == irn)
				++ref_cnt;
		}
	}

	if (edge_cnt != list_cnt) {
		w->problem_found = 1;
		ir_fprintf(stderr, "Edge Verifier: edge count is %d, but %d edge(s) are recorded in list at %+F\n",
			edge_cnt, list_cnt, irn);
	}

	if (ref_cnt != list_cnt) {
		w->problem_found = 1;
		ir_fprintf(stderr, "Edge Verifier: %+F reachable by %d node(s), but the list contains %d edge(s)\n",
			irn, ref_cnt, list_cnt);

		/* Matze: buggy if a node has multiple ins pointing at irn */
#if 0
		list_for_each(pos, head) {
			ir_edge_t *edge = list_entry(pos, ir_edge_t, list);
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
#endif
	}

	bitset_free(bs);
}

/**
 * Verifies the out edges of an irg.
 */
int edges_verify(ir_graph *irg) {
	struct build_walker w;
	int    problem_found = 0;

	/* verify normal edges only */
	problem_found  = edges_verify_kind(irg, EDGE_KIND_NORMAL);

	w.irg           = irg;
	w.kind          = EDGE_KIND_NORMAL;
	w.problem_found = 0;

	/* verify counter */
	irg_walk_anchors(irg, clear_links, count_user, &w);
	irg_walk_anchors(irg, NULL, verify_edge_counter, &w);

	return problem_found ? 1 : w.problem_found;
}

void init_edges(void) {
	FIRM_DBG_REGISTER(dbg, DBG_EDGES);
	/* firm_dbg_set_mask(dbg, -1); */
}

void edges_init_dbg(int do_dbg) {
	edges_dbg = do_dbg;
}

void edges_activate(ir_graph *irg) {
	edges_activate_kind(irg, EDGE_KIND_NORMAL);
	edges_activate_kind(irg, EDGE_KIND_BLOCK);
	if (get_irg_phase_state(irg) == phase_backend)
		edges_activate_kind(irg, EDGE_KIND_DEP);
}

void edges_deactivate(ir_graph *irg) {
	if (get_irg_phase_state(irg) == phase_backend)
		edges_deactivate_kind(irg, EDGE_KIND_DEP);
	edges_deactivate_kind(irg, EDGE_KIND_BLOCK);
	edges_deactivate_kind(irg, EDGE_KIND_NORMAL);
}

int edges_assure(ir_graph *irg) {
	int activated = edges_activated(irg);

	if (!activated)
		edges_activate(irg);

	return activated;
}

int edges_assure_kind(ir_graph *irg, ir_edge_kind_t kind) {
	int activated = edges_activated_kind(irg, kind);

	if (!activated)
		edges_activate_kind(irg, kind);

	return activated;
}

void edges_node_deleted(ir_node *irn, ir_graph *irg) {
	edges_node_deleted_kind(irn, EDGE_KIND_NORMAL, irg);
	edges_node_deleted_kind(irn, EDGE_KIND_BLOCK, irg);
}


const ir_edge_t *(get_irn_out_edge_first_kind)(const ir_node *irn, ir_edge_kind_t kind) {
	return _get_irn_out_edge_first_kind(irn, kind);
}

const ir_edge_t *(get_irn_out_edge_next)(const ir_node *irn, const ir_edge_t *last) {
	return _get_irn_out_edge_next(irn, last);
}

ir_node *(get_edge_src_irn)(const ir_edge_t *edge) {
	return _get_edge_src_irn(edge);
}

int (get_edge_src_pos)(const ir_edge_t *edge) {
	return _get_edge_src_pos(edge);
}

int (get_irn_n_edges_kind)(const ir_node *irn, ir_edge_kind_t kind) {
	return _get_irn_n_edges_kind(irn, kind);
}

void dump_all_out_edges(ir_node *irn) {
	int i;
	for (i = 0; i < EDGE_KIND_LAST; ++i) {
		const ir_edge_t *edge;

		printf("kind \"%s\"\n", get_kind_str(i));
		foreach_out_edge_kind(irn, edge, i) {
			ir_printf("\t%+F(%d)\n", edge->src, edge->pos);
		}
	}
}

static void irg_block_edges_walk2(ir_node *bl,
                                irg_walk_func *pre, irg_walk_func *post,
                                void *env) {
	const ir_edge_t *edge, *next;

	if (!Block_block_visited(bl)) {
		mark_Block_block_visited(bl);

		if (pre)
			pre(bl, env);

		foreach_out_edge_kind_safe(bl, edge, next, EDGE_KIND_BLOCK) {
			/* find the corresponding successor block. */
			ir_node *pred = get_edge_src_irn(edge);
			irg_block_edges_walk2(pred, pre, post, env);
		}

		if (post)
			post(bl, env);
	}
}

void irg_block_edges_walk(ir_node *node,
                          irg_walk_func *pre, irg_walk_func *post,
                          void *env) {

	assert(edges_activated(current_ir_graph));
	assert(is_Block(node));

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_BLOCK_VISITED);

	inc_irg_block_visited(current_ir_graph);
	irg_block_edges_walk2(node, pre, post, env);

	ir_free_resources(current_ir_graph, IR_RESOURCE_BLOCK_VISITED);
}
