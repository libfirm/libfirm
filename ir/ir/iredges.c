/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief
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
#include "error.h"
#include "irpass_t.h"

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
void ir_edgeset_init_size(ir_edgeset_t *self, size_t size);
#define hashset_init_size       ir_edgeset_init_size
#define hashset_destroy         ir_edgeset_destroy
#define hashset_insert          ir_edgeset_insert
#define hashset_remove          ir_edgeset_remove
ir_edge_t *ir_edgeset_find(const ir_edgeset_t *self, const ir_edge_t*);
#define hashset_find            ir_edgeset_find
size_t ir_edgeset_size(const ir_edgeset_t *self);
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

/**
 * A function that returns the "arity" of a given edge kind
 * for a node.
 */
typedef int (get_edge_src_arity_func_t)(const ir_node *src);

/**
 * A function that returns the pos'th edge of a given edge kind for a node.
 */
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
static ir_node *get_block_n(const ir_node *block, int pos)
{
	if (is_Block(block))
		return get_Block_cfgpred_block(block, pos);
	/* might be a Bad */
	return NULL;
}

static const ir_edge_kind_info_t edge_kind_info[EDGE_KIND_LAST] = {
	{ "normal"     , set_irn_n,   -1, get_irn_arity,  get_irn_n   },
	{ "block succs", NULL,         0, get_irn_arity,  get_block_n },
	{ "dependency",  set_irn_dep,  0, get_irn_deps,   get_irn_dep }
};

#define foreach_tgt(irn, i, n, kind) for (i = edge_kind_info[kind].first_idx, n = edge_kind_info[kind].get_arity(irn); i < n; ++i)
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

static size_t edges_private_size = 0;
#define EDGE_SIZE (sizeof(ir_edge_t) + edges_private_size)

/**
 * If set to 1, the list heads are checked every time an edge is changed.
 */
static int edges_dbg = 0;

/**
 * Returns an ID for the given edge.
 */
static inline long edge_get_id(const ir_edge_t *e)
{
	return (long)e;
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
size_t edges_register_private_data(size_t n)
{
	size_t res = edges_private_size;

	assert(!edges_used && "you cannot register private edge data, if edges have been initialized");

	edges_private_size += n;
	return res;
}

/*
 * Reset the user's private data at offset 'offset'
 * The user has to remember his offset and the size of his data!
 * Caution: Using wrong values here can destroy other users private data!
 */
void edges_reset_private_data(ir_graph *irg, int offset, unsigned size)
{
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
void edges_init_graph_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	if (edges_activated_kind(irg, kind)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg, kind);
		size_t amount = irg->estimated_node_count * 2;

		edges_used = 1;
		if (info->allocated) {
			amount = ir_edgeset_size(&info->edges);
			ir_edgeset_destroy(&info->edges);
			obstack_free(&info->edges_obst, NULL);
		}
		obstack_init(&info->edges_obst);
		INIT_LIST_HEAD(&info->free_edges);
		ir_edgeset_init_size(&info->edges, amount);
		info->allocated = 1;
	}
}

/**
 * Get the edge object of an outgoing edge at a node.
 * @param  irg  The graph, the node is in.
 * @param  src  The node at which the edge originates.
 * @param  pos  The position of the edge.
 * @param  kind The kind of the edge.
 * @return      The corresponding edge object or NULL,
 *              if no such edge exists.
 */
const ir_edge_t *get_irn_edge_kind(const ir_node *src, int pos, ir_edge_kind_t kind)
{
	ir_graph *irg = get_irn_irg(src);
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
 * Change the out count
 *
 * @param tgt  the edge target
 * @param kind the kind of the edge
 */
static inline void edge_change_cnt(ir_node *tgt, ir_edge_kind_t kind, int ofs)
{
	irn_edge_info_t *info = _get_irn_edge_info(tgt, kind);
	info->out_count += ofs;
}

/**
 * Verify the edge list of a node, ie. ensure it's a loop:
 * head -> e_1 -> ... -> e_n -> head
 */
static inline void verify_list_head(ir_node *irn, ir_edge_kind_t kind)
{
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

void edges_dump_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	irg_edge_info_t *info;
	ir_edgeset_t    *edges;
	ir_edgeset_iterator_t iter;
	ir_edge_t      *e;

	if (!edges_activated_kind(irg, kind))
		return;

	info  = _get_irg_edge_info(irg, kind);
	edges = &info->edges;
	foreach_ir_edgeset(edges, e, iter) {
		ir_printf("%+F %d %d\n", e->src, e->pos, e->invalid);
	}
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
			list_add(&edge->list, &info->free_edges);
			edge->invalid = 1;
			edge->pos = -2;
			edge->src = NULL;
			edge_change_cnt(old_tgt, kind, -1);
		} else {
			/* If the edge was not found issue a warning on the debug stream */
			msg = "edge to delete not found!\n";
		}
	} else {
		/*
		 * The target is not NULL and the old target differs
		 * from the new target, the edge shall be moved (if the
		 * old target was != NULL) or added (if the old target was
		 * NULL).
		 */
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
		} else {
			/* The old target was NULL, thus, the edge is newly created. */
			ir_edge_t *new_edge;
			ir_edge_t *edge;

			if (list_empty(&info->free_edges)) {
				edge = (ir_edge_t*)obstack_alloc(&info->edges_obst, EDGE_SIZE);
			} else {
				edge = list_entry(info->free_edges.next, ir_edge_t, list);
				list_del(&edge->list);
			}

			edge->src       = src;
			edge->pos       = pos;
			edge->invalid   = 0;
			edge->present   = 0;
			edge->kind      = kind;
			edge->list.next = NULL;
			edge->list.prev = NULL;
			memset(edge + 1, 0, edges_private_size);

			new_edge = ir_edgeset_insert(edges, edge);
			if (new_edge != edge) {
				panic("new edge exists already");
			}

			msg = "adding";
			list_add(&edge->list, head);
		}

		edge_change_cnt(tgt, kind, +1);
	} /* else */

#ifndef DEBUG_libfirm
	/* verify list heads */
	if (edges_dbg) {
		if (tgt)
			verify_list_head(tgt, kind);
		if (old_tgt)
			verify_list_head(old_tgt, kind);
	}
#endif

	DBG((dbg, LEVEL_5, "announce out edge: %+F %d-> %+F(%+F): %s\n", src, pos, tgt, old_tgt, msg));
}

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt,
                       ir_graph *irg)
{
	if (edges_activated_kind(irg, EDGE_KIND_NORMAL)) {
		edges_notify_edge_kind(src, pos, tgt, old_tgt, EDGE_KIND_NORMAL, irg);
	}

	if (edges_activated_kind(irg, EDGE_KIND_BLOCK)) {
		if (is_Block(src)) {
			ir_node *bl_old = old_tgt ? get_nodes_block(old_tgt) : NULL;
			ir_node *bl_tgt = NULL;

			if (tgt)
				bl_tgt = is_Bad(tgt) ? tgt : get_nodes_block(tgt);

			edges_notify_edge_kind(src, pos, bl_tgt, bl_old, EDGE_KIND_BLOCK, irg);
		} else if (get_irn_mode(src) == mode_X && old_tgt != NULL && is_Block(old_tgt)) {
			/* moving a jump node from one block to another */
			const ir_edge_t *edge;
			const ir_edge_t *next;
			foreach_out_edge_kind_safe(old_tgt, edge, next, EDGE_KIND_BLOCK) {
				ir_node *succ       = get_edge_src_irn(edge);
				int      pos        = get_edge_src_pos(edge);
				ir_node *block_pred = get_Block_cfgpred(succ, pos);
				if (block_pred != src)
					continue;
				edges_notify_edge_kind(succ, pos, tgt, old_tgt,
				                       EDGE_KIND_BLOCK, irg);
			}
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
static void edges_node_deleted_kind(ir_node *old, ir_edge_kind_t kind)
{
	int i, n;
	ir_graph *irg = get_irn_irg(old);

	if (!edges_activated_kind(irg, kind))
		return;

	DBG((dbg, LEVEL_5, "node deleted (kind: %s): %+F\n", get_kind_str(kind), old));

	foreach_tgt(old, i, n, kind) {
		ir_node *old_tgt = get_n(old, i, kind);
		edges_notify_edge_kind(old, i, NULL, old_tgt, kind, irg);
	}
}

/**
 * A node might be revivaled by CSE. Assure its edges.
 *
 * @param irn   the node
 * @param kind  the kind of edges to remove
 * @param irg   the irg of the old node
 */
static void edges_node_revival_kind(ir_node *irn, ir_edge_kind_t kind)
{
	irn_edge_info_t *info;
	int             i, n;
	ir_graph        *irg = get_irn_irg(irn);

	if (!edges_activated_kind(irg, kind))
		return;

	info = _get_irn_edge_info(irn, kind);
	if (info->edges_built)
		return;

	DBG((dbg, LEVEL_5, "node revivaled (kind: %s): %+F\n", get_kind_str(kind), irn));

	foreach_tgt(irn, i, n, kind) {
		ir_node *tgt = get_n(irn, i, kind);
		edges_notify_edge_kind(irn, i, tgt, NULL, kind, irg);
	}
	info->edges_built = 1;
}

typedef struct build_walker {
	ir_edge_kind_t kind;
	bitset_t       *reachable;
	unsigned       problem_found;
} build_walker;

/**
 * Post-Walker: notify all edges
 */
static void build_edges_walker(ir_node *irn, void *data)
{
	build_walker          *w = (build_walker*)data;
	int                   i, n;
	ir_edge_kind_t        kind = w->kind;
	ir_graph              *irg = get_irn_irg(irn);

	foreach_tgt(irn, i, n, kind) {
		ir_node *pred = get_n(irn, i, kind);
		edges_notify_edge_kind(irn, i, pred, NULL, kind, irg);
	}
	_get_irn_edge_info(irn, kind)->edges_built = 1;
}

/**
 * Pre-Walker: initializes the list-heads and set the out-count
 * of all nodes to 0.
 */
static void init_lh_walker(ir_node *irn, void *data)
{
	build_walker   *w    = (build_walker*)data;
	ir_edge_kind_t  kind = w->kind;
	list_head      *head = _get_irn_outs_head(irn, kind);
	INIT_LIST_HEAD(head);
	_get_irn_edge_info(irn, kind)->edges_built = 0;
	_get_irn_edge_info(irn, kind)->out_count   = 0;
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
static void init_lh_walker_dep(ir_node *irn, void *data)
{
	build_walker   *w    = (build_walker*)data;
	ir_edge_kind_t  kind = w->kind;
	list_head      *head = _get_irn_outs_head(irn, kind);
	int             i;

	INIT_LIST_HEAD(head);
	_get_irn_edge_info(irn, kind)->edges_built = 0;
	_get_irn_edge_info(irn, kind)->out_count   = 0;

	for (i = get_irn_deps(irn) - 1; i >= 0; --i) {
		ir_node *dep = get_irn_dep(irn, i);

		head = _get_irn_outs_head(dep, kind);

		INIT_LIST_HEAD(head);
		_get_irn_edge_info(dep, kind)->edges_built = 0;
		_get_irn_edge_info(dep, kind)->out_count   = 0;
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
static void visitor(ir_node *irn, void *data)
{
	visitor_info_t *info = (visitor_info_t*)data;

	if (is_Deleted(irn))
		return;
	if (!is_Block(irn) && is_Deleted(get_nodes_block(irn)))
		return;

	if (!irn_visited_else_mark(irn)) {
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

	w.kind = kind;

	visit.data = &w;

	assert(!info->activated);

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
void edges_reroute_kind(ir_node *from, ir_node *to, ir_edge_kind_t kind)
{
	ir_graph *irg = get_irn_irg(from);
	set_edge_func_t *set_edge = edge_kind_info[kind].set_edge;

	if (set_edge && edges_activated_kind(irg, kind)) {
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
	build_walker *w     = (build_walker*)data;
	ir_graph     *irg   = get_irn_irg(irn);
	ir_edgeset_t *edges = &_get_irg_edge_info(irg, w->kind)->edges;
	int i, n;

	foreach_tgt(irn, i, n, w->kind) {
		ir_edge_t templ, *e;

		templ.src = irn;
		templ.pos = i;

		e = ir_edgeset_find(edges, &templ);
		if (e != NULL) {
			e->present = 1;
		} else {
			w->problem_found = 1;
		}
	}
}

static void verify_list_presence(ir_node *irn, void *data)
{
	build_walker    *w = (build_walker*)data;
	const ir_edge_t *e;

	bitset_set(w->reachable, get_irn_idx(irn));

	/* check list heads */
	verify_list_head(irn, w->kind);

	foreach_out_edge_kind(irn, e, w->kind) {
		ir_node *tgt;

		if (w->kind == EDGE_KIND_NORMAL && get_irn_arity(e->src) <= e->pos) {
			w->problem_found = 1;
			continue;
		}

		tgt = get_n(e->src, e->pos, w->kind);

		if (irn != tgt) {
			w->problem_found = 1;
		}
	}
}

int edges_verify_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	struct build_walker w;
	ir_edgeset_t        *edges = &_get_irg_edge_info(irg, kind)->edges;
	ir_edge_t           *e;
	ir_edgeset_iterator_t  iter;

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
static void clear_links(ir_node *irn, void *env)
{
	bitset_t     *bs;
	ir_graph     *irg;
	(void) env;

	if (IGNORE_NODE(irn)) {
		set_irn_link(irn, NULL);
		return;
	}

	irg = get_irn_irg(irn);
	bs  = bitset_malloc(get_irg_last_idx(irg));
	set_irn_link(irn, bs);
}

/**
 * Increases count (stored in link field) for all operands of a node.
 */
static void count_user(ir_node *irn, void *env)
{
	int i;
	int first;
	(void) env;

	first = is_Block(irn) ? 0 : -1;
	for (i = get_irn_arity(irn) - 1; i >= first; --i) {
		ir_node  *op = get_irn_n(irn, i);
		bitset_t *bs = (bitset_t*)get_irn_link(op);

		if (bs)
			bitset_set(bs, get_irn_idx(irn));
	}
}

/**
 * Verifies if collected count, number of edges in list and stored edge count are in sync.
 */
static void verify_edge_counter(ir_node *irn, void *env)
{
	build_walker           *w = (build_walker*)env;
	bitset_t               *bs;
	int                    list_cnt;
	int                    ref_cnt;
	int                    edge_cnt;
	size_t                 idx;
	const struct list_head *head;
	const struct list_head *pos;
	ir_graph               *irg;

	if (IGNORE_NODE(irn))
		return;

	bs       = (bitset_t*)get_irn_link(irn);
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
	irg = get_irn_irg(irn);
	ref_cnt = 0;
	bitset_foreach(bs, idx) {
		int i, arity;
		ir_node *src = get_idx_irn(irg, idx);

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
	}

	bitset_free(bs);
}

/**
 * Verifies the out edges of an irg.
 */
int edges_verify(ir_graph *irg)
{
	struct build_walker w;
	int    problem_found = 0;

	/* verify normal edges only */
	problem_found  = edges_verify_kind(irg, EDGE_KIND_NORMAL);

	w.kind          = EDGE_KIND_NORMAL;
	w.problem_found = 0;

	/* verify counter */
	irg_walk_anchors(irg, clear_links, count_user, &w);
	irg_walk_anchors(irg, NULL, verify_edge_counter, &w);

	return problem_found ? 1 : w.problem_found;
}

typedef struct pass_t {
	ir_graph_pass_t pass;
	unsigned        assert_on_problem;
} pass_t;

/**
 * Wrapper to edges_verify to be run as an ir_graph pass.
 */
static int edges_verify_wrapper(ir_graph *irg, void *context)
{
	pass_t *pass           = (pass_t*)context;
	int     problems_found = edges_verify(irg);
	/* do NOT rerun the pass if verify is ok :-) */
	assert(problems_found && pass->assert_on_problem);
	return 0;
}

/* Creates an ir_graph pass for edges_verify(). */
ir_graph_pass_t *irg_verify_edges_pass(const char *name, unsigned assert_on_problem)
{
	pass_t *pass = XMALLOCZ(pass_t);

	def_graph_pass_constructor(
		&pass->pass, name ? name : "edges_verify", edges_verify_wrapper);

	/* neither dump nor verify */
	pass->pass.dump_irg   = (DUMP_ON_IRG_FUNC)ir_prog_no_dump;
	pass->pass.verify_irg = (RUN_ON_IRG_FUNC)ir_prog_no_verify;

	pass->assert_on_problem = assert_on_problem;
	return &pass->pass;
}

void init_edges(void)
{
	FIRM_DBG_REGISTER(dbg, DBG_EDGES);
}

void edges_init_dbg(int do_dbg)
{
	edges_dbg = do_dbg;
}

void edges_activate(ir_graph *irg)
{
	edges_activate_kind(irg, EDGE_KIND_NORMAL);
	edges_activate_kind(irg, EDGE_KIND_BLOCK);
	if (get_irg_phase_state(irg) == phase_backend)
		edges_activate_kind(irg, EDGE_KIND_DEP);
}

void edges_deactivate(ir_graph *irg)
{
	if (get_irg_phase_state(irg) == phase_backend)
		edges_deactivate_kind(irg, EDGE_KIND_DEP);
	edges_deactivate_kind(irg, EDGE_KIND_BLOCK);
	edges_deactivate_kind(irg, EDGE_KIND_NORMAL);
}

int edges_assure(ir_graph *irg)
{
	int activated = 0;

	if (edges_activated_kind(irg, EDGE_KIND_BLOCK)) {
		activated = 1;
	} else {
		edges_activate_kind(irg, EDGE_KIND_BLOCK);
	}
	if (edges_activated_kind(irg, EDGE_KIND_NORMAL)) {
		activated = 1;
	} else {
		edges_activate_kind(irg, EDGE_KIND_NORMAL);
	}

	return activated;
}

int edges_assure_kind(ir_graph *irg, ir_edge_kind_t kind)
{
	int activated = edges_activated_kind(irg, kind);

	if (!activated)
		edges_activate_kind(irg, kind);

	return activated;
}

void edges_node_deleted(ir_node *irn)
{
	edges_node_deleted_kind(irn, EDGE_KIND_NORMAL);
	edges_node_deleted_kind(irn, EDGE_KIND_BLOCK);
}

void edges_node_revival(ir_node *irn)
{
	edges_node_revival_kind(irn, EDGE_KIND_NORMAL);
	edges_node_revival_kind(irn, EDGE_KIND_BLOCK);
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

static void irg_walk_edges2(ir_node *node, irg_walk_func *pre,
                            irg_walk_func *post, void *env)
{
	const ir_edge_t *edge, *next;

	if (irn_visited_else_mark(node))
		return;

	if (pre != NULL)
		pre(node, env);

	foreach_out_edge_kind_safe(node, edge, next, EDGE_KIND_NORMAL) {
		/* find the corresponding successor block. */
		ir_node *pred = get_edge_src_irn(edge);
		irg_walk_edges2(pred, pre, post, env);
	}

	if (post != NULL)
		post(node, env);
}

void irg_walk_edges(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                    void *env)
{
	ir_graph *irg = get_irn_irg(node);

	assert(edges_activated(irg));
	assert(is_Block(node));

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	inc_irg_visited(irg);
	irg_walk_edges2(node, pre, post, env);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

static void irg_block_edges_walk2(ir_node *bl, irg_walk_func *pre,
                                  irg_walk_func *post, void *env)
{
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

void irg_block_edges_walk(ir_node *node, irg_walk_func *pre,
                          irg_walk_func *post, void *env)
{
	ir_graph *irg = get_irn_irg(node);

	assert(edges_activated(irg));
	assert(is_Block(node));

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);

	inc_irg_block_visited(irg);
	irg_block_edges_walk2(node, pre, post, env);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}
