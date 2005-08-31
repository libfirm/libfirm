/**
 * Always available outs.
 * @author Sebastian Hack
 * @date 14.1.2005
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "iredges_t.h"
#include "irdump_t.h"
#include "irprintf.h"
#include "irhooks.h"
#include "debug.h"
#include "set.h"

static firm_dbg_module_t *dbg;

#if FIRM_EDGES_INPLACE

/**
 * This flag is set to 1, if the edges get initialized for an irg.
 * Then register additional data is forbidden.
 */
static int edges_used = 0;

static int edges_private_size = 0;

int edges_register_private_data(size_t n)
{
	int res = edges_private_size;

	assert(!edges_used && "you cannot register private edge data, if edges have been initialized");

	edges_private_size += n;
	return res;
}

#define TIMES37(x) (((x) << 5) + ((x) << 2) + (x))

#define get_irn_out_list_head(irn) (&get_irn_out_info(irn)->outs)

static int edge_cmp(const void *p1, const void *p2, size_t len)
{
	const ir_edge_t *e1 = p1;
	const ir_edge_t *e2 = p2;
	int res = e1->src == e2->src && e1->pos == e2->pos;

	return !res;
}

#define edge_hash(edge) (TIMES37((edge)->pos) + HASH_PTR((edge)->src))

/**
 * Initialize the out information for a graph.
 * @note Dead node elim can call this on an already initialized graph.
 */
void edges_init_graph(ir_graph *irg)
{
	if(edges_activated(irg)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg);
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
const ir_edge_t *get_irn_edge(ir_graph *irg, const ir_node *src, int pos)
{
	if(edges_activated(irg)) {
		irg_edge_info_t *info = _get_irg_edge_info(irg);
		ir_edge_t templ;

		templ.src = (ir_node *) src;
		templ.pos = pos;
		return set_find(info->edges, &templ, sizeof(templ), edge_hash(&templ));
	}

	return NULL;
}

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_graph *irg)
{
	const char *msg = "";

	if(!edges_activated(irg))
		return;

	assert(node_is_in_irgs_storage(irg, src) && "source not in irg");

	/*
	 * Only do something, if the old and new target differ.
	 */
	if(tgt != old_tgt) {
		int is_block_edge = is_Block(src);
		set *edges = _get_irg_edge_info(irg)->edges;
		ir_block_edge_t space;
		ir_edge_t *templ = (ir_edge_t *) &space;
		ir_edge_t *edge;
		size_t size;

    /*
     * This is scray, but:
     * If two entries in a set do not have the same size, they are
     * treated as unequal, ignoring the comparison function.
     * So, edges from blocks have extra storage (they are
     * ir_block_edge_t's).
		 *
		 * Also add the amount of registered private data to the
		 * size of the edge.
     */
    size = edges_private_size +
			is_block_edge ? sizeof(ir_block_edge_t) : sizeof(ir_edge_t);

		/* Initialize the edge template to search in the set. */
    memset(templ, 0, size);
#ifdef DEBUG_libfirm
		templ->src_nr = get_irn_node_nr(src);
#endif
		templ->src = src;
		templ->pos = pos;
		templ->invalid = 0;
		templ->present = 0;

		/*
		 * If the target is NULL, the edge shall be deleted.
		 */
		if(tgt == NULL) {
			/* search the edge in the set. */
			edge = set_find(edges, templ, size, edge_hash(templ));

			/* mark the edge invalid if it was found */
			if(edge) {
        ir_block_edge_t *block_edge = (ir_block_edge_t *) edge;

				msg = "deleting";
				list_del(&edge->list);
				edge->invalid = 1;
				edge->pos = -2;
				edge->src = NULL;

        /*
         * If the edge is a cf edge, we delete it also
         * from the list of all block successor edges.
         */
        if(is_block_edge)
          list_del(&block_edge->succ_list);
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
			struct list_head *head = _get_irn_outs_head(tgt);

      /*
       * The list head in the block of the edges target.
       * Therein all control flow edges directed at that block
       * are recorded.
       */
      struct list_head *succ_head =
        is_block_edge ? _get_block_succ_head(get_nodes_block(tgt)) : NULL;

      ir_block_edge_t *block_edge;

			if(!node_is_in_irgs_storage(irg, tgt))
				return;

			assert(head->next && head->prev &&
					"target list head must have been initialized");

			/*
			 * insert the edge, if it is not yet in the set or return
			 * the instance in the set.
			 */
			edge = set_insert(edges, templ, size, edge_hash(templ));
      block_edge = (ir_block_edge_t *) edge;

#ifdef DEBUG_libfirm
			assert(!edge->invalid && "Invalid edge encountered");
#endif

			/* If the old target is not null, the edge is moved. */
			if(old_tgt) {
				msg = "redirecting";
				list_move(&edge->list, head);

        /* If the edge is a cf edge, move it from the successor list. */
        if(is_block_edge)
          list_move(&block_edge->succ_list, succ_head);

				_get_irn_edge_info(old_tgt)->out_count -= 1;
			}

			/* The old target was null, thus, the edge is newly created. */
			else {
				msg = "adding";
				list_add(&edge->list, head);

        /*
         * If the edge is cf edge, enter it into the successor list
         * of the target node's block.
         */
        if(is_block_edge)
          list_add(&block_edge->succ_list, succ_head);
			}

			_get_irn_edge_info(tgt)->out_count += 1;
		} /* else */
	}

	/* If the target and the old target are equal, nothing is done. */
	DBG((dbg, LEVEL_5, "announce out edge: %n[%p] %d-> %n[%p](%n[%p]): %s\n",
				src, src, pos, tgt, tgt, old_tgt, old_tgt, msg));
}

void edges_node_deleted(ir_node *old, ir_graph *irg)
{
	if(edges_activated(irg)) {
		int not_a_block = !is_Block(old);
		ir_edge_t templ;
		int i, n;

		templ.src = old;
		DBG((dbg, LEVEL_5, "node deleted: %n\n", old));

		/* Change to get_irn_n */
		for(i = -not_a_block, n = get_irn_arity(old); i < n; ++i) {
			ir_node *old_tgt = get_irn_n(old, i);
			DBG((dbg, LEVEL_5, "\tdelete to old target %n\n", old_tgt));
			edges_notify_edge(old, i, NULL, old_tgt, irg);
		}

	}
}

void edges_invalidate(ir_node *irn, ir_graph *irg)
{
	edges_node_deleted(irn, irg);
}

static void build_edges_walker(ir_node *irn, void *data)
{
	ir_graph *irg = data;
	int not_a_block = !is_Block(irn);
	int i, n;

	for(i = -not_a_block, n = get_irn_arity(irn); i < n; ++i)
		edges_notify_edge(irn, i, get_irn_n(irn, i), NULL, irg);
}

static void init_lh_walker(ir_node *irn, void *data)
{
	INIT_LIST_HEAD(_get_irn_outs_head(irn));
	if(is_Block(irn))
		INIT_LIST_HEAD(_get_block_succ_head(irn));
}

void edges_activate(ir_graph *irg)
{
	irg_edge_info_t *info = _get_irg_edge_info(irg);

	info->activated = 1;
	edges_init_graph(irg);
	irg_walk_graph(irg, init_lh_walker, build_edges_walker, irg);
}

void edges_deactivate(ir_graph *irg)
{
	irg_edge_info_t *info = _get_irg_edge_info(irg);

	info->activated = 0;
	if(info->edges) {
		del_set(info->edges);
    info->edges = NULL;
  }
}

int (edges_activated)(const ir_graph *irg)
{
	return _edges_activated(irg);
}


/**
 * Reroute all use-edges from a node to another.
 * @param from The node whose use-edges shall be withdrawn.
 * @param to The node to which all the use-edges of @p from shall be
 * sent to.
 */
void edges_reroute(ir_node *from, ir_node *to, ir_graph *irg)
{
	if(edges_activated(irg)) {
		struct list_head *head = _get_irn_outs_head(from);

		DBG((firm_dbg_register(DBG_EDGES), LEVEL_5,
					"reroute from %n to %n\n", from, to));

		while(head != head->next) {
			ir_edge_t *edge = list_entry(head->next, ir_edge_t, list);
			// DBG((dbg, LEVEL_5, "\t%n %d\n", edge->src, edge->pos));
			assert(edge->pos >= -1);
			set_irn_n(edge->src, edge->pos, to);
		}
	}
}

static void verify_set_presence(ir_node *irn, void *data)
{
	ir_graph *irg = data;
	set *edges = _get_irg_edge_info(irg)->edges;
	int not_a_block = !is_Block(irn);
	int i, n;

	for(i = 0, n = get_irn_arity(irn) + not_a_block; i < n; ++i) {
    ir_block_edge_t space;
		ir_edge_t *templ = (ir_edge_t *) &space;
		ir_edge_t *e;
    size_t size = not_a_block ? sizeof(ir_edge_t) : sizeof(ir_block_edge_t);

		templ->src = irn;
		templ->pos = i - not_a_block;

		e = set_find(edges, templ, size, edge_hash(templ));
		if(e != NULL)
			e->present = 1;
		else
			DBG((dbg, LEVEL_DEFAULT, "edge %n,%d is missing\n", irn, templ->pos));
	}
}

static void verify_list_presence(ir_node *irn, void *data)
{
	const ir_edge_t *e;

	foreach_out_edge(irn, e) {
		ir_node *tgt = get_irn_n(e->src, e->pos);
		if(irn != tgt)
			DBG((dbg, LEVEL_DEFAULT, "edge %n,%d is no out edge of %n but of %n\n",
					e->src, e->pos, irn, tgt));
	}

}

void edges_verify(ir_graph *irg)
{
	set *edges = _get_irg_edge_info(irg)->edges;
	ir_edge_t *e;

	/* Clear the present bit in all edges available. */
	for(e = set_first(edges); e; e = set_next(edges))
		e->present = 0;

	irg_walk_graph(irg, verify_set_presence, verify_list_presence, irg);

	/*
	 * Dump all edges which are not invalid and not present.
	 * These edges are superfluous and their presence in the
	 * edge set is wrong.
	 */
	for(e = set_first(edges); e; e = set_next(edges)) {
		if(!e->invalid && !e->present)
			DBG((dbg, LEVEL_DEFAULT, "edge %n,%d is superfluous\n", e->src, e->pos));
	}
}

void init_edges(void)
{
	dbg = firm_dbg_register(DBG_EDGES);
	/* firm_dbg_set_mask(dbg, -1); */
}


const ir_edge_t *(get_irn_out_edge_first)(const ir_node *irn)
{
	return _get_irn_out_edge_first(irn);
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

int (get_irn_n_edges)(const ir_node *irn)
{
  return _get_irn_n_edges(irn);
}


#endif /* FIRM_EDGES_INPLACE */
