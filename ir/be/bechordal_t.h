
/**
 * Internal datastructures for the chordal register allocator.
 * @author Sebastian Hack
 * @date 25.1.2005
 */

#ifndef _BECHORDAL_T_H
#define _BECHORDAL_T_H

#define BUILD_GRAPH

/**
 * A liveness interval border.
 */
typedef struct _border_t {
#ifdef DEBUG_libfirm
	unsigned magic;								/**< A magic number for checking. */
#endif
	struct list_head list;				/**< list head for queuing. */
	struct _border_t *other_end;	/**< The other end of the border. */
	const ir_node *irn;						/**< The node. */
	unsigned step;								/**< The number equal to the interval border. */
	unsigned pressure;						/**< The pressure at this interval border.
																	(The border itself is counting). */
	unsigned is_def : 1;					/**< Does this border denote a use or a def. */
	unsigned is_real : 1;					/**< Is the def/use real? Or is it just inserted
																	at block beginnings or ends to ensure that inside
																	a block, each value has one begin and one end. */
} border_t;

#ifdef BUILD_GRAPH
typedef struct _if_node_t {
	int nnr;
	pset *neighb;
} if_node_t;

typedef struct _if_edge_t {
	int src, tgt;
} if_edge_t;

set *be_ra_get_ifg(ir_graph *irg); /**< Deprecated. Use be_ra_get_ifg_edges. */
set *be_ra_get_ifg_edges(ir_graph *irg);
set *be_ra_get_ifg_nodes(ir_graph *irg);
#endif

extern void be_ra_chordal_spill(ir_graph *irg);

#endif /* _BECHORDAL_T_H */
