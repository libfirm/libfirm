
#ifndef _BEUTIL_H
#define _BEUTIL_H

#include "firm_config.h"

#include <stdio.h>

#include "pset.h"
#include "irnode.h"
#include "bearch.h"

/* iterate over a list of ir_nodes linked by link field */
#define foreach_linked_irns(head, iter) for ((iter) = (head); (iter); (iter) = get_irn_link((iter)))

/**
 * Get an empty set.
 * This function always returns the same set.
 */
pset *be_empty_set(void);


/** Undefine this to disable debugging mode. */
#define BE_DEBUG 1

/**
 * Convenient block getter.
 * Works also, if the given node is a block.
 * @param  irn The node.
 * @return The block of the node, or the node itself, if the node is a
 *         block.
 */
static INLINE const ir_node *get_block(const ir_node *irn)
{
	return is_Block(irn) ? irn : get_nodes_block(irn);
}

static INLINE int is_firm_be_mode(const ir_mode *mode)
{
	return mode_is_data(mode);
}

/**
 * Check, if a node produces or consumes a data value.
 * If it does, it is significant for scheduling and register allocation.
 * A node produces/consumes a data value, if one of its operands is of
 * mode datab, or his retuning mode is of mode datab.
 * @param irn The node to check for.
 * @return 1, if the node is a data node, 0 if not.
 */
static INLINE int is_data_node(const ir_node *irn)
{
	int i, n;

	/* If the node produces a data value, return immediately. */
	if(is_firm_be_mode(get_irn_mode(irn)))
		return 1;

	/* else check, if it takes a data value, if that is so, return */
	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);
		if(is_firm_be_mode(get_irn_mode(op)))
			return 1;
	}

	/* Else the node does not produce/consume a data value */
	return 0;
}

/**
 * Dump a vcg graph containing the controlflow graph, the schedule and
 * allocated registers.
 * @param irg The irg. Note that scheduling, register allocation must
 * have been performed.
 */
void dump_allocated_irg(arch_env_t *env, ir_graph *irg, char *suffix);

void be_clear_links(ir_graph *irg);

static INLINE FILE *ffopen(const char *base, const char *ext, const char *mode) {
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
	buf[sizeof(buf) - 1] = '\0';
	if (! (out = fopen(buf, mode))) {
		fprintf(stderr, "Cannot open file %s in mode %s\n", buf, mode);
		return NULL;
	}
	return out;
}

/**
 * Dump a graph with schedule edges.
 * @param irg The graph.
 * @param suffix A suffix to its file name.
 */
void dump_ir_block_graph_sched(ir_graph *irg, const char *suffix);

/**
 * Dump a extended block graph with schedule edges.
 * @param irg The graph.
 * @param suffix A suffix to its file name.
 */
void dump_ir_extblock_graph_sched(ir_graph *irg, const char *suffix);

/**
 * Dumps a graph and numbers all dumps.
 * @param irg    The graph
 * @param suffix A suffix to its file name.
 * @param dumper The dump function
 */
void be_dump(ir_graph *irg, const char *suffix, void (*dumper)(ir_graph *, const char *));

/**
 * Returns the number of reachable nodes in an irg.
 * @param irg The irg.
 * @return The number of reachable nodes.
 */
unsigned get_num_reachable_nodes(ir_graph *irg);

/**
 * Sets all node inputs to BAD node.
 * @param irn  The node to be killed.
 */
void be_kill_node(ir_node *irn);

/**
 * Search for an irn in @p accept.
 * The search is started at @p start_point_exclusive and continues upwards the dom-tree
 * @return The first node out of accept if found. Else NULL is returned.
 */
ir_node *dom_up_search(pset *accept, ir_node *start_point_exclusive);

/**
 * Gets the Proj with number pn from irn.
 */
ir_node *be_get_Proj_for_pn(const ir_node *irn, long pn);

#endif /* _BEUTIL_H */
