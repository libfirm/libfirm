
#ifndef _BEUTIL_H
#define _BEUTIL_H

#include <stdio.h>

#include "irnode.h"
#include "config.h"

#include "bearch.h"

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
 * Make each constant local to its use.
 * This duplicates all constants in order to simulate a realistic
 * register pressure.
 * @param irg The graph.
 */
void localize_consts(ir_graph *irg);

/**
 * Dump a vcg graph containing the controlflow graph, the schedule and
 * allocated registers.
 * @param irg The irg. Note that scheduling, register allocation must
 * have been performed.
 */
void dump_allocated_irg(arch_env_t *env, ir_graph *irg, char *suffix);



static INLINE FILE *ffopen(const char *base, const char *ext, const char *mode) {
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
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

#endif
