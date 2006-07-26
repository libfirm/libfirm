
#ifndef _BEJAVACOAL_H
#define _BEJAVACOAL_H

struct _java_coal_t;
typedef struct _java_coal_t java_coal_t;


#ifdef WITH_LIBCORE

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include "firm_config.h"

/**
 * Register libcore options.
 */
void java_coal_register_options(lc_opt_entry_t *grp)

#endif /* WITH_LIBCORE */

/**
 * Add an interference edge
 * @param n first node id.
 * @param m second node id.
 */
void java_coal_add_int_edge(java_coal_t *c, int n, int m);

/**
 * Add an affinity edge.
 * @param n first node id.
 * @param m second node id.
 * @param costs Costs for the edge.
 */
void java_coal_add_aff_edge(java_coal_t *c, int n, int m, int costs);

/**
 * Set the color of a node.
 * @param n The node.
 * @param col The color.
 */
void java_coal_set_color(java_coal_t *c, int n, int col);

/**
 * Forbid a color for a node.
 * Afterwards, the node may not be assigned that color.
 * @param n The node.
 * @param col The color.
 */
void java_coal_forbid_color(java_coal_t *c, int n, int col);

/**
 * Start the coalescing.
 */
void java_coal_coalesce(java_coal_t *c);

/**
 * Dump the graph into a dot file.
 * @param fn Filename to dump to.
 */
void java_coal_dump(java_coal_t *c, const char *fn);

/**
 * Get the color of a node.
 * @param n The node.
 * @return The color of the node.
 */
int java_coal_get_color(java_coal_t *c, int n);

/**
 * Init the JAVA coalescer.
 * @param graph_name The name of the graph to coalesce.
 * @param n_nodes The number of nodes in the graph. Each node has an ID which ranges from 0 to n_nodes - 1.
 * @param n_regs  The number of colors available.
 * @param dbg_level Te debug level for the coalescer. 0 means quiet. >0 more verbose.
 * @return The coalescing object.
 */
java_coal_t *java_coal_init(const char *graph_name, int n_nodes, int n_regs, int dbg_level);

/**
 * Destroy the coalescing object.
 */
void java_coal_destroy(java_coal_t *c);


#endif
