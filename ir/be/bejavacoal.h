
#ifndef _BEJAVACOAL_H
#define _BEJAVACOAL_H

struct _be_java_coal_t;
typedef struct _be_java_coal_t be_java_coal_t;

/**
 * Add an interference edge
 * @param n first node id.
 * @param m second node id.
 */
void be_java_coal_add_int_edge(be_java_coal_t *c, int n, int m);

/**
 * Add an affinity edge.
 * @param n first node id.
 * @param m second node id.
 * @param costs Costs for the edge.
 */
void be_java_coal_add_aff_edge(be_java_coal_t *c, int n, int m, int costs);

/**
 * Set the color of a node.
 * @param n The node.
 * @param col The color.
 */
void be_java_coal_set_color(be_java_coal_t *c, int n, int col);

/**
 * Set debug information for a node.
 * @param n The node.
 * @param dbg Some string copied to Java.
 */
void be_java_coal_set_debug(be_java_coal_t *c, int n, const char *dbg);

/**
 * Forbid a color for a node.
 * Afterwards, the node may not be assigned that color.
 * @param n The node.
 * @param col The color.
 */
void be_java_coal_forbid_color(be_java_coal_t *c, int n, int col);

/**
 * Start the coalescing.
 */
void be_java_coal_coalesce(be_java_coal_t *c);

/**
 * Dump the graph into a dot file.
 * @param fn Filename to dump to.
 */
void be_java_coal_dump(be_java_coal_t *c, const char *fn);

/**
 * Get the color of a node.
 * @param n The node.
 * @return The color of the node.
 */
int be_java_coal_get_color(be_java_coal_t *c, int n);

/**
 * Init the JAVA coalescer.
 * @param graph_name The name of the graph to coalesce.
 * @param n_nodes The number of nodes in the graph. Each node has an ID which ranges from 0 to n_nodes - 1.
 * @param n_regs  The number of colors available.
 * @param dbg_level Te debug level for the coalescer. 0 means quiet. >0 more verbose.
 * @return The coalescing object.
 */
be_java_coal_t *be_java_coal_init(const char *graph_name, int n_nodes, int n_regs, int dbg_level);

/**
 * Start the JVM.
 * This is also done lazily by be_java_coal_init() but as that is called by
 * the coalescing driver, it might tamper the runtime measurements. So here is
 * an extra call.
 */
void be_java_coal_start_jvm(void);

/**
 * Destroy the coalescing object.
 */
void be_java_coal_destroy(be_java_coal_t *c);

#endif
