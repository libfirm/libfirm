
/**
 * If conversion.
 * @author Sebastian Hack
 * @date 10.2.2005
 */

#ifndef _FIRM_IF_CONV_H
#define _FIRM_IF_CONV_H

/**
 * The parameters structure.
 */
typedef struct _opt_if_conv_info_t {
	int max_depth;		/**< The maximum depth up to which expressions
											are examined when it has to be decided if they
											can be placed into another block. */
} opt_if_conv_info_t;


/**
 * Perform If conversion on a graph.
 * @param irg The graph.
 * @param params The parameters for the if conversion.
 */
void opt_if_conv(ir_graph *irg, opt_if_conv_info_t *params);

#endif
