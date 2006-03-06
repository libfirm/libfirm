/**
 * A facility for nodes to "survive" the dead code elimination.
 */

#ifndef _FIRM_SURVIVE_DCE_H
#define _FIRM_SURVIVE_DCE_H

typedef struct _survive_dce_t survive_dce_t;

/**
 * Make a new dead code survive instance.
 */
survive_dce_t *new_survive_dce(void);

/**
 * Free a dead node survive instance.
 */
void           free_survive_dce(survive_dce_t *sd);

/**
 * Register a storage place for a node.
 * @param sd The survive dead code private data.
 * @param place A pointer to a node pointer which shall be actualized.
 * The location given by <code>place</code> will be updated with the substitute
 * of the node it is currently pointing to after dead node elimination.
 */
void survive_dce_register_irn(survive_dce_t *sd, ir_node **place);

#endif
