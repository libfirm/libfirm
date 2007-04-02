/**
 * Function prototypes for Firm into ia32-Firm transformations.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_TRANSFORM_H_
#define _IA32_TRANSFORM_H_

#include "firm_config.h"
#include "bearch_ia32_t.h"

/**
 * Transform firm nodes to x86 assembler nodes
 */
void ia32_transform_graph(ia32_code_gen_t *cg);

#if 0
/**
 * Enters all transform functions into the generic pointer
 */
void ia32_register_transformers(void);

/**
 * Transforms the given Firm node into one or more appropriate ia32 nodes.
 */
void ia32_transform_node(ir_node *node, void *env);
#endif

/**
 * The Psi selector can be a tree of compares combined with "And"s and "Or"s.
 * We create a Set node, respectively a xCmp in case the Psi is a float, for each
 * compare, which causes the compare result to be stores in a register.  The
 * "And"s and "Or"s are transformed later, we only adjust their mode.
 */
void ia32_transform_psi_cond_tree(ir_node *node, void *env);

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn);
#endif /* NDEBUG */

typedef enum {
	ia32_SSIGN, ia32_DSIGN, ia32_SABS, ia32_DABS, ia32_known_const_max
} ia32_known_const_t;

/**
 * Generate a known floating point constant
 */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t kct);

#endif /* _IA32_TRANSFORM_H_ */
