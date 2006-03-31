#ifndef _IA32_TRANSFORM_H_
#define _IA32_TRANSFORM_H_

#include "firm_config.h"
#include "bearch_ia32_t.h"

/**
 * Transforms the given Firm node into one or more appropriate ia32 nodes.
 */
void ia32_transform_node(ir_node *node, void *env);

/**
 * Transforms a Sub or fSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg);

/**
 * Transforms a LEA into an Add if possible
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_lea_to_add(ir_node *irn, ia32_code_gen_t *cg);

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn);
#endif /* NDEBUG */

#endif /* _IA32_TRANSFORM_H_ */
