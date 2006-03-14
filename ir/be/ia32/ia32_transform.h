#ifndef _IA32_TRANSFORM_H_
#define _IA32_TRANSFORM_H_

/**
 * Transforms the given Firm node into one or more appropriate ia32 nodes.
 */
void ia32_transform_node(ir_node *node, void *env);

/**
 * Transforms a Sub or fSub into Neg--Add iff OUT_REG == SRC2_REG.
 * THIS FUNCTIONS MUST BE CALLED AFTER REGISTER ALLOCATION.
 */
void ia32_transform_sub_to_neg_add(ir_node *irn, ia32_code_gen_t *cg);

#endif /* _IA32_TRANSFORM_H_ */
