#ifndef _ARM_TRANSFORM_H_
#define _ARM_TRANSFORM_H_

void arm_move_consts(ir_node *node, void *env);
void arm_move_symconsts(ir_node *node, void *env);

void arm_register_transformers(void);
void arm_transform_node(ir_node *node, void *env);

#endif /* _ARM_TRANSFORM_H_ */
