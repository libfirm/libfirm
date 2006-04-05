#ifndef _PPC32_TRANSFORM_H_
#define _PPC32_TRANSFORM_H_

void ppc32_register_transformers(void);
void ppc32_transform_node(ir_node *node, void *env);
void ppc32_transform_const(ir_node *node, void *env);

#endif /* _PPC32_TRANSFORM_H_ */
