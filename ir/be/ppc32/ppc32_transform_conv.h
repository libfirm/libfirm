#ifndef _PPC32_TRANSFORM_CONV_H_
#define _PPC32_TRANSFORM_CONV_H_

void ppc32_init_conv_walk(void);

void ppc32_conv_walk(ir_node *node, void *env);
void ppc32_pretransform_walk(ir_node *node, void *env);

#endif /* _PPC32_TRANSFORM_CONV_H_ */
