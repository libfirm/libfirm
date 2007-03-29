#ifndef _MIPS_TRANSFORM_H_
#define _MIPS_TRANSFORM_H_

/**
 * Create Firm assembler for a copyB node.
 *
 * @param blk   the block where to place the code
 * @param node  the copyB node to lower
 *
 * @return the memory from the lowered CopyB
 */
ir_node *gen_code_for_CopyB(ir_node *blk, ir_node *node);

void mips_pre_transform_node(ir_node *node, void *env);
void mips_transform_node(ir_node *node, void *env);
void mips_after_ra_walker(ir_node *node, void *env);

#endif /* _MIPS_TRANSFORM_H_ */
