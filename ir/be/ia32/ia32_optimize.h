#ifndef _IA32_OPTIMIZE_H_
#define _IA32_OPTIMIZE_H_

/**
 * Transforms a Firm Const into an ia32 Const and places it
 * in the Block where it's used.
 * Additionally all mode_P nodes are changed into mode_Is nodes.
 * This function is called by a walker.
 */
void ia32_place_consts_set_modes(ir_node *irn, void *env);

/**
 * Checks for address mode patterns and performs the
 * necessary transformations.
 * This function is called by a walker.
 */
void ia32_optimize_am(ir_node *irn, void *env);

#endif /* _IA32_OPTIMIZE_H_ */
