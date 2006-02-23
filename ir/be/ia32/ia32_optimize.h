#ifndef _IA32_OPTIMIZE_H_
#define _IA32_OPTIMIZE_H_

/**
 * Transforms a Firm Const into an ia32 Const and places it
 * in the Block where it's used.
 * This function is called by a walker.
 */
void ia32_place_consts(ir_node *irn, void *env);

/**
 * Checks for address mode patterns and performs the
 * necessary transformations.
 * This function is called by a walker.
 */
void ia32_optimize_am(ir_node *irn, void *env);

#endif /* _IA32_OPTIMIZE_H_ */
