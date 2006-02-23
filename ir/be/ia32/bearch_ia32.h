#ifndef _BEARCH_IA32_H_
#define _BEARCH_IA32_H_

#include "pset.h"
#include "../bearch.h"

/**
 * Creates the unique per irg GP NoReg node.
 */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg);

/**
 * Creates the unique per irg FP NoReg node.
 */
ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg);

extern const arch_isa_if_t ia32_isa_if;

#endif /* _BEARCH_IA32_H_ */
