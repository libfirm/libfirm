#ifndef _BEARCH_ARM_T_H_
#define _BEARCH_ARM_T_H_

#include <stdio.h>

#include "debug.h"
#include "bearch_arm.h"
#include "arm_nodes_attr.h"
#include "../be.h"
#include "set.h"

typedef struct _arm_code_gen_t {
	const arch_code_generator_if_t *impl;           /**< implementation */
	ir_graph                       *irg;            /**< current irg */
	FILE                           *out;            /**< output file */
	const arch_env_t               *arch_env;       /**< the arch env */
	set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
	int                             emit_decls;     /**< flag indicating if decls were already emitted */
	const be_irg_t                 *birg;           /**< The be-irg (contains additional information about the irg) */
	ir_type                        *int_tp;         /**< the int type, needed for Call conversion */
	int                             have_fp;        /**< non-zero, if fp hardware instructions are emitted */
	DEBUG_ONLY(firm_dbg_module_t   *mod;)            /**< debugging module */
} arm_code_gen_t;


typedef struct _arm_isa_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;            /**< The stack pointer register. */
	const arch_register_t *bp;            /**< The base pointer register. */
	const int              stack_dir;     /**< -1 for decreasing, 1 for increasing. */
	int                    num_codegens;
	int                    gen_reg_names; /**< use generic register names instead of SP, LR, PC */
	arm_code_gen_t        *cg;            /**< current code generator */
} arm_isa_t;


typedef struct _arm_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	arm_code_gen_t     *cg;
} arm_irn_ops_t;


/* this is a struct to minimize the number of parameters
   for transformation walker */
typedef struct _arm_transform_env_t {
	arm_code_gen_t    *cg;       /**< current code generator */
	dbg_info          *dbg;      /**< The node debug info */
	ir_graph          *irg;      /**< The irg, the node should be created in */
	ir_node           *block;    /**< The block, the node should belong to */
	ir_node           *irn;      /**< The irn, to be transformed */
	ir_mode           *mode;     /**< The mode of the irn */
	DEBUG_ONLY(firm_dbg_module_t *mod;)      /**< The firm debugger */
} arm_transform_env_t;


#endif /* _BEARCH_ARM_T_H_ */
