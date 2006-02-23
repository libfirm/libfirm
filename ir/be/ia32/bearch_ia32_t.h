#ifndef _BEARCH_IA32_T_H_
#define _BEARCH_IA32_T_H_

#include "debug.h"
#include "bearch_ia32.h"
#include "ia32_nodes_attr.h"

/* some typedefs */

typedef struct _ia32_code_gen_t {
	const arch_code_generator_if_t *impl;          /**< implementation */
	ir_graph                       *irg;           /**< current irg */
	FILE                           *out;           /**< output file */
	const arch_env_t               *arch_env;      /**< the arch env */
	set                            *reg_set;       /**< set to memorize registers for non-ia32 nodes (e.g. phi nodes) */
	firm_dbg_module_t              *mod;           /**< debugging module */
	int                             emit_decls;    /**< flag indicating if decls were already emitted */
	int                             has_alloca;    /**< indicates whether the irg contains an alloca or not */
	const ia32_register_req_t     **reg_param_req; /**< hold the requirements for the reg param nodes */
	pmap                           *types;         /**< A map of modes to primitive types */
	pmap                           *tv_ent;        /**< A map of entities that store tarvals */
	ir_node                        *noreg_gp;      /**< Holds the unique per irg GP NoReg node */
	ir_node                        *noreg_fp;      /**< Holds the unique per irg FP NoReg node */
} ia32_code_gen_t;

typedef struct _ia32_isa_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;            /** The stack pointer register. */
	const arch_register_t *bp;            /** The base pointer register. */
	const int              stack_dir;     /** -1 for decreasing, 1 for increasing. */
	int                    num_codegens;
	set                   *reg_projnum_map;
} ia32_isa_t;

typedef struct _ia32_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	ia32_code_gen_t         *cg;
} ia32_irn_ops_t;

/* this is a struct to minimize the number of parameters
   for transformation walker */
typedef struct _ia32_transform_env_t {
	const arch_env_t  *arch_env;   /**< The arch_env */
	firm_dbg_module_t *mod;        /**< The firm debugger */
	dbg_info          *dbg;        /**< The node debug info */
	ir_graph          *irg;        /**< The irg, the node should be created in */
	ir_node           *block;      /**< The block, the node should belong to */
	ir_node           *irn;        /**< The irn, to be transformed */
	ir_mode           *mode;       /**< The mode of the irn */
	ia32_code_gen_t   *cg;         /**< The code generator */
} ia32_transform_env_t;

/**
 * Creates the unique per irg GP NoReg node.
 */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg);

/**
 * Creates the unique per irg FP NoReg node.
 */
ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg);

#endif /* _BEARCH_IA32_T_H_ */
