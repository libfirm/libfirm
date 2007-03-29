#ifndef _BEARCH_TEMPLATE_T_H_
#define _BEARCH_TEMPLATE_T_H_

#include "debug.h"
#include "bearch_TEMPLATE.h"
#include "TEMPLATE_nodes_attr.h"
#include "../be.h"
#include "../beemitter.h"
#include "set.h"

typedef struct _TEMPLATE_isa_t TEMPLATE_isa_t;

typedef struct _TEMPLATE_code_gen_t {
	const arch_code_generator_if_t *impl;           /**< implementation */
	ir_graph                       *irg;            /**< current irg */
	const arch_env_t               *arch_env;       /**< the arch env */
	set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
	TEMPLATE_isa_t                 *isa;            /**< the isa instance */
	const be_irg_t                 *birg;           /**< The be-irg (contains additional information about the irg) */
	DEBUG_ONLY(firm_dbg_module_t   *mod;)           /**< debugging module */
} TEMPLATE_code_gen_t;

struct _TEMPLATE_isa_t {
	arch_isa_t             arch_isa;      /**< must be derived from arch_isa */
	be_emit_env_t          emit;          /**< emit datastructure */
};


typedef struct _TEMPLATE_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	TEMPLATE_code_gen_t     *cg;
} TEMPLATE_irn_ops_t;


/* this is a struct to minimize the number of parameters
   for transformation walker */
typedef struct _TEMPLATE_transform_env_t {
	dbg_info          *dbg;      /**< The node debug info */
	ir_graph          *irg;      /**< The irg, the node should be created in */
	ir_node           *block;    /**< The block, the node should belong to */
	ir_node           *irn;      /**< The irn, to be transformed */
	ir_mode           *mode;     /**< The mode of the irn */
	DEBUG_ONLY(firm_dbg_module_t *mod;)      /**< The firm debugger */
} TEMPLATE_transform_env_t;


#endif /* _BEARCH_TEMPLATE_T_H_ */
