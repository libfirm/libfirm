#ifndef _BEARCH_TEMPLATE_T_H_
#define _BEARCH_TEMPLATE_T_H_

#include "debug.h"
#include "bearch_TEMPLATE.h"
#include "TEMPLATE_nodes_attr.h"


typedef struct _TEMPLATE_code_gen_t {
	const arch_code_generator_if_t *impl;           /* implementation */
	ir_graph                       *irg;            /* current irg */
	FILE                           *out;            /* output file */
	const arch_env_t               *arch_env;       /* the arch env */
	set                            *reg_set;        /* set to memorize registers for FIRM nodes (e.g. phi) */
	firm_dbg_module_t              *mod;            /* debugging module */
	int                             emit_decls;     /* flag indicating if decls were already emitted */
	int                             has_alloca;     /* indicates whether the irg contains an alloca or not */
	const TEMPLATE_register_req_t **reg_param_req;  /* hold the requirements for the reg param nodes */
} TEMPLATE_code_gen_t;


typedef struct _TEMPLATE_isa_t {
	const arch_isa_if_t *impl;
	int                  num_codegens;
} TEMPLATE_isa_t;


typedef struct _TEMPLATE_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	TEMPLATE_code_gen_t         *cg;
} TEMPLATE_irn_ops_t;


#endif /* _BEARCH_TEMPLATE_T_H_ */
