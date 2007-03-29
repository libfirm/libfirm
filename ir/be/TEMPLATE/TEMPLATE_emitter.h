#ifndef _TEMPLATE_EMITTER_H_
#define _TEMPLATE_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"
#include "../beemitter.h"

#include "bearch_TEMPLATE_t.h"

typedef struct _TEMPLATE_emit_env_t {
	be_emit_env_t             *emit;
	const arch_env_t          *arch_env;
	const TEMPLATE_code_gen_t *cg;
	TEMPLATE_isa_t            *isa;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} TEMPLATE_emit_env_t;

void TEMPLATE_emit_source_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos);
void TEMPLATE_emit_dest_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos);
void TEMPLATE_emit_immediate(TEMPLATE_emit_env_t *env, const ir_node *node);

int get_TEMPLATE_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_TEMPLATE_in_reg_name(ir_node *irn, int pos);

void TEMPLATE_gen_routine(const TEMPLATE_code_gen_t *cg, ir_graph *irg);

#endif /* _TEMPLATE_EMITTER_H_ */
