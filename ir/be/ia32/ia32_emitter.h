#ifndef _IA32_EMITTER_H_
#define _IA32_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"

#include "bearch_ia32_t.h"

typedef struct _emit_env_t {
	firm_dbg_module_t     *mod;
	FILE                  *out;
	const arch_env_t      *arch_env;
	const ia32_code_gen_t *cg;
} emit_env_t;

const lc_arg_env_t *ia32_get_arg_env(void);

char *ia32_emit_binop(const ir_node *irn);
char *ia32_emit_unop(const ir_node *irn);
char *ia32_emit_am(const ir_node *irn);

int get_ia32_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_ia32_in_reg_name(ir_node *irn, int pos);

void ia32_register_emitters(void);
void ia32_gen_routine(FILE *F, ir_graph *irg, const ia32_code_gen_t *cg);

#endif /* _IA32_EMITTER_H_ */
