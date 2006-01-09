#ifndef _IA32_EMITTER_H_
#define _IA32_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"

typedef struct _emit_env_t {
	firm_dbg_module_t *mod;
	FILE              *out;
	const arch_env_t  *arch_env;
} emit_env_t;

const lc_arg_env_t *ia32_get_arg_env(void);

void equalize_dest_src(FILE *F, ir_node *n);

int get_ia32_in_regnr(ir_node *irn, int pos);
const char *get_ia32_in_reg_name(ir_node *irn, int pos);

void ia32_gen_routine(FILE *F, ir_graph *irg, const arch_env_t *env);

#endif /* _IA32_EMITTER_H_ */
