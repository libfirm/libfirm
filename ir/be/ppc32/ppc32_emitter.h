#ifndef _PPC32_EMITTER_H_
#define _PPC32_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"

#include "bearch_ppc32_t.h"

typedef struct _emit_env_t {
	FILE                      *out;
	const arch_env_t          *arch_env;
	const ppc32_code_gen_t    *cg;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} ppc32_emit_env_t;

const lc_arg_env_t *ppc32_get_arg_env(void);

void equalize_dest_src(FILE *F, ir_node *n);

int get_ppc32_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_ppc32_in_reg_name(ir_node *irn, int pos);

void ppc32_gen_routine(FILE *F, ir_graph *irg, const ppc32_code_gen_t *cg);

const char *ppc32_rlwimi_emit_helper(const ir_node *n, ppc32_emit_env_t *env);


#endif /* _PPC32_EMITTER_H_ */
