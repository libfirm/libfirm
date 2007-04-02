/**
 * Header file for ia32 emitter, containing some function definitions and types.
 * @author Christian Wuerdig, Matthias Braun
 * $Id$
 */
#ifndef _IA32_EMITTER_H_
#define _IA32_EMITTER_H_

#include "irargs_t.h"
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"
#include "../beemitter.h"

#include "bearch_ia32_t.h"

typedef struct ia32_emit_env_t {
	be_emit_env_t         *emit;
	const arch_env_t      *arch_env;
	const ia32_code_gen_t *cg;
	ia32_isa_t            *isa;
} ia32_emit_env_t;

void ia32_emit_source_register(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_dest_register(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_x87_name(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_immediate(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_mode_suffix(ia32_emit_env_t *env, const ir_mode *mode);
void ia32_emit_x87_mode_suffix(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_xmm_mode_suffix(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_xmm_mode_suffix_s(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_extend_suffix(ia32_emit_env_t *env, const ir_mode *mode);
void ia32_emit_binop(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_unop(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_am(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_adr(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_x87_binop(ia32_emit_env_t *env, const ir_node *node);

void ia32_gen_routine(ia32_code_gen_t *cg, ir_graph *irg);

#endif /* _IA32_EMITTER_H_ */
