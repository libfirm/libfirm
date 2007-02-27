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

#include "bearch_ia32_t.h"

typedef struct _ia32_emit_env_t {
	FILE                  *out;
	const arch_env_t      *arch_env;
	const ia32_code_gen_t *cg;
	ia32_isa_t            *isa;
	struct obstack        *obst;
	int                    linelength;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} ia32_emit_env_t;

static INLINE void ia32_emit_char(ia32_emit_env_t *env, char c)
{
	obstack_1grow(env->obst, c);
	env->linelength++;
}

static INLINE void ia32_emit_string_len(ia32_emit_env_t *env, const char *str, size_t l)
{
	obstack_grow(env->obst, str, l);
	env->linelength += l;
}

static INLINE void ia32_emit_string(ia32_emit_env_t *env, const char *str)
{
	size_t len = strlen(str);
	ia32_emit_string_len(env, str, len);
}

#define ia32_emit_cstring(env,x) { ia32_emit_string_len(env, x, sizeof(x)-1); }

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
void ia32_emit_finish_line(ia32_emit_env_t *env, const ir_node *node);

void ia32_gen_routine(ia32_code_gen_t *cg, FILE *F, ir_graph *irg);

/**
 * Sections.
 */
typedef enum section_t {
	NO_SECTION     = -1,  /**< no section selected yet. */
	SECTION_TEXT   = 0,   /**< text section */
	SECTION_DATA   = 1,   /**< data section */
	SECTION_RODATA = 2,   /**< rodata section */
	SECTION_COMMON = 3,   /**< common section */
	SECTION_TLS    = 4,   /**< thread local storage section */
	SECTION_CTOR   = 5,   /**< ctor section for instrumentation code init */
	SECTION_MAX    = 6
} section_t;

/**
 * Switch to a new section.
 */
void ia32_switch_section(FILE *f, section_t sec);

typedef enum asm_flavour_t {
	ASM_LINUX_GAS = 0,  /**< Linux gas */
	ASM_MINGW_GAS = 1,  /**< MinGW gas */
	ASM_MAX       = 2
} asm_flavour_t;

extern asm_flavour_t asm_flavour;

#endif /* _IA32_EMITTER_H_ */
