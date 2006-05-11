/**
 * Header file for ia32 emitter, containing some function definitions and types.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_EMITTER_H_
#define _IA32_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"

#include "bearch_ia32_t.h"

typedef struct _ia32_emit_env_t {
	FILE                  *out;
	const arch_env_t      *arch_env;
	const ia32_code_gen_t *cg;
	ia32_isa_t            *isa;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} ia32_emit_env_t;

const lc_arg_env_t *ia32_get_arg_env(void);

const char *ia32_emit_binop(const ir_node *irn, ia32_emit_env_t *env);
const char *ia32_emit_unop(const ir_node *irn, ia32_emit_env_t *env);
const char *ia32_emit_am(const ir_node *irn, ia32_emit_env_t *env);
const char *ia32_emit_adr(const ir_node *irn, ia32_emit_env_t *env);

const char *ia32_emit_x87_binop(const ir_node *n, ia32_emit_env_t *env);

int get_ia32_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_ia32_in_reg_name(ir_node *irn, int pos);

void ia32_gen_routine(FILE *F, ir_graph *irg, const ia32_code_gen_t *cg);

/**
 * Sections.
 */
typedef enum section_t {
	NO_SECTION     = -1,  /**< no section selected yet. */
	SECTION_TEXT   = 0,   /**< text section */
	SECTION_DATA   = 1,   /**< data section */
	SECTION_RODATA = 2,   /**< rodata section */
	SECTION_COMMON = 3,   /**< common section */
	SECTION_MAX    = 4
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
