/**
 * Header for ia32 assembler declarations dumper.
 * @author Christian Wuerdig, Matthias Braun
 * $Id$
 */
#ifndef _BE_GEN_DECLS_H_
#define _BE_GEN_DECLS_H_

#include "be.h"
#include "beemitter.h"

/**
 * Sections.
 */
typedef enum section_t {
	GAS_SECTION_TEXT   = 0,   /**< text section */
	GAS_SECTION_DATA   = 1,   /**< data section */
	GAS_SECTION_RODATA = 2,   /**< rodata section */
	GAS_SECTION_COMMON = 3,   /**< common section */
	GAS_SECTION_TLS    = 4,   /**< thread local storage section */
	GAS_SECTION_CTOR   = 5,   /**< ctor section for instrumentation code init */
	GAS_SECTION_MAX    = 6
} be_gas_section_t;

typedef enum asm_flavour_t {
	GAS_FLAVOUR_NORMAL = 0,  /**< normal gas */
	GAS_FLAVOUR_MINGW  = 1,  /**< MinGW variant */
	GAS_FLAVOUR_MAX    = 2
} be_gas_flavour_t;

extern be_gas_flavour_t be_gas_flavour;


/**
 * Generate all entities.
 */
void be_gas_emit_decls(be_emit_env_t *env, const be_main_env_t *main_env,
                       int only_emit_marked_entities);

void be_gas_emit_switch_section(be_emit_env_t *env, be_gas_section_t section);

#endif
