/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Dumps global variables and constants as gas assembler.
 * @author      Christian Wuerdig, Matthias Braun
 * @date        04.11.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BEGNUAS_H
#define FIRM_BE_BEGNUAS_H

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

/**
 * Support for some GAS "dialects".
 */
typedef enum asm_flavour_t {
	GAS_FLAVOUR_NORMAL = 0,  /**< normal gas (ELF) */
	GAS_FLAVOUR_MINGW  = 1,  /**< MinGW variant (no-ELF) */
	GAS_FLAVOUR_MAX    = 2
} be_gas_flavour_t;

/** The variable where the GAS dialect is stored. */
extern be_gas_flavour_t be_gas_flavour;

/**
 * Generate all entities.
 * @param env               the emitter environment
 * @param main_env          the main backend environment
 * @param emit_commons      if non-zero, emit commons (non-local uninitialized entities)
 * @param only_emit_marked  if non-zero, external allocated entities that do not have
 *                          its visited flag set are ignored
 */
void be_gas_emit_decls(be_emit_env_t *env, const be_main_env_t *main_env,
                       int only_emit_marked_entities);

/**
 * Switch the current output section to the given out.
 *
 * @param env      the emitter environment
 * @param section  the new output section
 */
void be_gas_emit_switch_section(be_emit_env_t *env, be_gas_section_t section);

#endif /* FIRM_BE_BEGNUAS_H */
