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

#ifndef _ARM_EMITTER_H_
#define _ARM_EMITTER_H_

#include "firm_types.h"
#include "irargs_t.h"
#include "debug.h"

#include "../bearch_t.h"

#include "bearch_arm_t.h"

/**
 * A SymConst entry.
 */
typedef struct _SymConstEntry {
	unsigned label;              /**< a label number for this label */
	ir_node  *symconst;          /**< the node holding this label */
	struct _SymConstEntry *next; /**< links all entries */
} SymConstEntry;

/**
 * The ARM emitter environment.
 */
typedef struct _arm_emit_env_t {
	FILE                      *out;      /**< the output stream */
	const arch_env_t          *arch_env; /**< the architecture environment */
	const arm_code_gen_t      *cg;       /**< the code generator object */
	struct obstack            obst;      /**< an temporary store for SymConstEntries */
	SymConstEntry             *symbols;  /**< list containing all SymConstEntries */
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} arm_emit_env_t;

const lc_arg_env_t *arm_get_arg_env(void);

void equalize_dest_src(FILE *F, ir_node *n);

int get_arm_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_arm_in_reg_name(ir_node *irn, int pos);

void arm_gen_routine(FILE *F, ir_graph *irg, const arm_code_gen_t *cg);

/**
 * Sections.
 */
typedef enum sections {
	NO_SECTION,      /**< no section selected yet. */
	SECTION_TEXT,    /**< text section */
	SECTION_DATA,    /**< data section */
	SECTION_RODATA,  /**< rodata section */
	SECTION_COMMON,  /**< common section */
} sections;

/**
 * Switch to a new section
 */
void arm_switch_section(FILE *f, sections sec);

#endif /* _ARM_EMITTER_H_ */
