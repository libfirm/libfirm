/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Internal backend global data structures.
 * @author  Sebastian Hack
 */
#ifndef FIRM_BE_BE_T_H
#define FIRM_BE_BE_T_H

#include <assert.h>
#include <stdbool.h>

#include "be.h"
#include "be_types.h"
#include "firm_types.h"
#include "pmap.h"
#include "timing.h"
#include "irdump.h"

typedef enum be_dump_flags_t {
	DUMP_NONE     = 0,
	DUMP_INITIAL  = 1 << 0,
	DUMP_SCHED    = 1 << 2,
	DUMP_PREPARED = 1 << 3,
	DUMP_RA       = 1 << 4,
	DUMP_FINAL    = 1 << 5,
	DUMP_BE       = 1 << 6
} be_dump_flags_t;

/** Backend options */
struct be_options_t {
	unsigned dump_flags;       /**< backend dumping flags */
	bool timing;               /**< time the backend phases */
	bool opt_profile_generate; /**< instrument code for profiling */
	bool opt_profile_use;      /**< use existing profile data */
	bool omit_fp;              /**< try to omit the frame pointer */
	bool do_verify;            /**< backend verify option */
	char ilp_solver[128];      /**< the ilp solver name */
	bool verbose_asm;          /**< dump verbose assembler */
};
extern be_options_t be_options;

extern after_transform_func be_after_transform;

extern asm_constraint_flags_t be_asm_constraint_flags[256];

struct be_main_env_t {
	const char *cup_name;             /**< name of the compilation unit */
	pmap       *ent_trampoline_map;   /**< A map containing PIC trampolines for methods. */
	ir_type    *pic_trampolines_type; /**< Class type containing all trampolines */
	pmap       *ent_pic_symbol_map;
	ir_type    *pic_symbols_type;
};

void be_set_constraint_support(asm_constraint_flags_t flags, char const *constraints);

void be_get_allocatable_regs(ir_graph const *irg, arch_register_class_t const *cls, unsigned *raw_bitset);

unsigned be_get_n_allocatable_regs(const ir_graph *irg,
                                   const arch_register_class_t *cls);

void be_after_irp_transform(const char *name);

void be_check_verify_result(bool fine, ir_graph *irg);

/**
 * Initialize the backend. Must be run first in init_firm();
 */
void firm_be_init(void);
void firm_be_finish(void);

extern int be_timing;

typedef enum {
	T_FIRST,
	T_ABI = T_FIRST,
	T_CODEGEN,
	T_RA_PREPARATION,
	T_SCHED,
	T_CONSTR,
	T_FINISH,
	T_EMIT,
	T_VERIFY,
	T_OTHER,
	T_HEIGHTS,
	T_LIVE,
	T_EXECFREQ,
	T_SSA_CONSTR,
	T_RA_EPILOG,
	T_RA_CONSTR,
	T_RA_SPILL,
	T_RA_SPILL_APPLY,
	T_RA_COLOR,
	T_RA_IFG,
	T_RA_COPYMIN,
	T_RA_SSA,
	T_RA_OTHER,
	T_LAST = T_RA_OTHER
} be_timer_id_t;
ENUM_COUNTABLE(be_timer_id_t)
extern ir_timer_t *be_timers[T_LAST+1];

static inline void be_timer_push(be_timer_id_t id)
{
	assert(id <= T_LAST);
	if (!be_timing)
		return;
	ir_timer_push(be_timers[id]);
}

static inline void be_timer_pop(be_timer_id_t id)
{
	assert(id <= T_LAST);
	if (!be_timing)
		return;
	ir_timer_pop(be_timers[id]);
}

/**
 * A wrapper around a firm dumper. Dumps only, if flags are enabled.
 *
 * @param mask    a bitmask containing the reason what will be dumped
 * @param irg     the IR graph to dump
 * @param suffix  the suffix for the dumper
 */
static inline void be_dump(be_dump_flags_t const mask, ir_graph *const irg,
                           char const *const suffix)
{
	if (be_options.dump_flags & mask)
		dump_ir_graph(irg, suffix);
}

void be_initialize(void);

bool be_set_arch(char const *arch);

/**
 * @defgroup beconvenience Convenience Function for driving code generation.
 * @{
 */
void be_begin(FILE *output, const char *cup_name);
void be_finish(void);

bool be_step_first(ir_graph *irg);
void be_step_regalloc(ir_graph *irg, const regalloc_if_t *regif);
void be_step_schedule(ir_graph *irg);
void be_step_last(ir_graph *irg);
/** @} */

#endif
