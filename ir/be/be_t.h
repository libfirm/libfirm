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

#include "be.h"
#include "be_types.h"
#include "firm_types.h"
#include "pmap.h"
#include "timing.h"
#include "irdump.h"

enum {
	DUMP_NONE     = 0,
	DUMP_INITIAL  = 1 << 0,
	DUMP_SCHED    = 1 << 2,
	DUMP_PREPARED = 1 << 3,
	DUMP_RA       = 1 << 4,
	DUMP_FINAL    = 1 << 5,
	DUMP_BE       = 1 << 6
};

enum {
	BE_TIME_OFF,
	BE_TIME_ON
};

enum {
	BE_VERIFY_OFF,
	BE_VERIFY_WARN,
	BE_VERIFY_ASSERT
};

/** Backend options */
struct be_options_t {
	unsigned dump_flags;       /**< backend dumping flags */
	int  timing;               /**< time the backend phases */
	int  opt_profile_generate; /**< instrument code for profiling */
	int  opt_profile_use;      /**< use existing profile data */
	int  omit_fp;              /**< try to omit the frame pointer */
	int  pic;                  /**< create position independent code */
	int  verify_option;        /**< backend verify option */
	char ilp_server[128];      /**< the ilp server name */
	char ilp_solver[128];      /**< the ilp solver name */
	int  verbose_asm;          /**< dump verbose assembler */
};
extern be_options_t be_options;

struct be_main_env_t {
	arch_env_t   *arch_env;
	const char   *cup_name;             /**< name of the compilation unit */
	pmap         *ent_trampoline_map;   /**< A map containing PIC trampolines for methods. */
	ir_type      *pic_trampolines_type; /**< Class type containing all trampolines */
	pmap         *ent_pic_symbol_map;
	ir_type      *pic_symbols_type;
};

extern asm_constraint_flags_t asm_constraint_flags[256];

void be_get_allocatable_regs(ir_graph const *irg, arch_register_class_t const *cls, unsigned *raw_bitset);

unsigned be_get_n_allocatable_regs(const ir_graph *irg,
                                   const arch_register_class_t *cls);

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
static inline void be_dump(int mask, ir_graph *irg, const char *suffix)
{
	if (be_options.dump_flags & mask)
		dump_ir_graph(irg, suffix);
}

#endif
