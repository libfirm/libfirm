/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Main Backend driver.
 * @author      Sebastian Hack
 * @date        25.11.2004
 */
#include "be_t.h"
#include "beasm.h"
#include "bechordal_t.h"
#include "bediagnostic.h"
#include "beemitter.h"
#include "begnuas.h"
#include "beifg.h"
#include "beirg.h"
#include "belistsched.h"
#include "belive.h"
#include "belower.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "bespillutil.h"
#include "bessadestr.h"
#include "bestack.h"
#include "bestat.h"
#include "beutil.h"
#include "beverify.h"
#include "execfreq_t.h"
#include "ident_t.h"
#include "ircons.h"
#include "irdom_t.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irgopt.h"
#include "irloop_t.h"
#include "iroptimize.h"
#include "irprofile.h"
#include "irprog.h"
#include "irtools.h"
#include "irverify.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "obst.h"
#include "statev.h"
#include "target_t.h"
#include "util.h"
#include <stdio.h>
#include <vhdl/vhdl_bearch_t.h>
#include <vhdl/method_cloning.h>

static struct obstack obst;
static be_main_env_t  env;

/* options visible for anyone */
be_options_t be_options = {
	.dump_flags           = DUMP_NONE,
	.timing               = false,
	.opt_profile_generate = false,
	.opt_profile_use      = false,
	.omit_fp              = false,
	.do_verify            = true,
	.ilp_solver           = "",
	.verbose_asm          = true,
};

/* possible dumping options */
static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "none",       DUMP_NONE },
	{ "initial",    DUMP_INITIAL },
	{ "sched",      DUMP_SCHED  },
	{ "prepared",   DUMP_PREPARED },
	{ "regalloc",   DUMP_RA },
	{ "final",      DUMP_FINAL },
	{ "be",         DUMP_BE },
	{ "all",        2 * DUMP_BE - 1 },
	{ NULL,         0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&be_options.dump_flags, dump_items
};

static const lc_opt_table_entry_t be_main_options[] = {
	LC_OPT_ENT_ENUM_MASK("dump",       "dump irg on several occasions",                       &dump_var),
	LC_OPT_ENT_BOOL     ("omitfp",     "omit frame pointer",                                  &be_options.omit_fp),
	LC_OPT_ENT_BOOL     ("verify",     "verify the backend irg",                              &be_options.do_verify),
	LC_OPT_ENT_BOOL     ("time",       "get backend timing statistics",                       &be_options.timing),
	LC_OPT_ENT_BOOL     ("profilegenerate", "instrument the code for execution count profiling", &be_options.opt_profile_generate),
	LC_OPT_ENT_BOOL     ("profileuse",      "use existing profile data",                         &be_options.opt_profile_use),
	LC_OPT_ENT_BOOL     ("verboseasm", "enable verbose assembler output",                        &be_options.verbose_asm),

	LC_OPT_ENT_STR("ilp.solver", "the ilp solver name", &be_options.ilp_solver),
	LC_OPT_LAST
};

asm_constraint_flags_t be_asm_constraint_flags[256];

void be_set_constraint_support(asm_constraint_flags_t const flags, char const *const constraints)
{
	for (char const *i = constraints; *i != '\0'; ++i) {
		be_asm_constraint_flags[(unsigned char)*i] = flags;
	}
}

static void be_init_default_asm_constraint_flags(void)
{
	for (size_t i = 0; i != ARRAY_SIZE(be_asm_constraint_flags); ++i) {
		be_asm_constraint_flags[i] = ASM_CONSTRAINT_FLAG_INVALID;
	}

	be_set_constraint_support(ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER, "&");

	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER, "0123456789");
	/* List of constraints supported by gcc for any machine (or at least
	 * recognized). Mark them as NO_SUPPORT so we can differentiate them
	 * from INVALID. Backends should change the flags they support. */
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_NO_SUPPORT, ",<>EFGHIJKLMNOPVXgimoprs");
	/* Skip whitespace.
	 * TODO '*' actually penalizes the selection of the next constraint letter.
	 * We do not support this, yet.
	 * TODO '!' and '?' actually penalize an alternative of a multi alternative
	 * constraint.  We do not support this, yet. */
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_NONE, "\t\n\r !*?");
}

bool be_set_arch(char const *const arch)
{
	/* TODO: generalize to all backends */
	bool res = false;
	char buf[64];
	snprintf(buf, sizeof(buf), "ia32-arch=%s", arch);
	res |= ir_target_option(buf);
	snprintf(buf, sizeof(buf), "sparc-cpu=%s", arch);
	res |= ir_target_option(buf);

	return res;
}

void be_initialize(void)
{
	assert(!ir_target.isa_initialized);
	be_init_default_asm_constraint_flags();
	obstack_init(&obst);
}

static void finish_isa(void)
{
	assert(ir_target.isa_initialized);
	ir_target.isa->finish();
	obstack_free(&obst, NULL);
}

asm_constraint_flags_t be_parse_asm_constraints(const char *constraint)
{
	assert(ir_target.isa_initialized);

	asm_constraint_flags_t flags;
	char            const *c = constraint;
	switch (*c) {
	case '=': ++c; flags = ASM_CONSTRAINT_FLAG_MODIFIER_WRITE; break;
	case '+': ++c; flags = ASM_CONSTRAINT_FLAG_MODIFIER_READ | ASM_CONSTRAINT_FLAG_MODIFIER_WRITE; break;
	case '%': ++c; /* FALLTHROUGH */ /* TODO stub, should mark this operand as commutative with the next. */
	default:       flags = ASM_CONSTRAINT_FLAG_MODIFIER_READ; break;
	}

	for (; *c != '\0'; ++c) {
		switch (*c) {
		case '#':
			/* text until comma is a comment */
			while (*c != 0 && *c != ',')
				++c;
			break;

		default:
			flags |= be_asm_constraint_flags[(unsigned char)*c];
			break;
		}
	}

	return flags;
}

int be_is_valid_clobber(const char *clobber)
{
	assert(ir_target.isa_initialized);

	/* memory is a valid clobber. (the frontend has to detect this case too,
	 * because it has to add memory edges to the asm) */
	if (streq(clobber, "memory"))
		return 1;
	/* cc (condition code) is always valid */
	if (streq(clobber, "cc"))
		return 1;

	return be_parse_register_name(clobber) != NULL;
}

static void be_opt_register(void)
{
	static bool run_once = false;
	if (run_once)
		return;
	run_once = true;

	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_add_table(be_grp, be_main_options);
}

void be_check_verify_result(bool fine, ir_graph *irg)
{
	if (!fine) {
		be_errorf(NULL, "verifier failed; trying to write assert graph and abort");
		dump_ir_graph(irg, "assert");
		abort();
	}
}

/* Perform schedule verification if requested. */
static void be_sched_verify(ir_graph *irg)
{
	if (be_options.do_verify) {
		be_timer_push(T_VERIFY);
		bool fine = be_verify_schedule(irg);
		be_check_verify_result(fine, irg);
		be_timer_pop(T_VERIFY);
	}
}

static void be_regalloc_verify(ir_graph *const irg)
{
	if (be_options.do_verify) {
		be_timer_push(T_VERIFY);
		bool const fine = be_verify_register_allocation(irg);
		be_check_verify_result(fine, irg);
		be_timer_pop(T_VERIFY);
	}
}

/* Initialize the Firm backend. Must be run first in init_firm()! */
void firm_be_init(void)
{
	be_opt_register();
	be_init_modules();
}

static ir_timer_t *bemain_timer;

/**
 * Prepare a backend graph for code generation and initialize its irg
 */
static void initialize_birg(be_irg_t *birg, ir_graph *irg, be_main_env_t *env)
{
	/* don't duplicate locals in backend when dumping... */
	ir_remove_dump_flags(ir_dump_flag_consts_local);

	be_dump(DUMP_INITIAL, irg, "begin");

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_MANY_RETURNS);

	memset(birg, 0, sizeof(*birg));
	birg->main_env = env;
	obstack_init(&birg->obst);
	irg->be_data = birg;

	be_info_init_irg(irg);
	birg->lv = be_liveness_new(irg);

	/* Verify the initial graph */
	if (be_options.do_verify) {
		be_timer_push(T_VERIFY);
		bool fine = irg_verify(irg);
		be_check_verify_result(fine, irg);
		be_timer_pop(T_VERIFY);
	}
}

static ir_graph *be_prepare_profile(const char *const cup_name)
{
	obstack_printf(&obst, "%s.prof", cup_name);
	obstack_1grow(&obst, '\0');
	const char *prof_filename = obstack_finish(&obst);

	bool have_profile = false;
	if (be_options.opt_profile_use) {
		bool res = ir_profile_read(prof_filename);
		if (!res) {
			be_warningf(NULL, "could not read profile data '%s'", prof_filename);
		} else {
			ir_create_execfreqs_from_profile();
			ir_profile_free();
			have_profile = true;
		}
	}

	ir_graph *prof_init_irg = NULL;
	if (be_options.opt_profile_generate)
		prof_init_irg = ir_profile_instrument(prof_filename);

	if (!have_profile) {
		be_timer_push(T_EXECFREQ);
		foreach_irp_irg(i, irg) {
			ir_estimate_execfreq(irg);
		}
		be_timer_pop(T_EXECFREQ);
	}
	return prof_init_irg;
}

void be_begin(FILE *file_handle, const char *cup_name)
{
	memset(be_asm_constraint_flags, 0, sizeof(be_asm_constraint_flags));

	bemain_timer = NULL;
	if (be_options.timing) {
		bemain_timer = ir_timer_new();

		if (ir_timer_enter_high_priority())
			be_warningf(NULL, "could not enter high priority mode");

		ir_timer_reset_and_start(bemain_timer);
	}

	if (stat_ev_enabled) {
		const char *dot = strrchr(cup_name, '.');
		const char *pos = dot ? dot : cup_name + strlen(cup_name);
		char       *buf = ALLOCAN(char, pos - cup_name + 1);
		strncpy(buf, cup_name, pos - cup_name);
		buf[pos - cup_name] = '\0';

		stat_ev_ctx_push_str("bemain_compilation_unit", cup_name);
	}

	be_timing = be_options.timing;

	/* perform target lowering if it didn't happen yet */
	if (get_irp_n_irgs() > 0 && !irg_is_constrained(get_irp_irg(0), IR_GRAPH_CONSTRAINT_TARGET_LOWERED))
		be_lower_for_target();

	if (be_timing) {
		for (be_timer_id_t t = T_FIRST; t < T_LAST+1; ++t) {
			be_timers[t] = ir_timer_new();
			ir_timer_init_parent(be_timers[t]);
		}
	}

	be_emit_init(file_handle);

	memset(&env, 0, sizeof(env));
	env.ent_trampoline_map   = pmap_create();
	env.pic_trampolines_type = new_type_segment(NEW_IDENT("$PIC_TRAMPOLINE_TYPE"), tf_none);
	env.ent_pic_symbol_map   = pmap_create();
	env.pic_symbols_type     = new_type_segment(NEW_IDENT("$PIC_SYMBOLS_TYPE"), tf_none);
	env.cup_name             = cup_name;

	be_info_init();

	/* First: initialize all birgs */
	size_t          num_birgs = 0;
	/* we might need 1 birg more for instrumentation constructor */
	be_irg_t *const birgs     = OALLOCN(&obst, be_irg_t, get_irp_n_irgs()+1);
	foreach_irp_irg(i, irg) {
		ir_entity *entity = get_irg_entity(irg);
		if (get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN)
			continue;
		initialize_birg(&birgs[num_birgs++], irg, &env);
		if (ir_target.isa->handle_intrinsics)
			ir_target.isa->handle_intrinsics(irg);
		be_dump(DUMP_INITIAL, irg, "prepared");
	}

	/* Prepare basicblock profile generation/usage. Note: You should avoid
	 * introducing new control flow after this point or you won't have profile
	 * data for the new basic blocks. */
	ir_graph *prof_init_irg = be_prepare_profile(cup_name);
	if (prof_init_irg != NULL)
		initialize_birg(&birgs[num_birgs++], prof_init_irg, &env);

	be_gas_begin_compilation_unit(&env);
}

void firm_be_finish(void)
{
	finish_isa();
	be_quit_modules();
}

int be_timing;

static const char *get_timer_name(be_timer_id_t id)
{
	switch (id) {
	case T_ABI:            return "abi";
	case T_CODEGEN:        return "codegen";
	case T_RA_PREPARATION: return "ra_preparation";
	case T_SCHED:          return "sched";
	case T_CONSTR:         return "constr";
	case T_FINISH:         return "finish";
	case T_EMIT:           return "emit";
	case T_VERIFY:         return "verify";
	case T_OTHER:          return "other";
	case T_HEIGHTS:        return "heights";
	case T_LIVE:           return "live";
	case T_EXECFREQ:       return "execfreq";
	case T_SSA_CONSTR:     return "ssa_constr";
	case T_RA_EPILOG:      return "ra_epilog";
	case T_RA_CONSTR:      return "ra_constr";
	case T_RA_SPILL:       return "ra_spill";
	case T_RA_SPILL_APPLY: return "ra_spill_apply";
	case T_RA_COLOR:       return "ra_color";
	case T_RA_IFG:         return "ra_ifg";
	case T_RA_COPYMIN:     return "ra_copymin";
	case T_RA_SSA:         return "ra_ssa";
	case T_RA_OTHER:       return "ra_other";
	}
	return "unknown";
}
ir_timer_t *be_timers[T_LAST+1];

static void dummy_after_transform(ir_graph *irg, const char *name)
{
	(void)irg;
	(void)name;
}

after_transform_func be_after_transform = dummy_after_transform;

void be_set_after_transform_func(after_transform_func after_transform)
{
	be_after_transform = after_transform;
}

void be_after_irp_transform(const char *name)
{
	if (be_after_transform == NULL)
		return;

	foreach_irp_irg_r(i, irg) {
		be_after_transform(irg, name);
	}
}

void be_lower_for_target(void)
{
	assert(ir_target.isa_initialized);
	ir_target.isa->lower_for_target();
	/* set the phase to low */
	foreach_irp_irg_r(i, irg) {
		assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_TARGET_LOWERED));
		add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_TARGET_LOWERED);
	}
}

static int cse_setting;

bool be_step_first(ir_graph *irg)
{
	ir_entity *const entity = get_irg_entity(irg);
	if (get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN)
		return false;

	be_timer_push(T_OTHER);
	if (stat_ev_enabled) {
		stat_ev_ctx_push_fmt("bemain_irg", "%+F", irg);
		stat_ev_ull("bemain_insns_start", be_count_insns(irg));
		stat_ev_ull("bemain_blocks_start", be_count_blocks(irg));
	}
	cse_setting = get_opt_cse();
	return true;
}

void be_step_schedule(ir_graph *irg)
{
	/* We generally disable CSE after scheduling as we now may want to duplicate
	 * operations on purpose, new operations should not merge with existing ones
	 * before they are scheduled. */
	set_opt_cse(0);

	be_timer_push(T_SCHED);
	be_schedule_graph(irg);
	be_timer_pop(T_SCHED);
	be_dump(DUMP_SCHED, irg, "sched");
	be_sched_verify(irg);
}

void be_step_regalloc(ir_graph *irg, const regalloc_if_t *regif)
{
	if (stat_ev_enabled) {
		stat_ev_dbl("bemain_costs_before_ra", be_estimate_irg_costs(irg));
		stat_ev_ull("bemain_insns_before_ra", be_count_insns(irg));
		stat_ev_ull("bemain_blocks_before_ra", be_count_blocks(irg));
		be_stat_values(irg);
	}

	/* Do register allocation */
	be_allocate_registers(irg, regif);
	be_regalloc_verify(irg);

	if (stat_ev_enabled) {
		stat_ev_dbl("bemain_costs_after_ra", be_estimate_irg_costs(irg));
		stat_ev_ull("bemain_insns_after_ra", be_count_insns(irg));
		stat_ev_ull("bemain_blocks_after_ra", be_count_blocks(irg));
	}

	be_dump(DUMP_RA, irg, "ra");
}

void be_step_last(ir_graph *irg)
{
	if (stat_ev_enabled) {
		stat_ev_ull("bemain_insns_finish", be_count_insns(irg));
		stat_ev_ull("bemain_blocks_finish", be_count_blocks(irg));
	}

	be_dump(DUMP_FINAL, irg, "final");
	be_regalloc_verify(irg);

	be_timer_pop(T_OTHER);

	if (be_timing) {
		if (stat_ev_enabled) {
			for (be_timer_id_t t = T_FIRST; t < T_LAST+1; ++t) {
				char buf[128];
				snprintf(buf, sizeof(buf), "bemain_time_%s",
				         get_timer_name(t));
				stat_ev_dbl(buf, ir_timer_elapsed_usec(be_timers[t]));
			}
		} else {
			printf("==>> IRG %s <<==\n", get_entity_name(get_irg_entity(irg)));
			for (be_timer_id_t t = T_FIRST; t < T_LAST+1; ++t) {
				double val = ir_timer_elapsed_usec(be_timers[t]) / 1000.0;
				printf("%-20s: %10.3f msec\n", get_timer_name(t), val);
			}
		}
		for (be_timer_id_t t = T_FIRST; t < T_LAST+1; ++t) {
			ir_timer_reset(be_timers[t]);
		}
	}

	be_free_birg(irg);
	stat_ev_ctx_pop("bemain_irg");

	set_opt_cse(cse_setting);
}

void be_finish(void)
{
	be_gas_end_compilation_unit(&env);

	if (be_options.timing) {
		ir_timer_stop(bemain_timer);
		ir_timer_leave_high_priority();
		if (stat_ev_enabled) {
			stat_ev_dbl("bemain_backend_time", ir_timer_elapsed_msec(bemain_timer));
		} else {
			double val = ir_timer_elapsed_usec(bemain_timer) / 1000.0;
			printf("%-20s: %10.3f msec\n", "BEMAINLOOP", val);
		}
	}

	if (stat_ev_enabled) {
		stat_ev_ctx_pop("bemain_compilation_unit");
	}

	be_emit_exit();
	be_info_free();

	pmap_destroy(env.ent_trampoline_map);
	pmap_destroy(env.ent_pic_symbol_map);
	free_type(env.pic_trampolines_type);
	free_type(env.pic_symbols_type);
}

void be_main(FILE *file_handle, const char *cup_name)
{
	bool use_vhdl = false;
	foreach_irp_irg(i, irg) {
		ir_entity *ent = get_irg_entity(irg);
		if (mtp_special_instruction & get_entity_additional_properties(ent)) {
			ir_entity *new_ent = clone_method(irg);
			set_entity_additional_properties(ent, (get_entity_additional_properties(ent) & ~mtp_special_instruction));
			set_entity_linkage(new_ent, IR_LINKAGE_NO_CODEGEN);
			use_vhdl = true;
		}
	}

	/* Let the target control how the codegeneration works. */
	ir_target.isa->generate_code(file_handle, cup_name);

	/* Write additional VHDL if desired */
	if (use_vhdl) {
		vhdl_generate_code(cup_name);
	}
}

ir_jit_function_t *be_jit_compile(ir_jit_segment_t *const segment,
                                  ir_graph *const irg)
{
	if (ir_target.isa->jit_compile == NULL)
		return NULL;

	obstack_init(&obst);

	ir_entity *entity = get_irg_entity(irg);
	if (get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN)
		return NULL;
	be_irg_t *const birg = OALLOCZ(&obst, be_irg_t);
	initialize_birg(birg, irg, &env);
	if (ir_target.isa->handle_intrinsics)
		ir_target.isa->handle_intrinsics(irg);
	be_dump(DUMP_INITIAL, irg, "prepared");

	return ir_target.isa->jit_compile(segment, irg);
}

void be_emit_function(char *const buffer, ir_jit_function_t *const function)
{
	ir_target.isa->emit_function(buffer, function);
}
