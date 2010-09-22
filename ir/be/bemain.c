/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Main Backend driver.
 * @author      Sebastian Hack
 * @date        25.11.2004
 * @version     $Id$
 */
#include "config.h"

#include <stdarg.h>
#include <stdio.h>

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "obst.h"
#include "bitset.h"

#include "irprog.h"
#include "irgopt.h"
#include "irgraph.h"
#include "irdump.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irtools.h"
#include "irverify.h"
#include "irprintf.h"
#include "iroptimize.h"
#include "firmstat.h"
#include "execfreq.h"
#include "irprofile.h"
#include "irpass_t.h"

#include "bearch.h"
#include "be_t.h"
#include "bemodule.h"
#include "beutil.h"
#include "benode.h"
#include "beirgmod.h"
#include "besched.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bera.h"
#include "bechordal_t.h"
#include "beifg.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "beabi.h"
#include "belower.h"
#include "beschedmris.h"
#include "bestat.h"
#include "beverify.h"
#include "be_dbgout.h"
#include "beirg.h"
#include "bestack.h"

#define NEW_ID(s) new_id_from_chars(s, sizeof(s) - 1)

/* options visible for anyone */
static be_options_t be_options = {
	DUMP_NONE,                         /* dump flags */
	BE_TIME_OFF,                       /* no timing */
	0,                                 /* no opt profile */
	0,                                 /* try to omit frame pointer */
	0,                                 /* create PIC code */
	0,                                 /* create gprof compatible profiling code */
	BE_VERIFY_WARN,                    /* verification level: warn */
	"linux",                           /* target OS name */
	"i44pc52.info.uni-karlsruhe.de",   /* ilp server */
	"cplex",                           /* ilp solver */
	0,                                 /* enable statistic event dumping */
	"",                                /* print stat events */
};

/* config file. */
static char config_file[256] = { 0 };

/* back end instruction set architecture to use */
static const arch_isa_if_t *isa_if = NULL;

/* possible dumping options */
static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "none",       DUMP_NONE },
	{ "initial",    DUMP_INITIAL },
	{ "abi",        DUMP_ABI    },
	{ "sched",      DUMP_SCHED  },
	{ "prepared",   DUMP_PREPARED },
	{ "regalloc",   DUMP_RA },
	{ "final",      DUMP_FINAL },
	{ "be",         DUMP_BE },
	{ "all",        2 * DUMP_BE - 1 },
	{ NULL,         0 }
};

/* verify options. */
static const lc_opt_enum_int_items_t verify_items[] = {
	{ "off",    BE_VERIFY_OFF    },
	{ "warn",   BE_VERIFY_WARN   },
	{ "assert", BE_VERIFY_ASSERT },
	{ NULL,     0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&be_options.dump_flags, dump_items
};

static lc_opt_enum_int_var_t verify_var = {
	&be_options.verify_option, verify_items
};

static const lc_opt_table_entry_t be_main_options[] = {
	LC_OPT_ENT_STR      ("config",     "read another config file containing backend options", config_file, sizeof(config_file)),
	LC_OPT_ENT_ENUM_MASK("dump",       "dump irg on several occasions",                       &dump_var),
	LC_OPT_ENT_BOOL     ("omitfp",     "omit frame pointer",                                  &be_options.omit_fp),
	LC_OPT_ENT_BOOL     ("pic",        "create PIC code",                                     &be_options.pic),
	LC_OPT_ENT_BOOL     ("gprof",      "create gprof profiling code",                         &be_options.gprof),
	LC_OPT_ENT_ENUM_PTR ("verify",     "verify the backend irg",                              &verify_var),
	LC_OPT_ENT_BOOL     ("time",       "get backend timing statistics",                       &be_options.timing),
	LC_OPT_ENT_BOOL     ("profile",    "instrument the code for execution count profiling",   &be_options.opt_profile),
	LC_OPT_ENT_STR      ("os",         "specify target operating system",                     &be_options.target_os, sizeof(be_options.target_os)),
#ifdef FIRM_STATISTICS
	LC_OPT_ENT_BOOL     ("statev",     "dump statistic events",                               &be_options.statev),
	LC_OPT_ENT_STR      ("filtev",     "filter for stat events (regex if support is active",  &be_options.filtev, sizeof(be_options.filtev)),
#endif

#ifdef WITH_ILP
	LC_OPT_ENT_STR ("ilp.server", "the ilp server name", be_options.ilp_server, sizeof(be_options.ilp_server)),
	LC_OPT_ENT_STR ("ilp.solver", "the ilp solver name", be_options.ilp_solver, sizeof(be_options.ilp_solver)),
#endif /* WITH_ILP */
	LC_OPT_LAST
};

static be_module_list_entry_t *isa_ifs = NULL;


unsigned short asm_constraint_flags[256];

void be_init_default_asm_constraint_flags(void)
{
	asm_constraint_flags['?'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['!'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['&'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT
		| ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER;
	asm_constraint_flags['%'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT
		| ASM_CONSTRAINT_FLAG_MODIFIER_COMMUTATIVE;
	asm_constraint_flags['!'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;

	asm_constraint_flags['='] = ASM_CONSTRAINT_FLAG_MODIFIER_WRITE
		| ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ;
	asm_constraint_flags['+'] = ASM_CONSTRAINT_FLAG_MODIFIER_READ
		| ASM_CONSTRAINT_FLAG_MODIFIER_WRITE;

	asm_constraint_flags['i'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['s'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['E'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['F'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['G'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['H'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['I'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['J'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['K'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['L'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['M'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['N'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['O'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['P'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;

	asm_constraint_flags['m'] = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;
	asm_constraint_flags['o'] = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;
	asm_constraint_flags['V'] = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;
	asm_constraint_flags['<'] = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;
	asm_constraint_flags['>'] = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;

	asm_constraint_flags['p'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['0'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['1'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['2'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['3'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['4'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['5'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['6'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['7'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['8'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	asm_constraint_flags['9'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;

	asm_constraint_flags['X'] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER
		| ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP
		| ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;

	/* these should have been catched by the parsing code already */
	asm_constraint_flags['#']  = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['*']  = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags[' ']  = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['\t'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['\n'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
	asm_constraint_flags['\r'] = ASM_CONSTRAINT_FLAG_NO_SUPPORT;
}

asm_constraint_flags_t be_parse_asm_constraints(const char *constraint)
{
	asm_constraint_flags_t  flags = 0;
	const char             *c;
	asm_constraint_flags_t  tflags;

	for (c = constraint; *c != '\0'; ++c) {
		switch (*c) {
		case '#':
			/* 'comment' stuff */
			while (*c != 0 && *c != ',')
				++c;
			break;
		case '*':
			/* 'comment' character */
			++c;
			break;
		case ' ':
		case '\t':
		case '\n':
		case '\r':
			break;
		default:
			tflags = asm_constraint_flags[(int) *c];
			if (tflags != 0) {
				flags |= tflags;
			} else {
				flags |= isa_if->parse_asm_constraint(&c);
			}
			break;
		}
	}

	if ((
	    	flags & ASM_CONSTRAINT_FLAG_MODIFIER_WRITE &&
	    	flags & ASM_CONSTRAINT_FLAG_MODIFIER_NO_WRITE
	    ) || (
	    	flags & ASM_CONSTRAINT_FLAG_MODIFIER_READ &&
	    	flags & ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ
	    )) {
		flags |= ASM_CONSTRAINT_FLAG_INVALID;
	}
	if (!(flags & (ASM_CONSTRAINT_FLAG_MODIFIER_READ     |
	               ASM_CONSTRAINT_FLAG_MODIFIER_WRITE    |
	               ASM_CONSTRAINT_FLAG_MODIFIER_NO_WRITE |
	               ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ)
	    )) {
		flags |= ASM_CONSTRAINT_FLAG_MODIFIER_READ;
	}

	return flags;
}

int be_is_valid_clobber(const char *clobber)
{
	/* memory is a valid clobber. (the frontend has to detect this case too,
	 * because it has to add memory edges to the asm) */
	if (strcmp(clobber, "memory") == 0)
		return 1;
	/* cc (condition code) is always valid */
	if (strcmp(clobber, "cc") == 0)
		return 1;

	return isa_if->is_valid_clobber(clobber);
}

void be_register_isa_if(const char *name, const arch_isa_if_t *isa)
{
	if (isa_if == NULL)
		isa_if = isa;

	be_add_module_to_list(&isa_ifs, name, (void*) isa);
}

void be_opt_register(void)
{
	lc_opt_entry_t *be_grp;
	static int run_once = 0;

	if (run_once)
		return;
	run_once = 1;

	be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_add_table(be_grp, be_main_options);

	be_add_module_list_opt(be_grp, "isa", "the instruction set architecture",
	                       &isa_ifs, (void**) &isa_if);

	be_init_modules();
}

/* Parse one argument. */
int be_parse_arg(const char *arg)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	if (strcmp(arg, "help") == 0 || (arg[0] == '?' && arg[1] == '\0')) {
		lc_opt_print_help_for_entry(be_grp, '-', stdout);
		return -1;
	}
	return lc_opt_from_single_arg(be_grp, NULL, arg, NULL);
}

/* Perform schedule verification if requested. */
static void be_sched_verify(ir_graph *irg, int verify_opt)
{
	if (verify_opt == BE_VERIFY_WARN) {
		be_verify_schedule(irg);
	} else if (verify_opt == BE_VERIFY_ASSERT) {
		assert(be_verify_schedule(irg) && "Schedule verification failed.");
	}
}

/* Initialize the Firm backend. Must be run first in init_firm()! */
void firm_be_init(void)
{
	be_opt_register();
	be_init_modules();
}

/* Finalize the Firm backend. */
void firm_be_finish(void)
{
	be_quit_modules();
}

/* Returns the backend parameter */
const backend_params *be_get_backend_param(void)
{
	return isa_if->get_params();
}

/**
 * Initializes the main environment for the backend.
 *
 * @param env          an empty environment
 * @param file_handle  the file handle where the output will be written to
 */
static be_main_env_t *be_init_env(be_main_env_t *env, FILE *file_handle)
{
	memset(env, 0, sizeof(*env));
	env->options              = &be_options;
	env->ent_trampoline_map   = pmap_create();
	env->pic_trampolines_type = new_type_class(NEW_ID("$PIC_TRAMPOLINE_TYPE"));
	env->ent_pic_symbol_map   = pmap_create();
	env->pic_symbols_type     = new_type_struct(NEW_ID("$PIC_SYMBOLS_TYPE"));

	remove_irp_type(env->pic_trampolines_type);
	remove_irp_type(env->pic_symbols_type);
	set_class_final(env->pic_trampolines_type, 1);

	memset(asm_constraint_flags, 0, sizeof(asm_constraint_flags));
	env->arch_env = arch_env_init(isa_if, file_handle, env);

	be_dbg_open();
	return env;
}

/**
 * Called when the be_main_env_t can be destroyed.
 */
static void be_done_env(be_main_env_t *env)
{
	arch_env_done(env->arch_env);
	be_dbg_close();

	pmap_destroy(env->ent_trampoline_map);
	pmap_destroy(env->ent_pic_symbol_map);
	free_type(env->pic_trampolines_type);
	free_type(env->pic_symbols_type);
}

/**
 * A wrapper around a firm dumper. Dumps only, if
 * flags are enabled.
 *
 * @param mask    a bitmask containing the reason what will be dumped
 * @param irg     the IR graph to dump
 * @param suffix  the suffix for the dumper
 * @param dumper  the dumper to be called
 */
static void dump(int mask, ir_graph *irg, const char *suffix)
{
	if (be_options.dump_flags & mask)
		dump_ir_graph(irg, suffix);
}

/**
 * Prepare a backend graph for code generation and initialize its irg
 */
static void initialize_birg(be_irg_t *birg, ir_graph *irg, be_main_env_t *env)
{
	/* don't duplicate locals in backend when dumping... */
	ir_remove_dump_flags(ir_dump_flag_consts_local);

	dump(DUMP_INITIAL, irg, "begin");

	irg->be_data = birg;

	memset(birg, 0, sizeof(*birg));
	birg->irg = irg;
	birg->main_env = env;
	obstack_init(&birg->obst);

	edges_deactivate_kind(irg, EDGE_KIND_DEP);
	edges_activate_kind(irg, EDGE_KIND_DEP);

	/* set the current graph (this is important for several firm functions) */
	current_ir_graph = irg;

	/* we do this before critical edge split. As this produces less returns,
	   because sometimes (= 164.gzip) multiple returns are slower */
	normalize_n_returns(irg);

	/* Remove critical edges */
	remove_critical_cf_edges_ex(irg, /*ignore_exception_edges=*/0);

	/* Ensure, that the ir_edges are computed. */
	edges_assure(irg);

	set_irg_phase_state(irg, phase_backend);
	be_info_init_irg(irg);

	dump(DUMP_INITIAL, irg, "prepared");
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
	case T_RA_PROLOG:      return "ra_prolog";
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

/**
 * The Firm backend main loop.
 * Do architecture specific lowering for all graphs
 * and call the architecture specific code generator.
 *
 * @param file_handle   the file handle the output will be written to
 * @param cup_name      name of the compilation unit
 */
static void be_main_loop(FILE *file_handle, const char *cup_name)
{
	static const char suffix[] = ".prof";

	int           i, num_birgs, stat_active = 0;
	be_main_env_t env;
	char          prof_filename[256];
	be_irg_t      *birgs;
	ir_graph      **irg_list, **backend_irg_list;
	arch_env_t    *arch_env;

	be_timing = (be_options.timing == BE_TIME_ON);

	if (be_timing) {
		for (i = 0; i < T_LAST+1; ++i) {
			be_timers[i] = ir_timer_new();
		}
	}

	be_init_env(&env, file_handle);
	env.cup_name = cup_name;

	be_dbg_so(cup_name);
	be_dbg_types();

	arch_env = env.arch_env;

	/* backend may provide an ordered list of irgs where code should be
	 * generated for */
	irg_list         = NEW_ARR_F(ir_graph *, 0);
	backend_irg_list = arch_env_get_backend_irg_list(arch_env, &irg_list);

	/* we might need 1 birg more for instrumentation constructor */
	num_birgs = backend_irg_list ? ARR_LEN(backend_irg_list) : get_irp_n_irgs();
	birgs     = ALLOCAN(be_irg_t, num_birgs + 1);

	be_info_init();

	/* First: initialize all birgs */
	for (i = 0; i < num_birgs; ++i) {
		ir_graph *irg = backend_irg_list ? backend_irg_list[i] : get_irp_irg(i);
		initialize_birg(&birgs[i], irg, &env);
	}
	arch_env_handle_intrinsics(arch_env);
	DEL_ARR_F(irg_list);

	/*
		Get the filename for the profiling data.
		Beware: '\0' is already included in sizeof(suffix)
	*/
	memset(prof_filename, 0, sizeof(prof_filename));
	strncpy(prof_filename, cup_name, sizeof(prof_filename) - sizeof(suffix));
	strcat(prof_filename, suffix);

	/*
		Next: Either instruments all irgs with profiling code
		or try to read in profile data for current translation unit.
	*/
	if (be_options.opt_profile) {
		ir_graph *prof_init_irg = ir_profile_instrument(prof_filename, profile_default);
		initialize_birg(&birgs[num_birgs], prof_init_irg, &env);
		num_birgs++;
	} else {
		ir_profile_read(prof_filename);
	}

#ifdef FIRM_STATISTICS
	stat_active = stat_is_active();
#endif /* FIRM_STATISTICS */

	/* For all graphs */
	for (i = 0; i < num_birgs; ++i) {
		be_irg_t *birg = &birgs[i];
		ir_graph *irg  = birg->irg;
		optimization_state_t state;

		/* set the current graph (this is important for several firm functions) */
		current_ir_graph = irg;

		stat_ev_if {
			stat_ev_ctx_push_fobj("bemain_irg", irg);
			be_stat_ev("bemain_insns_start", be_count_insns(irg));
			be_stat_ev("bemain_blocks_start", be_count_blocks(irg));
		}

		/* stop and reset timers */
		be_timer_push(T_OTHER);

		/* Verify the initial graph */
		be_timer_push(T_VERIFY);
		if (be_options.verify_option == BE_VERIFY_WARN) {
			irg_verify(irg, VERIFY_ENFORCE_SSA);
			be_check_dominance(irg);
		} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
			assert(irg_verify(irg, VERIFY_ENFORCE_SSA) && "irg verification failed");
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}
		be_timer_pop(T_VERIFY);

		/* get a code generator for this graph. */
		arch_env->impl->init_graph(irg);

		/* some transformations need to be done before abi introduce */
		if (arch_env->impl->before_abi != NULL)
			arch_env->impl->before_abi(irg);

		/* implement the ABI conventions. */
		be_timer_push(T_ABI);
		be_abi_introduce(irg);
		be_timer_pop(T_ABI);

		if (!arch_env->custom_abi) {
			dump(DUMP_ABI, irg, "abi");
		}

		/* we have to do cfopt+remove_critical_edges as we can't have Bad-blocks
		 * or critical edges in the backend */
		optimize_cf(irg);
		remove_critical_cf_edges(irg);

		/* We often have dead code reachable through out-edges here. So for
		 * now we rebuild edges (as we need correct user count for code
		 * selection) */
		edges_deactivate(irg);
		edges_activate(irg);

		dump(DUMP_PREPARED, irg, "before-code-selection");

		if (be_options.verify_option == BE_VERIFY_WARN) {
			be_check_dominance(irg);
		} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}

		/* perform codeselection */
		be_timer_push(T_CODEGEN);
		if (arch_env->impl->prepare_graph != NULL)
			arch_env->impl->prepare_graph(irg);
		be_timer_pop(T_CODEGEN);

		if (be_options.verify_option == BE_VERIFY_WARN) {
			be_check_dominance(irg);
		} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}

		be_timer_push(T_EXECFREQ);
		/**
		 * Create execution frequencies from profile data or estimate some
		 */
		if (ir_profile_has_data())
			birg->exec_freq = ir_create_execfreqs_from_profile(irg);
		else {
			/* TODO: edges are corrupt for EDGE_KIND_BLOCK after the local
			 * optimize graph phase merges blocks in the x86 backend */
			edges_deactivate(irg);
			birg->exec_freq = compute_execfreq(irg, 10);
		}
		be_timer_pop(T_EXECFREQ);


		/* disabled for now, fails for EmptyFor.c and XXEndless.c */
		/* be_live_chk_compare(irg); */

		/* schedule the irg */
		be_timer_push(T_SCHED);
		list_sched(irg);
		be_timer_pop(T_SCHED);

		dump(DUMP_SCHED, irg, "sched");

		/* check schedule */
		be_timer_push(T_VERIFY);
		be_sched_verify(irg, be_options.verify_option);
		be_timer_pop(T_VERIFY);

		/* introduce patterns to assure constraints */
		be_timer_push(T_CONSTR);
		/* we switch off optimizations here, because they might cause trouble */
		save_optimization_state(&state);
		set_optimize(0);
		set_opt_cse(0);

		/* add Keeps for should_be_different constrained nodes  */
		/* beware: needs schedule due to usage of be_ssa_constr */
		assure_constraints(irg);
		be_timer_pop(T_CONSTR);

		dump(DUMP_SCHED, irg, "assured");

		/* stuff needs to be done after scheduling but before register allocation */
		be_timer_push(T_RA_PREPARATION);
		if (arch_env->impl->before_ra != NULL)
			arch_env->impl->before_ra(irg);
		be_timer_pop(T_RA_PREPARATION);

		/* connect all stack modifying nodes together (see beabi.c) */
		be_timer_push(T_ABI);
		be_abi_fix_stack_nodes(irg);
		be_timer_pop(T_ABI);

		dump(DUMP_SCHED, irg, "fix_stack");

		/* check schedule */
		be_timer_push(T_VERIFY);
		be_sched_verify(irg, be_options.verify_option);
		be_timer_pop(T_VERIFY);

		stat_ev_if {
			stat_ev_dbl("bemain_costs_before_ra",
					be_estimate_irg_costs(irg, birg->exec_freq));
			be_stat_ev("bemain_insns_before_ra", be_count_insns(irg));
			be_stat_ev("bemain_blocks_before_ra", be_count_blocks(irg));
		}

		/* Do register allocation */
		be_allocate_registers(irg);

#ifdef FIRM_STATISTICS
		stat_ev_dbl("bemain_costs_before_ra", be_estimate_irg_costs(irg, birg->exec_freq));
#endif

		dump(DUMP_RA, irg, "ra");

		/* let the code generator prepare the graph for emitter */
		be_timer_push(T_FINISH);
		if (arch_env->impl->after_ra != NULL)
			arch_env->impl->after_ra(irg);
		be_timer_pop(T_FINISH);

		/* fix stack offsets */
		be_timer_push(T_ABI);
		be_abi_fix_stack_nodes(irg);
		be_remove_dead_nodes_from_schedule(irg);
		be_abi_fix_stack_bias(irg);
		be_timer_pop(T_ABI);

		dump(DUMP_SCHED, irg, "fix_stack_after_ra");

		be_timer_push(T_FINISH);
		if (arch_env->impl->finish != NULL)
			arch_env->impl->finish(irg);
		be_timer_pop(T_FINISH);

		dump(DUMP_FINAL, irg, "finish");

		stat_ev_if {
			be_stat_ev("bemain_insns_finish", be_count_insns(irg));
			be_stat_ev("bemain_blocks_finish", be_count_blocks(irg));
		}

		/* check schedule and register allocation */
		be_timer_push(T_VERIFY);
		if (be_options.verify_option == BE_VERIFY_WARN) {
			irg_verify(irg, VERIFY_ENFORCE_SSA);
			be_check_dominance(irg);
			be_verify_schedule(irg);
			be_verify_register_allocation(irg);
		} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
			assert(irg_verify(irg, VERIFY_ENFORCE_SSA) && "irg verification failed");
			assert(be_check_dominance(irg) && "Dominance verification failed");
			assert(be_verify_schedule(irg) && "Schedule verification failed");
			assert(be_verify_register_allocation(irg)
			       && "register allocation verification failed");

		}
		be_timer_pop(T_VERIFY);

		/* emit assembler code */
		be_timer_push(T_EMIT);
		if (arch_env->impl->emit != NULL)
			arch_env->impl->emit(irg);
		be_timer_pop(T_EMIT);

		dump(DUMP_FINAL, irg, "end");

		be_timer_push(T_ABI);
		be_abi_free(irg);
		be_timer_pop(T_ABI);

		restore_optimization_state(&state);

		be_timer_pop(T_OTHER);

		if (be_timing) {
			int t;
			if (stat_ev_enabled) {
				for (t = 0; t < T_LAST+1; ++t) {
					char buf[128];
					snprintf(buf, sizeof(buf), "bemain_time_%s",
					         get_timer_name(t));
					stat_ev_dbl(buf, ir_timer_elapsed_usec(be_timers[i]));
				}
			} else {
				printf("==>> IRG %s <<==\n",
				       get_entity_name(get_irg_entity(irg)));
				for (t = 0; t < T_LAST+1; ++t) {
					double val = ir_timer_elapsed_usec(be_timers[t]) / 1000.0;
					printf("%-20s: %8.3lf msec\n", get_timer_name(t), val);
				}
			}
			for (t = 0; t < T_LAST+1; ++t) {
				ir_timer_stop(be_timers[t]);
				ir_timer_reset(be_timers[t]);
			}
		}

		be_free_birg(irg);
		stat_ev_ctx_pop("bemain_irg");
	}
	ir_profile_free();
	be_done_env(&env);

	be_info_free();
}

/* Main interface to the frontend. */
void be_main(FILE *file_handle, const char *cup_name)
{
	ir_timer_t *t = NULL;

	/* The user specified another config file to read. do that now. */
	if (config_file[0] != '\0') {
		FILE *f = fopen(config_file, "rt");

		if (f != NULL) {
			lc_opt_from_file(config_file, f, NULL);
			fclose(f);
		} else {
			fprintf(stderr, "Warning: Cannot open config file '%s'\n", config_file);
		}
	}

	if (be_options.timing == BE_TIME_ON) {
		t = ir_timer_new();

		if (ir_timer_enter_high_priority()) {
			fprintf(stderr, "Warning: Could not enter high priority mode.\n");
		}

		ir_timer_reset_and_start(t);
	}

#ifdef FIRM_STATISTICS
	if (be_options.statev) {
		const char *dot = strrchr(cup_name, '.');
		const char *pos = dot ? dot : cup_name + strlen(cup_name);
		char       *buf = ALLOCAN(char, pos - cup_name + 1);
		strncpy(buf, cup_name, pos - cup_name);
		buf[pos - cup_name] = '\0';

		be_options.statev = 1;
		stat_ev_begin(buf, be_options.filtev);
		stat_ev_ctx_push_str("bemain_compilation_unit", cup_name);
	}
#endif

	be_main_loop(file_handle, cup_name);

	if (be_options.timing == BE_TIME_ON) {
		ir_timer_stop(t);
		ir_timer_leave_high_priority();
		stat_ev_if {
			stat_ev_dbl("bemain_backend_time", ir_timer_elapsed_msec(t));
		} else {
			double val = ir_timer_elapsed_usec(t) / 1000.0;
			printf("%-20s: %8.3lf msec\n", "BEMAINLOOP", val);
		}
	}

#ifdef FIRM_STATISTICS
	if (be_options.statev) {
		stat_ev_ctx_pop("bemain_compilation_unit");
		stat_ev_end();
	}
#endif
}

static int do_lower_for_target(ir_prog *irp, void *context)
{
	const backend_params *be_params = be_get_backend_param();
	be_params->lower_for_target();
	(void) context;
	(void) irp;
	return 0;
}

ir_prog_pass_t *lower_for_target_pass(const char *name)
{
	ir_prog_pass_t *pass = XMALLOCZ(ir_prog_pass_t);
	return def_prog_pass_constructor(pass,
	                                 name ? name : "lower_for_target",
	                                 do_lower_for_target);
}

unsigned be_put_ignore_regs(const ir_graph *irg,
                            const arch_register_class_t *cls, bitset_t *bs)
{
	if (bs == NULL)
		bs = bitset_alloca(cls->n_regs);
	else
		bitset_clear_all(bs);

	assert(bitset_size(bs) == cls->n_regs);
	arch_put_non_ignore_regs(cls, bs);
	bitset_flip_all(bs);
	be_abi_put_ignore_regs(be_get_irg_abi(irg), cls, bs);

	return bitset_popcount(bs);
}
