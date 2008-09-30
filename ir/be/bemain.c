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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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
#include "phiclass.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irtools.h"
#include "irvrfy.h"
#include "irprintf.h"
#include "iroptimize.h"
#include "firmstat.h"
#include "execfreq.h"
#include "irprofile.h"

#include "bearch_t.h"
#include "be_t.h"
#include "bemodule.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bera.h"
#include "bechordal_t.h"
#include "beifg.h"
#include "beifg_impl.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "beabi.h"
#include "belower.h"
#include "beschedmris.h"
#include "bestat.h"
#include "beverify.h"
#include "be_dbgout.h"
#include "beirg_t.h"

#define NEW_ID(s) new_id_from_chars(s, sizeof(s) - 1)

#ifdef WITH_ILP
#include "beilpsched.h"
#endif /* WITH_ILP */

/* options visible for anyone */
static be_options_t be_options = {
	DUMP_NONE,                         /* dump flags */
	BE_TIME_OFF,                       /* no timing */
	0,                                 /* no opt profile */
	0,                                 /* try to omit frame pointer */
	0,                                 /* try to omit leaf frame pointer */
	0,                                 /* create PIC code */
	0,                                 /* create gprof compatible profiling code */
	BE_VRFY_WARN,                      /* verification level: warn */
	BE_SCHED_LIST,                     /* scheduler: list scheduler */
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
static const lc_opt_enum_int_items_t vrfy_items[] = {
	{ "off",    BE_VRFY_OFF    },
	{ "warn",   BE_VRFY_WARN   },
	{ "assert", BE_VRFY_ASSERT },
	{ NULL,     0 }
};

/* scheduling options. */
static const lc_opt_enum_int_items_t sched_items[] = {
	{ "list", BE_SCHED_LIST },
#ifdef WITH_ILP
	{ "ilp",  BE_SCHED_ILP  },
#endif /* WITH_ILP */
	{ NULL,   0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&be_options.dump_flags, dump_items
};

static lc_opt_enum_int_var_t vrfy_var = {
	&be_options.vrfy_option, vrfy_items
};

static lc_opt_enum_int_var_t sched_var = {
	&be_options.scheduler, sched_items
};

static const lc_opt_table_entry_t be_main_options[] = {
	LC_OPT_ENT_STR      ("config",     "read another config file containing backend options", config_file, sizeof(config_file)),
	LC_OPT_ENT_ENUM_MASK("dump",       "dump irg on several occasions",                       &dump_var),
	LC_OPT_ENT_BOOL     ("omitfp",     "omit frame pointer",                                  &be_options.omit_fp),
	LC_OPT_ENT_BOOL     ("omitleaffp", "omit frame pointer in leaf routines",                 &be_options.omit_leaf_fp),
	LC_OPT_ENT_BOOL     ("pic",        "create PIC code",                                     &be_options.pic),
	LC_OPT_ENT_BOOL     ("gprof",      "create gprof profiling code",                         &be_options.gprof),
	LC_OPT_ENT_ENUM_PTR ("vrfy",       "verify the backend irg",                              &vrfy_var),
	LC_OPT_ENT_BOOL     ("time",       "get backend timing statistics",                       &be_options.timing),
	LC_OPT_ENT_BOOL     ("profile",    "instrument the code for execution count profiling",   &be_options.opt_profile),
	LC_OPT_ENT_ENUM_PTR ("sched",      "select a scheduler",                                  &sched_var),
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
			while(*c != 0 && *c != ',')
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
				flags |= isa_if->parse_asm_constraint(isa_if, &c);
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

	return isa_if->is_valid_clobber(isa_if, clobber);
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

	if (run_once) {
		return;
	}
	run_once     = 1;

	be_init_modules();

	be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_add_table(be_grp, be_main_options);

	be_add_module_list_opt(be_grp, "isa", "the instruction set architecture",
	                       &isa_ifs, (void**) &isa_if);
}

/* Parse one argument. */
int be_parse_arg(const char *arg) {
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	if (strcmp(arg, "help") == 0 || (arg[0] == '?' && arg[1] == '\0')) {
		lc_opt_print_help(be_grp, stdout);
		return -1;
	}
	return lc_opt_from_single_arg(be_grp, NULL, arg, NULL);
}

/** The be parameters returned by default, all off. */
static const backend_params be_params = {
	0,    /* need dword lowering */
	0,    /* don't support inline assembler yet */
	0,     /* no immediate floating point mode. */
	NULL, /* no additional opcodes */
	NULL, /* will be set later */
	NULL, /* but yet no creator function */
	NULL, /* context for create_intrinsic_fkt */
	NULL, /* no if conversion settings */
	NULL   /* no immediate fp mode */
};

/* Perform schedule verification if requested. */
static void be_sched_vrfy(be_irg_t *birg, int vrfy_opt) {
	if (vrfy_opt == BE_VRFY_WARN) {
		be_verify_schedule(birg);
	} else if (vrfy_opt == BE_VRFY_ASSERT) {
		assert(be_verify_schedule(birg) && "Schedule verification failed.");
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
	if (isa_if->get_params)
		return isa_if->get_params();
	return &be_params;
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

	be_phi_handler_new(env);

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
	be_phi_handler_free();

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
static void dump(int mask, ir_graph *irg, const char *suffix,
                 void (*dumper)(ir_graph *, const char *))
{
	if(be_options.dump_flags & mask)
		be_dump(irg, suffix, dumper);
}

/**
 * Prepare a backend graph for code generation and initialize its birg
 */
static void initialize_birg(be_irg_t *birg, ir_graph *irg, be_main_env_t *env)
{
	memset(birg, 0, sizeof(*birg));
	birg->irg = irg;
	birg->main_env = env;

	edges_deactivate_kind(irg, EDGE_KIND_DEP);
	edges_activate_kind(irg, EDGE_KIND_DEP);

	dump(DUMP_INITIAL, irg, "-begin", dump_ir_block_graph);

	be_stat_init_irg(env->arch_env, irg);
	be_do_stat_nodes(irg, "01 Begin");

	/* set the current graph (this is important for several firm functions) */
	current_ir_graph = irg;

	/* Normalize proj nodes. */
	normalize_proj_nodes(irg);

	/* we do this before critical edge split. As this produces less returns,
	   because sometimes (= 164.gzip) multiple returns are slower */
	normalize_n_returns(irg);

	/* Remove critical edges */
	remove_critical_cf_edges_ex(irg, /*ignore_exception_edges=*/0);

	/* Ensure, that the ir_edges are computed. */
	edges_assure(irg);

	set_irg_phase_state(irg, phase_backend);

	dump(DUMP_INITIAL, irg, "-prepared", dump_ir_block_graph);
}

#define BE_TIMER_ONLY(code)   do { if (be_timing) { code; } } while(0)

int be_timing;
ir_timer_t *t_abi;
ir_timer_t *t_codegen;
ir_timer_t *t_sched;
ir_timer_t *t_constr;
ir_timer_t *t_finish;
ir_timer_t *t_emit;
ir_timer_t *t_other;
ir_timer_t *t_verify;
ir_timer_t *t_heights;
ir_timer_t *t_live;
ir_timer_t *t_execfreq;
ir_timer_t *t_ssa_constr;
ir_timer_t *t_ra_constr;
ir_timer_t *t_ra_prolog;
ir_timer_t *t_ra_epilog;
ir_timer_t *t_ra_spill;
ir_timer_t *t_ra_spill_apply;
ir_timer_t *t_ra_color;
ir_timer_t *t_ra_ifg;
ir_timer_t *t_ra_copymin;
ir_timer_t *t_ra_ssa;
ir_timer_t *t_ra_other;

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
	int i;
	be_main_env_t env;
	char prof_filename[256];
	static const char suffix[] = ".prof";
	be_irg_t *birgs;
	int num_birgs;
	ir_graph **irg_list, **backend_irg_list;
	arch_env_t *arch_env;

	be_timing = (be_options.timing == BE_TIME_ON);

	if (be_timing) {
		t_abi        = ir_timer_register("bemain_time_beabi",       "be abi introduction");
		t_codegen    = ir_timer_register("bemain_time_codegen",     "codegeneration");
		t_sched      = ir_timer_register("bemain_time_sched",       "scheduling");
		t_constr     = ir_timer_register("bemain_time_constr",      "assure constraints");
		t_finish     = ir_timer_register("bemain_time_finish",      "graph finish");
		t_emit       = ir_timer_register("bemain_time_emiter",      "code emiter");
		t_verify     = ir_timer_register("bemain_time_verify",      "graph verification");
		t_other      = ir_timer_register("bemain_time_other",       "other");
		t_heights    = ir_timer_register("bemain_time_heights",     "heights");
		t_live       = ir_timer_register("bemain_time_liveness",    "be liveness");
		t_execfreq   = ir_timer_register("bemain_time_execfreq",    "execfreq");
		t_ssa_constr = ir_timer_register("bemain_time_ssa_constr",  "ssa reconstruction");
		t_ra_prolog  = ir_timer_register("bemain_time_ra_prolog",   "regalloc prolog");
		t_ra_epilog  = ir_timer_register("bemain_time_ra_epilog",   "regalloc epilog");
		t_ra_constr  = ir_timer_register("bemain_time_ra_constr",   "regalloc constraints");
		t_ra_spill   = ir_timer_register("bemain_time_ra_spill",    "spiller");
		t_ra_spill_apply
			= ir_timer_register("bemain_time_ra_spill_apply", "apply spills");
		t_ra_color   = ir_timer_register("bemain_time_ra_color",    "graph coloring");
		t_ra_ifg     = ir_timer_register("bemain_time_ra_ifg",      "interference graph");
		t_ra_copymin = ir_timer_register("bemain_time_ra_copymin",  "copy minimization");
		t_ra_ssa     = ir_timer_register("bemain_time_ra_ssadestr", "ssa destruction");
		t_ra_other   = ir_timer_register("bemain_time_ra_other",    "regalloc other");
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
	birgs     = alloca(sizeof(birgs[0]) * (num_birgs + 1));

	/* First: initialize all birgs */
	for(i = 0; i < num_birgs; ++i) {
		ir_graph *irg = backend_irg_list ? backend_irg_list[i] : get_irp_irg(i);
		initialize_birg(&birgs[i], irg, &env);
	}
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

	/* For all graphs */
	for (i = 0; i < num_birgs; ++i) {
		be_irg_t *birg = &birgs[i];
		ir_graph *irg  = birg->irg;
		optimization_state_t state;
		const arch_code_generator_if_t *cg_if;

		/* set the current graph (this is important for several firm functions) */
		current_ir_graph = irg;

		be_sched_init_phase(irg);

		/* reset the phi handler. */
		be_phi_handler_reset();

		stat_ev_ctx_push_fobj("bemain_irg", irg);

		/* stop and reset timers */
		BE_TIMER_PUSH(t_other);   /* t_other */

		/* Verify the initial graph */
		BE_TIMER_PUSH(t_verify);
		if (be_options.vrfy_option == BE_VRFY_WARN) {
			irg_verify(irg, VRFY_ENFORCE_SSA);
			be_check_dominance(irg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(irg_verify(irg, VRFY_ENFORCE_SSA) && "irg verification failed");
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}
		BE_TIMER_POP(t_verify);

		/* Get the code generator interface. */
		cg_if = arch_env_get_code_generator_if(arch_env);

		/* get a code generator for this graph. */
		birg->cg = cg_if->init(birg);

		/* some transformations need to be done before abi introduce */
		arch_code_generator_before_abi(birg->cg);

		/* implement the ABI conventions. */
		BE_TIMER_PUSH(t_abi);
		birg->abi = be_abi_introduce(birg);
		BE_TIMER_POP(t_abi);

		dump(DUMP_ABI, irg, "-abi", dump_ir_block_graph);
		be_do_stat_nodes(irg, "02 Abi");

		if (be_options.vrfy_option == BE_VRFY_WARN) {
			be_check_dominance(irg);
			be_verify_out_edges(irg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(be_verify_out_edges(irg));
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}

		/* generate code */
		BE_TIMER_PUSH(t_codegen);
		arch_code_generator_prepare_graph(birg->cg);
		BE_TIMER_POP(t_codegen);

		/* reset the phi handler. */
		be_phi_handler_reset();

		be_do_stat_nodes(irg, "03 Prepare");

		dump(DUMP_PREPARED, irg, "-prepared", dump_ir_block_graph);

		if (be_options.vrfy_option == BE_VRFY_WARN) {
			be_check_dominance(irg);
			be_verify_out_edges(irg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(be_verify_out_edges(irg));
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}

		BE_TIMER_PUSH(t_execfreq);
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
		BE_TIMER_POP(t_execfreq);


		/* disabled for now, fails for EmptyFor.c and XXEndless.c */
		/* be_live_chk_compare(birg); */

		/* let backend prepare scheduling */
		BE_TIMER_PUSH(t_codegen);
		arch_code_generator_before_sched(birg->cg);
		BE_TIMER_POP(t_codegen);

		/* schedule the irg */
		BE_TIMER_PUSH(t_sched);
		switch (be_options.scheduler) {
			default:
				fprintf(stderr, "Warning: invalid scheduler (%d) selected, falling back to list scheduler.\n", be_options.scheduler);
			case BE_SCHED_LIST:
				list_sched(birg, &be_options);
				break;
#ifdef WITH_ILP
			case BE_SCHED_ILP:
				be_ilp_sched(birg, &be_options);
				break;
#endif /* WITH_ILP */
		};
		BE_TIMER_POP(t_sched);

		dump(DUMP_SCHED, irg, "-sched", dump_ir_block_graph_sched);

		/* check schedule */
		BE_TIMER_PUSH(t_verify);
		be_sched_vrfy(birg, be_options.vrfy_option);
		BE_TIMER_POP(t_verify);

		be_do_stat_nodes(irg, "04 Schedule");

		/* introduce patterns to assure constraints */
		BE_TIMER_PUSH(t_constr);
		/* we switch off optimizations here, because they might cause trouble */
		save_optimization_state(&state);
		set_optimize(0);
		set_opt_normalize(0);
		set_opt_cse(0);

		/* add Keeps for should_be_different constrained nodes  */
		/* beware: needs schedule due to usage of be_ssa_constr */
		assure_constraints(birg);
		BE_TIMER_POP(t_constr);

		dump(DUMP_SCHED, irg, "-assured", dump_ir_block_graph_sched);
		be_do_stat_nodes(irg, "05 Constraints");

		/* stuff needs to be done after scheduling but before register allocation */
		BE_TIMER_PUSH(t_codegen);
		arch_code_generator_before_ra(birg->cg);
		BE_TIMER_POP(t_codegen);

		/* connect all stack modifying nodes together (see beabi.c) */
		BE_TIMER_PUSH(t_abi);
		be_abi_fix_stack_nodes(birg->abi);
		BE_TIMER_POP(t_abi);

		dump(DUMP_SCHED, irg, "-fix_stack", dump_ir_block_graph_sched);

		/* check schedule */
		BE_TIMER_PUSH(t_verify);
		be_sched_vrfy(birg, be_options.vrfy_option);
		BE_TIMER_POP(t_verify);

#ifdef FIRM_STATISTICS
		stat_ev_dbl("bemain_costs_before_ra", be_estimate_irg_costs(irg, arch_env, birg->exec_freq));
#endif

		/* Do register allocation */
		be_allocate_registers(birg);

#ifdef FIRM_STATISTICS
		stat_ev_dbl("bemain_costs_before_ra", be_estimate_irg_costs(irg, arch_env, birg->exec_freq));
#endif

		dump(DUMP_RA, irg, "-ra", dump_ir_block_graph_sched);
		be_do_stat_nodes(irg, "06 Register Allocation");

		/* let the code generator prepare the graph for emitter */
		BE_TIMER_PUSH(t_finish);
		arch_code_generator_after_ra(birg->cg);
		BE_TIMER_POP(t_finish);

		/* fix stack offsets */
		BE_TIMER_PUSH(t_abi);
		be_abi_fix_stack_nodes(birg->abi);
		be_remove_dead_nodes_from_schedule(birg);
		be_abi_fix_stack_bias(birg->abi);
		BE_TIMER_POP(t_abi);

		dump(DUMP_SCHED, irg, "-fix_stack_after_ra", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(t_finish);
		arch_code_generator_finish(birg->cg);
		BE_TIMER_POP(t_finish);

		dump(DUMP_FINAL, irg, "-finish", dump_ir_block_graph_sched);

		/* check schedule and register allocation */
		BE_TIMER_PUSH(t_verify);
		if (be_options.vrfy_option == BE_VRFY_WARN) {
			irg_verify(irg, VRFY_ENFORCE_SSA);
			be_check_dominance(irg);
			be_verify_out_edges(irg);
			be_verify_schedule(birg);
			be_verify_register_allocation(birg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(irg_verify(irg, VRFY_ENFORCE_SSA) && "irg verification failed");
			assert(be_verify_out_edges(irg) && "out edge verification failed");
			assert(be_check_dominance(irg) && "Dominance verification failed");
			assert(be_verify_schedule(birg) && "Schedule verification failed");
			assert(be_verify_register_allocation(birg)
			       && "register allocation verification failed");

		}
		BE_TIMER_POP(t_verify);

		/* emit assembler code */
		BE_TIMER_PUSH(t_emit);
		arch_code_generator_done(birg->cg);
		BE_TIMER_POP(t_emit);

		dump(DUMP_FINAL, irg, "-end", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(t_abi);
		be_abi_free(birg->abi);
		BE_TIMER_POP(t_abi);

		be_do_stat_nodes(irg, "07 Final");
		restore_optimization_state(&state);

		BE_TIMER_POP(t_other);

#define STOP_AND_RESET_TIMER(timer) do { ir_timer_stop(timer); ir_timer_reset(timer); } while(0)

#define LC_EMIT(timer)  \
		stat_ev_if {    \
			stat_ev_dbl(ir_timer_get_name(timer), ir_timer_elapsed_msec(timer));  \
		} else { \
			printf("%-20s: %8.3lf msec\n", ir_timer_get_description(timer), (double)ir_timer_elapsed_usec(timer) / 1000.0); \
		} \
		STOP_AND_RESET_TIMER(timer);

		BE_TIMER_ONLY(
			stat_ev_if {
			} else {
				printf("==>> IRG %s <<==\n", get_entity_name(get_irg_entity(irg)));
			}
			LC_EMIT(t_abi);
			LC_EMIT(t_codegen);
			LC_EMIT(t_sched);
			LC_EMIT(t_live);
			LC_EMIT(t_heights);
			LC_EMIT(t_ssa_constr);
			LC_EMIT(t_constr);
			LC_EMIT(t_execfreq);
			LC_EMIT(t_ra_prolog);
			LC_EMIT(t_ra_spill);
			LC_EMIT(t_ra_spill_apply);
			LC_EMIT(t_ra_constr);
			LC_EMIT(t_ra_color);
			LC_EMIT(t_ra_ifg);
			LC_EMIT(t_ra_copymin);
			LC_EMIT(t_ra_ssa);
			LC_EMIT(t_ra_epilog);
			LC_EMIT(t_ra_other);
			LC_EMIT(t_finish);
			LC_EMIT(t_emit);
			LC_EMIT(t_verify);
			LC_EMIT(t_other);
		);
#undef LC_EMIT

		be_sched_free_phase(irg);

		be_free_birg(birg);
		stat_ev_ctx_pop("bemain_irg");
	}
	ir_profile_free();
	be_done_env(&env);
}

/* Main interface to the frontend. */
void be_main(FILE *file_handle, const char *cup_name)
{
	ir_timer_t *t = NULL;

	/* The user specified another config file to read. do that now. */
	if (config_file[0] != '\0') {
		FILE *f;

		if ((f = fopen(config_file, "rt")) != NULL) {
			lc_opt_from_file(config_file, f, NULL);
			fclose(f);
		}
	}

	if (be_options.timing == BE_TIME_ON) {
		t = ir_timer_register("bemain", "measure complete bemain loop");

		if (ir_timer_enter_high_priority()) {
			fprintf(stderr, "Warning: Could not enter high priority mode.\n");
		}

		ir_timer_reset_and_start(t);
	}

#ifdef FIRM_STATISTICS
	if (be_options.statev) {
		const char *dot = strrchr(cup_name, '.');
		const char *pos = dot ? dot : cup_name + strlen(cup_name);
		char       *buf = alloca(pos - cup_name + 1);
		strncpy(buf, cup_name, pos - cup_name);
		buf[pos - cup_name] = '\0';

		be_options.statev = 1;
		stat_ev_begin(buf, be_options.filtev);
		stat_ev_ctx_push_str("bemain_compilation_unit", cup_name);
	}
#endif

	/* never build code for pseudo irgs */
	set_visit_pseudo_irgs(0);

 	be_node_init();

	be_main_loop(file_handle, cup_name);

	if (be_options.timing == BE_TIME_ON) {
		ir_timer_stop(t);
		ir_timer_leave_high_priority();
		stat_ev_if {
			stat_ev_dbl("bemain_backend_time", ir_timer_elapsed_msec(t));
		} else {
			printf("%-20s: %lu msec\n", "BEMAINLOOP", ir_timer_elapsed_msec(t));
		}
	}

#ifdef FIRM_STATISTICS
	if (be_options.statev) {
		stat_ev_ctx_pop("bemain_compilation_unit");
		stat_ev_end();
	}
#endif
}

unsigned be_put_ignore_regs(const be_irg_t *birg, const arch_register_class_t *cls, bitset_t *bs)
{
	if (bs == NULL)
		bs = bitset_alloca(cls->n_regs);
	else
		bitset_clear_all(bs);

	assert(bitset_size(bs) == (unsigned)cls->n_regs);
	arch_put_non_ignore_regs(birg->main_env->arch_env, cls, bs);
	bitset_flip_all(bs);
	be_abi_put_ignore_regs(birg->abi, cls, bs);

	return bitset_popcnt(bs);
}
