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

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>

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
#include "firmstat.h"
#include "execfreq.h"

#include "bearch_t.h"
#include "be_t.h"
#include "bemodule.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bespillbelady.h"
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
#include "beprofile.h"
#include "be_dbgout.h"
#include "beirg_t.h"

#ifdef WITH_ILP
#include "beilpsched.h"
#endif /* WITH_ILP */

/* options visible for anyone */
static be_options_t be_options = {
	DUMP_NONE,                         /* dump flags */
	BE_TIME_OFF,                       /* no timing */
	0,                                 /* no opt profile */
	1,                                 /* try to omit frame pointer */
	0,                                 /* no stabs debugging output */
	BE_VRFY_WARN,                      /* verification level: warn */
	BE_SCHED_LIST,                     /* scheduler: list scheduler */
	"i44pc52.info.uni-karlsruhe.de",   /* ilp server */
	"cplex",                           /* ilp solver */
	0,                                 /* enable statistic event dumping */
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
	LC_OPT_ENT_STR      ("config",   "read another config file containing backend options", config_file, sizeof(config_file)),
	LC_OPT_ENT_ENUM_MASK("dump",     "dump irg on several occasions",                       &dump_var),
	LC_OPT_ENT_NEGBOOL  ("noomitfp", "do not omit frame pointer",                           &be_options.omit_fp),
	LC_OPT_ENT_BOOL     ("stabs",    "enable stabs debug support",                          &be_options.stabs_debug_support),
	LC_OPT_ENT_ENUM_PTR ("vrfy",     "verify the backend irg",                              &vrfy_var),
	LC_OPT_ENT_BOOL     ("time",     "get backend timing statistics",                       &be_options.timing),
	LC_OPT_ENT_BOOL     ("profile",  "instrument the code for execution count profiling",   &be_options.opt_profile),
	LC_OPT_ENT_ENUM_PTR ("sched",    "select a scheduler",                                  &sched_var),
	LC_OPT_ENT_BOOL     ("statev",   "dump statistic events",                               &be_options.statev),

#ifdef WITH_ILP
	LC_OPT_ENT_STR ("ilp.server", "the ilp server name", be_options.ilp_server, sizeof(be_options.ilp_server)),
	LC_OPT_ENT_STR ("ilp.solver", "the ilp solver name", be_options.ilp_solver, sizeof(be_options.ilp_solver)),
#endif /* WITH_ILP */
	LC_OPT_LAST
};

static be_module_list_entry_t *isa_ifs = NULL;

void be_register_isa_if(const char *name, const arch_isa_if_t *isa)
{
	if(isa_if == NULL)
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
	0,    /* don't support inlien assembler yet */
	NULL, /* no additional opcodes */
	NULL, /* will be set later */
	NULL, /* but yet no creator function */
	NULL, /* context for create_intrinsic_fkt */
	NULL, /* no if conversion settings */
};

/* Perform schedule verification if requested. */
static void be_sched_vrfy(be_irg_t *birg, int vrfy_opt) {
	if (vrfy_opt == BE_VRFY_WARN) {
		be_verify_schedule(birg);
	}
	else if (vrfy_opt == BE_VRFY_ASSERT) {
		assert(be_verify_schedule(birg) && "Schedule verification failed.");
	}
}

/* Initialize the Firm backend. Must be run BEFORE init_firm()! */
const backend_params *be_init(void)
{
	be_opt_register();
	be_init_modules();

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
	obstack_init(&env->obst);
	env->arch_env = obstack_alloc(&env->obst, sizeof(env->arch_env[0]));
	env->options  = &be_options;

	arch_env_init(env->arch_env, isa_if, file_handle, env);

	/* Register the irn handler of the architecture */
	if (arch_isa_get_irn_handler(env->arch_env->isa))
		arch_env_push_irn_handler(env->arch_env, arch_isa_get_irn_handler(env->arch_env->isa));

	/*
	 * Register the node handler of the back end infrastructure.
	 * This irn handler takes care of the platform independent
	 * spill, reload and perm nodes.
	 */
	arch_env_push_irn_handler(env->arch_env, &be_node_irn_handler);
	env->phi_handler = be_phi_handler_new(env->arch_env);
	arch_env_push_irn_handler(env->arch_env, env->phi_handler);

	env->db_handle = be_options.stabs_debug_support ? be_stabs_open(file_handle) : be_nulldbg_open();
	return env;
}

static void be_done_env(be_main_env_t *env)
{
	env->arch_env->isa->impl->done(env->arch_env->isa);
	be_dbg_close(env->db_handle);
	be_phi_handler_free(env->phi_handler);
	obstack_free(&env->obst, NULL);
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
	remove_critical_cf_edges(irg);

	/* Ensure, that the ir_edges are computed. */
	edges_assure(irg);

	/* reset the phi handler. */
	be_phi_handler_reset(env->phi_handler);

	set_irg_phase_state(irg, phase_backend);

	dump(DUMP_INITIAL, irg, "-prepared", dump_ir_block_graph);
}

#define BE_TIMER_PUSH(timer)                                                        \
	if (be_options.timing == BE_TIME_ON) {                                          \
		int res = lc_timer_push(timer);                                             \
		if (be_options.vrfy_option == BE_VRFY_ASSERT)                               \
			assert(res && "Timer already on stack, cannot be pushed twice.");       \
		else if (be_options.vrfy_option == BE_VRFY_WARN && ! res)                   \
			fprintf(stderr, "Timer %s already on stack, cannot be pushed twice.\n", \
				lc_timer_get_name(timer));                                          \
	}
#define BE_TIMER_POP(timer)                                                                    \
	if (be_options.timing == BE_TIME_ON) {                                                     \
		lc_timer_t *tmp = lc_timer_pop();                                                      \
		if (be_options.vrfy_option == BE_VRFY_ASSERT)                                          \
			assert(tmp == timer && "Attempt to pop wrong timer.");                             \
		else if (be_options.vrfy_option == BE_VRFY_WARN && tmp != timer)                       \
			fprintf(stderr, "Attempt to pop wrong timer. %s is on stack, trying to pop %s.\n", \
				lc_timer_get_name(tmp), lc_timer_get_name(timer));                             \
		timer = tmp;                                                                           \
	}

#define BE_TIMER_ONLY(code)   do { if (be_options.timing == BE_TIME_ON) { code; } } while(0)

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
	arch_isa_t *isa;
	be_main_env_t env;
	unsigned num_nodes_b = 0;
	unsigned num_nodes_a = 0;
	unsigned num_nodes_r = 0;
	char prof_filename[256];
	static const char suffix[] = ".prof";
	be_irg_t *birgs;
	int num_birgs;
	ir_graph **irg_list, **backend_irg_list;

	lc_timer_t *t_abi      = NULL;
	lc_timer_t *t_codegen  = NULL;
	lc_timer_t *t_sched    = NULL;
   	lc_timer_t *t_constr   = NULL;
	lc_timer_t *t_regalloc = NULL;
	lc_timer_t *t_finish   = NULL;
	lc_timer_t *t_emit     = NULL;
	lc_timer_t *t_other    = NULL;
	lc_timer_t *t_verify   = NULL;

	if (be_options.timing == BE_TIME_ON) {
		t_abi      = lc_timer_register("beabi",    "be abi introduction");
		t_codegen  = lc_timer_register("codegen",  "codegeneration");
		t_sched    = lc_timer_register("sched",    "scheduling");
		t_constr   = lc_timer_register("constr",   "assure constraints");
		t_regalloc = lc_timer_register("regalloc", "register allocation");
		t_finish   = lc_timer_register("finish",   "graph finish");
		t_emit     = lc_timer_register("emiter",   "code emiter");
		t_verify   = lc_timer_register("verify",   "graph verification");
		t_other    = lc_timer_register("other",    "other");
	}

	be_init_env(&env, file_handle);

	isa = arch_env_get_isa(env.arch_env);

	be_dbg_so(env.db_handle, cup_name);
	be_dbg_types(env.db_handle);

	/* backend may provide an ordered list of irgs where code should be generated for */
	irg_list         = NEW_ARR_F(ir_graph *, 0);
	backend_irg_list = arch_isa_get_backend_irg_list(isa, &irg_list);

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
		ir_graph *prof_init_irg = be_profile_instrument(prof_filename, profile_default);
		initialize_birg(&birgs[num_birgs], prof_init_irg, &env);
		num_birgs++;
		set_method_img_section(get_irg_entity(prof_init_irg), section_constructors);
	} else {
		be_profile_read(prof_filename);
	}

	/* For all graphs */
	for (i = 0; i < num_birgs; ++i) {
		be_irg_t *birg = &birgs[i];
		ir_graph *irg  = birg->irg;
		optimization_state_t state;
		const arch_code_generator_if_t *cg_if;

		/* set the current graph (this is important for several firm functions) */
		current_ir_graph = irg;

#ifdef FIRM_STATISTICS
		stat_ev_ctx_push_fobj("irg", irg);
#endif

		/* stop and reset timers */
		BE_TIMER_ONLY(
			LC_STOP_AND_RESET_TIMER(t_abi);
			LC_STOP_AND_RESET_TIMER(t_codegen);
			LC_STOP_AND_RESET_TIMER(t_sched);
			LC_STOP_AND_RESET_TIMER(t_constr);
			LC_STOP_AND_RESET_TIMER(t_regalloc);
			LC_STOP_AND_RESET_TIMER(t_finish);
			LC_STOP_AND_RESET_TIMER(t_emit);
			LC_STOP_AND_RESET_TIMER(t_verify);
			LC_STOP_AND_RESET_TIMER(t_other);
		);
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

		BE_TIMER_ONLY(num_nodes_b = get_num_reachable_nodes(irg));

		/* Get the code generator interface. */
		cg_if = isa->impl->get_code_generator_if(isa);

		/* get a code generator for this graph. */
		birg->cg = cg_if->init(birg);

		/* some transformations need to be done before abi introduce */
		arch_code_generator_before_abi(birg->cg);

		/* reset the phi handler. */
		be_phi_handler_reset(env.phi_handler);

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

		be_do_stat_nodes(irg, "03 Prepare");

		dump(DUMP_PREPARED, irg, "-prepared", dump_ir_block_graph);
		BE_TIMER_ONLY(num_nodes_r = get_num_reachable_nodes(irg));

		if (be_options.vrfy_option == BE_VRFY_WARN) {
			be_check_dominance(irg);
			be_verify_out_edges(irg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(be_verify_out_edges(irg));
			assert(be_check_dominance(irg) && "Dominance verification failed");
		}

		/**
		 * Create execution frequencies from profile data or estimate some
		 */
		if (be_profile_has_data())
			birg->exec_freq = be_create_execfreqs_from_profile(irg);
		else
			birg->exec_freq = compute_execfreq(irg, 10);


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

		/* do some statistics */
		be_do_stat_reg_pressure(birg);

#ifdef FIRM_STATISTICS
		stat_ev_dbl("costs_before_ra", be_estimate_irg_costs(irg, env.arch_env, birg->exec_freq));
#endif

		/* Do register allocation */
		BE_TIMER_PUSH(t_regalloc);
		be_allocate_registers(birg);
		BE_TIMER_POP(t_regalloc);

#ifdef FIRM_STATISTICS
		stat_ev_dbl("costs_before_ra", be_estimate_irg_costs(irg, env.arch_env, birg->exec_freq));
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
			be_verify_register_allocation(env.arch_env, irg);
		} else if (be_options.vrfy_option == BE_VRFY_ASSERT) {
			assert(irg_verify(irg, VRFY_ENFORCE_SSA) && "irg verification failed");
			assert(be_verify_out_edges(irg) && "out edge verification failed");
			assert(be_check_dominance(irg) && "Dominance verification failed");
			assert(be_verify_schedule(birg) && "Schedule verification failed");
			assert(be_verify_register_allocation(env.arch_env, irg)
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

		BE_TIMER_ONLY(num_nodes_a = get_num_reachable_nodes(irg));
		BE_TIMER_POP(t_other);

#define LC_EMIT(timer)  \
		printf("%-20s: %.3lf msec\n", lc_timer_get_description(timer), (double)lc_timer_elapsed_usec(timer) / 1000.0); \
		stat_ev_dbl(lc_timer_get_name(timer), lc_timer_elapsed_msec(timer));

#define LC_EMIT_RA(timer) \
		printf("\t%-20s: %.3lf msec\n", lc_timer_get_description(timer), (double)lc_timer_elapsed_usec(timer) / 1000.0); \
		stat_ev_dbl(lc_timer_get_name(timer), lc_timer_elapsed_msec(timer)); \

		BE_TIMER_ONLY(
			printf("==>> IRG %s <<==\n", get_entity_name(get_irg_entity(irg)));
			printf("# nodes at begin:  %u\n", num_nodes_b);
			printf("# nodes before ra: %u\n", num_nodes_r);
			printf("# nodes at end:    %u\n\n", num_nodes_a);
			LC_EMIT(t_abi);
			LC_EMIT(t_codegen);
			LC_EMIT(t_sched);
			LC_EMIT(t_constr);
			LC_EMIT(t_regalloc);
			if(global_ra_timer != NULL) {
				LC_EMIT_RA(global_ra_timer->t_prolog);
				LC_EMIT_RA(global_ra_timer->t_live);
				LC_EMIT_RA(global_ra_timer->t_spill);
				LC_EMIT_RA(global_ra_timer->t_spillslots);
				LC_EMIT_RA(global_ra_timer->t_color);
				LC_EMIT_RA(global_ra_timer->t_ifg);
				LC_EMIT_RA(global_ra_timer->t_copymin);
				LC_EMIT_RA(global_ra_timer->t_ssa);
				LC_EMIT_RA(global_ra_timer->t_epilog);
				LC_EMIT_RA(global_ra_timer->t_verify);
				LC_EMIT_RA(global_ra_timer->t_other);
			}
			LC_EMIT(t_finish);
			LC_EMIT(t_emit);
			LC_EMIT(t_verify);
			LC_EMIT(t_other);
		);
#undef LC_EMIT_RA
#undef LC_EMIT

		be_free_birg(birg);

        /* switched off due to statistics (statistic module needs all irgs) */
#if 0   /* STA needs irgs */
#ifdef FIRM_STATISTICS
		if (! stat_is_active())
#endif /* FIRM_STATISTICS */
			free_ir_graph(irg);
#endif /* if 0 */
		stat_ev_ctx_pop();
	}
	be_profile_free();
	be_done_env(&env);

#undef BE_TIMER_POP
#undef BE_TIMER_PUSH
#undef BE_TIMER_ONLY
}

/* Main interface to the frontend. */
void be_main(FILE *file_handle, const char *cup_name)
{
	lc_timer_t *t = NULL;

	/* The user specified another config file to read. do that now. */
	if(strlen(config_file) > 0) {
		FILE *f;

		if((f = fopen(config_file, "rt")) != NULL) {
			lc_opt_from_file(config_file, f, NULL);
			fclose(f);
		}
	}

	if (be_options.timing == BE_TIME_ON) {
		t = lc_timer_register("bemain", "measure complete bemain loop");

		if (lc_timer_enter_high_priority()) {
			fprintf(stderr, "Warning: Could not enter high priority mode.\n");
		}

		lc_timer_reset_and_start(t);
	}

	if (be_options.statev) {
		const char *dot = strrchr(cup_name, '.');
		const char *pos = dot ? dot : cup_name + strlen(cup_name);
		char       *buf = alloca(pos - cup_name + 1);
		strncpy(buf, cup_name, pos - cup_name);

		stat_ev_begin(buf);
	}

	/* never build code for pseudo irgs */
	set_visit_pseudo_irgs(0);

 	be_node_init();

	be_main_loop(file_handle, cup_name);

	if (be_options.timing == BE_TIME_ON) {
		lc_timer_stop(t);
		lc_timer_leave_high_priority();
		stat_ev_dbl("backend_time", lc_timer_elapsed_msec(t));
		printf("%-20s: %lu msec\n", "BEMAINLOOP", lc_timer_elapsed_msec(t));
	}

	if (be_options.statev)
		stat_ev_end();
}

/** The debug info retriever function. */
static retrieve_dbg_func retrieve_dbg = NULL;

/* Sets a debug info retriever. */
void be_set_debug_retrieve(retrieve_dbg_func func) {
	retrieve_dbg = func;
}

/* Retrieve the debug info. */
const char *be_retrieve_dbg_info(const dbg_info *dbg, unsigned *line) {
	if (retrieve_dbg)
		return retrieve_dbg(dbg, line);

	*line = 0;
	return NULL;
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
