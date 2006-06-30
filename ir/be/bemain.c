/**
 * Backend driver.
 * @author Sebastian Hack
 * @date   25.11.2004
 * @cvsid  $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>
#endif /* WITH_LIBCORE */

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
#include "return.h"

#include "bearch.h"
#include "firm/bearch_firm.h"
#include "ia32/bearch_ia32.h"
#include "arm/bearch_arm.h"
#include "ppc32/bearch_ppc32.h"
#include "mips/bearch_mips.h"

#include "be_t.h"
#include "benumb_t.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bespillilp.h"
#include "bespillbelady.h"
#include "bera.h"
#include "beraextern.h"
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

/* options visible for anyone */
static be_options_t be_options = {
	DUMP_NONE,                         /* dump options */
	BE_TIME_OFF,                       /* no timing */
	"i44pc52.info.uni-karlsruhe.de",   /* ilp server */
	"cplex"                            /* ilp solver */
};

/* dump flags */
static unsigned dump_flags = 0;

/* verify options */
static unsigned vrfy_option = BE_VRFY_WARN;

/* register allocator to use. */
static const be_ra_t *ra = &be_ra_chordal_allocator;

/* back end instruction set architecture to use */
static const arch_isa_if_t *isa_if = &ia32_isa_if;

/* mris option */
static int be_enable_mris = 0;

#ifdef WITH_LIBCORE

static lc_opt_entry_t *be_grp_root = NULL;

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

/* register allocators */
static const lc_opt_enum_const_ptr_items_t ra_items[] = {
	{ "chordal",  &be_ra_chordal_allocator },
	{ "external", &be_ra_external_allocator },
	{ NULL,      NULL }
};

/* instruction set architectures. */
static const lc_opt_enum_const_ptr_items_t isa_items[] = {
	{ "ia32",    &ia32_isa_if },
#if 0
	{ "arm",     &arm_isa_if },
	{ "ppc32",   &ppc32_isa_if },
	{ "mips",    &mips_isa_if },
#endif
	{ NULL,      NULL }
};

/* verify options. */
static const lc_opt_enum_int_items_t vrfy_items[] = {
	{ "off",    BE_VRFY_OFF    },
	{ "warn",   BE_VRFY_WARN   },
	{ "assert", BE_VRFY_ASSERT },
	{ NULL,     0 }
};

static lc_opt_enum_mask_var_t dump_var = {
	&dump_flags, dump_items
};

static lc_opt_enum_const_ptr_var_t ra_var = {
	(const void **) &ra, ra_items
};

static lc_opt_enum_const_ptr_var_t isa_var = {
	(const void **) &isa_if, isa_items
};

static lc_opt_enum_int_var_t vrfy_var = {
	&vrfy_option, vrfy_items
};

static const lc_opt_table_entry_t be_main_options[] = {
	LC_OPT_ENT_ENUM_MASK("dump",     "dump irg on several occasions",     &dump_var),
	LC_OPT_ENT_ENUM_PTR ("ra",       "register allocator",                &ra_var),
	LC_OPT_ENT_ENUM_PTR ("isa",      "the instruction set architecture",  &isa_var),
	LC_OPT_ENT_NEGBOOL  ("noomitfp", "do not omit frame pointer",         &be_omit_fp),
	LC_OPT_ENT_BOOL     ("mris",     "enable mris schedule preparation",  &be_enable_mris),
	LC_OPT_ENT_ENUM_PTR ("vrfy",     "verify the backend irg (off, warn, assert)",  &vrfy_var),
	LC_OPT_ENT_BOOL     ("time",     "get backend timing statistics",     &be_options.timing),

#ifdef WITH_ILP
	LC_OPT_ENT_STR ("ilp.server", "the ilp server name", be_options.ilp_server, sizeof(be_options.ilp_server)),
	LC_OPT_ENT_STR ("ilp.solver", "the ilp solver name", be_options.ilp_solver, sizeof(be_options.ilp_solver)),
#endif /* WITH_ILP */
	{ NULL }
};

#endif /* WITH_LIBCORE */

void be_opt_register(void)
{
#ifdef WITH_LIBCORE
	int i;
	lc_opt_entry_t *be_grp_ra;
	static int run_once = 0;

	if (! run_once) {
		run_once    = 1;
		be_grp_root = lc_opt_get_grp(firm_opt_get_root(), "be");
		be_grp_ra   = lc_opt_get_grp(be_grp_root, "ra");

		lc_opt_add_table(be_grp_root, be_main_options);

		/* register allocator options */
		for(i = 0; ra_items[i].name != NULL; ++i) {
			const be_ra_t *ra = ra_items[i].value;
			ra->register_options(be_grp_ra);
		}

		/* register isa options */
		for(i = 0; isa_items[i].name != NULL; ++i) {
			const arch_isa_if_t *isa = isa_items[i].value;
			isa->register_options(be_grp_root);
		}
	}
#endif /* WITH_LIBCORE */
}

/* Parse one argument. */
int be_parse_arg(const char *arg) {
#ifdef WITH_LIBCORE
	if (strcmp(arg, "help") == 0 || (arg[0] == '?' && arg[1] == '\0')) {
		lc_opt_print_help(be_grp_root, stdout);
		return -1;
	}
	return lc_opt_from_single_arg(be_grp_root, NULL, arg, NULL);
#else
	return 0;
#endif /* WITH_LIBCORE */
}

/** The be parameters returned by default, all off. */
const static backend_params be_params = {
	NULL,
	NULL,
	0,
	NULL,
};

/* Perform schedule verification if requested. */
static void be_sched_vrfy(ir_graph *irg, int vrfy_opt) {
	if (vrfy_opt == BE_VRFY_WARN) {
		be_verify_schedule(irg);
	}
	else if (vrfy_opt == BE_VRFY_ASSERT) {
		assert(be_verify_schedule(irg) && "Schedule verification failed.");
	}
}

/* Initialize the Firm backend. Must be run BEFORE init_firm()! */
const backend_params *be_init(void)
{
	be_opt_register();

	be_sched_init();
	be_liveness_init();
	be_numbering_init();
	be_copy_opt_init();
	copystat_init();
	phi_class_init();

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
	env->options->dump_flags = dump_flags;
	FIRM_DBG_REGISTER(env->dbg, "be.main");

	arch_env_init(env->arch_env, isa_if, file_handle);

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

	return env;
}

static void be_done_env(be_main_env_t *env)
{
	env->arch_env->isa->impl->done(env->arch_env->isa);
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
	if(dump_flags & mask)
		be_dump(irg, suffix, dumper);
}

/**
 * Prepare a backend graph for code generation.
 */
static void prepare_graph(be_irg_t *birg)
{
	ir_graph *irg = birg->irg;

	/* Normalize proj nodes. */
	normalize_proj_nodes(irg);

	/* Make just one return node. */
	normalize_one_return(irg);

	/* Remove critical edges */
	remove_critical_cf_edges(irg);

	/* Compute the dominance information. */
	free_dom(irg);
	compute_doms(irg);

	/* Ensure, that the ir_edges are computed. */
	edges_activate(irg);

	/* check, if the dominance property is fulfilled. */
	be_check_dominance(irg);

	/* reset the phi handler. */
	be_phi_handler_reset(birg->main_env->phi_handler);
}

/**
 * The Firm backend main loop.
 * Do architecture specific lowering for all graphs
 * and call the architecture specific code generator.
 *
 * @param file_handle   the file handle the output will be written to
 */
static void be_main_loop(FILE *file_handle)
{
	int i, n;
	arch_isa_t *isa;
	be_main_env_t env;
	unsigned num_nodes_b = 0;
	unsigned num_nodes_a = 0;
	unsigned num_nodes_r = 0;
	unsigned ra_prolog   = 0;
	unsigned ra_epilog   = 0;
	unsigned ra_live     = 0;
	unsigned ra_spill    = 0;
	unsigned ra_color    = 0;
	unsigned ra_ifg      = 0;
	unsigned ra_copymin  = 0;
	unsigned ra_ssa      = 0;
	lc_timer_t *t_prolog, *t_abi, *t_codegen, *t_sched, *t_constr, *t_regalloc, *t_finish, *t_emit;
	be_ra_timer_t *ra_timer;

	if (be_options.timing == BE_TIME_ON) {
		t_prolog   = lc_timer_register("prolog",   "prolog");
		t_abi      = lc_timer_register("beabi",    "be abi introduction");
		t_codegen  = lc_timer_register("codegen",  "codegeneration");
		t_sched    = lc_timer_register("sched",    "scheduling");
		t_constr   = lc_timer_register("constr",   "assure constraints");
		t_regalloc = lc_timer_register("regalloc", "register allocation");
		t_finish   = lc_timer_register("finish",   "graph finish");
		t_emit     = lc_timer_register("emiter",   "code emiter");
	}

	be_init_env(&env, file_handle);

	isa = arch_env_get_isa(env.arch_env);

	/* for debugging, anchors helps */
	// dump_all_anchors(1);

#define BE_TIME_START(timer) if (be_options.timing == BE_TIME_ON) lc_timer_start(timer);
#define BE_TIME_STOP(timer)  if (be_options.timing == BE_TIME_ON) lc_timer_stop(timer);
#define BE_TIME_ONLY(code)   if (be_options.timing == BE_TIME_ON) { code; }

	/* For all graphs */
	for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);
		const arch_code_generator_if_t *cg_if;
		be_irg_t birg;
		optimization_state_t state;

		/* do now create a body for graphs that are forced to have no one */
		if (get_irg_inline_property(irg) == irg_inline_forced_no_body)
			continue;

		/* stop and reset timers */
		if (be_options.timing == BE_TIME_ON) {
			LC_STOP_AND_RESET_TIMER(t_prolog);
			LC_STOP_AND_RESET_TIMER(t_abi);
			LC_STOP_AND_RESET_TIMER(t_codegen);
			LC_STOP_AND_RESET_TIMER(t_sched);
			LC_STOP_AND_RESET_TIMER(t_constr);
			LC_STOP_AND_RESET_TIMER(t_regalloc);
			LC_STOP_AND_RESET_TIMER(t_finish);
			LC_STOP_AND_RESET_TIMER(t_emit);
		}

		BE_TIME_ONLY(num_nodes_b = get_num_reachable_nodes(irg));

		birg.irg      = irg;
		birg.main_env = &env;

		BE_TIME_START(t_prolog);

		DBG((env.dbg, LEVEL_2, "====> IRG: %F\n", irg));
		dump(DUMP_INITIAL, irg, "-begin", dump_ir_block_graph);

		be_stat_init_irg(env.arch_env, irg);
		be_do_stat_nodes(irg, "01 Begin");

		/* set the current graph (this is important for several firm functions) */
		current_ir_graph = birg.irg;

		/* Get the code generator interface. */
		cg_if = isa->impl->get_code_generator_if(isa);

		/* get a code generator for this graph. */
		birg.cg = cg_if->init(&birg);

		/* create the code generator and generate code. */
		prepare_graph(&birg);

		/* some transformations need to be done before abi introduce */
		arch_code_generator_before_abi(birg.cg);

		BE_TIME_STOP(t_prolog);
		BE_TIME_START(t_abi);

		/* implement the ABI conventions. */
		birg.abi = be_abi_introduce(&birg);
		dump(DUMP_ABI, irg, "-abi", dump_ir_block_graph);

		be_do_stat_nodes(irg, "02 Abi");

		BE_TIME_STOP(t_abi);
		BE_TIME_START(t_codegen);

		/* generate code */
		arch_code_generator_prepare_graph(birg.cg);

		be_do_stat_nodes(irg, "03 Prepare");

		/*
		 * Since the code generator made a lot of new nodes and skipped
		 * a lot of old ones, we should do dead node elimination here.
		 * Note that this requires disabling the edges here.
		 */
		edges_deactivate(irg);
		//dead_node_elimination(irg);
		edges_activate(irg);

		/* Compute loop nesting information (for weighting copies) */
		construct_cf_backedges(irg);

		dump(DUMP_PREPARED, irg, "-prepared", dump_ir_block_graph);

		BE_TIME_STOP(t_codegen);
		BE_TIME_ONLY(num_nodes_r = get_num_reachable_nodes(irg));
		BE_TIME_START(t_sched);

		/* Schedule the graphs. */
		arch_code_generator_before_sched(birg.cg);
		list_sched(&birg, be_enable_mris);
		dump(DUMP_SCHED, irg, "-sched", dump_ir_block_graph_sched);

		/* check schedule */
		be_sched_vrfy(birg.irg, vrfy_option);

		be_do_stat_nodes(irg, "04 Schedule");

		BE_TIME_STOP(t_sched);
		BE_TIME_START(t_constr);

		/* we switch off optimizations here, because they might cause trouble */
		save_optimization_state(&state);
		set_optimize(0);
		set_opt_normalize(0);

		/* add Keeps for should_be_different constrained nodes  */
		/* beware: needs schedule due to usage of be_ssa_constr */
		assure_constraints(&birg);
		dump(DUMP_SCHED, irg, "-assured", dump_ir_block_graph_sched);

		be_do_stat_nodes(irg, "05 Constraints");

		/* connect all stack modifying nodes together (see beabi.c) */
		be_abi_fix_stack_nodes(birg.abi);
		dump(DUMP_SCHED, irg, "-fix_stack", dump_ir_block_graph_sched);

		/* check schedule */
		be_sched_vrfy(birg.irg, vrfy_option);

		/* do some statistics */
		be_do_stat_reg_pressure(&birg);

		/* stuff needs to bo done after scheduling but before register allocation */
		arch_code_generator_before_ra(birg.cg);

		BE_TIME_STOP(t_constr);
		BE_TIME_START(t_regalloc);

		/* Do register allocation */
		ra_timer = ra->allocate(&birg);
		dump(DUMP_RA, irg, "-ra", dump_ir_block_graph_sched);

		if (be_options.timing == BE_TIME_ON && ra_timer) {
			ra_prolog  = lc_timer_elapsed_msec(ra_timer->t_prolog);
			ra_epilog  = lc_timer_elapsed_msec(ra_timer->t_epilog);
			ra_live    = lc_timer_elapsed_msec(ra_timer->t_live);
			ra_spill   = lc_timer_elapsed_msec(ra_timer->t_spill);
			ra_color   = lc_timer_elapsed_msec(ra_timer->t_color);
			ra_copymin = lc_timer_elapsed_msec(ra_timer->t_copymin);
			ra_ssa     = lc_timer_elapsed_msec(ra_timer->t_ssa);
			ra_ifg     = lc_timer_elapsed_msec(ra_timer->t_ifg);
		}

		be_do_stat_nodes(irg, "06 Register Allocation");

		BE_TIME_STOP(t_regalloc);
		BE_TIME_START(t_finish);

		arch_code_generator_after_ra(birg.cg);
		be_abi_fix_stack_bias(birg.abi);

		/* check schedule */
		be_sched_vrfy(birg.irg, vrfy_option);

		BE_TIME_STOP(t_finish);
		BE_TIME_START(t_emit);

		arch_code_generator_done(birg.cg);
		dump(DUMP_FINAL, irg, "-end", dump_ir_extblock_graph_sched);
		be_abi_free(birg.abi);

		be_do_stat_nodes(irg, "07 Final");

		BE_TIME_STOP(t_emit);

		restore_optimization_state(&state);

		BE_TIME_ONLY(num_nodes_a = get_num_reachable_nodes(irg));

		/* switched off due to statistics (statistic module needs all irgs) */
		//		free_ir_graph(irg);
#define LC_EMIT(timer)     printf("%10s: %u msec\n", lc_timer_get_name(timer), lc_timer_elapsed_msec(timer))
#define EMIT_RA_TIME(n, t) printf("%20s: %u msec\n", n, t)
		if (be_options.timing == BE_TIME_ON) {
			printf("==>> IRG %s <<==\n", get_entity_name(get_irg_entity(irg)));
			printf("# nodes at begin:  %u\n", num_nodes_b);
			printf("# nodes before ra: %u\n", num_nodes_r);
			printf("# nodes at end:    %u\n\n", num_nodes_a);
			LC_EMIT(t_prolog);
			LC_EMIT(t_abi);
			LC_EMIT(t_codegen);
			LC_EMIT(t_sched);
			LC_EMIT(t_constr);
			LC_EMIT(t_regalloc);
			EMIT_RA_TIME("prolog",    ra_prolog);
			EMIT_RA_TIME("liveness",  ra_live);
			EMIT_RA_TIME("spilling",  ra_spill);
			EMIT_RA_TIME("coloring",  ra_color);
			EMIT_RA_TIME("ifg build", ra_ifg);
			EMIT_RA_TIME("copymin",   ra_copymin);
			EMIT_RA_TIME("ssa destr", ra_ssa);
			EMIT_RA_TIME("epilog",    ra_epilog);
			LC_EMIT(t_finish);
			LC_EMIT(t_emit);
		}
#undef LC_EMIT
	}
	be_done_env(&env);

#undef BE_TIME_START
#undef BE_TIME_STOP
#undef BE_TIME_ONLY
}

/* Main interface to the frontend. */
void be_main(FILE *file_handle)
{
#ifdef WITH_LIBCORE
	lc_timer_t *t;

	if (be_options.timing == BE_TIME_ON) {
		t = lc_timer_register("bemain", "measure complete bemain loop");

		if (lc_timer_enter_high_priority()) {
			fprintf(stderr, "Warning: Could not enter high priority mode.\n");
		}

		lc_timer_reset_and_start(t);
	}
#endif /* WITH_LIBCORE */

	/* never build code for pseudo irgs */
	set_visit_pseudo_irgs(0);

	be_node_init();
	be_main_loop(file_handle);

#ifdef WITH_LIBCORE
	if (be_options.timing == BE_TIME_ON) {
		lc_timer_stop(t);
		lc_timer_leave_high_priority();
		printf("BEMAINLOOP: %lu msec\n", lc_timer_elapsed_msec(t));
	}
#endif /* WITH_LIBCORE */
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
