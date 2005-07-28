/**
 * Backend driver.
 * @author Sebastian Hack
 * @date 25.11.2004
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>

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

#include "be_t.h"
#include "bechordal_t.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "bechordal.h"
#include "bearch.h"
#include "becopyoptmain.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "bearch_firm.h"
#include "benode_t.h"
#include "beirgmod.h"

#include "beasm_dump_globals.h"
#include "beasm_asm_gnu.h"

#undef DUMP_ALLOCATED

#define N_PHASES 256


void be_init(void)
{
	be_sched_init();
	be_liveness_init();
	be_numbering_init();
	be_ra_chordal_init();
	be_copy_opt_init();
	copystat_init();
}

static be_main_env_t *be_init_env(be_main_env_t *env)
{
  const arch_isa_if_t *isa = &firm_isa;

  obstack_init(&env->obst);
  env->dbg = firm_dbg_register("be.main");

  env->arch_env = obstack_alloc(&env->obst, sizeof(env->arch_env[0]));
  arch_env_init(env->arch_env, isa);
  env->arch_env->isa->init();

  env->node_factory = obstack_alloc(&env->obst, sizeof(*env->node_factory));
  be_node_factory_init(env->node_factory, isa);

  arch_env_add_irn_handler(env->arch_env, &firm_irn_handler);
  arch_env_add_irn_handler(env->arch_env,
      be_node_get_irn_handler(env->node_factory));

  return env;
}

static be_main_session_env_t *
be_init_session_env(be_main_session_env_t *env,
    be_main_env_t *main_env, ir_graph *irg)
{
  env->main_env = main_env;
  env->irg = irg;

  return env;
}

static void prepare_graph(be_main_session_env_t *s)
{
	/*
	 * Duplicate consts in each block
	 * (This is just for the firm dummy backend)
	 */
	// localize_consts(s->irg);

	/* Remove critical edges */
	remove_critical_cf_edges(s->irg);

	/* Compute the dominance information. */
	free_dom_and_peace(s->irg);
	compute_doms(s->irg);

	/* Compute the dominance frontiers */
	s->dom_front = be_compute_dominance_frontiers(s->irg);

	/* Ensure, that the ir_edges are computed. */
	edges_assure(s->irg);

	/* Compute loop nesting information (for weighting copies) */
	if (get_irg_loopinfo_state(s->irg) != (loopinfo_valid & loopinfo_cf_consistent))
		construct_cf_backedges(s->irg);

	dump_dominator_information(true);
	dump_ir_block_graph(s->irg, "-prepared");
	dump_dominator_information(false);
}

static void be_main_loop(void)
{
	int i, n;
	be_main_env_t env;
	const arch_isa_if_t *isa;

	be_init_env(&env);
	isa = arch_env_get_isa(env.arch_env);

	/* For all graphs */
	for(i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		int j, m;
		ir_graph *irg = get_irp_irg(i);
		be_main_session_env_t session;

		DBG((env.dbg, LEVEL_1, "be irg: %F\n", irg));

		/* Init the session. */
		be_init_session_env(&session, &env, irg);

		/* Compute some analyses and prepare the graph for backend use. */
		prepare_graph(&session);

		/* Schedule the graphs. */
		list_sched(irg, trivial_selector);
		dump_ir_block_graph_sched(irg, "-sched");

		/* Verify the schedule */
		sched_verify_irg(irg);

		/* Build liveness information */
		be_liveness(irg);

		/* Liveness analysis */
		be_liveness(irg);

		copystat_reset();
		copystat_collect_irg(irg, env.arch_env);

		/*
		 * Verifying the schedule once again cannot hurt.
		 */
		sched_verify_irg(irg);

		/* Perform the following for each register class. */
		for(j = 0, m = isa->get_n_reg_class(); j < m; ++j) {
			be_chordal_env_t *chordal_env;
			const arch_register_class_t *cls = isa->get_reg_class(j);

			DBG((env.dbg, LEVEL_1, "\treg class: %s\n", cls->name));

			be_numbering(irg);
			be_liveness(irg);

			chordal_env = be_ra_chordal(irg, env.arch_env, cls);

#ifdef DUMP_ALLOCATED
			dump_allocated_irg(env.arch_env, irg, "");
#endif
			copystat_collect_cls(chordal_env);

			be_copy_opt(chordal_env);

			be_ssa_destruction(&session, chordal_env);
			be_ssa_destruction_check(&session, chordal_env);
			be_ra_chordal_check(chordal_env);

			be_ra_chordal_done(chordal_env);
			be_numbering_done(irg);
		}

		copystat_dump_pretty(irg);
	}
}

void be_main(int argc, const char *argv[])
{
	assembler_t *gnu_assembler;
	FILE *asm_output_file;

	be_main_loop();
	gnu_assembler = gnuasm_create_assembler();
	asm_output_file = fopen("asm_output.asm", "w");

	asm_dump_globals(gnu_assembler);
	gnuasm_dump(gnu_assembler, asm_output_file);
	gnuasm_delete_assembler(gnu_assembler);
	fclose(asm_output_file);
}
