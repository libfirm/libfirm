/**
 * Backend driver.
 * @author Sebastian Hack
 * @date 25.11.2004
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <mcheck.h>

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
#include "bespillilp.h"
#include "bespillbelady.h"

#include "beasm_dump_globals.h"
#include "beasm_asm_gnu.h"

#undef DUMP_BEGIN
#undef DUMP_PREPARED
#define DUMP_SCHED
#define DUMP_SPILL
#undef DUMP_ALLOCATED
#undef DUMP_COPYMIN
#undef DUMP_SSADESTR
#undef DUMP_END

#define N_PHASES 256

void be_init(void)
{
	be_sched_init();
	be_liveness_init();
	be_numbering_init();
	be_ra_chordal_init();
	be_copy_opt_init();
	copystat_init();
	phi_class_init();
}

static be_main_env_t *be_init_env(be_main_env_t *env)
{
  const arch_isa_if_t *isa = &firm_isa;

  obstack_init(&env->obst);
  env->dbg = firm_dbg_register("be.main");
  firm_dbg_set_mask(env->dbg, SET_LEVEL_1);

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
	/* set the current graph (this is important for several firm functions) */
	current_ir_graph = s->irg;

	/* Let the isa prepare the graph. */
	s->main_env->arch_env->isa->prepare_graph(s->irg);

	/* Normalize proj nodes. */
	normalize_proj_nodes(s->irg);

	/* Remove critical edges */
	remove_critical_cf_edges(s->irg);

	/* Compute the dominance information. */
	free_dom_and_peace(s->irg);
	compute_doms(s->irg);

	/* Compute the dominance frontiers */
	s->dom_front = be_compute_dominance_frontiers(s->irg);

	/* Ensure, that the ir_edges are computed. */
	edges_activate(s->irg);

	/* Compute loop nesting information (for weighting copies) */
	if (get_irg_loopinfo_state(s->irg) != (loopinfo_valid & loopinfo_cf_consistent))
		construct_cf_backedges(s->irg);

	be_check_dominance(s->irg);
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

		DBG((env.dbg, LEVEL_1, "====> IRG: %F\n", irg));
#ifdef DUMP_BEGIN
		dump_ir_block_graph(irg, "-begin");
#endif

		/* Init the session. */
		be_init_session_env(&session, &env, irg);

		/* Compute some analyses and prepare the graph for backend use. */
		prepare_graph(&session);
#ifdef DUMP_PREPARED
		dump_dominator_information(true);
		dump_ir_block_graph(irg, "-prepared");
		dump_dominator_information(false);
#endif

		/* Schedule the graphs. */
		list_sched(irg, trivial_selector);
		be_sched_imm(irg);

#ifdef DUMP_SCHED
		dump_ir_block_graph_sched(irg, "-sched");
#endif

		/* Verify the schedule */
		sched_verify_irg(irg);

		/* Build liveness information */
		be_liveness(irg);

		/* Perform the following for each register class. */
		for(j = 0, m = isa->get_n_reg_class(); j < m; ++j) {
			be_chordal_env_t *chordal_env;
			const arch_register_class_t *cls = isa->get_reg_class(j);
			DBG((env.dbg, LEVEL_1, "----> Reg class: %s\n", cls->name));

			/* spilling */
			//be_spill_ilp(&session, cls);
			be_spill_belady(&session, cls);
#ifdef DUMP_SPILL
			dump_ir_block_graph_sched(session.irg, "-spill");
#endif
			be_liveness(irg);
			be_numbering(irg);
			be_check_pressure(&session, cls);

#if 0
			{
				FILE *f;
				char buf[128];
				ir_snprintf(buf, sizeof(buf), "%F_%s-live.txt", irg, cls->name);
				if((f = fopen(buf, "wt")) != NULL) {
					be_liveness_dump(session.irg, f);
					fclose(f);
				}
			}
#endif

			/* allocation */
			chordal_env = be_ra_chordal(&session, cls);
#ifdef DUMP_ALLOCATED
			dump_allocated_irg(env.arch_env, irg, "");
#endif

			/* copy minimization */
			copystat_collect_cls(chordal_env);
			be_copy_opt(chordal_env);
#ifdef DUMP_COPYMIN
			dump_allocated_irg(env.arch_env, irg, "-copymin");
#endif

			/* ssa destruction */
			be_ssa_destruction(chordal_env);
			be_ssa_destruction_check(chordal_env);
			be_ra_chordal_check(chordal_env);
#ifdef DUMP_SSADESTR
			dump_allocated_irg(env.arch_env, irg, "-ssadestr");
#endif

			be_ra_chordal_done(chordal_env);
			be_numbering_done(irg);
		}
#ifdef DUMP_END
		dump_ir_block_graph_sched(session.irg, "-end");
#endif
		copystat_dump(irg);
	}
}

void be_main(int argc, const char *argv[])
{
	assembler_t *gnu_assembler;
	FILE *asm_output_file;

	be_main_loop();

#if 0
	gnu_assembler = gnuasm_create_assembler();
	asm_output_file = fopen("asm_output.asm", "w");

	asm_dump_globals(gnu_assembler);
	gnuasm_dump(gnu_assembler, asm_output_file);
	gnuasm_delete_assembler(gnu_assembler);
	fclose(asm_output_file);
#endif
}
