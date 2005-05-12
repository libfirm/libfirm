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
#include "irgraph.h"
#include "irdump.h"
#include "phiclass.h"

#include "be_t.h"
#include "bera_t.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "bechordal.h"
#include "bearch.h"
#include "becopyoptmain.h"
#include "becopystat.h"
#include "bearch_firm.h"

#include "beasm_dump_globals.h"
#include "beasm_asm_gnu.h"

#undef DUMP_ALLOCATED
#undef DUMP_LOCALIZED

#define N_PHASES 256


void be_init(void)
{
	be_sched_init();
	be_liveness_init();
	be_numbering_init();
	be_ra_init();
	be_ra_chordal_init();
	be_copy_opt_init();
#ifdef DO_STAT
	stat_init();
#endif
}

static void be_init_arch_env(arch_env_t *env)
{
  arch_env_init(env, &firm_isa);
  arch_env_add_irn_handler(env, &firm_irn_handler);

  env->isa->init();
}

static void be_main_loop(void)
{
	int i, n;
  arch_env_t env;
  const arch_isa_if_t *isa;

  be_init_arch_env(&env);

  isa = arch_env_get_isa(&env);

	for(i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		int j, m;
		ir_graph *irg = get_irp_irg(i);

		localize_consts(irg);
#ifdef DUMP_LOCALIZED
		dump_consts_local(0);
		dump_ir_block_graph(irg, "-local-const");
#endif
		be_numbering(irg);

		/* Schedule the graphs. */
		list_sched(irg, trivial_selector);

		/* Liveness analysis */
		be_liveness(irg);

#ifdef DO_STAT
		stat_reset();
#endif
		/* Perform the following for each register class. */
		for(j = 0, m = isa->get_n_reg_class(); j < m; ++j) {
			const arch_register_class_t *cls = isa->get_reg_class(j);

			be_ra_chordal(irg, &env, cls);

#ifdef DUMP_ALLOCATED
			dump_allocated_irg(&env, irg, "");
#endif
#ifdef DO_STAT
			stat_collect_irg(irg);
#endif
			be_copy_opt(irg, &env, cls);
			be_ra_chordal_done(irg);
		}
#ifdef DO_STAT
		stat_dump(irg);
#endif
	    be_numbering_done(irg);
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
