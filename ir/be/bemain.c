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

#include "beasm_dump_globals.h"
#include "beasm_asm_gnu.h"

#undef DUMP_ALLOCATED
#define DUMP_LOCALIZED

#define N_PHASES 256

typedef struct _be_graph_info_t {
	bitset_t *applied_phases;
} be_graph_info_t;

static size_t be_info_offset = 0;

#define get_irg_be_info(irg) get_irg_data(irg, be_graph_info_t, be_info_offset)

static int phase_ids = 1;
static struct obstack obst;

int phase_register(phase_t *phase)
{
	phase->id = phase_ids;
	return phase_ids++;
}

void phase_applied(const ir_graph *irg, const phase_t *phase)
{
	be_graph_info_t *info = get_irg_be_info(irg);

	if(!info->applied_phases)
		info->applied_phases = bitset_obstack_alloc(&obst, N_PHASES);

	bitset_set(info->applied_phases, phase->id);
}

int phase_depends_on(const ir_graph *irg, const phase_t *phase, int n, ...)
{
	int errors = 0;
	int i;
	va_list args;

	if(n > 0) {
		const be_graph_info_t *info = get_irg_be_info(irg);
		const bitset_t *applied_phases = info->applied_phases;

		va_start(args, n);

		for(i = 0; i < n; ++i) {
			const phase_t *dep_phase = va_arg(args, const phase_t *);

			if(!applied_phases || !bitset_is_set(applied_phases, dep_phase->id)) {
				errors++;
				fprintf(stderr, "phase dependency unfulfilled: \"%s\" depends on \"%s\"\n",
						phase->name, dep_phase->name);
			}
		}

		va_end(args);

		assert(errors > 0 && "There were phase dependency errors");
	}

	return errors;
}

void be_init(void)
{
	obstack_init(&obst);
	be_info_offset = register_additional_graph_data(sizeof(be_graph_info_t));

	be_sched_init();
	be_liveness_init();
	be_numbering_init();
	be_ra_init();
	be_ra_chordal_init();
#ifdef DO_STAT
	stat_init();
#endif
	be_copy_opt_init();
}

/* The preliminary Firm backend isa. */
extern arch_isa_if_t arch_isa_if_firm;

static void be_main_loop(void)
{
	int i, n;
	const arch_isa_if_t *isa = &arch_isa_if_firm;

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
		stat_collect_irg(irg);
#endif
		/* Perform the following for each register class. */
		for(j = 0, m = isa->get_n_reg_class(); j < m; ++j) {
			const arch_register_class_t *cls = isa->get_reg_class(j);

			be_ra_chordal(irg, isa, cls);

#ifdef DUMP_ALLOCATED
			dump_allocated_irg(irg, "");
#endif
			be_copy_opt(irg, isa, cls);
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
