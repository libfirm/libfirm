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

#include "be_t.h"
#include "bera_t.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "phiclass_t.h"
#include "bechordal.h"
#include "bechordal.h"
#include "bephiopt.h"
#include "phistat.h"

#define DUMP_LOCAL 1

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
	be_phi_opt_init();
}

extern void be_ra_chordal(ir_graph *irg);

static void be_main_loop(void)
{
	int i, n;

	for(i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);

		localize_consts(irg);
		if (DUMP_LOCAL) {
			dump_consts_local(0);
			dump_ir_block_graph(irg, "-local");
		}

		be_numbering(irg);
		list_sched(irg, trivial_selector, NULL);
		be_liveness(irg);
		be_ra_chordal(irg);
		//be_phi_opt(irg);
		//be_phi_destruction(irg);

		be_ra_chordal_done(irg);
		be_numbering_done(irg);
	}
}

void be_main(int argc, const char *argv[])
{
	be_main_loop();
}
