/**
 * Backend driver.
 * @author Sebastian Hack
 * @date 25.11.2004
 */

#include "besched.h"
#include "belistsched.h"
#include "belive_t.h"
#include "belive.h"

void be_init(void)
{
	be_sched_init();
	be_liveness_init();
}

static void be_main_loop(void)
{
	int i, n;

	for(i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);

		list_sched(irg, trivial_selector, NULL);
		be_liveness(irg);

		be_sched_dump(stdout, irg);
		be_liveness_dump(stdout, irg);
	}
}

void be_main(int argc, const char *argv[])
{
	be_main_loop();
}
