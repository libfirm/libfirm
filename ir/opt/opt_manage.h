#include "config.h"

#include <stdbool.h>

#include "irgraph_t.h"

typedef struct optdesc_t {
	/**
	 * The short name of the optimization
	 *
	 * Should not contain spaces, since it is used for the dumper filenames.
	 */
	const char * const name;

	/**
	 * required irg_state for this optimization
	 */
	ir_graph_state_t requirements;

	/**
	 * The optimization function itself
	 *
	 * @returns  zero by default; set some flags, if you guarantee some irg_state properties
	 **/
	ir_graph_state_t (*const optimization)(ir_graph *irg);
} optdesc_t;

void perform_irg_optimization(ir_graph *irg, optdesc_t *opt);
