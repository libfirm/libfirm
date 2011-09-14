#include "config.h"

#include <assert.h>
#include <stdbool.h>

#include "irgraph_t.h"
#include "irprog_t.h"

#include "iroptimize.h"
#include "irgopt.h"
#include "irdom.h"
#include "iredges.h"
#include "irouts.h"
#include "irverify.h"
#include "irdump.h"
#include "opt_manage.h"

static void deactivate_entity_usage(ir_graph *irg)
{
	set_irg_entity_usage_state(irg, ir_entity_usage_not_computed);
}

static void nop(ir_graph *irg) {
	(void)irg;
}

void perform_irg_optimization(ir_graph *irg, optdesc_t *opt)
{
	ir_graph_state_t new_irg_state;
	ir_graph_state_t required = opt->requirements;
	const bool dump = get_irp_optimization_dumps();

	/* no bad block requires no unreachable code */
	if (required & IR_GRAPH_STATE_NO_BAD_BLOCKS)
		required |= IR_GRAPH_STATE_NO_UNREACHABLE_BLOCKS;

	/** Some workarounds because information is currently duplicated */
	// FIXME should not be necessary!
	if (loopinfo_inconsistent == get_irg_loopinfo_state(irg))
		clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_LOOPINFO);
	if (ir_entity_usage_not_computed == get_irg_entity_usage_state(irg))
		clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_ENTITY_USAGE);

	/* assure that all requirements for the optimization are fulfilled */
#define PREPARE(st,func) if (st & (required ^ irg->state)) {func(irg); set_irg_state(irg,st);}
	PREPARE(IR_GRAPH_STATE_ONE_RETURN,               normalize_one_return)
	PREPARE(IR_GRAPH_STATE_NO_CRITICAL_EDGES,        remove_critical_cf_edges)
	PREPARE(IR_GRAPH_STATE_NO_UNREACHABLE_BLOCKS,    remove_unreachable_blocks)
	PREPARE(IR_GRAPH_STATE_NO_BAD_BLOCKS,            remove_bads)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_DOMINANCE,     assure_doms)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_POSTDOMINANCE, assure_postdoms)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_OUT_EDGES,     edges_assure)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_OUTS,          assure_irg_outs)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_LOOPINFO,      assure_cf_loop)
	PREPARE(IR_GRAPH_STATE_CONSISTENT_ENTITY_USAGE,  assure_irg_entity_usage_computed)
	PREPARE(IR_GRAPH_STATE_VALID_EXTENDED_BLOCKS,    compute_extbb)

	/* now all the requirements for the optimization are fulfilled */
	if (dump)
		dump_ir_graph(irg, opt->name);

	new_irg_state = opt->optimization(irg);

	if (dump)
		dump_ir_graph(irg, opt->name);

	/* unless the optimization returned that some state is retained,
	 * we disable the corresponding irg state.
	 * Since we currently duplicate information, sometimes another func must be called too.
	 */
#define INVALIDATE(state,func) if (!(state & new_irg_state)) {clear_irg_state(irg,state); func(irg);}
	INVALIDATE(IR_GRAPH_STATE_NO_CRITICAL_EDGES,        nop)
	INVALIDATE(IR_GRAPH_STATE_NO_UNREACHABLE_BLOCKS,    nop)
	INVALIDATE(IR_GRAPH_STATE_NO_BAD_BLOCKS,            nop)
	INVALIDATE(IR_GRAPH_STATE_ONE_RETURN,               nop)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_DOMINANCE,     nop)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_POSTDOMINANCE, nop)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_OUTS,          nop)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_OUT_EDGES,     edges_deactivate)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_LOOPINFO,      set_irg_loopinfo_inconsistent)
	INVALIDATE(IR_GRAPH_STATE_CONSISTENT_ENTITY_USAGE,  deactivate_entity_usage)
	INVALIDATE(IR_GRAPH_STATE_VALID_EXTENDED_BLOCKS,    set_irg_extblk_inconsistent)

	if (!(new_irg_state & IR_GRAPH_STATE_BROKEN_FOR_VERIFIER)) {
		irg_verify(irg, VERIFY_ENFORCE_SSA);
	}

}
