/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Convert VFirm graphs to firm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "firm_types.h"
#include "iroptimize.h"
#include "irflag.h"
#include "irdump.h"
#include "irnode.h"
#include "irgraph.h"
#include "iredges.h"
#include "vf_dstr_build.h"

//#define VA_DEBUG_DSTR 1

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

void vf_destruct(ir_graph *irg)
{
	ir_node *ins[0];

	/* Turn on edges. Note that don't turning them on here leads to errors.
	 * The reason for this is not entirely clear yet. However it might well
	 * be better performance-wise to just keep them on, so that's what we do
	 * in here. */
	int had_edges = edges_assure(irg);

	/* 50-16 = 34 */
#if VA_DEBUG_DSTR
	printf("+================================================+\n");
	printf("| Destructing %-34s |\n", get_entity_name(get_irg_entity(irg)));
	printf("+================================================+\n");
#endif

	/* Turn off optimizations, enable edges. */
	optimization_state_t opt;
	save_optimization_state(&opt);
	all_optimizations_off();

	/* Remove all keepalives. We shouldn't need them and they add confusion. */
	set_End_keepalives(get_irg_end(irg), 0, ins);

	/* Dump the old graph. */
	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "vfirm");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

	/* Create the Firm graph. */
	vb_build(irg);
	dump_ir_graph(irg, "firm");

	/* Cleanup. */
	restore_optimization_state(&opt);

	if (!had_edges) edges_deactivate(irg);

	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);
}
