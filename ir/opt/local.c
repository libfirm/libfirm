/*
 * Copyright (C) 2011 Karlsruhe Institute of Technology.  All right reserved.
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
 * @brief   Apply local opts until fixpoint
 * @author  Andreas Zwinkau
 */
#include "irgopt.h"
#include "opt_manage.h"

static ir_graph_state_t do_optimize_graph_df(ir_graph *irg)
{
	optimize_graph_df(irg);
	return 0;
}

static optdesc_t opt_local = {
	"localopts",
	0, // TODO optimize_graph_df handles preconditions itself
	do_optimize_graph_df,
};

void local_opts(ir_graph *irg)
{
	perform_irg_optimization(irg, &opt_local);
}
