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
 * @brief       Backend irg - a ir_graph with additional analysis information.
 * @author      Matthias Braun
 * @date        13.12.2006
 */
#include "config.h"

#include "execfreq.h"
#include "beirg.h"
#include "absgraph.h"
#include "belive.h"

void be_invalidate_live_sets(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	be_liveness_invalidate_sets(birg->lv);
}

void be_invalidate_live_chk(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	be_liveness_invalidate_chk(birg->lv);
}

void be_assure_live_sets(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	be_liveness_compute_sets(birg->lv);
}

void be_assure_live_chk(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	be_liveness_compute_chk(birg->lv);
}

void be_free_birg(ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);

	if (birg->lv != NULL) {
		be_liveness_free(birg->lv);
		birg->lv = NULL;
	}

	obstack_free(&birg->obst, NULL);
	irg->be_data = NULL;
}
