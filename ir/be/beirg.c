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
 * @version     $Id$
 */
#include "config.h"

#include "execfreq.h"
#include "beirg.h"
#include "absgraph.h"
#include "belive.h"
#include "bedomfront.h"

be_lv_t *be_assure_liveness(be_irg_t *birg)
{
	if (birg->lv != NULL)
		return birg->lv;

	return birg->lv = be_liveness(birg->irg);
}

void be_assure_dom_front(be_irg_t *birg)
{
	if (birg->dom_front != NULL)
		return;

	birg->dom_front = be_compute_dominance_frontiers(birg->irg);
}

void be_invalidate_dom_front(be_irg_t *birg)
{
	if (birg->dom_front == NULL)
		return;

	be_free_dominance_frontiers(birg->dom_front);
	birg->dom_front = NULL;
}

void be_free_birg(be_irg_t *birg)
{
	free_execfreq(birg->exec_freq);
	birg->exec_freq = NULL;

	if (birg->dom_front != NULL) {
		be_free_dominance_frontiers(birg->dom_front);
		birg->dom_front = NULL;
	}
	if (birg->lv != NULL) {
		be_liveness_free(birg->lv);
		birg->lv = NULL;
	}
}
