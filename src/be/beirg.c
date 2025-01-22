/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend irg - a ir_graph with additional analysis information.
 * @author      Matthias Braun
 * @date        13.12.2006
 */
#include "beirg.h"

#include "belive.h"
#include "execfreq.h"

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
	be_liveness_free(birg->lv);
	birg->lv = NULL;

	obstack_free(&birg->obst, NULL);
	irg->be_data = NULL;
}
