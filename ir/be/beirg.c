/*
 * Author:      Matthias Braun
 * Date:		13.12.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "execfreq.h"
#include "beirg_t.h"

void be_assure_liveness(be_irg_t *birg)
{
	if(birg->lv != NULL)
		return;

	birg->lv = be_liveness(birg->irg);
}

void be_invalidate_liveness(be_irg_t *birg)
{
	if(birg->lv == NULL)
		return;

	be_liveness_free(birg->lv);
	birg->lv = NULL;
}

void be_assure_dom_front(be_irg_t *birg)
{
	if(birg->dom_front != NULL)
		return;

	birg->dom_front = be_compute_dominance_frontiers(birg->irg);
}

void be_invalidate_dom_front(be_irg_t *birg)
{
	if(birg->dom_front == NULL)
		return;

	be_free_dominance_frontiers(birg->dom_front);
	birg->dom_front = NULL;
}

void be_free_birg(be_irg_t *birg)
{
	free_execfreq(birg->exec_freq);
	birg->exec_freq = NULL;

	if(birg->dom_front != NULL) {
		be_free_dominance_frontiers(birg->dom_front);
		birg->dom_front = NULL;
	}
	if(birg->lv != NULL) {
		be_liveness_free(birg->lv);
		birg->lv = NULL;
	}
}
