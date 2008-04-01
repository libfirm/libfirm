
/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/grs/plan.h
 * Purpose:     provides an interface for search planers planing
 * 				the matching process with respect to information
 * 				provided by appropriate analyzers
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		16. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */



#include "matchplan_t.h"


/**dump a search plans to stdout */
void ext_grs_dump_match_plan(ext_grs_match_plan_t *plan) {
	int i, j;

	printf("\nA plan for action %s: \n", plan->action->name);

	for(j = 0; j < plan->num_progs; j++)
	{
		if(j == 0)
			printf("Positive plan:\n");
		else
			printf("NAC plan %d:\n", j - 1);

		for (i = 0; i < plan->length[j]; i++) {
			ext_grs_match_op_t *mop;
			mop = & plan->progs[j][i];
			switch(mop->kind)
			{
			case ext_grs_k_mop_node:
				printf("  node(%s)\n", mop->node->name);
				break;
			case ext_grs_k_mop_edge:
				printf("  edge(%s, %s)\n", mop->node->name, mop->edge->name);
				break;
			case ext_grs_k_mop_condition:
				printf("  condition\n");
				break;
			case ext_grs_k_mop_preset:
				printf("  preset_node(%s)\n", mop->node->name);
				break;
			case ext_grs_k_mop_preset_edge:
				printf("  preset_edge(%s, %s)\n", mop->node->name, mop->edge->name);
				break;
			default:
				printf("  invalid matcher op\n");
				break;
			}
		}
	}
}

/** initialize a planer */
void ext_grs_init_planer(ext_grs_planer_t *planer, ext_grs_analyzer_t *alz) {
	planer->init(planer, alz);
}

/** compute a match plan */
ext_grs_match_plan_t *ext_grs_compute_plan(
	ext_grs_planer_t *planer, ir_graph *irg, ext_grs_action_t *act)
{
	return planer->compute_plan(planer, irg, act);
}
