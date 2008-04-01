#ifndef _EXT_GRS_MATCHPLAN_H_
#define _EXT_GRS_MATCHPLAN_H_

/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/grs/plan.h
 * Purpose:     provides an interface for search planers planing
 * 				the matching process with respect to information
 * 				provided by appropriate analyzers
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		6. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#include "action.h"
#include "analyze.h"



/** a plan the matching process will follow */
typedef struct _ext_grs_match_plan_t ext_grs_match_plan_t;

/** a planer for the creation of matching plans */
typedef struct _ext_grs_planer_t ext_grs_planer_t;

typedef struct {
	int n_nodes;
	ext_grs_node_t **pat_nodes;
	ir_node **host_nodes;
	int n_edges;
	ext_grs_edge_t **pat_edges;
	ir_edge_t **host_edges;
} ext_grs_graph_fragment_t;






/** initialize a planer */
void ext_grs_init_planer(ext_grs_planer_t *planer, ext_grs_analyzer_t *alz);

/** compute a match plan */
ext_grs_match_plan_t *ext_grs_compute_plan(
	ext_grs_planer_t *planer, ir_graph *irg, ext_grs_action_t *act);

/** compute a match plan according to analysis data of all ir graphs */
ext_grs_match_plan_t *ext_grs_compute_plan_global(
	ext_grs_planer_t *planer, ext_grs_action_t *act);

/**dump a search plans to stdout */
void ext_grs_dump_match_plan(ext_grs_match_plan_t *);



ext_grs_match_plan_t *ext_grs_compute_plan_pivot(ext_grs_planer_t *planer,
	ir_graph *irg, ext_grs_action_t *act, ext_grs_graph_fragment_t *bound);
ext_grs_match_plan_t *ext_grs_compute_plan_global_pivot(ext_grs_planer_t *planer,
	ext_grs_action_t *act, ext_grs_graph_fragment_t *bound);





#endif /*_EXT_GRS_MATCHPLAN_H_*/
