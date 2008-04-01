#ifndef _EXT_GRS_MATCHPLAN_T_H_
#define _EXT_GRS_MATCHPLAN_T_H_

/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/grs/plan_t.h
 * Purpose:     provides an interface for search planers planing
 * 				the matching process with respect to information
 * 				provided by appropriate analyzers
 * Author:      Veit Batz
 * Created:		7. July 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "action_t.h"
#include "analyze_t.h"
#include "matchplan.h"

typedef enum {
	ext_grs_k_mop_node,
	ext_grs_k_mop_edge,
	ext_grs_k_mop_condition,
	ext_grs_k_mop_preset,
	ext_grs_k_mop_preset_edge
} ext_grs_mop_kind_t;

typedef struct {
	/** tells wether this op matches a node or an edge */
	ext_grs_mop_kind_t kind;
	/** if this match op matches an edge, then this is the respective pattern edge */
	ext_grs_edge_t *edge;
	/** if this match op matches a node, then this is the respective pattern node,
	 * if this op matches an edge, then this node is the pattern node where the
	 * edge is retrieved from */
	ext_grs_node_t *node;
	/** a condition to be checked by this matching op */
	ext_grs_condition_func_t condition;
	/** an op condition to be checked by this matching op (for node-ops only) */
	ext_grs_op_condition_func_t op_condition;
	/** a mode condition to be checked by this matching op (for node-ops only) */
	ext_grs_mode_condition_func_t mode_condition;
} ext_grs_match_op_t;

/** a plan the matching process will follow */
struct _ext_grs_match_plan_t {
	/** the action this matching plan belongs to */
	ext_grs_action_t *action;
	/** the number of matching ops of this search plan */
	/*int length;*/
	int num_progs;
	int *length;
	/** a sequence (array) of match ops (i.e. the matching 'program') */
	/* Contains pointers to all search plans (positive and negative) */
	ext_grs_match_op_t **progs;
	/* Just for "backward compatibility:
	   Contains pointer to the positive search plan */
	ext_grs_match_op_t *prog;
};

/** a planer fir the creation of matching plans */
struct _ext_grs_planer_t {
	/** a tag identifying a planer */
	const char *tag;
	/** this planers analyzer  */
	ext_grs_analyzer_t *analyzer;

	/** initialize this planer */
	void (*init) (struct _ext_grs_planer_t *, ext_grs_analyzer_t *);
	/** compute a match plan according to analysis data of a given  ir graph */
	ext_grs_match_plan_t * (*compute_plan)
		(struct _ext_grs_planer_t *, ir_graph *, ext_grs_action_t *);
	/** compute a match plan according to analysis data of all ir graphs */
	ext_grs_match_plan_t * (*ext_grs_compute_plan_global)
		(struct _ext_grs_planer_t *, ext_grs_action_t *);

	/** this planers private data area */
	void *data;
};



#endif /*_EXT_GRS_MATCHPLAN_T_H_*/
