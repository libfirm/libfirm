/**
 * @author Daniel Grund
 * @date 11.04.2005
 */
#define DO_STAT

#ifdef DO_STAT

#ifndef _BECOPYSTAT_H
#define _BECOPYSTAT_H

#include "irgraph.h"

#define MAX_ARITY 10
#define MAX_CLS_SIZE 10
#define MAX_PHASE 2
/**
 * For an explanation of these values see phi_stat_dump_pretty
 */
enum vals_t {
	I_ALL_NODES = 0,
	I_BLOCKS,

	I_PHI_CNT,			/* number of phi nodes */
	I_PHI_ARG_CNT,		/* number of arguments of phis */
	I_PHI_ARG_SELF,		/* number of arguments of phis being the phi itself */
	I_PHI_ARG_CONST,	/* number of arguments of phis being consts */
	I_PHI_ARG_PRED,		/* ... being defined in a cf-pred */
	I_PHI_ARG_GLOB,		/* ... being defined elsewhere */
	I_PHI_ARITY_S,
	I_PHI_ARITY_E    = I_PHI_ARITY_S+MAX_ARITY,

	I_CLS_CNT,			/* number of phi classes */
	I_CLS_IF_FREE,		/* number of pc having no interference */
	I_CLS_IF_MAX,		/* number of possible interferences in all classes */
	I_CLS_IF_CNT,		/* number of actual interferences in all classes */
	I_CLS_SIZE_S,
	I_CLS_SIZE_E = I_CLS_SIZE_S+MAX_CLS_SIZE,

	I_COPIES_MAX,		/* max number of copies possible */
	I_COPIES_INIT,		/* !external set! number of copies in initial allocation */
	I_COPIES_HEUR,		/* !external set! number of copies after heuristic */
	I_COPIES_OPT,		/* !external set! number of copies after ilp */
	I_COPIES_LB,		/* !external set! the lower bound used for number of copies */
	I_COPIES_IF,		/* number of copies inevitable due to root-arg-interf */

	ASIZE
};

/**
 * Holds current values. Values are added till next phi_stat_reset
 */
int curr_vals[ASIZE];

void stat_init(void);

/**
 * Resets the array holding the data
 */
void stat_reset(void);

/**
 * Collect common irg data
 */
void stat_collect_irg(ir_graph *irg);

/**
 * Dumps the current contents of the internal values to a file.
 */
void stat_dump(ir_graph *irg);

/**
 * Dumps the current contents of the values array and annotations to a file.
 */
void stat_dump_pretty(ir_graph *irg);

#endif
#endif
