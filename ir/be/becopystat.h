/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _BECOPYSTAT_H
#define _BECOPYSTAT_H

#define DO_STAT
#ifdef DO_STAT

#include "irgraph.h"

#define MAX_ARITY 10
#define MAX_CLS_SIZE 10
#define MAX_PHASE 2

/**
 * For an explanation of these values see phi_copystat_dump_pretty
 */
enum vals_t {
	I_ALL_NODES = 0,
	I_BLOCKS,

	/* phi nodes */
	I_PHI_CNT,			/* number of phi nodes */
	I_PHI_ARG_CNT,		/* number of arguments of phis */
	I_PHI_ARG_SELF,		/* number of arguments of phis being the phi itself */
	I_PHI_ARG_CONST,	/* number of arguments of phis being consts */
	I_PHI_ARG_PRED,		/* ... being defined in a cf-pred */
	I_PHI_ARG_GLOB,		/* ... being defined elsewhere */
	I_PHI_ARITY_S,
	I_PHI_ARITY_E    = I_PHI_ARITY_S+MAX_ARITY,

	/* copy nodes */
	I_CPY_CNT,			/* number of copynodes */

	/* phi classes */
	I_CLS_CNT,			/* number of phi classes */
	I_CLS_IF_FREE,		/* number of pc having no interference */
	I_CLS_IF_MAX,		/* number of possible interferences in all classes */
	I_CLS_IF_CNT,		/* number of actual interferences in all classes */
	I_CLS_SIZE_S,
	I_CLS_SIZE_E = I_CLS_SIZE_S+MAX_CLS_SIZE,

	/* ilp values */
	I_ILP_TIME,			/* !external set! solving time in seconds */
	I_ILP_ITER,			/* !external set! number of simplex iterations */

	/* copy instructions */
	I_COPIES_MAX,		/* max number of copies possible */
	I_COPIES_IF,		/* number of copies inevitable due to root-arg-interf */
	I_COPIES_INIT,		/* !external set! number of copies in initial allocation */
	I_COPIES_HEUR,		/* !external set! number of copies after heuristic */
	I_COPIES_OPT,		/* !external set! number of copies after ilp */

	ASIZE
};

/**
 * Holds current values. Values are added till next copystat_reset
 */
int curr_vals[ASIZE];

void copystat_init(void);
void copystat_reset(void);
void copystat_collect_irg(ir_graph *irg);
void copystat_collect_cls(be_chordal_env_t *chordal_env);
void copystat_dump(ir_graph *irg);
void copystat_dump_pretty(ir_graph *irg);

#else /* DO_STAT */

#define copy_copystat_init();
#define	copystat_reset();
#define copystat_collect_irg(irg);
#define copystat_collect_cls(env);
#define copystat_dump(irg);
#define copystat_dump(irg);
#define copystat_dump_pretty(irg);

#endif /* DO_STAT */

#endif /* _BECOPYSTAT_H */
