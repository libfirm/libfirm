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
#include "bearch.h"
#include "bechordal_t.h"

void copystat_init(void);
void copystat_reset(void);
void copystat_collect_cls(be_chordal_env_t *chordal_env);
void copystat_add_max_costs(int costs);
void copystat_add_inevit_costs(int costs);
void copystat_add_init_costs(int costs);
void copystat_add_heur_costs(int costs);
void copystat_add_opt_costs(int costs);
void copystat_add_heur_time(int time);
void copystat_add_ilp_time(int time);
void copystat_add_ilp_vars(int vars);
void copystat_add_ilp_csts(int csts);
void copystat_add_ilp_iter(int iters);
void copystat_dump(ir_graph *irg);
void copystat_dump_pretty(ir_graph *irg);


#else /* DO_STAT */

#define copy_copystat_init();
#define	copystat_reset();
#define copystat_collect_cls(env);
#define copystat_add_max_costs(costs);
#define copystat_add_inevit_costs(costs);
#define copystat_add_init_costs(costs);
#define copystat_add_heur_costs(costs);
#define copystat_add_opt_costs(costs);
#define copystat_add_heur_time(time);
#define copystat_add_ilp_time(time);
#define copystat_add_ilp_vars(vars);
#define copystat_add_ilp_csts(csts);
#define copystat_add_ilp_iter(iters);
#define copystat_dump(irg);
#define copystat_dump(irg);
#define copystat_dump_pretty(irg);

#endif /* DO_STAT */

#endif /* _BECOPYSTAT_H */
