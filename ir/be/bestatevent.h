/**
 * Statistic events
 * @date 3.9.2006
 * @author Sebastian Hack
 */

#ifndef _BESTATEVENT_H
#define _BESTATEVENT_H

#include <stdio.h>

#define BE_STAT_EV_N_INSN                    "n_insn"
#define BE_STAT_EV_PHI_BEFORE_SPILL          "phi_before_spill"
#define BE_STAT_EV_PHI_AFTER_SPILL           "phi_after_spill"

void be_stat_ev_push(const char **tags, int n_tags, FILE *f);
void be_stat_ev_pop(void);

void be_stat_ev(const char *ev, int value);
void be_stat_ev_dbl(const char *ev, double value);

int be_stat_ev_is_active(void);

#endif /* _BESTATEVENT_H */
