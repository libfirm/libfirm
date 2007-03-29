/**
 * Statistic events
 * @date 3.9.2006
 * @author Sebastian Hack
 */

#ifndef _BESTATEVENT_H
#define _BESTATEVENT_H

#include <stdio.h>

#include "firm_types.h"

void be_stat_ev_push(const char **tags, int n_tags, FILE *f);
void be_stat_ev_pop(void);

void be_stat_ev(const char *ev, int value);
void be_stat_ev_l(const char *ev, long value);
void be_stat_ev_dbl(const char *ev, double value);
void be_stat_ev_ull(const char *ev, ulong64 value);

int be_stat_ev_is_active(void);

#endif /* _BESTATEVENT_H */
