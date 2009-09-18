#ifndef KAPS_HEURISTICAL_H
#define KAPS_HEURISTICAL_H

#include "pbqp_t.h"

#include "plist.h"

void solve_pbqp_heuristical(pbqp *pbqp);
void solve_pbqp_heuristical_co(pbqp *pbqp, plist_t *rpeo);
void solve_pbqp_brute_force(pbqp *pbqp);

void apply_edge(pbqp *pbqp);

void apply_RI(pbqp *pbqp);
void apply_RII(pbqp *pbqp);
void apply_RN(pbqp *pbqp);
void apply_RN_co(pbqp *pbqp, plist_t *rpeo);

void back_propagate_RI(pbqp *pbqp, pbqp_node *node);
void back_propagate_RII(pbqp *pbqp, pbqp_node *node);

int node_is_reduced(pbqp_node *node);

#endif /* KAPS_HEURISTICAL_H */
