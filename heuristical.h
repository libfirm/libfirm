#ifndef KAPS_HEURISTICAL_H
#define KAPS_HEURISTICAL_H

#include "pbqp_t.h"

void solve_pbqp_heuristical(pbqp *pbqp);

void apply_RI(pbqp *pbqp);
void apply_RII(pbqp *pbqp);
void apply_RN(pbqp *pbqp);

void back_propagate_RI(pbqp *pbqp, pbqp_node *node);
void back_propagate_RII(pbqp *pbqp, pbqp_node *node);

int node_is_reduced(pbqp_node *node);

#endif /* KAPS_HEURISTICAL_H */
