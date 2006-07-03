#ifndef EXECFREQ_H_
#define EXECFREQ_H_

#include "irgraph_t.h"
#include "irnode_t.h"

typedef struct _freq_t {
  const ir_node    *irn;
  double            freq;
} freq_t;

void compute_execfreq(ir_graph * irg, double loop_weight);

void free_execfreq();

double get_block_execfreq(const ir_node * irn);

#endif
