#ifndef EXECFREQ_H_
#define EXECFREQ_H_

#include "irgraph_t.h"
#include "irnode_t.h"

typedef struct _freq_t {
  const ir_node    *irn;
  double            freq;
} freq_t;

set *compute_execfreq(ir_graph * irg);

void free_execfreq(set * freqs);

double get_block_execfreq(set * freqs, const ir_node * irn);

#endif
