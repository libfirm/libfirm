#ifndef EXECFREQ_H_
#define EXECFREQ_H_

#include "irgraph_t.h"
#include "irnode_t.h"
#include "set.h"

typedef struct _exec_freq_t exec_freq_t;

exec_freq_t * compute_execfreq(ir_graph *irg, double loop_weight);

void free_execfreq(exec_freq_t *ef);

double get_block_execfreq(const exec_freq_t *ef, const ir_node * irn);
unsigned long get_block_execfreq_ulong(const exec_freq_t *ef, const ir_node *bb);

#endif
