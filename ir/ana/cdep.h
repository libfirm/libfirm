#ifndef CDEP_H
#define CDEP_H

#include "firm_types.h"

typedef struct cdep cdep;
struct cdep {
  ir_node *node;
  cdep *next;
};

/** Compute the control dependence graph for a graph. */
void compute_cdep(ir_graph *irg);
void free_cdep(ir_graph *irg);

cdep *find_cdep(const ir_node *block);

void exchange_cdep(ir_node *old, const ir_node *nw);

int is_cdep_on(const ir_node *dependee, const ir_node *candidate);

int is_iterated_cdep_on(ir_node *dependee, ir_node *candidate);

ir_node *get_unique_cdep(const ir_node *block);
int has_multiple_cdep(const ir_node *block);

#endif /* CDEP_H */
