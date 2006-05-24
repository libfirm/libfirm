#ifndef CDEP_H
#define CDEP_H

#include "irnode.h"

typedef struct cdep cdep;
struct cdep {
  ir_node* node;
  cdep* next;
};

void compute_cdep(ir_graph*);
void free_cdep(ir_graph*);

cdep* find_cdep(const ir_node* block);

int is_cdep_on(const ir_node* dependee, const ir_node* candidate);

int is_iterated_cdep_on(ir_node* dependee, ir_node* candidate);

ir_node* get_unique_cdep(const ir_node* block);
int has_multiple_cdep(const ir_node* block);

#endif
