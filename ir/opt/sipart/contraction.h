#ifndef CONTRACTION_H
#define CONTRACTION_H

/** Walk the subgraph of a Contraction */
void walk_contraction(ir_node *node, irg_walk_func *cb, void *data);

ir_nodemap *hide_contractions(ir_graph *irg);

void unhide_contractions(ir_graph *irg, ir_nodemap *handle);

void remove_contraction_keepalives(ir_graph *irg);
void add_contraction_keepalives(ir_graph *irg);

ir_graph *create_contracted_irg_copy(ir_graph *irg);

void fix_contractions_after_irg_copy(ir_graph *irg);

int count_contractions(ir_graph *irg);

void node_dump_Contraction(FILE *out, const ir_node *self, dump_reason_t reason);

#endif
