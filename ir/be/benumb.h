/**
 * Numbering for nodes.
 * @author Sebastian Hack
 * @date 8.11.2004
 */

#ifndef _BENUMB_H
#define _BENUMB_H

#include "irgraph.h"

#include "be_t.h"

int (get_irn_graph_nr)(const ir_node *irn);
int (get_irn_block_nr)(const ir_node *irn);
int (get_block_graph_nr)(const ir_node *irn);
int (get_block_node_count)(const ir_node *irn);
int (get_graph_block_count)(const ir_graph *irn);

ir_node *(get_irn_for_graph_nr)(const ir_graph *irg, int nr);

extern const phase_t *phase_numbering;

void be_numbering(ir_graph *irg);
void be_numbering_done(ir_graph *irg);
void be_numbering_init(void);

#endif
