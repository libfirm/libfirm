#ifndef KAPS_HTML_DUMPER_H
#define KAPS_HTML_DUMPER_H

#include "pbqp_t.h"

void pbqp_dump_input(pbqp *pbqp);

void pbqp_dump_graph(pbqp *pbqp);

void dump_simplifyedge(pbqp *pbqp, pbqp_edge *edge);

void dump_section(FILE *f, int level, char *txt);

void dump_node(pbqp *pbqp, unsigned index);
void dump_edge(pbqp *pbqp, pbqp_edge *edge);

#endif /* KAPS_HTML_DUMPER_H */
