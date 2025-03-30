/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   HTML dumper for PBQP.
 * @date    03.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_HTML_DUMPER_H
#define KAPS_HTML_DUMPER_H

#include "pbqp_t.h"

void pbqp_dump_input(pbqp_t *pbqp);

void pbqp_dump_graph(pbqp_t *pbqp);

void pbqp_dump_simplifyedge(pbqp_t *pbqp, pbqp_edge_t *edge);

void pbqp_dump_section(FILE *f, int level, const char *txt);

void pbqp_dump_node(FILE *file, pbqp_node_t *node);
void pbqp_dump_edge(FILE *file, pbqp_edge_t *edge);

#endif
