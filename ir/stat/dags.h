/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm. DAG's in graphs.
 * @author  Michael Beck
 */
#ifndef FIRM_STAT_DAGS_H
#define FIRM_STAT_DAGS_H

#include "firmstat_t.h"
/*
 * count the DAG's size of a graph
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph);

#endif
