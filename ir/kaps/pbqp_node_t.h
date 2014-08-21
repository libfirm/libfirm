/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP node data types.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_PBQP_NODE_T_H
#define KAPS_PBQP_NODE_T_H

#include "pbqp_t.h"

struct pbqp_node_t {
	pbqp_edge_t **edges;
	vector_t     *costs;
	unsigned      bucket_index;
	unsigned      solution;
	unsigned      index;
};

#endif
