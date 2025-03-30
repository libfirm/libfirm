/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP edge data types.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_PBQP_EDGE_T_H
#define KAPS_PBQP_EDGE_T_H

#include "pbqp_t.h"

struct pbqp_edge_t {
	pbqp_node_t   *src;                  /* Source index. */
	pbqp_node_t   *tgt;                  /* Target index. */
	pbqp_matrix_t *costs;                /* Cost matrix. */
	unsigned       bucket_index;         /* Index of edge bucket. */
};

#endif
