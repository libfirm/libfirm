/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP matrix data types.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_MATRIX_T_H
#define KAPS_MATRIX_T_H

#include "pbqp_t.h"

typedef struct pbqp_matrix_t pbqp_matrix_t;

struct pbqp_matrix_t {
	unsigned rows;
	unsigned cols;
	num entries[];
};

#endif
