/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP vector data types.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_VECTOR_T_H
#define KAPS_VECTOR_T_H

#include "pbqp_t.h"

typedef struct vec_elem_t vec_elem_t;

struct vec_elem_t {
	num data;
#if KAPS_ENABLE_VECTOR_NAMES
	const char *name;
#endif
};

typedef struct vector_t vector_t;

struct vector_t {
	unsigned   len;
	vec_elem_t entries[];
};

#endif
