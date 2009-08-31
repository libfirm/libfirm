#ifndef KAPS_VECTOR_T_H
#define KAPS_VECTOR_T_H

#include "pbqp_t.h"

typedef struct vec_elem vec_elem;

struct vec_elem {
	num data;
#if KAPS_ENABLE_VECTOR_NAMES
	char *name;
#endif
};

typedef struct vector vector;

struct vector {
	unsigned len;
	vec_elem entries[];
};

#endif /* KAPS_VECTOR_T_H */
