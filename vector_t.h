#ifndef KAPS_VECTOR_T_H
#define KAPS_VECTOR_T_H

#include "pbqp_t.h"

struct vector;
typedef struct vector vector;

struct vector {
	int len;
	int entries[];
};

#endif /* KAPS_VECTOR_T_H */
