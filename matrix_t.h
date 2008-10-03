#ifndef KAPS_MATRIX_T_H
#define KAPS_MATRIX_T_H

#include "pbqp_t.h"

typedef struct pbqp_matrix pbqp_matrix;

struct pbqp_matrix {
	unsigned rows;
	unsigned cols;
	num entries[];
};

#endif /* KAPS_MATRIX_T_H */
