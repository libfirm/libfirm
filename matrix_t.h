#ifndef KAPS_MATRIX_T_H
#define KAPS_MATRIX_T_H

typedef struct matrix matrix;

struct matrix {
	int rows;
	int cols;
	int entries[];
};

#endif /* KAPS_MATRIX_T_H */
