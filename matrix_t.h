#ifndef KAPS_MATRIX_T_H
#define KAPS_MATRIX_T_H

struct matrix;
typedef struct matrix matrix;

struct matrix {
	int rows;
	int cols;
	num entries[];
};

#endif /* KAPS_MATRIX_T_H */
