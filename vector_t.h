#ifndef KAPS_VECTOR_T_H
#define KAPS_VECTOR_T_H

struct vector;
typedef struct vector vector;

struct vector {
	int len;
	num entries[];
};

#endif /* KAPS_VECTOR_T_H */
