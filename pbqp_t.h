#ifndef KAPS_PBQP_T_H
#define KAPS_PBQP_T_H

#include <limits.h>
#include <stdint.h>
#include <stdio.h>

#include "adt/obstack.h"

typedef intmax_t num;

#include "matrix_t.h"
#include "vector_t.h"

typedef struct pbqp_edge pbqp_edge;
typedef struct pbqp_node pbqp_node;
typedef struct pbqp      pbqp;

static const num INF_COSTS = INTMAX_MAX;

#define KAPS_STATISTIC 0

struct pbqp {
	struct obstack obstack;            /* Obstack. */
	num            solution;           /* Computed solution. */
	size_t         num_nodes;          /* Number of PBQP nodes. */
	pbqp_node    **nodes;              /* Nodes of PBQP. */
	FILE          *dump_file;          /* File to dump in. */
#if KAPS_STATISTIC
	unsigned       num_bf;             /* Number of brute force reductions. */
	unsigned       num_edges;          /* Number of independent edges. */
	unsigned       num_r0;             /* Number of trivial solved nodes. */
	unsigned       num_r1;             /* Number of R1 reductions. */
	unsigned       num_r2;             /* Number of R2 reductions. */
	unsigned       num_rn;             /* Number of RN reductions. */
#endif
};

#endif /* KAPS_PBQP_T_H */
