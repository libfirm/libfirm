#ifndef KAPS_PBQP_T_H
#define KAPS_PBQP_T_H

#include <limits.h>

#include "adt/obstack.h"

#include "matrix_t.h"
#include "vector_t.h"

typedef int num;

typedef struct pbqp_edge pbqp_edge;
typedef struct pbqp_node pbqp_node;
typedef struct pbqp      pbqp;

static const num INF_COST = INT_MAX;

struct pbqp {
	struct obstack obstack;            /* Obstack. */
	num            solution;           /* Computed solution. */
	size_t         num_nodes;          /* Number of PBQP nodes. */
	pbqp_node    **nodes;              /* Nodes of PBQP. */
};

#endif /* KAPS_PBQP_T_H */
