/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
**  dead node elemination
**  walks one time through the whole graph and copies it into another graph,
**  so unreachable nodes will be lost.
*/

# ifndef _IRGOPT_H_
# define _IRGOPT_H_

# include "irgraph.h"


void local_optimize_graph (ir_graph *irg);

void dead_node_elemination(ir_graph *irg);

# endif /* _IRGOPT_H_ */
