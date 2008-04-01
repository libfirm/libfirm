/*******************************************************************************************
 * Program:  normalize_t.h
 * Function: Part of the simd optimization. Provides functions to normalize a FIRM
 *		 	 graph in order to increase the number of patterns matched.
 * Author:   Andreas Schoesser
 * Date:     08.03.2007
 *******************************************************************************************/

#ifndef __NORMALIZE_T_H__
#define __NORMALIZE_T_H__

#include "irnode.h"

static void ac_walk_pre(ir_node *n, void * env);
int  add_add_preds(ir_node *add, ir_node **ins, int *max_len, int *num_preds);
void enforce_cmp_gt(ir_node *proj);

typedef struct
{
	ir_graph *irg;
	struct pmap *replaced_ptrs;
} normalize_info_t;

#endif
