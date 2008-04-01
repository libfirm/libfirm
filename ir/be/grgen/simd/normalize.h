/*******************************************************************************************
* Program:  normalize.h
* Function: Part of the simd optimization. Provides functions to normalize a FIRM
*			graph in order to increase the number of patterns matched.
* Author:   Andreas Schoesser
* Date:     08.03.2007
*******************************************************************************************/

#ifndef __NORMALIZE_H__
#define __NORMALIZE_H__

/** */
struct pmap *normalize_address_calculation(ir_graph *irg, int decomposition_possible);
void decompose_normalization(struct pmap *replaced_ptrs);
ir_node *get_org_addr(ir_node *madd, struct pmap *replaced_ptrs);

#endif
