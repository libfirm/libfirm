/*
 * ces_normalize.h
 *
 *  Created on: 10 Apr 2014
 *      Author: haass
 */

#ifndef __CES_NORMALIZE_H__
#define __CES_NORMALIZE_H__
#include <firm.h>
#include <irnodemap.h>
int ces_reassociate_mul_proj(ir_node** add);
int ces_reassociate_mul_mul(ir_node** add);
int ces_reassociate_mul_add(ir_node** add);
void ces_fix_mode(ir_node* node);
ir_node* ces_duplicate_subgraph(ir_node* n, ir_node* block, dbg_info* dbgi);
ir_node* ces_normalize_load_ptr(ir_node* node,  ir_node* load);

//--------- implementation in ces_normalize2 ------------
ir_node* ces_normalize_memop(ir_graph* irg);

#endif /* __CES_NORMALIZE_H__ */
