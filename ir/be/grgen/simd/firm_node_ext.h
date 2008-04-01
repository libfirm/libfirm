/*************************************************************************
* Program:  firm_node_ext.h
* Function: Provides functions to extend firm by nodes needed by the
*           SIMD optimizer
* Author:   Andreas Schoesser
* Date:     2007-02-01
*************************************************************************/

#ifndef __FIRM_NODE_EXT__
#define __FIRM_NODE_EXT__

#include "irnode.h"
#include "irmemory.h"

extern ir_opcode iro_VProj;
extern		 ir_op     *op_VProj;

extern ir_opcode iro_MultipleAdd;
extern		 ir_op	 *op_MultipleAdd;

extern ir_opcode iro_IrNode;
extern ir_op *op_IrNode;

extern ir_opcode iro_Complex;
extern ir_op *op_Complex;

extern ir_opcode iro_FakeComplex;
extern ir_op *op_FakeComplex;


//extern ir_mode *mode_DLu;

ir_node *new_ir_node_with_update(dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode, int arity, ir_node *in[]);
int  get_VProj_proj(ir_node *n);
void set_VProj_proj(ir_node *n, int proj);
void ext_firm_nodes();
int  is_MultipleAdd(ir_node *n);
ir_alias_relation get_multadd_alias_relation(ir_node *n1, ir_node *n2);

#endif
