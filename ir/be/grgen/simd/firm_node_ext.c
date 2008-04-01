/*************************************************************************
* Program:  firm_node_ext.c
* Function: Provides functions to extend firm by nodes needed by the
*           SIMD optimizer
* Author:   Andreas Schoesser
* Date:     2007-02-01
*************************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "simd_presets.h"

#include "irnode.h"
#include "irtools.h"
#include "irmemory.h"

#include "ia32/gen_ia32_regalloc_if.h"
#include "grs/base_t.h"

ir_opcode iro_VProj;
ir_op  *op_VProj;

ir_opcode iro_MultipleAdd;
ir_op *op_MultipleAdd;

ir_opcode iro_IrNode;
ir_op *op_IrNode;

ir_opcode iro_Complex;
ir_op *op_Complex;

ir_opcode iro_FakeComplex;
ir_op *op_FakeComplex;


ir_opcode iro_be_Keep;
extern ir_op *op_be_Keep;



/************************************************************************
 * Initialize the pattern creation.
 * - Creates new Firm opcodes needed by the transformation
 ************************************************************************/

void ext_firm_nodes()
{
	// The SIMD optimization inserts ia32 specific nodes
	// Create the opcodes for those nodes here
	// Reason: We need those opcodes when inserting ia32_nodes anyway.
	ia32_register_init(NULL);
	ia32_create_opcodes();
	be_node_init();

	iro_VProj = get_next_ir_opcode();
	op_VProj = new_ir_op(iro_VProj, "VProj", op_pin_state_pinned, 0, oparity_unary, 0, sizeof(long), NULL);

	iro_MultipleAdd = get_next_ir_opcode();
	op_MultipleAdd = new_ir_op(iro_MultipleAdd, "MultipleAdd", op_pin_state_pinned, 0, oparity_dynamic, 0, 0, NULL);

	iro_IrNode = get_next_ir_opcode();
	op_IrNode = new_ir_op(iro_IrNode, "IR_node", op_pin_state_pinned, 0, oparity_dynamic, 0, 0, NULL);

	iro_Complex = get_next_ir_opcode();
	op_Complex = new_ir_op(iro_Complex, "Complex", op_pin_state_pinned, irop_flag_none, oparity_dynamic,  0, 0, NULL);

	iro_FakeComplex = get_next_ir_opcode();
	op_FakeComplex = new_ir_op(iro_FakeComplex, "FakeComplex", op_pin_state_pinned, irop_flag_none, oparity_dynamic,  0, 0, NULL);

	iro_be_Keep = get_op_code(op_be_Keep);

	// Mode LLu now exists:
	//	mode_DLu = new_ir_vector_mode("DLu", irms_int_number, 128, 4, 0, irma_none, 128);
}



/************************************************************************
 * Inserts a new ir_node an also updates the opcode lists of the GRS
 ************************************************************************/

ir_node *new_ir_node_with_update(dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode, int arity, ir_node *in[])
{
	ir_node *new_node = new_ir_node(db, irg, block, op, mode, arity, in);

	// Update the GRS' node list
	ext_grs_irn_private_t *pr_n = _ext_grs_get_irn_private(new_node);
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);

	memset(pr_n, 0, sizeof(*pr_n));

	// Do we have to initialize pr_n?

	lc_list_add(&pr_n->node_list, &_ext_grs_NODE_LIST(pr_g, get_op_code(op), get_mode_modecode(mode)));

	// Do we have to call this?
	// (_ext_grs_N_INSTANCES(pr_g, get_op_code(op), get_mode_modecode(mode)))++;

	return(new_node);
}




/* __  __       _ _      _       _     _
  |  \/  |_   _| | |_   / \   __| | __| |
  | |\/| | | | | | __| / _ \ / _` |/ _` |
  | |  | | |_| | | |_ / ___ \ (_| | (_| |
  |_|  |_|\__,_|_|\__/_/   \_\__,_|\__,_| */

/************************************************************************
 * Returns 1 if the given node is a MultipleAdd operation
 ************************************************************************/

int is_MultipleAdd(ir_node *n)
{
	return(get_irn_opcode(n) == iro_MultipleAdd);
}



/************************************************************************
 * Returns 1 if the 2 MultipeAdd nodes have the same base addresses
 * but different offsets > size -> no alias
 * 0 otherwise -> may alias
 ************************************************************************/

ir_alias_relation get_multadd_alias_relation(ir_node *n1, ir_node *n2)
{
#if 1
	int i, offset_diff, same_base = 1, pos1, pos2, summands_not_equal = 0, size;
	ir_node *const1 = NULL, *const2 = NULL, *base1 = NULL, *base2 = NULL, *ma1, *ma2;
	ir_mode *mode1, *mode2;


	switch(get_irn_opcode(n1))
	{
		case iro_Store: ma1 = get_Store_ptr(n1); mode1 = get_irn_mode(get_Store_value(n1)); break;
		case iro_Load:  ma1 = get_Load_ptr(n1);  mode1 = get_Load_mode(n1); /*mode_F;*/ /* Hack */ break;		// TODO: Get Load mode
		default: return(no_alias);
	}

	switch(get_irn_opcode(n2))
	{
		case iro_Store: ma2 = get_Store_ptr(n2); mode2 = get_irn_mode(get_Store_value(n1)); break;
		case iro_Load:  ma2 = get_Load_ptr(n2);  mode2 = get_Load_mode(n1); /*mode_F;*/ /* Hack */ break;		 // TODO: Get LOAD mode
		default: return(no_alias);
	}

	assert(is_MultipleAdd(ma1) && is_MultipleAdd(ma2));

	size = MAX(get_mode_size_bytes(mode1), get_mode_size_bytes(mode2));

	// Different number of summands. Return "may alias"
	// Rethink. If they contain different Sel's or restrict pointers
	if(get_irn_arity(ma1) != get_irn_arity(ma2))
		return(may_alias);

	// Since MultipleAdd ins are ordered, the constant value has to be in_0 in both cases
	const1 = get_irn_n(ma1, 0);
	const2 = get_irn_n(ma2, 0);
	if(!is_Const(const1) && !is_Const(const2))
		return(may_alias);

	offset_diff = abs(get_tarval_long(get_Const_tarval(const1)) - get_tarval_long(get_Const_tarval(const2)));

	// Go through both in-arrays simultaneously and search for the base pointers
	// Also check the rest of the ins for equalness

	for(pos1 = 1, pos2 = 1; pos1 < get_irn_arity(ma1) && pos2 <  get_irn_arity(ma1); )
	{
		ir_node *n1, *n2;

		if((n1 = get_irn_n(ma1, pos1)) == (n2 = get_irn_n(ma2, pos2)))
		{
			pos1++;
			pos2++;
			continue;
		}

		summands_not_equal = 1;

		n1 = get_irn_n(ma1, pos1);
		n2 = get_irn_n(ma2, pos2);

		// Look if we have the base pointer
		if((get_irn_opcode(n1) == iro_Proj || get_irn_opcode(n1) == iro_Sel))
		{
			assert(base1 == NULL);
			base1 = n1;
			pos1++;
		}

		if((get_irn_opcode(n2) == iro_Proj || get_irn_opcode(n2) == iro_Sel))
		{
			assert(base2 == NULL);
			base2 = n2;
			pos2++;
		}
	}

	/*if(summands_not_equal)
		return(may_alias); */

	if(base1 == NULL || base2 == NULL)
	{
		if(!summands_not_equal)
		{
			// Summands are exaclty the same (including the base) so check the constant
			if(offset_diff < size)
				return(sure_alias);  // Offsets are overlapping
			return(no_alias);		 // Offsets are not overlapping
		}
		else
		{
			// Summands not the same and no (or just 1) base found. Bad luck, bail out
			return(may_alias);
		}
	}
	else
	{
		// Summands are different, but 2 bases were found. check them:
		// Alias relation depends on the alias relation of the base addresses
		assert(base1 != NULL && base2 != NULL && "We should have found 2 bases!");
		return(get_alias_relation(get_irn_irg(base1), base1, mode_F, base2, mode_F));
	}
#else
	return(may_alias);
#endif
}



/*__     ______            _
  \ \   / /  _ \ _ __ ___ (_)
   \ \ / /| |_) | '__/ _ \| |
    \ V / |  __/| | | (_) | |
     \_/  |_|   |_|  \___// |
                        |__/ */

/************************************************************************
 * Gets and sets the vproj number
 ************************************************************************/

int get_VProj_proj(ir_node *node)
{
	int *pa = (int *) get_irn_generic_attr(node);
	return *pa;
}

void set_VProj_proj(ir_node *node, int proj)
{
	int *pa = get_irn_generic_attr(node);
	*pa = proj;
}

ir_opcode get_VProj_op()
{
	return(iro_VProj);
}
