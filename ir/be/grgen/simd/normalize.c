/*******************************************************************************************
* Program:  normalize.c
* Function: Part of the simd optimization. Provides functions to normalize a FIRM
*			graph in order to increase the number of patterns matched.
* Author:   Andreas Schoesser
* Date:     08.03.2007
*******************************************************************************************/

#include <malloc.h>
#include <assert.h>

#include "simd_presets.h"

#include "pmap.h"
#include "irgwalk.h"
#include "ircons.h"
#include "tv.h"
#include "irgmod.h"
#include "irnode.h"

#include "normalize_t.h"
#include "firm_node_ext.h"



/************************************************************************
 * Starts normalization of address calculations for Add and Store
 * nodes. Folds Add-chains to one big MultipeAdd node.
 ************************************************************************/

struct pmap *normalize_address_calculation(ir_graph *irg, int decomposition_possible)
{
	normalize_info_t norm_info;

	norm_info.irg = irg;
	norm_info.replaced_ptrs = (decomposition_possible)	? pmap_create() : NULL;

	irg_walk_graph(irg, ac_walk_pre, NULL, &norm_info);

	return(norm_info.replaced_ptrs);
}



/************************************************************************
 * Called for each ir_node, in which the main work for normalization
 * is done.
 ************************************************************************/

static void ac_walk_pre(ir_node *n, void * env)
{
	ir_node *addr, *multAdd;
	normalize_info_t *norm_info = (normalize_info_t *) env;
	ir_graph *irg = norm_info->irg;
	int i, exchanged = 0;

	switch(get_irn_opcode(n))
	{
		case iro_Load: addr = get_Load_ptr(n); break;
		case iro_Store: addr = get_Store_ptr(n); break;
		case iro_Proj:	if(get_irn_opcode(get_irn_n(n, 0)) == iro_Cmp)
							enforce_cmp_gt(n);
						return;
		default: return;
	}

	if(get_irn_opcode(addr) == iro_Add)
	{
		// Convert a ADD-chain to a single MultipleAdd node.

		ir_node *add = addr;
		int arity = 0;
		int max_ins = 30;
		ir_node **ins = malloc(max_ins * sizeof(ir_node *));

		if(add_add_preds(add, ins, &max_ins, &arity) == 0)
		{
			// We found no constant, add 0x0
			ins[arity++] = new_Const(mode_Is, new_tarval_from_long(0, mode_Is));
		}
		multAdd = new_ir_node(NULL, irg, get_nodes_block(addr), op_MultipleAdd, get_irn_mode(addr), arity, ins);

		switch(get_irn_opcode(n))
		{
			case iro_Load: set_Load_ptr(n, multAdd); break;
			case iro_Store: set_Store_ptr(n, multAdd); break;
			default: return;
		}

		// exchange(add, multAdd); Exchange wouldn't work here since the original ADD node would be killed.
		if(norm_info->replaced_ptrs != NULL)
			pmap_insert(norm_info->replaced_ptrs, multAdd, add);
		free(ins);
	}
	else if(get_irn_opcode(addr) != iro_MultipleAdd)
	{
		// Insert a new MultipleAdd node.
		ir_node **ins = alloca(2 * sizeof(ir_node *));

		ins[0] = addr;
		ins[1] = new_Const(mode_Is, new_tarval_from_long(0, mode_Is));
		multAdd = new_ir_node(NULL, irg, get_nodes_block(n), op_MultipleAdd, get_irn_mode(addr), 2, ins);
		//exchange(addr, multAdd);
		if(norm_info->replaced_ptrs != NULL)
			pmap_insert(norm_info->replaced_ptrs, multAdd, addr);

		switch(get_irn_opcode(n))
		{
			case iro_Load: set_Load_ptr(n, multAdd); break;
			case iro_Store: set_Store_ptr(n, multAdd); break;
			default: return;
		}
	}

	/* Order the MultAdd ins to be compared quickly later. Bubble Sort */
	do
	{
		exchanged = 0;
		for(i = 0; i < get_irn_arity(multAdd) - 1; i++)
		{
			ir_node *n1 = get_irn_n(multAdd, i),
					*n2 = get_irn_n(multAdd, i + 1);

			// Move the Constant to pos0
			if(is_Const(n2) && !is_Const(n1))
			{
				set_irn_n(multAdd, i, n2);
				set_irn_n(multAdd, i + 1, n1);
				exchanged = 1;
				continue;
			}

			// Otherwise order by pointer value
			if(n1 > n2 && !is_Const(n1))
			{
				set_irn_n(multAdd, i, n2);
				set_irn_n(multAdd, i + 1, n1);
				exchanged = 1;
			}
		}
	} while(exchanged);

}



/************************************************************************
 * Recursive function. Collects all adds of an "add chain" and inserts
 * all the predecessors of the add chain into one ins array
 ************************************************************************/

int add_add_preds(ir_node *add, ir_node **ins, int *max_len, int *num_preds)
{
	int i, found_constant = 0;

	for(i = 0; i < 2; i++)
	{
		ir_node *pred = get_irn_n(add, i);
		switch(get_irn_opcode(pred))
		{
			case iro_Add:	found_constant |= add_add_preds(pred, ins, max_len, num_preds);
							break;
			case iro_Const:	found_constant |= 1;
							/* Fall through */
			default:		assert(*num_preds < *max_len && "Reallocation of temp ins array not implemented yet!");
							ins[*num_preds] = pred;
							(*num_preds)++;
		}
	}

	return(found_constant);
}



/*******************************************************
 * Decompose the normalization, that is replace all
 * introduced MultipleAdds by their original ADD-Chain.
 *******************************************************/

void decompose_normalization(struct pmap *replaced_ptrs)
{
	pmap_entry *entry;

	pmap_foreach(replaced_ptrs, entry)
	{
		ir_node *multAdd = (ir_node *) entry->key;
		ir_node *orgAddr = (ir_node *) entry->value;

		//if(get_irn_n_edges(multAdd) > 0)
/*		switch(get_irn_opcode(loadStore))
		{
			case iro_Load: set_Load_ptr(loadStore, orgAddr); break;
			case iro_Store: set_Store_ptr(loadStore, orgAddr); break;
			default: break; //return; //assert(0 && "Wrong opcode!");
		}*/
		exchange(multAdd, orgAddr);
//		edges_node_deleted(multAdd, get_irn_irg(multAdd));
	}

	pmap_destroy(replaced_ptrs);
}


/************************************************************************
 * Returns the original address that was replaced by a multiple add
 ************************************************************************/

ir_node *get_org_addr(ir_node *madd, struct pmap *replaced_ptrs)
{
	assert(get_irn_opcode(madd) == iro_MultipleAdd);
	return(pmap_get(replaced_ptrs, madd));
}



/************************************************************************
 * Enforces all compares lt to be gt
 ************************************************************************/

void enforce_cmp_gt(ir_node *proj)
{
	if(get_Proj_proj(proj) == pn_Cmp_Lt)
	{
		pn_Cmp new_pnc = get_inversed_pnc(get_Proj_proj(proj));
		ir_node *tmp, *cmp = get_irn_n(proj, 0);

		tmp = get_Cmp_left(cmp);
		set_Cmp_left(cmp, get_Cmp_right(cmp));
		set_Cmp_right(cmp, tmp);
		set_Proj_proj(proj, new_pnc);
	}
}


/************************************************************************/
/*                                                                      */
/************************************************************************/
#if 0
void normalize_direct_jump(ir_node *block)
{
	int block_arity = get_irn_arity(block);
    int i, j;

	if(block_arity > 1)
	{
		for(i = 0; i < block_arity; i++)
		{
			ir_node *pred1 = get_irn_n(block, i);
			ir_node *cond;
			if(get_irn_opcode(pred1) == iro_Proj && get_irn_mode(pred1) == mode_X && get_Proj_proj(pred1) == 0 /* False */)
			{
				cond = get_irn_n(pred1, 0);
				assert(get_irn_op(cond) == iro_Cond);

				for(j = 0; i < block_arity; j++)
				{
					ir_node *jmp = get_irn_n(block, j);

					if(j == i)
						continue;

					if(get_irn_opcode(jmp) == iro_Jmp)
					{
						ir_node *jmp_block = get_nodes_block(jmp);
						if(get_irn_arity(jmp_block) == 1 && get_irn_n(jmp_block)
					}

				}
		}
	}
}
#endif
