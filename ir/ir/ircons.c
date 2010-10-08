/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Various irnode constructors. Automatic construction of SSA
 *          representation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Boris Boesler
 *          Michael Beck, Matthias Braun
 * @version $Id$
 */
#include "config.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irverify.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "irhooks.h"
#include "array_t.h"
#include "irbackedge_t.h"
#include "irflag_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "error.h"

#include "gen_ir_cons.c.inl"

/**
 * Language dependent variable initialization callback.
 */
static uninitialized_local_variable_func_t *default_initialize_local_variable = NULL;

ir_node *new_rd_Start(dbg_info *db, ir_graph *irg)
{
	ir_node  *block = get_irg_start_block(irg);
	ir_node  *res   = new_ir_node(db, irg, block, op_Start, mode_T, 0, NULL);

	res = optimize_node(res);
	irn_verify_irg(res, irg);
	return res;
}

ir_node *new_rd_End(dbg_info *db, ir_graph *irg)
{
	ir_node  *block = get_irg_end_block(irg);
	ir_node  *res   = new_ir_node(db, irg, block, op_End, mode_X, -1, NULL);

	res = optimize_node(res);
	irn_verify_irg(res, irg);
	return res;
}

/**
 * Creates a Phi node with all predecessors.  Calling this constructor
 * is only allowed if the corresponding block is mature.
 */
ir_node *new_rd_Phi(dbg_info *db, ir_node *block, int arity, ir_node **in,
                    ir_mode *mode)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(db, irg, block, op_Phi, mode, arity, in);
	res->attr.phi.u.backedge = new_backedge_arr(irg->obst, arity);

	res = optimize_node(res);
	irn_verify_irg(res, irg);

	/* Memory Phis in endless loops must be kept alive.
	   As we can't distinguish these easily we keep all of them alive. */
	if (is_Phi(res) && mode == mode_M)
		add_End_keepalive(get_irg_end(irg), res);
	return res;
}

ir_node *new_rd_Const(dbg_info *db, ir_graph *irg, ir_tarval *con)
{
	ir_node  *block = get_irg_start_block(irg);
	ir_mode  *mode  = get_tarval_mode(con);
	ir_node  *res   = new_ir_node(db, irg, block, op_Const, mode, 0, NULL);
	res->attr.con.tarval = con;

	res = optimize_node (res);
	irn_verify_irg(res, irg);

	return res;
}

ir_node *new_rd_Const_long(dbg_info *db, ir_graph *irg, ir_mode *mode,
                           long value)
{
	return new_rd_Const(db, irg, new_tarval_from_long(value, mode));
}

ir_node *new_rd_defaultProj(dbg_info *db, ir_node *arg, long max_proj)
{
	ir_node *res;

	assert(is_Cond(arg));
	arg->attr.cond.default_proj = max_proj;
	res = new_rd_Proj(db, arg, mode_X, max_proj);
	return res;
}

ir_node *new_rd_Sync(dbg_info *db, ir_node *block, int arity, ir_node *in[])
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(db, irg, block, op_Sync, mode_M, -1, NULL);
	int       i;

	for (i = 0; i < arity; ++i)
		add_Sync_pred(res, in[i]);

	res = optimize_node(res);
	irn_verify_irg(res, irg);
	return res;
}

ir_node *new_rd_ASM(dbg_info *db, ir_node *block, int arity, ir_node *in[],
                    ir_asm_constraint *inputs, int n_outs,
	                ir_asm_constraint *outputs, int n_clobber,
	                ident *clobber[], ident *text)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(db, irg, block, op_ASM, mode_T, arity, in);

	res->attr.assem.pin_state = op_pin_state_pinned;
	res->attr.assem.input_constraints
		= NEW_ARR_D(ir_asm_constraint, irg->obst, arity);
	res->attr.assem.output_constraints
		= NEW_ARR_D(ir_asm_constraint, irg->obst, n_outs);
	res->attr.assem.clobbers = NEW_ARR_D(ident *, irg->obst, n_clobber);
	res->attr.assem.text     = text;

	memcpy(res->attr.assem.input_constraints,  inputs,  sizeof(inputs[0]) * arity);
	memcpy(res->attr.assem.output_constraints, outputs, sizeof(outputs[0]) * n_outs);
	memcpy(res->attr.assem.clobbers, clobber, sizeof(clobber[0]) * n_clobber);

	res = optimize_node(res);
	irn_verify_irg(res, irg);
	return res;
}

ir_node *new_rd_simpleSel(dbg_info *db, ir_node *block, ir_node *store,
                          ir_node *objptr, ir_entity *ent)
{
	return new_rd_Sel(db, block, store, objptr, 0, NULL, ent);
}

ir_node *new_rd_SymConst(dbg_info *db, ir_graph *irg, ir_mode *mode,
                         symconst_symbol value, symconst_kind symkind)
{
	ir_node *block = get_irg_start_block(irg);
	ir_node *res   = new_ir_node(db, irg, block, op_SymConst, mode, 0, NULL);
	res->attr.symc.kind = symkind;
	res->attr.symc.sym  = value;

	res = optimize_node(res);
	irn_verify_irg(res, irg);
	return res;
}

ir_node *new_rd_SymConst_addr_ent(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_entity *symbol)
{
	symconst_symbol sym;
	sym.entity_p = symbol;
	return new_rd_SymConst(db, irg, mode, sym, symconst_addr_ent);
}

ir_node *new_rd_SymConst_ofs_ent(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_entity *symbol)
{
	symconst_symbol sym;
	sym.entity_p = symbol;
	return new_rd_SymConst(db, irg, mode, sym, symconst_ofs_ent);
}

ir_node *new_rd_SymConst_type_tag(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol)
{
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst(db, irg, mode, sym, symconst_type_tag);
}

ir_node *new_rd_SymConst_size(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol)
{
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst(db, irg, mode, sym, symconst_type_size);
}

ir_node *new_rd_SymConst_align(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol)
{
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst(db, irg, mode, sym, symconst_type_align);
}

ir_node *new_r_Start(ir_graph *irg)
{
	return new_rd_Start(NULL, irg);
}
ir_node *new_r_End(ir_graph *irg)
{
	return new_rd_End(NULL, irg);
}
ir_node *new_r_Const(ir_graph *irg, ir_tarval *con)
{
	return new_rd_Const(NULL, irg, con);
}
ir_node *new_r_Const_long(ir_graph *irg, ir_mode *mode, long value)
{
	return new_rd_Const_long(NULL, irg, mode, value);
}
ir_node *new_r_SymConst(ir_graph *irg, ir_mode *mode, symconst_symbol value,
                        symconst_kind symkind)
{
	return new_rd_SymConst(NULL, irg, mode, value, symkind);
}
ir_node *new_r_simpleSel(ir_node *block, ir_node *store, ir_node *objptr,
                         ir_entity *ent)
{
	return new_rd_Sel(NULL, block, store, objptr, 0, NULL, ent);
}
ir_node *new_r_Phi(ir_node *block, int arity, ir_node **in, ir_mode *mode)
{
	return new_rd_Phi(NULL, block, arity, in, mode);
}
ir_node *new_r_Sync(ir_node *block, int arity, ir_node *in[])
{
	return new_rd_Sync(NULL, block, arity, in);
}
ir_node *new_r_defaultProj(ir_node *arg, long max_proj)
{
	return new_rd_defaultProj(NULL, arg, max_proj);
}
ir_node *new_r_Bad(ir_graph *irg)
{
	return get_irg_bad(irg);
}
ir_node *new_r_NoMem(ir_graph *irg)
{
	return get_irg_no_mem(irg);
}
ir_node *new_r_ASM(ir_node *block,
                   int arity, ir_node *in[], ir_asm_constraint *inputs,
                   int n_outs, ir_asm_constraint *outputs,
                   int n_clobber, ident *clobber[], ident *text)
{
	return new_rd_ASM(NULL, block, arity, in, inputs, n_outs, outputs, n_clobber, clobber, text);
}

/** ********************/
/** public interfaces  */
/** construction tools */

ir_node *new_d_Start(dbg_info *db)
{
	ir_node *res;

	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	res = new_ir_node(db, current_ir_graph, current_ir_graph->current_block,
	                  op_Start, mode_T, 0, NULL);

	res = optimize_node(res);
	irn_verify_irg(res, current_ir_graph);
	return res;
}

ir_node *new_d_End(dbg_info *db)
{
	ir_node *res;
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	res = new_ir_node(db, current_ir_graph,  current_ir_graph->current_block,
	                  op_End, mode_X, -1, NULL);
	res = optimize_node(res);
	irn_verify_irg(res, current_ir_graph);

	return res;
}

/* ***********************************************************************/
/* Methods necessary for automatic Phi node creation                     */
/*
  ir_node *phi_merge            (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
  ir_node *get_r_value_internal (ir_node *block, int pos, ir_mode *mode);
  ir_node *new_rd_Phi0          (ir_graph *irg, ir_node *block, ir_mode *mode)
  ir_node *new_rd_Phi_in        (ir_graph *irg, ir_node *block, ir_mode *mode, ir_node **in, int ins)

  Call Graph:   ( A ---> B == A "calls" B)

       get_value         mature_immBlock
          |                   |
          |                   |
          |                   |
          |          ---> phi_merge
          |         /       /   \
          |        /       /     \
         \|/      /      |/_      \
       get_r_value_internal        |
                |                  |
                |                  |
               \|/                \|/
           new_rd_Phi0          new_rd_Phi_in

* *************************************************************************** */

/** Creates a Phi node with 0 predecessors. */
static inline ir_node *new_rd_Phi0(ir_node *block, ir_mode *mode)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(NULL, irg, block, op_Phi, mode, 0, NULL);
	irn_verify_irg(res, irg);
	return res;
}

/**
 * Internal constructor of a Phi node by a phi_merge operation.
 *
 * @param block  the block in which the Phi will be constructed
 * @param mode   the mod eof the Phi node
 * @param in     the input array of the phi node
 * @param n_in   number of elements in the input array
 * @param phi0   in non-NULL: the Phi0 node in the same block that represents
 *               the value for which the new Phi is constructed
 */
static ir_node *new_rd_Phi_in(ir_node *block, ir_mode *mode,
                              int n_in, ir_node **in, ir_node *phi0)
{
	int i;
	ir_node *res, *known;
	ir_graph *irg = get_irn_irg(block);

	/* Allocate a new node on the obstack.  The allocation copies the in
	   array. */
	res = new_ir_node(NULL, irg, block, op_Phi, mode, n_in, in);
	res->attr.phi.u.backedge = new_backedge_arr(irg->obst, n_in);

	/* This loop checks whether the Phi has more than one predecessor.
	   If so, it is a real Phi node and we break the loop.  Else the
	   Phi node merges the same definition on several paths and therefore
	   is not needed.
	   Note: We MUST consider Bad nodes, else we might get data flow cycles in dead loops! */
	known = res;
	for (i = n_in - 1; i >= 0; --i) {
		assert(in[i]);

		in[i] = skip_Id(in[i]);  /* increases the number of freed Phis. */

		/* Optimize self referencing Phis:  We can't detect them yet properly, as
		they still refer to the Phi0 they will replace.  So replace right now. */
		if (phi0 && in[i] == phi0)
			in[i] = res;

		if (in[i] == res || in[i] == known)
			continue;

		if (known == res)
			known = in[i];
		else
			break;
	}

	/* i < 0: there is at most one predecessor, we don't need a phi node. */
	if (i < 0) {
		if (res != known) {
			edges_node_deleted(res, current_ir_graph);
			obstack_free(current_ir_graph->obst, res);
			if (is_Phi(known)) {
				/* If pred is a phi node we want to optimize it: If loops are matured in a bad
				   order, an enclosing Phi know may get superfluous. */
				res = optimize_in_place_2(known);
				if (res != known)
					exchange(known, res);
			}
			else
				res = known;
		} else {
			/* A undefined value, e.g., in unreachable code. */
			res = new_r_Bad(irg);
		}
	} else {
		res = optimize_node(res);  /* This is necessary to add the node to the hash table for cse. */
		irn_verify_irg(res, irg);
		/* Memory Phis in endless loops must be kept alive.
		   As we can't distinguish these easily we keep all of them alive. */
		if (is_Phi(res) && mode == mode_M)
			add_End_keepalive(get_irg_end(irg), res);
	}

	return res;
}

static ir_node *get_r_value_internal(ir_node *block, int pos, ir_mode *mode);

/**
 * Computes the predecessors for the real phi node, and then
 * allocates and returns this node.  The routine called to allocate the
 * node might optimize it away and return a real value.
 * This function must be called with an in-array of proper size.
 */
static ir_node *phi_merge(ir_node *block, int pos, ir_mode *mode,
                          int n_ins, ir_node **ins)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node *prevBlock, *res, *phi0, *phi0_all;
	int i;

	/* If this block has no value at pos create a Phi0 and remember it
	   in graph_arr to break recursions.
	   Else we may not set graph_arr as there a later value is remembered. */
	phi0 = NULL;
	if (block->attr.block.graph_arr[pos] == NULL) {

		if (block == get_irg_start_block(irg)) {
			/* Collapsing to Bad tarvals is no good idea.
			   So we call a user-supplied routine here that deals with this
			   case as appropriate for the given language. Sorrily the only
			   help we can give here is the position.

			   Even if all variables are defined before use, it can happen that
			   we get to the start block, if a Cond has been replaced by a tuple
			   (bad, jmp).  In this case we call the function needlessly,
			   eventually generating an non existent error.
			   However, this SHOULD NOT HAPPEN, as bad control flow nodes are
			   intercepted before recurring.
			 */
			if (default_initialize_local_variable != NULL) {
				ir_node *rem = get_r_cur_block(irg);

				set_r_cur_block(irg, block);
				block->attr.block.graph_arr[pos] = default_initialize_local_variable(irg, mode, pos - 1);
				set_r_cur_block(irg, rem);
			} else {
				block->attr.block.graph_arr[pos] = new_r_Unknown(irg, mode);
			}
			return block->attr.block.graph_arr[pos];
		} else {
			phi0 = new_rd_Phi0(block, mode);
			block->attr.block.graph_arr[pos] = phi0;
		}
	}

	/* This loop goes to all predecessor blocks of the block the Phi node
	   is in and there finds the operands of the Phi node by calling
	   get_r_value_internal.  */
	for (i = 1; i <= n_ins; ++i) {
		ir_node *cf_pred = block->in[i];
		ir_node *prevCfOp = skip_Proj(cf_pred);
		assert(prevCfOp);
		if (is_Bad(prevCfOp)) {
			/* In case a Cond has been optimized we would get right to the start block
			with an invalid definition. */
			ins[i-1] = new_r_Bad(irg);
			continue;
		}
		prevBlock = prevCfOp->in[0]; /* go past control flow op to prev block */
		assert(prevBlock);
		if (!is_Bad(prevBlock)) {
			ins[i-1] = get_r_value_internal(prevBlock, pos, mode);
		} else {
			ins[i-1] = new_r_Bad(irg);
		}
	}

	/* We want to pass the Phi0 node to the constructor: this finds additional
	   optimization possibilities.
	   The Phi0 node either is allocated in this function, or it comes from
	   a former call to get_r_value_internal(). In this case we may not yet
	   exchange phi0, as this is done in mature_immBlock(). */
	if (phi0 == NULL) {
		phi0_all = block->attr.block.graph_arr[pos];
		if (! is_Phi0(phi0_all)            ||
		    get_irn_arity(phi0_all) != 0   ||
		    get_nodes_block(phi0_all) != block)
			phi0_all = NULL;
	} else {
		phi0_all = phi0;
	}

	/* After collecting all predecessors into the array ins a new Phi node
	   with these predecessors is created.  This constructor contains an
	   optimization: If all predecessors of the Phi node are identical it
	   returns the only operand instead of a new Phi node.  */
	res = new_rd_Phi_in(block, mode, n_ins, ins, phi0_all);

	/* In case we allocated a Phi0 node at the beginning of this procedure,
	   we need to exchange this Phi0 with the real Phi. */
	if (phi0 != NULL) {
		exchange(phi0, res);
		block->attr.block.graph_arr[pos] = res;
	}

	return res;
}

/**
 * This function returns the last definition of a value.  In case
 * this value was last defined in a previous block, Phi nodes are
 * inserted.  If the part of the firm graph containing the definition
 * is not yet constructed, a dummy Phi node is returned.
 *
 * @param block   the current block
 * @param pos     the value number of the value searched
 * @param mode    the mode of this value (needed for Phi construction)
 */
static ir_node *get_r_value_internal(ir_node *block, int pos, ir_mode *mode)
{
	ir_node *res;
	/* There are 4 cases to treat.

	   1. The block is not mature and we visit it the first time.  We can not
	      create a proper Phi node, therefore a Phi0, i.e., a Phi without
	      predecessors is returned.  This node is added to the linked list (block
	      attribute "phis") of the containing block to be completed when this block is
	      matured. (Completion will add a new Phi and turn the Phi0 into an Id
	      node.)

	   2. The value is already known in this block, graph_arr[pos] is set and we
	      visit the block the first time.  We can return the value without
	      creating any new nodes.

	   3. The block is mature and we visit it the first time.  A Phi node needs
	      to be created (phi_merge).  If the Phi is not needed, as all it's
	      operands are the same value reaching the block through different
	      paths, it's optimized away and the value itself is returned.

	   4. The block is mature, and we visit it the second time.  Now two
	      subcases are possible:
	      * The value was computed completely the last time we were here. This
	        is the case if there is no loop.  We can return the proper value.
	      * The recursion that visited this node and set the flag did not
	        return yet.  We are computing a value in a loop and need to
	        break the recursion.  This case only happens if we visited
	    the same block with phi_merge before, which inserted a Phi0.
	    So we return the Phi0.
	*/

	/* case 4 -- already visited. */
	if (irn_visited(block)) {
		/* As phi_merge allocates a Phi0 this value is always defined. Here
		is the critical difference of the two algorithms. */
		assert(block->attr.block.graph_arr[pos]);
		return block->attr.block.graph_arr[pos];
	}

	/* visited the first time */
	mark_irn_visited(block);

	/* Get the local valid value */
	res = block->attr.block.graph_arr[pos];

	/* case 2 -- If the value is actually computed, return it. */
	if (res != NULL)
		return res;

	if (block->attr.block.is_matured) { /* case 3 */

		/* The Phi has the same amount of ins as the corresponding block. */
		int n_in = get_irn_arity(block);
		ir_node **in;
		NEW_ARR_A(ir_node *, in, n_in);

		/* Phi merge collects the predecessors and then creates a node. */
		res = phi_merge(block, pos, mode, n_in, in);
	} else {  /* case 1 */
		/* The block is not mature, we don't know how many in's are needed.  A Phi
		   with zero predecessors is created.  Such a Phi node is called Phi0
		   node.  The Phi0 is then added to the list of Phi0 nodes in this block
		   to be matured by mature_immBlock later.
		   The Phi0 has to remember the pos of it's internal value.  If the real
		   Phi is computed, pos is used to update the array with the local
		   values. */
		res = new_rd_Phi0(block, mode);
		res->attr.phi.u.pos    = pos;
		res->attr.phi.next     = block->attr.block.phis;
		block->attr.block.phis = res;
	}

	assert(is_ir_node(res) && "phi_merge() failed to construct a definition");

	/* The local valid value is available now. */
	block->attr.block.graph_arr[pos] = res;

	return res;
}

/* ************************************************************************** */

/*
 * Finalize a Block node, when all control flows are known.
 * Acceptable parameters are only Block nodes.
 */
void mature_immBlock(ir_node *block)
{
	int ins;
	ir_node *n, **nin;
	ir_node *next;

	assert(is_Block(block));
	if (!get_Block_matured(block)) {
		ir_graph *irg = current_ir_graph;

		ins = ARR_LEN(block->in) - 1;
		/* Fix block parameters */
		block->attr.block.backedge = new_backedge_arr(irg->obst, ins);

		/* An array for building the Phi nodes. */
		NEW_ARR_A(ir_node *, nin, ins);

		/* Traverse a chain of Phi nodes attached to this block and mature
		these, too. **/
		for (n = block->attr.block.phis; n; n = next) {
			inc_irg_visited(irg);
			next = n->attr.phi.next;
			exchange(n, phi_merge(block, n->attr.phi.u.pos, n->mode, ins, nin));
		}

		block->attr.block.is_matured = 1;

		/* Now, as the block is a finished Firm node, we can optimize it.
		   Since other nodes have been allocated since the block was created
		   we can not free the node on the obstack.  Therefore we have to call
		   optimize_in_place().
		   Unfortunately the optimization does not change a lot, as all allocated
		   nodes refer to the unoptimized node.
		   We can call optimize_in_place_2(), as global cse has no effect on blocks. */
		block = optimize_in_place_2(block);
		irn_verify_irg(block, irg);
	}
}

ir_node *new_d_Phi(dbg_info *db, int arity, ir_node **in, ir_mode *mode)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_Phi(db, current_ir_graph->current_block, arity, in, mode);
}

ir_node *new_d_Const(dbg_info *db, ir_tarval *con)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_Const(db, current_ir_graph, con);
}

ir_node *new_d_Const_long(dbg_info *db, ir_mode *mode, long value)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_Const_long(db, current_ir_graph, mode, value);
}

ir_node *new_d_defaultProj(dbg_info *db, ir_node *arg, long max_proj)
{
	ir_node *res;
	assert(is_Cond(arg));
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	arg->attr.cond.default_proj = max_proj;
	res = new_d_Proj(db, arg, mode_X, max_proj);
	return res;
}

ir_node *new_d_simpleSel(dbg_info *db, ir_node *store, ir_node *objptr,
                         ir_entity *ent)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_Sel(db, current_ir_graph->current_block,
	                  store, objptr, 0, NULL, ent);
}

ir_node *new_d_SymConst(dbg_info *db, ir_mode *mode, symconst_symbol value,
                        symconst_kind kind)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_SymConst(db, current_ir_graph, mode, value, kind);
}

ir_node *new_d_Sync(dbg_info *db, int arity, ir_node *in[])
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_Sync(db, current_ir_graph->current_block, arity, in);
}

ir_node *new_d_ASM(dbg_info *db, int arity, ir_node *in[],
                   ir_asm_constraint *inputs,
                   int n_outs, ir_asm_constraint *outputs, int n_clobber,
                   ident *clobber[], ident *text)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return new_rd_ASM(db, current_ir_graph->current_block, arity, in, inputs,
	                  n_outs, outputs, n_clobber, clobber, text);
}

ir_node *new_rd_immBlock(dbg_info *dbgi, ir_graph *irg)
{
	ir_node *res;

	assert(get_irg_phase_state(irg) == phase_building);
	/* creates a new dynamic in-array as length of in is -1 */
	res = new_ir_node(dbgi, irg, NULL, op_Block, mode_BB, -1, NULL);

	res->attr.block.is_matured  = 0;
	res->attr.block.is_dead     = 0;
	res->attr.block.irg.irg     = irg;
	res->attr.block.backedge    = NULL;
	res->attr.block.in_cg       = NULL;
	res->attr.block.cg_backedge = NULL;
	res->attr.block.extblk      = NULL;
	res->attr.block.region      = NULL;
	res->attr.block.entity      = NULL;

	set_Block_block_visited(res, 0);

	/* Create and initialize array for Phi-node construction. */
	res->attr.block.graph_arr = NEW_ARR_D(ir_node *, irg->obst, irg->n_loc);
	memset(res->attr.block.graph_arr, 0, sizeof(ir_node*) * irg->n_loc);

	/* Immature block may not be optimized! */
	irn_verify_irg(res, irg);

	return res;
}

ir_node *new_r_immBlock(ir_graph *irg)
{
	return new_rd_immBlock(NULL, irg);
}

ir_node *new_d_immBlock(dbg_info *dbgi)
{
	return new_rd_immBlock(dbgi, current_ir_graph);
}

ir_node *new_immBlock(void)
{
	return new_rd_immBlock(NULL, current_ir_graph);
}

void add_immBlock_pred(ir_node *block, ir_node *jmp)
{
	int n = ARR_LEN(block->in) - 1;

	assert(is_Block(block) && "Error: Must be a Block");
	assert(!block->attr.block.is_matured && "Error: Block already matured!\n");
	assert(is_ir_node(jmp));

	ARR_APP1(ir_node *, block->in, jmp);
	/* Call the hook */
	hook_set_irn_n(block, n, jmp, NULL);
}

void set_cur_block(ir_node *target)
{
	current_ir_graph->current_block = target;
}

void set_r_cur_block(ir_graph *irg, ir_node *target)
{
	irg->current_block = target;
}

ir_node *get_r_cur_block(ir_graph *irg)
{
	return irg->current_block;
}

ir_node *get_cur_block(void)
{
	return get_r_cur_block(current_ir_graph);
}

ir_node *get_r_value(ir_graph *irg, int pos, ir_mode *mode)
{
	assert(get_irg_phase_state(irg) == phase_building);
	inc_irg_visited(irg);

	assert(pos >= 0);

	return get_r_value_internal(irg->current_block, pos + 1, mode);
}

ir_node *get_value(int pos, ir_mode *mode)
{
	return get_r_value(current_ir_graph, pos, mode);
}

/**
 * helper function for guess_mode: recursively look for a definition for
 * local variable @p pos, returns its mode if found.
 */
static ir_mode *guess_recursively(ir_node *block, int pos)
{
	ir_node *value;
	int      n_preds;
	int      i;

	if (irn_visited(block))
		return NULL;
	mark_irn_visited(block);

	/* already have a defintion -> we can simply look at its mode */
	value = block->attr.block.graph_arr[pos];
	if (value != NULL)
		return get_irn_mode(value);

	/* now we try to guess, by looking at the predecessor blocks */
	n_preds = get_irn_arity(block);
	for (i = 0; i < n_preds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_mode *mode       = guess_recursively(pred_block, pos);
		if (mode != NULL)
			return mode;
	}

	/* no way to guess */
	return NULL;
}

ir_mode *ir_guess_mode(int pos)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *block = irg->current_block;
	ir_node  *value = block->attr.block.graph_arr[pos+1];
	ir_mode  *mode;

	/* already have a defintion -> we can simply look at its mode */
	if (value != NULL)
		return get_irn_mode(value);

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(current_ir_graph);
	mode = guess_recursively(block, pos+1);
	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	return mode;
}

void set_r_value(ir_graph *irg, int pos, ir_node *value)
{
	assert(get_irg_phase_state(irg) == phase_building);
	assert(pos >= 0);
	assert(pos+1 < irg->n_loc);
	assert(is_ir_node(value));
	irg->current_block->attr.block.graph_arr[pos + 1] = value;
}

void set_value(int pos, ir_node *value)
{
	set_r_value(current_ir_graph, pos, value);
}

int find_value(ir_node *value)
{
	int i;
	ir_node *bl = current_ir_graph->current_block;

	for (i = ARR_LEN(bl->attr.block.graph_arr) - 1; i >= 1; --i)
		if (bl->attr.block.graph_arr[i] == value)
			return i - 1;
	return -1;
}

ir_node *get_r_store(ir_graph *irg)
{
	assert(get_irg_phase_state(irg) == phase_building);
	inc_irg_visited(irg);
	return get_r_value_internal(irg->current_block, 0, mode_M);
}

ir_node *get_store(void)
{
	return get_r_store(current_ir_graph);
}

void set_r_store(ir_graph *irg, ir_node *store)
{
	ir_node *load, *pload, *pred, *in[2];

	assert(get_irg_phase_state(irg) == phase_building);
	/* Beware: due to dead code elimination, a store might become a Bad node even in
	   the construction phase. */
	assert((get_irn_mode(store) == mode_M || is_Bad(store)) && "storing non-memory node");

	if (get_opt_auto_create_sync()) {
		/* handle non-volatile Load nodes by automatically creating Sync's */
		load = skip_Proj(store);
		if (is_Load(load) && get_Load_volatility(load) == volatility_non_volatile) {
			pred = get_Load_mem(load);

			if (is_Sync(pred)) {
				/* a Load after a Sync: move it up */
				ir_node *mem = skip_Proj(get_Sync_pred(pred, 0));

				set_Load_mem(load, get_memop_mem(mem));
				add_Sync_pred(pred, store);
				store = pred;
			} else {
				pload = skip_Proj(pred);
				if (is_Load(pload) && get_Load_volatility(pload) == volatility_non_volatile) {
					/* a Load after a Load: create a new Sync */
					set_Load_mem(load, get_Load_mem(pload));

					in[0] = pred;
					in[1] = store;
					store = new_r_Sync(irg->current_block, 2, in);
				}
			}
		}
	}
	irg->current_block->attr.block.graph_arr[0] = store;
}

void set_store(ir_node *store)
{
	set_r_store(current_ir_graph, store);
}

void keep_alive(ir_node *ka)
{
	add_End_keepalive(get_irg_end(current_ir_graph), ka);
}

void ir_set_uninitialized_local_variable_func(
		uninitialized_local_variable_func_t *func)
{
	default_initialize_local_variable = func;
}

void irg_finalize_cons(ir_graph *irg)
{
	set_irg_phase_state(irg, phase_high);
}

void irp_finalize_cons(void)
{
	int i;
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		irg_finalize_cons(get_irp_irg(i));
	}
	irp->phase_state = phase_high;
}

ir_node *new_Start(void)
{
	return new_d_Start(NULL);
}
ir_node *new_End(void)
{
	return new_d_End(NULL);
}
ir_node *new_Const(ir_tarval *con)
{
	return new_d_Const(NULL, con);
}

ir_node *new_Const_long(ir_mode *mode, long value)
{
	return new_d_Const_long(NULL, mode, value);
}

ir_node *new_SymConst(ir_mode *mode, symconst_symbol value, symconst_kind kind)
{
	return new_d_SymConst(NULL, mode, value, kind);
}
ir_node *new_simpleSel(ir_node *store, ir_node *objptr, ir_entity *ent)
{
	return new_d_simpleSel(NULL, store, objptr, ent);
}
ir_node *new_Phi(int arity, ir_node **in, ir_mode *mode)
{
	return new_d_Phi(NULL, arity, in, mode);
}
ir_node *new_Sync(int arity, ir_node *in[])
{
	return new_d_Sync(NULL, arity, in);
}
ir_node *new_defaultProj(ir_node *arg, long max_proj)
{
	return new_d_defaultProj(NULL, arg, max_proj);
}
ir_node *new_Bad(void)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return get_irg_bad(current_ir_graph);
}
ir_node *new_NoMem(void)
{
	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	return get_irg_no_mem(current_ir_graph);
}
ir_node *new_ASM(int arity, ir_node *in[], ir_asm_constraint *inputs,
                 int n_outs, ir_asm_constraint *outputs,
                 int n_clobber, ident *clobber[], ident *text)
{
	return new_d_ASM(NULL, arity, in, inputs, n_outs, outputs, n_clobber, clobber, text);
}

ir_node *new_r_Anchor(ir_graph *irg)
{
	ir_node *in[anchor_last];
	ir_node *res;
	memset(in, 0, sizeof(in));
	res = new_ir_node(NULL, irg, NULL, op_Anchor, mode_ANY, anchor_last, in);

	/* hack to get get_irn_irg working: set block to ourself and allow
	 * get_Block_irg for anchor */
	res->attr.irg.irg = irg;
	res->in[0] = res;

	return res;
}
