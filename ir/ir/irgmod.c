/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod: ir graph modification
*/

# include "irgmod.h"
# include "iropt.h"

/*  ir_node * */
/*  arg_access (ir_mode *mode, long proj) */
/*  { */
/*    return new_r_Proj (current_ir_graph, current_ir_graph->start,  */
/*  		     current_ir_graph->args, mode, proj); */
/*  } */


/* these two functions are used in phi_merge, but defined in ircons.c */
inline ir_node * get_r_value_internal (ir_node *block,
				       int pos, ir_mode *mode);
inline ir_node * new_r_Phi_in (ir_graph *irg, ir_node *block, ir_mode *mode,
			       ir_node **in, int ins);

/** This function computes the predecessors for a real Phi node, and then
    allocates and returns this node.  The routine called to allocate the
    node might optimize it away and return a real value, or even a pointer
    to a deallocated Phi node on top of the obstack!
    This function is called with an in-array of proper size. **/
static inline ir_node *
phi_merge (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
{
  ir_node *prevBlock, *res;
  int i;

  /* This loop goes to all predecessor blocks of the block the Phi node is in
     and there finds the operands of the Phi node by calling get_r_value_internal. */
  for (i = 1;  i <= ins;  ++i) {
    assert (block->in[i]);
    prevBlock = block->in[i]->in[0]; /* go past control flow op to prev block */
    assert (prevBlock);
    nin[i-1] = get_r_value_internal (prevBlock, pos, mode);
  }

  /* After collecting all predecessors into the array nin a new Phi node
     with these predecessors is created.  This constructor contains an
     optimization: If all predecessors of the Phi node are identical it
     returns the only operand instead of a new Phi node.  If the value
     passes two different control flow edges without being defined, and
     this is the second path treated, a pointer to the node that will be
     allocated for the first path (recurstion) is returned.  We already
     know the address of this node, as it is the next node to be allocated
     and will be placed on top of the obstack. (The obstack is a _stack_!) */
  res = new_r_Phi_in (current_ir_graph, block, mode, nin, ins);

  /* Now we now the value "pos" and can enter it in the array with
     all known local variables.  Attention: this might be a pointer to
     a node, that later will be allocated!!! See new_r_Phi_in.
     If this is called in mature, after some set_value in the same block,
     the proper value must not be overwritten:
     The call order
       get_value    (makes Phi0, put's it into graph_arr)
       set_value    (overwrites Phi0 in graph_arr)
       mature_block (upgrades Phi0, puts it again into graph_arr, overwriting
                     the proper value.)
     fails. */
  if (!block->attr.block.graph_arr[pos]) {
    block->attr.block.graph_arr[pos] = res;
  } else {
    //  printf(" value already computed by %s\n",
    //  id_to_str(block->attr.block.graph_arr[pos]->op->name));
  }

  return res;
}


/* this function is used in get_r_value_internal, but defined in ircons.c */
inline ir_node * new_r_Phi0 (ir_graph *irg, ir_node *block, ir_mode *mode);


/* This function returns the last definition of a variable.  In case
   this variable was last defined in a previous block, Phi nodes are
   inserted.  If the part of the firm graph containing the definition
   is not yet constructed, a dummy Phi node is returned. */
inline ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode)
{
  ir_node *res;
  /* There are 4 cases to treat.

     1. The block is not mature and we visit it the first time.  We can not
        create a proper Phi node, therefore a Phi0, i.e., a Phi without
        predecessors is returned.  This node is added to the linked list (field
        "link") of the containing block to be completed when this block is
        matured. (Comlpletion will add a new Phi and turn the Phi0 into a Id
        node.)

     2. The value is already known in this block, graph_arr[pos] is set and we
        visit the block the first time.  We can return the value without
        creating any new nodes.

     3. The block is mature and we visit it the first time.  A Phi node needs
        to be created (phi_merge).  If the Phi is not needed, as all it's
        operands are the same value reaching the block through different
        paths, it's optimizes away and the value itself is returned.

     4. The block is mature, and we visit it the second time.  Now two
        subcases are possible:
        * The value was computed completely the last time we were here.
          This is the case if there is no loop.  We can return the proper value.
        * The recursion that visited this node and set the flag did not
          return yet.  We are computing a value in a loop and need to
          break the recursion without knowing the result yet.
        There is no simple check for the second subcase.  Therefore we check
        for a second visit and treat all such cases as the second subcase.
        Anyways, the basic situation is the same:  we reached a block
        on two paths without finding a definition of the value:  No Phi
        nodes are needed on both paths.
        We return this information "Two paths, no Phi needed" by a very tricky
        implementation that relies on the fact that an obstack is a stack and
        will return a node with the same address on different allocations.
        Look also at phi_merge and get_r_phi_in to understand this.
  */

  /* case 4 -- already visited. */
  if (block->visit == ir_visited) return NULL;

  /* visited the first time */
  block->visit = ir_visited;

  /* Get the local valid value */
  res = block->attr.block.graph_arr[pos];

  /* case 2 -- If the value is actually computed, return it. */
  if (res) { return res;};

  if (block->attr.block.matured) { /* case 3 */

    /* The Phi has the same amount of ins as the corresponding block. */
    int ins = get_irn_arity(block); // ARR_LEN (block->in)-1;
    ir_node **nin;
    NEW_ARR_A (ir_node *, nin, ins);

    /* Phi merge collects the predecessors and then creates a node. */
    res = phi_merge (block, pos, mode, nin, ins);

  } else {  /* case 1 */
    /* The block is not mature, we don't know how many in's are needed.  A Phi
       with zero predecessors is created.  Such a Phi node is called Phi0
       node.  (There is also an obsolete Phi0 opcode.) The Phi0 is then added
       to the list of Phi0 nodes in this block to be matured by mature_block
       later.
       The Phi0 has to remember the pos of it's internal value.  If the real Phi
       is computed, pos is used to update the array with the local values. */

    res = new_r_Phi0 (current_ir_graph, block, mode);
    res->attr.phi0_pos = pos;
    res->link = block->link;
    block->link = res;
  }

  /* If we get here, the frontend missed a use-before-definition error */
  if (!res) {
    /* Error Message */
    printf("Error: no value set\n");
    assert (mode->code >= irm_f && mode->code <= irm_p);
    res = new_r_Const (current_ir_graph, block, mode,
		       tarval_mode_null[mode->code]);
  }

  /* The local valid value is available now. */
  block->attr.block.graph_arr[pos] = res;

  return res;
}


/* add an adge to a jmp node */
void
add_in_edge (ir_node *block, ir_node *jmp)
{
  if (block->attr.block.matured) {
    printf("Error: Block already matured!\n");
  }
  else {
    assert (jmp != NULL);
    ARR_APP1 (ir_node *, block->in, jmp);
  }
}


/****************************/
/* parameter administration */

/* get a value from the parameter array from the current block by its index */
ir_node *
get_value (int pos, ir_mode *mode)
{
  ++ir_visited;
  return get_r_value_internal (current_ir_graph->current_block, pos + 1, mode);
}


/* set a value at position pos in the parameter array from the current block */
inline void
set_value (int pos, ir_node *value)
{
  current_ir_graph->current_block->attr.block.graph_arr[pos + 1] = value;
}


/* get the current store */
inline ir_node *
get_store (void)
{
  /* GL: one could call get_value instead */
  ++ir_visited;
  return get_r_value_internal (current_ir_graph->current_block, 0, mode_M);
}


/* set the current store */
inline void
set_store (ir_node *store)
{
  /* GL: one could call set_value instead */
  current_ir_graph->current_block->attr.block.graph_arr[0] = store;
}

/** Finalize a Block node, when all control flows are known.  */
/** Acceptable parameters are only Block nodes.               */
void
mature_block (ir_node *block)
{

  int ins;
  ir_node *n, **nin;
  ir_node *next;

  assert (get_irn_opcode(block) == iro_Block);

  if (!get_Block_matured(block)) {

    /* turn the dynamic in-array into a static one. */
    ins = ARR_LEN (block->in)-1;
    NEW_ARR_A (ir_node *, nin, ins);

    /* GL, 7.2.2000.  seems to be not complete. added this: but does'nt work.*
    memcpy (nin, block->in, sizeof (ir_node *) * ins);
    block->in = nin;
    */

    /* Traverse a chain of Phi nodes attached to this block and mature these, too. **/
    for (n = block->link;  n;  n=next) {
      ir_visited++;
      next = n->link;
      exchange (n, phi_merge (block, n->attr.phi0_pos, n->mode, nin, ins));
    }

    block->attr.block.matured = 1;

    block = optimize_in_place(block);
    ir_vrfy(block);
  }

}

/* changing the current block */
void
switch_block (ir_node *target)
{
  current_ir_graph->current_block = target;
}


void
turn_into_tuple (ir_node *node, int arity)
{
  assert(node);
  set_irn_op(node, op_Tuple);
  if (get_irn_arity(node) == arity) {
    /* keep old array */
  } else {
    /* allocate new array, remove old one. */
    /* !!!??? free old in_array */
    node->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, arity+1);
  }
}



/* Insert irnode `new' in place of irnode `old'
   Since `new' may be bigger than `old' replace `old'
   by an op_Id which is smaller than everything */
inline void
exchange (ir_node *old, ir_node *new)
{
  ir_node *block = old->in[0];

  old->op = op_Id;
  old->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, 2);
  old->in[0] = block;
  old->in[1] = new;
}
