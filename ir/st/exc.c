/*
 * Project:     libFIRM
 * File name:   ir/st/exc.c
 * Purpose:     Helper functions for jack exceptions.
 * Author:      Florian Liekweg
 * Modified by:
 * Created:     4.3.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
   NAME
     exc
   PURPOSE
     Helper functions for exceptions
   S
     not quite complete
***/

# include "exc.h"

static char* exc_strings [] = {
  "Invalid",					/* invalid */
  "Normal",						/* normal */
  "Entry",						/* entry to region */
  "Exit",						/* exit of region */
  "Handler",					/* entry to handler */
  "Cont"
};


/*
  Return true iff (block b is a handler entry AND a dominates b) OR
  (b has a CFG successor that is both a handler entry AND is dominated
  by a)
*/
static bool has_handler (ir_graph *graph, ir_node *b, ir_node *a)
{
  int i         = 0;
  int n         = get_irn_n_outs (b);
  ir_node *succ = 0;

  assert (0 && "Wrongly implemented");
  /* must check for _immediate_ dominance !!! */

  if (is_handler_entry (graph, b) && dominates (graph, a, b))
	return (true);

  for (i = 0; i < n; i ++)
	{
	  succ = get_irn_out (b, i);

	  if (has_handler (graph, succ, a))
		return (true);
	}

  return (false);
}

/*
  Return true iff the given node represents an exception jump
*/
static bool is_exc_jmp (ir_node *node)
{
  ir_op *op = get_irn_op (node);

  /* Proj_X (Load), Proj_X (Sto), Proj_X (Div_Int),
     Proj_X (Raise), Proj_X (Call), Proj_X (Alloc) */
  if (op == op_Proj)
	{
	  op = get_irn_op (get_Proj_pred (node));

	  assert ((is_fragile_op(get_Proj_pred(node))) &&
		  (op != op_Bad) /*&& (op != op_Unknown)*/ &&
		  (get_irn_mode(node) == mode_X));/* Check for proper Proj attr */
	  return (true);
	}
  else
	{
	  return (false);
	}
}

/*
Return true iff the given node represents a normal cfg jump
*/
#if 0
static bool is_cfg_jmp (ir_node *node)
{
  ir_op *op = get_irn_op (node);

  if (op == op_Proj)
    {
      op = get_irn_op (get_Proj_pred (node));

      /* Proj_X (Proj_Cmp (Cond)) */
      if (op_Proj == op)
	return (true);	/* check for op == op_Cmp and op == op_Cond */
    }

  return (false);
}
#endif

void set_Block_exc(ir_node *n, exc_t exc) {
}

exc_t get_Block_exc(ir_node *n) {
  return 0;
}


/* handler handling for Blocks */
void
set_Block_handler (ir_node *block, ir_node *handler)  {
  assert (is_Block(block));
  assert (is_Block(handler));
}

ir_node *
get_Block_handler (ir_node *block) {
  assert (is_Block(block));
  return (NULL);
}

/* handler handling for Nodes */
void
set_Node_handler (ir_node *node, ir_node *handler) {

}

ir_node *
get_Node_handler (ir_node *node) {
  return (NULL);
}


/*
 Return true iff a new exception region must be left upon entry of this block.

 If all CFG preds of this block are exception jumps, then we must
 return true.
*/
bool is_handler_entry (ir_graph *graph, ir_node *block)
{
  bool is_entry = true;
  int  i        = 0;
  int  n        = get_irn_arity (block);

  if (exc_invalid == get_Block_exc (block))
	{
	  for (i = 0; (i < n) && (is_entry == true); i ++)
		if (is_exc_jmp (get_irn_n (block, i)))
		  continue;
		else
		  is_entry = false;

	  if (true == is_entry)
		set_Block_exc (block, exc_handler);
	}

  return (exc_handler == get_Block_exc (block));
}

/*
 Return true iff a new exception region must be started upon entry of this block.

 If this block immediately dominates a handler entry, we must return true.
*/
bool is_region_entry  (ir_graph *graph, ir_node *block)
{
  assert (0 && "Not implemented");

  if (exc_invalid == get_Block_exc (block))
	{
	  int i         = 0;
	  int n         = get_irn_n_outs (block);
	  ir_node *succ = 0;

	  bool no_handler = true;

	  for (i = 0; (i < n) && (no_handler == true); i ++)
		{
		  succ = get_irn_out (block, i);

		  if (has_handler (graph, succ, block))
			no_handler = false;
		}

	  if (false == no_handler)
		set_Block_exc (block, exc_region);
	}

  return (exc_region == get_Block_exc (block));

  return (true);
}

/*
 Return true iff this block is part of handler code.

 If this block is dominated by a block for which {@link
 is_handler_entry} is true, then this block is part of the handler.
*/
bool is_handler_block (ir_graph *graph, ir_node *block)
{
  assert (0 && "Not implemented");

  if (exc_invalid == get_Block_exc (block))
	{
	  bool no_handler = true;
	  dom_env_t *env  = get_dom_env (graph, block);
	  int block_index = env->index_a;
	  bs_t block_mask = 0x00000001 << block_index;
	  int n_blocks    = env->dt->n_blocks;
	  int i           = 0;

	  for (i = 0; (i < n_blocks) && (no_handler == true); i ++)
		if (0 != (env->dt->masks [i] & block_mask)) /* if dominator */
		  if (is_handler_entry (graph, env->dt->blocks [i])) /* is handler entry */
			no_handler = false;

	  delete_dom_env (env);

	  if (false == no_handler)
		set_Block_exc (block, exc_handler);
	}

  return (exc_handler == get_Block_exc (block));
}

/*
  Convert a value of type exc_t to a descriptive string.
  Returns a reference to a statically allocated, constant string.
*/

const char *exc_to_string (exc_t exc)
{
  int exc_val = (int) exc;

  assert ((0 <= (int) exc_val) && (exc_val < (int) exc_max));

  return (exc_strings [exc_val]);
}
