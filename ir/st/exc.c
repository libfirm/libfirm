/* Copyright (c) 2002 by Universität Karlsruhe (TH).  All Rights Reserved */
//
// Time-stamp: <02/03/04 17:24:07 liekweg>
//

/***
   NAME
     exc
   PURPOSE
     Helper functions for exceptions
   NOTES
     not quite complete
   HISTORY
     liekweg - Mar 4, 2002: Created.
   CVS:
     $Id$
***/

# include "exc.h"

static char* exc_strings [] = {
  "Invalid",
  "Normal",
  "Region Entry",
  "Handler Entry",
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
  // must check for _immediate_ dominance !!!

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

  // Proj_X (Load), Proj_X (Sto), Proj_X (Div_Int),
  // Proj_X (Raise), Proj_X (Call), Proj_X (Alloc)
  if (op == op_Proj)
	{
	  op = get_irn_op (get_Proj_pred (node));

	  // ToDo: Check for proper Proj attr?!?
	  if ((op == op_Load) || (op == op_Store)  ||
		  (op == op_Div ) || (op == op_Raise)  ||
		  (op == op_Call) || (op == op_Alloc))
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
static bool is_cfg_jmp (ir_node *node)
{
  ir_op *op = get_irn_op (node);

  if (op == op_Proj)
	{
	  op = get_irn_op (get_Proj_pred (node));

	  // Proj_X (Proj_Cmp (Cond))
	  if (op_Proj == op)
		return (true);			/* check for op == op_Cmp and op == op_Cond */
	}

  return (false);
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
  ir_node *node = 0;
  ir_op* op     = op_Bad;

  if (exc_invalid == get_Block_exc (block))
	{
	  for (i = 0; (i < n) && is_entry; i ++)
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

	  for (i = 0; (i < n) && no_handler; i ++)
		{
		  succ = get_irn_out (block, i);

		  if (has_handler (graph, succ, block))
			no_handler = false;
		}

	  if (false == no_handler)
		set_Block_exc (block, exc_region);
	}

  return (exc_region == get_Block_exc (block));

  return (TRUE);
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

	  for (i = 0; (i < n_blocks) && no_handler; i ++)
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
 Return true iff a new exception region must be left upon exit of the
 non-exception blocks among the CFG predecessors of this block.

 If this block has CFG predecessors that are partly handler blocks and
 partly normal blocks, then we must return true.
*/
bool is_cont_entry    (ir_graph *graph, ir_node *block)
{
  assert (0 && "Not implemented");

  if (exc_invalid == get_Block_exc (block))
	{
	  bool has_exc = false;		/* wether we have exception cfg predecessors */
	  bool has_cfg = false;		/* wether we have normal cfg predecessors */

	  int i = 0;
	  int n = get_irn_arity (block);

	  ir_node *pred = 0;

	  for (i = 0; (i < n) && (!has_exc && !has_cfg); i ++)
		{
		  pred = get_irn_n (block, i);

		  if (is_exc_jmp (pred))
			has_exc = true;
		  else if (is_cfg_jmp (pred))
			has_cfg = true;
		}

	  if (has_cfg && has_exc)
		set_Block_exc (block, exc_cont);
	}

  return (exc_cont == get_Block_exc (block));
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
