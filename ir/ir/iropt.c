/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** iropt --- optimizations intertwined with IR construction.
*/

# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt_t.h"
# include "ircons.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "tv.h"
# include "tune.h"


/* Make types visible to allow most efficient access */
# include "entity_t.h"

/* Trivial inlineable routine for copy propagation.
   Does follow Ids, needed to optimize inlined code. */
static inline ir_node *
follow_Id (ir_node *n)
{
  while (get_irn_op (n) == op_Id) n = get_Id_pred (n);
  return n;
}

static inline tarval *
value_of (ir_node *n)
{
  if ((n != NULL) && (get_irn_op(n) == op_Const))
    return get_Const_tarval(n);
  else
    return NULL;
}

/* if n can be computed, return the value, else NULL. Performs
   constant folding. GL: Only if n is arithmetic operator? */
tarval *
computed_value (ir_node *n)
{
  tarval *res;

  ir_node *a = NULL, *b = NULL;	 /* initialized to shut up gcc */
  tarval *ta = NULL, *tb = NULL; /* initialized to shut up gcc */

  res = NULL;

  /* get the operands we will work on for simple cases. */
  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  /* if the operands are constants, get the target value, else set it NULL.
     (a and b may be NULL if we treat a node that is no computation.) */
  ta = value_of (a);
  tb = value_of (b);

  /* Perform the constant evaluation / computation. */
  switch (get_irn_opcode(n)) {
  case iro_Const:
    res = get_Const_tarval(n);
  case iro_Add:
    if (ta && tb && (get_irn_mode(a) == get_irn_mode(b))
	&& (get_irn_mode(a) != mode_p)) {
      res = tarval_add (ta, tb);
    }
    break;
  case iro_Sub:
    if (ta && tb && (get_irn_mode(a) == get_irn_mode(b))
	&& (get_irn_mode(a) != mode_p)) {
      res = tarval_sub (ta, tb);
    } else if (a == b) {
      res = tarval_mode_null [get_irn_modecode (n)];
    }
    break;
  case iro_Minus:
      if (ta && mode_is_float(get_irn_mode(a)))
        res = /*tarval_minus (ta);*/ res;
    break;
  case iro_Mul:
    if (ta && tb) /* tarval_mul tests for equivalent modes itself */ {
      res = tarval_mul (ta, tb);
    } else {
      /* calls computed_value recursive and returns the 0 with proper
         mode.  Why is this an extra case? */
      tarval *v;
      if (   (tarval_classify ((v = computed_value (a))) == 0)
	  || (tarval_classify ((v = computed_value (b))) == 0)) {
	res = v;
      }
    }
    break;
  case iro_Abs:
    if (ta)
      res = /*tarval_abs (ta);*/ res;
    /* allowed or problems with max/min ?? */
    break;
  case iro_And:
    if (ta && tb) {
      res = tarval_and (ta, tb);
    } else {
      tarval *v;
      if (   (tarval_classify ((v = computed_value (a))) == 0)
	  || (tarval_classify ((v = computed_value (b))) == 0)) {
	res = v;
      }
    }
    break;
  case iro_Or:
    if (ta && tb) {
      res = tarval_or (ta, tb);
    } else {
      tarval *v;
      if (   (tarval_classify ((v = computed_value (a))) == -1)
	  || (tarval_classify ((v = computed_value (b))) == -1)) {
	res = v;
      }
    }
    break;
  case iro_Eor: if (ta && tb) { res = tarval_eor (ta, tb); } break;
  case iro_Not: if (ta)       { /*res = tarval_not (ta)*/; } break;
  case iro_Shl: if (ta && tb) { res = tarval_shl (ta, tb); } break;
  case iro_Shr: if (ta && tb) { res = tarval_shr (ta, tb); } break;
  case iro_Shrs: if(ta && tb) { /*res = tarval_shrs (ta, tb)*/; } break;
  case iro_Rot: if (ta && tb) { /*res = tarval_rot (ta, tb)*/; } break;
  case iro_Conv: if(ta)    { res = tarval_convert_to (ta, get_irn_mode (n)); }
    break;
  case iro_Proj:
    {
      ir_node *aa, *ab;

      a = get_Proj_pred(n);
      /* Optimize Cmp nodes.
	 This performs a first step of unreachable code elimination.
	 Proj can not be computed, but folding a Cmp above the Proj here is
	 not as wasteful as folding a Cmp into a Tuple of 16 Consts of which
	 only 1 is used.
         There are several case where we can evaluate a Cmp node:
         1. The nodes compared are both the same.  If we compare for
            equal, this will return true, else it will return false.
            This step relies on cse.
         2. The predecessors of Cmp are target values.  We can evaluate
            the Cmp.
         3. The predecessors are Allocs or void* constants.  Allocs never
            return NULL, they raise an exception.   Therefore we can predict
            the Cmp result. */
      if (get_irn_op(a) == op_Cmp) {
        aa = get_Cmp_left(a);
        ab = get_Cmp_right(a);
	if (aa == ab) { /* 1.: */
          /* This is a tric with the bits used for encoding the Cmp
             Proj numbers, the following statement is not the same:
          res = tarval_from_long (mode_b, (get_Proj_proj(n) == Eq)):    */
	  res = tarval_from_long (mode_b, (get_Proj_proj(n) & irpn_Eq));
	} else {
	  tarval *taa = computed_value (aa);
	  tarval *tab = computed_value (ab);
	  if (taa && tab) { /* 2.: */
            /* strange checks... */
	    ir_pncmp flags = tarval_comp (taa, tab);
	    if (flags != irpn_False) {
              res = tarval_from_long (mode_b, get_Proj_proj(n) & flags);
	    }
	  } else {  /* check for 3.: */
            ir_node *aaa = skip_nop(skip_Proj(aa));
            ir_node *aba = skip_nop(skip_Proj(ab));
	    if (   (   (/* aa is ProjP and aaa is Alloc */
                           (get_irn_op(aa) == op_Proj)
    	                && (get_irn_mode(aa) == mode_p)
                        && (get_irn_op(aaa) == op_Alloc))
                    && (   (/* ab is constant void */
                               (get_irn_op(ab) == op_Const)
                            && (get_irn_mode(ab) == mode_p)
                            && (get_Const_tarval(ab) == tarval_p_void))
		        || (/* ab is other Alloc */
                               (get_irn_op(ab) == op_Proj)
    	                    && (get_irn_mode(ab) == mode_p)
                            && (get_irn_op(aba) == op_Alloc)
			    && (aaa != aba))))
		|| (/* aa is void and aba is Alloc */
                       (get_irn_op(aa) == op_Const)
                    && (get_irn_mode(aa) == mode_p)
                    && (get_Const_tarval(aa) == tarval_p_void)
                    && (get_irn_op(ab) == op_Proj)
    	            && (get_irn_mode(ab) == mode_p)
                    && (get_irn_op(aba) == op_Alloc)))
	      /* 3.: */
	      res = tarval_from_long (mode_b, get_Proj_proj(n) & irpn_Ne);
	  }
	}
      } else {
        /* printf(" # comp_val: Proj node, not optimized\n"); */
      }
    }
    break;
  default:  ;
  }

  return res;
}  /* compute node */



/* returns 1 if the a and b are pointers to different locations. */
bool
different_identity (ir_node *a, ir_node *b)
{
  assert (get_irn_mode (a) == mode_p
          && get_irn_mode (b) == mode_p);

  if (get_irn_op (a) == op_Proj && get_irn_op(b) == op_Proj) {
    ir_node *a1 = get_Proj_pred (a);
    ir_node *b1 = get_Proj_pred (b);
    if (a1 != b1 && get_irn_op (a1) == op_Alloc
                 && get_irn_op (b1) == op_Alloc)
      return 1;
  }
  return 0;
}


/* equivalent_node returns a node equivalent to N.  It skips all nodes that
   perform no actual computation, as, e.g., the Id nodes.  It does not create
   new nodes.  It is therefore safe to free N if the node returned is not N.
   If a node returns a Tuple we can not just skip it.  If the size of the
   in array fits, we transform n into a tuple (e.g., Div). */
static ir_node *
equivalent_node (ir_node *n)
{
  int ins;
  ir_node *a = NULL; /* to shutup gcc */
  ir_node *b = NULL; /* to shutup gcc */
  ir_node *c = NULL; /* to shutup gcc */

  ins = get_irn_arity (n);

  /* get the operands we will work on */
  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  /* skip unnecessary nodes. */
  switch (get_irn_opcode (n)) {
  case iro_Block:
    {
      /* The Block constructor does not call optimize, but mature_block
	 calls the optimization. */
      assert(get_Block_matured(n));

      /* A single entry Block following a single exit Block can be merged,
         if it is not the Start block. */
      /* !!! Beware, all Phi-nodes of n must have been optimized away.
	 This is true, as the block is matured before optimize is called.   */
      if (get_Block_n_cfgpreds(n) == 1
	  && get_irn_op(get_Block_cfgpred(n, 0)) == op_Jmp) {
	n = get_nodes_Block(get_Block_cfgpred(n, 0));

      } else if (n != current_ir_graph->start_block) {
	int i;
	/* If all inputs are dead, this block is dead too, except if it is
           the start block.  This is a step of unreachable code elimination */
	for (i = 0; i < get_Block_n_cfgpreds(n); i++) {
	  if (!is_Bad(get_Block_cfgpred(n, i))) break;
	}
	if (i == get_Block_n_cfgpreds(n))
	  n = new_Bad();
      }
    }
    break;

  case iro_Jmp:  /* GL: Why not same for op_Raise?? */
    /* unreachable code elimination */
    if (is_Bad(get_nodes_Block(n)))  n = new_Bad();
    break;
  /* We do not evaluate Cond here as we replace it by a new node, a Jmp.
     See cases for iro_Cond and iro_Proj in transform_node. */
  /** remove stuff as x+0, x*1 x&true ... constant expression evaluation **/
  case iro_Or:  if (a == b) {n = a; break;}
  case iro_Add:
  case iro_Eor:
    { tarval *tv;
      ir_node *on;
      /* After running compute_node there is only one constant predecessor.
         Find this predecessors value and remember the other node: */
      if ((tv = computed_value (a))) {
	on = b;
      } else if ((tv = computed_value (b))) {
	on = a;
      } else break;

      /* If this predecessors constant value is zero, the operation is
         unnecessary. Remove it: */
      if (tarval_classify (tv) == 0) {
	n = on;
      }
    }
    break;
  case iro_Sub:
  case iro_Shl:
  case iro_Shr:
  case iro_Shrs:
  case iro_Rot:
    /* these operations are not commutative.  Test only one predecessor. */
    if (tarval_classify (computed_value (b)) == 0) {
      n = a;
      /* Test if b > #bits of a ==> return 0 / divide b by #bits
         --> transform node? */
    }
    break;
  case iro_Not:   /* NotNot x == x */
  case iro_Minus: /* --x == x */  /* ??? Is this possible or can --x raise an
		                     out of bounds exception if min =! max? */
    if (get_irn_op(get_unop_op(n)) == get_irn_op(n))
      n = get_unop_op(get_unop_op(n));
    break;
  case iro_Mul:
    /* Mul is commutative and has again an other neutral element. */
    if (tarval_classify (computed_value (a)) == 1) {
      n = b;
    } else if (tarval_classify (computed_value (b)) == 1) {
      n = a;
    }
    break;
  case iro_Div:
    /* Div is not commutative. */
    if (tarval_classify (computed_value (b)) == 1) { /* div(x, 1) == x */
      /* Turn Div into a tuple (mem, bad, a) */
      ir_node *mem = get_Div_mem(n);
      turn_into_tuple(n, 3);
      set_Tuple_pred(n, 0, mem);
      set_Tuple_pred(n, 1, new_Bad());
      set_Tuple_pred(n, 2, a);
    }
    break;
    /* GL: Why are they skipped?  DivMod allocates new nodes --> it's
       teated in transform node.
  case iro_Mod, Quot, DivMod
    */
  case iro_And:
    if (a == b) n = a;
    /* And has it's own neutral element */
    else if (tarval_classify (computed_value (a)) == -1) {
      n = b;
    } else if (tarval_classify (computed_value (b)) == -1) {
      n = a;
    }
    break;
  case iro_Conv:
    if (get_irn_mode(n) == get_irn_mode(a)) { /* No Conv necessary */
      n = a;
    } else if (get_irn_mode(n) == mode_b) {
      if (get_irn_op(a) == op_Conv &&
	  get_irn_mode (get_Conv_op(a)) == mode_b) {
	n = get_Conv_op(a);	/* Convb(Conv*(xxxb(...))) == xxxb(...) */
      }
    }
    break;

  case iro_Phi:
    {
      /* Several optimizations:
         - no Phi in start block.
         - remove Id operators that are inputs to Phi
         - fold Phi-nodes, iff they have only one predecessor except
           themselves.
      */
      int i, n_preds;
      ir_node *block = NULL;     /* to shutup gcc */
      ir_node *first_val = NULL; /* to shutup gcc */
      ir_node *scnd_val = NULL;  /* to shutup gcc */

      n_preds = get_Phi_n_preds(n);

      block = get_nodes_Block(n);
      assert(get_irn_op (block) == op_Block);

      /* there should be no Phi nodes in the Start region. */
      if (block == current_ir_graph->start_block) {
	n = new_Bad();
	break;
      }

      if (n_preds == 0) {	/* Phi of dead Region without predecessors. */
        /* GL: why not return new_Bad? */
	break;
      }

#if 0
      /* first we test for a special case: */
      /* Confirm is a special node fixing additional information for a
         value that is known at a certain point.  This is useful for
         dataflow analysis. */
      if (n_preds == 2) {
	ir_node *a = follow_Id (get_Phi_pred(n, 0));
	ir_node *b = follow_Id (get_Phi_pred(n, 1));
	if (   (get_irn_op(a) == op_Confirm)
            && (get_irn_op(b) == op_Confirm)
            && (follow_Id (get_irn_n(a, 0)) == follow_Id(get_irn_n(b, 0)))
            && (get_irn_n(a, 1) == get_irn_n (b, 1))
	    && (a->data.num == (~b->data.num & irpn_True) )) {
	  n = follow_Id (get_irn_n(a, 0));
	  break;
	}
      }
#endif
      /* Find first non-self-referencing input */
      for (i = 0;  i < n_preds;  ++i) {
        first_val = follow_Id(get_Phi_pred(n, i));
        /* skip Id's */
        set_Phi_pred(n, i, first_val);
	if (   (first_val != n)                            /* not self pointer */
	       && (get_irn_op(first_val) != op_Bad)        /* value not dead */
            && !(is_Bad (get_Block_cfgpred(block, i))) ) { /* not dead control flow */
	  break;                         /* then found first value. */
	}
      }

      /* A totally Bad or self-referencing Phi (we didn't break the above loop) */
      if (i > n_preds) { n = new_Bad();  break; }

      scnd_val = NULL;

      /* follow_Id () for rest of inputs, determine if any of these
	 are non-self-referencing */
      while (++i < n_preds) {
        scnd_val = follow_Id(get_Phi_pred(n, i));
        /* skip Id's */
        set_Phi_pred(n, i, scnd_val);
        if (   (scnd_val != n)
	    && (scnd_val != first_val)
            && (get_irn_op(scnd_val) != op_Bad)
            && !(is_Bad (get_Block_cfgpred(block, i))) ) {
          break;
	}
      }

      /* Fold, if no multiple distinct non-self-referencing inputs */
      if (i > n_preds) {
	n = a;
      } else {
      /* skip the remaining Ids. */
	while (++i < n_preds) {
	  set_Phi_pred(n, i, follow_Id(get_Phi_pred(n, i)));
	}
      }
    }
    break;

  case iro_Load:
    {
      a = skip_Proj(get_Load_mem(n));
      b = skip_Proj(get_Load_ptr(n));

      if (get_irn_op(a) == op_Store) {
        if ( different_identity (b, get_Store_ptr(a))) {
          /* load and store use different pointers, therefore load
             needs not take store's memory but the state before. */
          set_Load_mem (n, get_Store_mem(a));
	} else if (( 0 /* ???didn't get cryptic test that returns 0 */ )) {
        }
      }
    }
      break;
  case iro_Store:
    /* remove unnecessary store. */
    {
      a = skip_Proj(get_Store_mem(n));
      b = get_Store_ptr(n);
      c = skip_Proj(get_Store_value(n));

      if (get_irn_op(a) == op_Store
          && get_Store_ptr(a) == b
          && skip_Proj(get_Store_value(a)) == c) {
        /* We have twice exactly the same store -- a write after write. */
	n = a;
      } else if (get_irn_op(c) == op_Load
		 && (a == c || skip_Proj(get_Load_mem(c)) == a)
                 && get_Load_ptr(c) == b )
	         /* !!!??? and a cryptic test */ {
        /* We just loaded the value from the same memory, i.e., the store
           doesn't change the memory -- a write after read. */
        turn_into_tuple(n, 2);
        set_Tuple_pred(n, 0, a);
        set_Tuple_pred(n, 1, new_Bad());
      }
    }
    break;

  case iro_Proj:
    {
      a = get_Proj_pred(n);

      if ( get_irn_op(a) == op_Tuple) {
        /* Remove the Tuple/Proj combination. */
	if ( get_Proj_proj(n) <= get_Tuple_n_preds(a) ) {
	  n = get_Tuple_pred(a, get_Proj_proj(n));
	} else {
          assert(0); /* This should not happen! (GL added this assert) */
	  n = new_Bad();
	}
      } else if (get_irn_mode(n) == mode_X &&
		 is_Bad(get_nodes_Block(n))) {
        /* Remove dead control flow. */
	n = new_Bad();
      }
    }
    break;

  case iro_Id:
    n = follow_Id (n);
    break;

  default: break;
  }

  return n;
} /* end equivalent_node() */


/* tries several [inplace] [optimizing] transformations and returns a
   equivalent node.  The difference to equivalent_node is that these
   transformations _do_ generate new nodes, and thus the old node must
   not be freed even if the equivalent node isn't the old one. */
static ir_node *
transform_node (ir_node *n)
{

  ir_node *a = NULL, *b;
  tarval *ta, *tb;

  switch (get_irn_opcode(n)) {
  case iro_DivMod: {

    int evaluated = 0;
    ir_mode *mode;

    a = get_DivMod_left(n);
    b = get_DivMod_right(n);
    mode = get_irn_mode(a);

    if (!(   mode_is_int(get_irn_mode(a))
	  && mode_is_int(get_irn_mode(b))))
      break;

    if (a == b) {
      a = new_Const (mode, tarval_from_long (mode, 1));
      b = new_Const (mode, tarval_from_long (mode, 0));
      evaluated = 1;
    } else {
      ta = value_of(a);
      tb = value_of(b);

      if (tb) {
	if (tarval_classify(tb) == 1) {
	  b = new_Const (mode, tarval_from_long (mode, 0));
	  evaluated = 1;
	} else if (ta) {
	  tarval *resa, *resb;
          resa = tarval_div (ta, tb);
          if (!resa) break; /* Causes exception!!! Model by replacing through
			       Jmp for X result!? */
          resb = tarval_mod (ta, tb);
          if (!resb) break; /* Causes exception! */
	  a = new_Const (mode, resa);
	  b = new_Const (mode, resb);
	  evaluated = 1;
	}
      } else if (tarval_classify (ta) == 0) {
        b = a;
	evaluated = 1;
      }
    }
    if (evaluated) { /* replace by tuple */
      ir_node *mem = get_DivMod_mem(n);
      turn_into_tuple(n, 4);
      set_Tuple_pred(n, 0, mem);
      set_Tuple_pred(n, 1, new_Bad());  /* no exception */
      set_Tuple_pred(n, 2, a);
      set_Tuple_pred(n, 3, b);
      assert(get_nodes_Block(n));
    }
  }
  break;

  case iro_Cond: {
    /* Replace the Cond by a Jmp if it branches on a constant
       condition. */
    ir_node *jmp;
    a = get_Cond_selector(n);
    ta = value_of(a);

    if (ta && (get_irn_mode(a) == mode_b)) {
      /* It's a boolean Cond, branching on a boolean constant.
         Replace it by a tuple (Bad, Jmp) or (Jmp, Bad) */
      jmp = new_r_Jmp(current_ir_graph, get_nodes_Block(n));
      turn_into_tuple(n, 2);
      if (tv_val_b(ta) == 1)  /* GL: I hope this returns 1 if true */ {
	set_Tuple_pred(n, 0, new_Bad());
	set_Tuple_pred(n, 1, jmp);
      } else {
	set_Tuple_pred(n, 0, jmp);
	set_Tuple_pred(n, 1, new_Bad());
      }
    } else if (ta && (get_irn_mode(a) == mode_I)) {
      /* I don't want to allow Tuples smaller than the biggest Proj.
         Also this tuple might get really big...
         I generate the Jmp here, and remember it in link.  Link is used
         when optimizing Proj. */
      set_irn_link(n, new_r_Jmp(current_ir_graph, get_nodes_Block(n)));
    } else if (   (get_irn_op(get_Cond_selector(n)) == op_Eor)
               && (get_irn_mode(get_Cond_selector(n)) == mode_b)
               && (tarval_classify(computed_value(get_Eor_right(a))) == 1)) {
      /* The Eor is a negate.  Generate a new Cond without the negate,
         simulate the negate by exchanging the results. */
      set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_Block(n),
				 get_Eor_left(a)));
    } else if (   (get_irn_op(get_Cond_selector(n)) == op_Not)
               && (get_irn_mode(get_Cond_selector(n)) == mode_b)) {
      /* A Not before the Cond.  Generate a new Cond without the Not,
         simulate the Not by exchanging the results. */
      set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_Block(n),
				 get_Not_op(a)));
    }
  }
  break;

  case iro_Proj: {
    a = get_Proj_pred(n);

    if (  (get_irn_op(a) == op_Cond)
        && get_irn_link(a)
        && get_irn_op(get_irn_link(a)) == op_Cond) {
    /* Use the better Cond if the Proj projs from a Cond which get's
       its result from an Eor/Not. */
      assert (   (   (get_irn_op(get_Cond_selector(a)) == op_Eor)
		     || (get_irn_op(get_Cond_selector(a)) == op_Not))
              && (get_irn_mode(get_Cond_selector(a)) == mode_b)
	      && (get_irn_op(get_irn_link(a)) == op_Cond)
              && (get_Cond_selector(get_irn_link(a)) ==
                  get_Eor_left(get_Cond_selector(a))));
      set_Proj_pred(n, get_irn_link(a));
      if (get_Proj_proj(n) == 0)
        set_Proj_proj(n, 1);
      else
        set_Proj_proj(n, 0);
    } else if (   (get_irn_op(a) == op_Cond)
               && (get_irn_mode(get_Cond_selector(a)) == mode_I)
               && value_of(a)) {
      /* The Cond is a Switch on a Constant */
      if (get_Proj_proj(n) == tv_val_CHIL(value_of(a))) {
        /* The always taken branch, reuse the existing Jmp. */
        if (!get_irn_link(a)) /* well, if it exists ;-> */
          set_irn_link(a, new_r_Jmp(current_ir_graph, get_nodes_Block(n)));
        assert(get_irn_op(get_irn_link(a)) == op_Jmp);
        n = get_irn_link(a);
      } else {
        /* a never taken branch */
        n = new_Bad();
      }
    }
  }
  break;
  case iro_Eor: { /* @@@ not tested as boolean Eor not allowed any more. */
    a = get_Eor_left(n);
    b = get_Eor_right(n);

    if (   (get_irn_mode(n) == mode_b)
	&& (get_irn_op(a) == op_Proj)
        && (get_irn_mode(a) == mode_b)
	&& (tarval_classify (computed_value (b)) == 1)
	&& (get_irn_op(get_Proj_pred(a)) == op_Cmp))
      /* The Eor negates a Cmp. The Cmp has the negated result anyways! */
      n = new_r_Proj(current_ir_graph, get_nodes_Block(n), get_Proj_pred(a),
                     mode_b, get_negated_pnc(get_Proj_proj(a)));
    else if (   (get_irn_mode(n) == mode_b)
	     && (tarval_classify (computed_value (b)) == 1))
      /* The Eor is a Not. Replace it by a Not. */
      /*   ????!!!Extend to bitfield 1111111. */
      n = new_r_Not(current_ir_graph, get_nodes_Block(n), a, mode_b);
  }
  break;
  case iro_Not: {
    a = get_Not_op(n);

    if (   (get_irn_mode(n) == mode_b)
	&& (get_irn_op(a) == op_Proj)
        && (get_irn_mode(a) == mode_b)
	&& (get_irn_op(get_Proj_pred(a)) == op_Cmp))
      /* We negate a Cmp. The Cmp has the negated result anyways! */
      n = new_r_Proj(current_ir_graph, get_nodes_Block(n), get_Proj_pred(a),
                     mode_b, get_negated_pnc(get_Proj_proj(a)));
  }
  break;
  default: ;
  }
  return n;
}

/***************** Common Subexpression Elimination *****************/

/* Compare function for two nodes in the hash table.   Gets two     */
/* nodes as parameters.                                             */
/* @@@  a+b != b+a ? */
static int
vt_cmp (const void *elt, const void *key)
{
  ir_node *a, *b;
  int i;

  a = (void *)elt;
  b = (void *)key;

  if (a == b) return 0;

  if ((get_irn_op(a) != get_irn_op(b)) ||
      (get_irn_mode(a) != get_irn_mode(b))) return 1;

  /* compare if a's in and b's in are equal */
  /* GL: we optimize only nodes with in arrays of fixed sizes.
  if (get_irn_arity (a) != -2) {
    ins = get_irn_arity (a);
    if (ins != get_irn_arity (b)) return 1;
    ain = get_irn_in (a);
    bin = get_irn_in (b);
  }
  */
  if (get_irn_arity (a) != get_irn_arity(b))
    return 1;

  /* compare a->in[0..ins] with b->in[0..ins], i.e., include the block. */
  /* do if (*ain++ != *bin++) return 1; while (ins--); */
  for (i = -1; i < get_irn_arity(a); i++)
    if (get_irn_n(a, i) != get_irn_n(b, i))
      return 1;


  switch (get_irn_opcode(a)) {
  case iro_Const:
    return get_irn_const_attr (a) != get_irn_const_attr (b);
  case iro_Proj:
    return get_irn_proj_attr (a) != get_irn_proj_attr (b);
  case iro_Alloc:
    return (get_irn_alloc_attr(a).where != get_irn_alloc_attr(b).where)
      || (get_irn_alloc_attr(a).type != get_irn_alloc_attr(b).type);
  case iro_Free:
    return (get_irn_free_attr(a) != get_irn_free_attr(b));
  case iro_SymConst:
    return (get_irn_symconst_attr(a).num != get_irn_symconst_attr(b).num)
      || (get_irn_symconst_attr(a).tori.typ != get_irn_symconst_attr(b).tori.typ);
  case iro_Call:
    return (get_irn_call_attr(a)->kind != get_irn_call_attr(b)->kind)
      || (get_irn_call_attr(a)->arity != get_irn_call_attr(b)->arity);
  case iro_Sel:
    return (get_irn_sel_attr(a).ent->kind != get_irn_sel_attr(b).ent->kind)
      || (get_irn_sel_attr(a).ent->name != get_irn_sel_attr(b).ent->name)
      || (get_irn_sel_attr(a).ent->owner != get_irn_sel_attr(b).ent->owner)
      || (get_irn_sel_attr(a).ent->ld_name != get_irn_sel_attr(b).ent->ld_name)
      || (get_irn_sel_attr(a).ent->type != get_irn_sel_attr(b).ent->type)
      || (get_irn_sel_attr(a).ltyp != get_irn_sel_attr(b).ltyp);
  case iro_Phi:
    return get_irn_phi_attr (a) != get_irn_phi_attr (b);
  default: ;
  }

  return 0;
}

static unsigned
ir_node_hash (ir_node *node)
{
  unsigned h;
  int i;

  /* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
  h = get_irn_arity(node);

  /* consider all in nodes... except the block. */
  for (i = 0;  i < get_irn_arity(node);  i++) {
    h = 9*h + (unsigned long)get_irn_n(node, i);
  }

  /* ...mode,... */
  h = 9*h + (unsigned long) get_irn_mode (node);
  /* ...and code */
  h = 9*h + (unsigned long) get_irn_op (node);

  return h;
}

pset *
new_identities (void)
{
  return new_pset (vt_cmp, TUNE_NIR_NODES);
}

void
del_identities (pset *value_table)
{
  del_pset (value_table);
}

/* Return the canonical node computing the same value as n.
   Looks up the node in a hash table. */
static inline ir_node *
identify (pset *value_table, ir_node *n)
{
  ir_node *o = NULL;

  if (!value_table) return n;

  switch (get_irn_opcode (n)) {
  case iro_Add:
  case iro_Mul:
  case iro_Or:
  case iro_And:
  case iro_Eor:
    {
      /* for commutative operators perform  a OP b == b OP a */
      if (get_binop_left(n) > get_binop_right(n)) {
	ir_node *h = get_binop_left(n);
	set_binop_left(n, get_binop_right(n));
	set_binop_right(n, h);
      }
    }
  break;
  default: break;
  }
  o = pset_find (value_table, n, ir_node_hash (n));
  if (!o) return n;

  return o;
}

/* Return the canonical node computing the same value as n.
   Looks up the node in a hash table, enters it in the table
   if it isn't there yet. */
static ir_node *
identify_remember (pset *value_table, ir_node *node)
{
  ir_node *o = NULL;

  if (!value_table) return node;

  /* lookup or insert in hash table with given hash key. */
  o = pset_insert (value_table, node, ir_node_hash (node));

  if (o == node) return node;

  return o;
}

/* garbage in, garbage out. If a node has a dead input, i.e., the
   Bad node is input to the node, return the Bad node.  */
static inline ir_node *
gigo (ir_node *node)
{
  int i;
  ir_op* op = get_irn_op(node);

  /* Blocks, Phis and Tuples may have dead inputs, e.g., if one of the
     blocks predecessors is dead. */
  if ( op != op_Block && op != op_Phi && op != op_Tuple) {
    for (i = -1; i < get_irn_arity(node); i++) {
      if (is_Bad(get_irn_n(node, i))) {
        node = new_Bad();
        break;
      }
    }
  }
  return node;
}


/* These optimizations deallocate nodes from the obstack.
   It can only be called if it is guaranteed that no other nodes
   reference this one, i.e., right after construction of a node.  */
ir_node *
optimize (ir_node *n)
{
  tarval *tv;
  ir_node *old_n = n;

  /* Allways optimize Phi nodes: part of the construction. */
  if ((!get_optimize()) && (get_irn_op(n) != op_Phi)) return n;

  /* if not optimize return n */
  if (n == NULL) {
    printf(" attention: empty node!!! \n");
    return n;
  }

  /* constant expression evaluation / constant folding */
  if (get_opt_constant_folding()) {
    /* constants can not be evaluated */
    if  (get_irn_op(n) != op_Const) {
      /* try to evaluate */
      tv = computed_value (n);
      if (tv != NULL) {
        /* evaluation was succesful -- replace the node. */
	obstack_free (current_ir_graph->obst, n);
	return new_Const (get_tv_mode (tv), tv);
      }
    }
  }

  /* remove unnecessary nodes */
  if (get_opt_constant_folding() || get_irn_op(n) == op_Phi)
    n = equivalent_node (n);

  /** common subexpression elimination **/
  /* Checks whether n is already available. */
  /* The block input is used to distinguish different subexpressions. Right
     now all nodes are pinned to blocks, i.e., the cse only finds common
     subexpressions within a block. */
  if (get_opt_cse())
    n = identify (current_ir_graph->value_table, n);
  /* identify found a cse, so deallocate the old node. */
  if (n != old_n) {
    obstack_free (current_ir_graph->obst, old_n);
    /* The AmRoq fiasco returns n here.  Martin's version doesn't. */
  }

  /* Some more constant expression evaluation that does not allow to
     free the node. */
  if (get_opt_constant_folding())
    n = transform_node (n);

  /* Remove nodes with dead (Bad) input. */
  n = gigo (n);
  /* Now we can verify the node, as it has no dead inputs any more. */
  irn_vrfy(n);

  /* Now we have a legal, useful node. Enter it in hash table for cse */
  if (get_opt_cse()) {
    n = identify_remember (current_ir_graph->value_table, n);
  }

#if 0  /* GL: what's the use of this?? */
  if ((current_ir_graph->state & irgs_building) && IR_KEEP_ALIVE (n)) {
    assert (~current_ir_graph->state & irgs_keep_alives_in_arr);
    pdeq_putr (current_ir_graph->keep.living, n);
  }
#endif
  return n;
}


/* These optimizations never deallocate nodes.  This can cause dead
   nodes lying on the obstack.  Remove these by a dead node elimination,
   i.e., a copying garbage collection. */
ir_node *
optimize_in_place (ir_node *n)
{

  tarval *tv;
  ir_node *old_n = n;

  /* if not optimize return n */
  if (n == NULL) {
    /* Here this is possible.  Why? */
    return n;
  }

  /* constant expression evaluation / constant folding */
  if (get_opt_constant_folding()) {
    /* constants can not be evaluated */
    if  (get_irn_op(n) != op_Const) {
      /* try to evaluate */
      tv = computed_value (n);
      if (tv != NULL) {
        /* evaluation was succesful -- replace the node. */
	return new_Const (get_tv_mode (tv), tv);
        /* xprintf("* optimize: computed node %I\n", n->op->name);*/
      }
    }
  }

  /* remove unnecessary nodes */
  if (get_opt_constant_folding())
    //  if (get_opt_constant_folding() || get_irn_op(n) == op_Phi)
    n = equivalent_node (n);

  /** common subexpression elimination **/
  /* Checks whether n is already available. */
  /* The block input is used to distinguish different subexpressions.  Right
     now all nodes are pinned to blocks, i.e., the cse only finds common
     subexpressions within a block. */
  if (get_opt_cse())
    n = identify (current_ir_graph->value_table, n);

  /* identify found a cse, so deallocate the old node. */
  if (n != old_n) {
    /* The AmRoq fiasco returns n here.  Martin's version doesn't. */
  }

  /* Some more constant expression evaluation. */
  if (get_opt_constant_folding())
    n = transform_node (n);

  /* Remove nodes with dead (Bad) input. */
  n = gigo (n);
  /* Now we can verify the node, as it has no dead inputs any more. */
  irn_vrfy(n);

  /* Now we have a legal, useful node. Enter it in hash table for cse */
  if (get_opt_cse()) {
    /* aborts ??! set/pset can not handle several hash tables??!
       No, suddenly it works. */
    n = identify_remember (current_ir_graph->value_table, n);
  }

  return n;
}
