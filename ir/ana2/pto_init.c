/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto_init.c
 * Purpose:     Pto Initialization
 * Author:      Florian
 * Modified by:
 * Created:     Wed  3 Nov 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

# include "pto.h"
# include "pto_util.h"

# include "entity.h"
# include "irnode.h"
# include "xmalloc.h"

# define DBGPRINT(lvl, msg) if (get_pto_verbose () > lvl) { fprintf msg; }

static void pto_init_proj_load (ir_node *proj, ir_node *load)
{
  assert ((mode_P == get_irn_mode (proj)) && "wrong proj(load)");

# ifdef PTO_DUMMY
  ir_node *ptr = get_Load_ptr (load);
  entity *ent = get_ptr_ent (ptr);
  type *tp = get_entity_type (ent);
# endif /* defined PTO_DUMMY */

  pto_t *pto = pto_new_empty (proj);

  DBGPRINT (1, (stdout, "%s: pto (%s[%li]) = 0x%08x\n",
                __FUNCTION__,
                get_op_name (get_irn_op (proj)),
                get_irn_node_nr (proj),
                (int) pto));

  set_pto (proj, pto);
}

static void pto_init_call (ir_node *call)
{
  /* check return value: */
  ir_node *ptr = get_Call_ptr (call);
  entity *ent = get_ptr_ent (ptr);
  type *meth_tp = get_entity_type (ent);

  if (0 == get_method_n_ress (meth_tp)) {
    /* can't be a pointer */
    return;
  }

  type *ret_tp  = get_method_res_type (meth_tp, 0);

  if (mode_P != get_type_mode (ret_tp)) {
    return;
  }

# ifdef PTO_DUMMY
  ir_node *ptr = get_Call_ptr (call);
  entity *ent = get_ptr_ent (ptr);
  type *tp = get_entity_type (ent);

  obj_desc_t *obj_desc = obj_desc_new (tp);
  obj_desc_set_dummy (obj_desc);
# endif /* defined PTO_DUMMY */

  pto_t *pto = pto_new_empty (call);

  DBGPRINT (1, (stdout, "%s: pto (%s[%li]) = 0x%08x\n",
                __FUNCTION__,
                get_op_name (get_irn_op (call)),
                get_irn_node_nr (call),
                (int) pto));

  set_pto (call, pto);
}

static void pto_init_raise (ir_node *raise)
{
  /* assert (0 && "initialise raise?"); */

  /* right now, do nothing and hope that a raise can always be
     analysed on-the-fly. */
}

static void pto_init_proj (ir_node *proj)
{
  ir_node *in = get_Proj_pred (proj);
  const opcode in_op = get_irn_opcode (in);

  switch (in_op) {
  case (iro_Proj): {
    ir_node *in_in = get_Proj_pred (in);
    const opcode in_in_op = get_irn_opcode (in_in);

    switch (in_in_op) {
    case (iro_Start): {
      /* nothing (always initialised with actual values) */
    } break;

    case (iro_Call): {
      /* nothing (must use call itself) */
    } break;

    default: {
      fprintf (stderr, "%s: proj(proj(%s[%ld])) not handled\n",
               __FUNCTION__,
               get_op_name (get_irn_op (in_in)),
               get_irn_node_nr (in_in));
      assert (0);
    }
    } /* end switch(Proj.Proj.op) */
  } break; /* iro_Proj */

  case (iro_Start): {
    /* ProjM (start) or ProjT (start) --- nothing */
  } break;

  case (iro_Call): {
    /* ProjT (start) --- nothing */
  } break;

  case (iro_Load): {
    /* Todo: ProjM (load) or ProjV(load) */
    if (mode_P == get_irn_mode (proj)) {
      pto_init_proj_load (proj, in);
    } else {
      /* nothing to do */
    }
  } break;

  case (iro_Store): {
    /* ProjM (store) --- nothing */
  } break;

  case (iro_Alloc): {
    /* nothing to do --- can always be computed on-the-fly */
  } break;

  case (iro_Raise): {
    /* ProjM (raise) or Proj???(raise) --- TODO */
  } break;

  case (iro_Cast): {
    /* not needed */
  } break;

  default: {
    fprintf (stderr, "%s: proj(%s[%ld]) not handled\n",
             __FUNCTION__,
             get_op_name (get_irn_op (in)),
             get_irn_node_nr (in));
    assert (0);
  }
  } /* end switch (Proj.op) */

}

void pto_init_node (ir_node *node)
{
  const opcode op = get_irn_opcode (node);

  DBGPRINT (1, (stdout, "%s (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (node)),
                get_irn_node_nr (node)));

  switch (op) {
  case (iro_Start): {
    /* nothing (not needed) */
  } break;

  case (iro_Load): {
    /* nothing (not needed) */
  } break;

  case (iro_Store): {
    /* nothing (not needed) */
  } break;

  case (iro_Alloc): {
    /* nothing (can always be computed on-the-fly) */
  } break;

  case (iro_Raise): {
    /* Todo: Check how a Raise works */
    pto_init_raise (node);
  } break;

  case (iro_Call): {
    /* pretend we have a return value */
    pto_init_call (node);
  } break;

  case (iro_Proj): {
    /* this actually does most of the work */
    pto_init_proj (node);
  } break;

  case (iro_Cast): {
    /* nothing (can always be computed on-the-fly) */
  } break;

  case (iro_SymConst): {
    /* nothing (can always be computed on-the-fly) */
  } break;

  case (iro_Const): {
    /* nothing (can always be computed on-the-fly) */
  } break;

  case (iro_Block): {
    /* nothing (this is only interesting for the end block, and that
       can always be computed on-the-fly) */
  } break;

  case (iro_Phi): {
    /* nothing (This would need the predecessors to be initialized! Do this on-the-fly) */
  } break;

  /* now, enumerate everything else that is uninteresting */
  case (iro_Return):
  case (iro_Div):
  case (iro_Quot):
  case (iro_Mod):
  case (iro_DivMod): {
    /* nothing (not needed) */
  } break;

  default: {
    fprintf (stderr, "%s: %s[%ld] not handled\n",
             __FUNCTION__,
             get_op_name (get_irn_op (node)),
             get_irn_node_nr (node));
    assert (0);
  } break;

  }
}



/*
 * $Log$
 * Revision 1.1  2004/11/04 14:58:59  liekweg
 * added initialisation
 *
 *
 */
