/*
 * Project:     libFIRM
 * File name:   ir/ana/analyze_irg_agrs.c
 * Purpose:     read/write analyze of graph argument, which have mode reference.
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file analyze_irg_agrs.c
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#include "irouts.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "array.h"
#include "irprog.h"
#include "entity_t.h"

#include "analyze_irg_args.h"

/**
 * A struct to save if a graph argument with mode reference is
 * read/write or both.
 */
typedef struct {
  unsigned char read;
  unsigned char write;
} rw_info_t;

enum access {
  ACC_UNKNOWN,
  ACC_ACCESSED,
  ACC_NOT_ACCESSED
};

#define ACCESS(a)	if ((a) == ACC_UNKNOWN) (a) = ACC_ACCESSED
#define NOT_ACCESS(a)	if ((a) == ACC_UNKNOWN) (a) = ACC_NOT_ACCESSED

static char v;
static void *VISITED = &v;

/**
 * Walk recursive the successors of a graph argument
 * with mode reference and mark in the "irg_args" if it will be read,
 * written or both.
 *
 * @param arg   The graph argument with mode reference,
 *             that must be checked.
 */
static unsigned analyze_arg(ir_node *arg, unsigned bits)
{
  int i, p;
  ir_node *succ;

  /* We must visit a node once to avoid endless recursion.*/
  set_irn_link(arg, VISITED);

  for (i = get_irn_n_outs(arg) - 1; i >= 0; --i) {
    succ = get_irn_out(arg, i);

    /* We was here.*/
    if (get_irn_link(succ) == VISITED)
      continue;

    /* We should not walk over the memory edge.*/
    if (get_irn_mode(succ) == mode_M)
      continue;

    /* A addition or subtraction of the argument with output, that
       isn't with mode reference must not be checked.*/
    if ((get_irn_op(succ) == op_Add || get_irn_op(succ) == op_Sub) &&
        !mode_is_reference(get_irn_mode(succ)))
      continue;

    /* A Block as successor isn't interesting.*/
    if (get_irn_op(succ) == op_Block)
      continue;

    /* If we reach with the recursion a Call node and our reference
       isn't the address of this Call we accept that the reference will
       be read and written if the graph of the method represented by
       "Call" isn't computed else we analyze that graph. If our
       reference is the address of this
       Call node that mean the reference will be read.*/
    if (get_irn_op(succ) == op_Call) {
      ir_node *Call_ptr  = get_Call_ptr(succ);

      if (Call_ptr == arg)
        return bits | ptr_access_read;
      else if (op_SymConst == get_irn_op(Call_ptr) &&
	      get_SymConst_kind(Call_ptr) == symconst_addr_ent) {
        entity *meth_ent = get_SymConst_entity(Call_ptr);

	      for (p = get_Call_n_params(succ) - 1; p >= 0; --p){
	        if (get_Call_param(succ, p) == arg) {
            /* an arg can be used more than once ! */
            bits |= get_method_param_access(meth_ent, p);
	        }
	      }
      } else
        bits |= ptr_access_rw;
    }
    else if (get_irn_op(succ) == op_Store) {
      /* We have reached a Store node => the reference is written. */
      bits |= ptr_access_write;
    }
    else if (get_irn_op(succ) == op_Load) {
      /* We have reached a Load node => the reference is read. */
      bits |= ptr_access_read;
    }

    /* If we know that, the argument will be read and written, we
       can break the recursion.*/
    if (bits == ptr_access_rw)
      return ptr_access_rw;

    bits = analyze_arg(succ, bits);
  }
  set_irn_link(arg, NULL);
  return bits;
}


/**
 * Check if a argument of the ir graph with mode
 * reference is read, write or both.
 *
 * @param irg   The ir graph to analyze.
 */
static void analyze_ent_args(entity *ent)
{
  ir_graph *irg;
  ir_node *irg_args, *arg;
  ir_mode *arg_mode;
  int nparams, i;
  long proj_nr;
  type *mtp;
  unsigned bits;
  rw_info_t *rw_info;

  mtp     = get_entity_type(ent);
  nparams = get_method_n_params(mtp);

  ent->param_access = NEW_ARR_F(ptr_access_kind, nparams);

  /* If the method haven't parameters we have
   * nothing to do.*/
  if (nparams <= 0)
    return;

  irg = get_entity_irg(ent);

  if (! irg) {
    /* if we could not determine the graph, set RW access */
    for (i = nparams - 1; i >= 0; --i)
      ent->param_access[i] =
        is_Pointer_type(get_method_param_type(mtp, i)) ? ptr_access_rw : ptr_access_none;

    return;
  }

  /* Call algorithm that computes the out edges */
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_outs(irg);

  irg_args = get_irg_args(irg);

  /* A array to save the information for each argument with
     mode reference.*/
  NEW_ARR_A(rw_info_t, rw_info, nparams);

  /* We initialize the element with UNKNOWN state. */
  for (i = nparams - 1; i >= 0; --i) {
    rw_info[i].read  = ACC_UNKNOWN;
    rw_info[i].write = ACC_UNKNOWN;
  }

  /* search for arguments with mode reference
     to analyze them.*/
  for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
    arg      = get_irn_out(irg_args, i);
    arg_mode = get_irn_mode(arg);
    proj_nr  = get_Proj_proj(arg);

    if (mode_is_reference(arg_mode)) {
      bits = analyze_arg(arg, ptr_access_none);

      if (bits & ptr_access_read)
        ACCESS(rw_info[proj_nr].read);
      if (bits & ptr_access_write)
        ACCESS(rw_info[proj_nr].write);
    }
  }

  /* set all unknown values to NOT_ACCESSED */
  for (i = nparams - 1; i >= 0; --i) {
    NOT_ACCESS(rw_info[i].read);
    NOT_ACCESS(rw_info[i].write);

    ent->param_access[i] = (rw_info[i].read  == ACC_ACCESSED ? ptr_access_read : ptr_access_none) |
                           (rw_info[i].write == ACC_ACCESSED ? ptr_access_write : ptr_access_none);
  }

  printf("%s:\n", get_entity_name(ent));
  for (i = 0; i < nparams; ++i) {
    if (is_Pointer_type(get_method_param_type(mtp, i)))
      printf("Pointer Arg %i wird %s %s\n", i,
        ent->param_access[i] & ptr_access_read  ? "gelesen" : "",
        ent->param_access[i] & ptr_access_write ? "geschrieben" : "");
  }
}

/*
 * Compute for a method with pointer parameter(s)
 * if they will be read or written.
 */
ptr_access_kind get_method_param_access(entity *ent, int pos)
{
  type *mtp = get_entity_type(ent);
  int  is_variadic = get_method_variadicity(mtp) == variadicity_variadic;

  assert(0 <= pos && (is_variadic || pos < get_method_n_params(mtp)));

  if (ent->param_access) {
    if (pos < ARR_LEN(ent->param_access))
      return ent->param_access[pos];
    else
      return ptr_access_rw;
  }

  analyze_ent_args(ent);

  if (pos < ARR_LEN(ent->param_access))
    return ent->param_access[pos];
  else
    return ptr_access_rw;
}

/**
 * Analyze how pointer arguments of a given
 * ir graph are accessed.
 *
 * @param irg   The ir graph to analyze.
 */
void analyze_irg_args(ir_graph *irg)
{
  entity *ent;

  if (irg == get_const_code_irg())
    return;

  ent = get_irg_entity(irg);
  if (! ent)
    return;

  if (! ent->param_access)
    analyze_ent_args(ent);
}
