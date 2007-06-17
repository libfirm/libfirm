/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Construct some loop information.
 * @author      Beyhan Veliev
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compute_loop_info.h"
#include "strength_red_t.h"
#include "tv.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irloop_t.h"
#include "irgopt.h"
#include "irflag.h"
#include "irouts.h"
#include "tv.h"

#undef TRUE
#define TRUE 1   /**< For loops, that are count loop. */

static char _x;
static void *NODE_VISITED = &_x;

/**
 * Walk over the successors of the loop iteration variable and
 * decided if the loop a count loop is.
 *
 * @param *node          A ir node.
 * @param *loop_head     The head of the loop that be checked.
 */
static int is_count_loop(ir_node *node, ir_node *loop_head)
{
  int i,  res = 1;
  set_irn_link(node, NODE_VISITED);
  for (i = get_irn_n_outs(node) - 1; i >= 0; --i) {
    ir_node *succ = get_irn_out(node, i);
    /* The start position is reached.*/
    if(get_irn_link(succ) == NODE_VISITED)
      return 1;
    /* The iteration variable musn't be changed aside the iteration and the loop mussn't have
       a break.*/
    if ((is_loop_invariant(succ, loop_head) && get_irn_op(node) == op_Proj) ||
      (get_irn_op(succ) == op_Phi && !is_loop_invariant(succ, loop_head)))
      return 0;
    else if(get_irn_link(succ) != NODE_VISITED)
      res = is_count_loop(succ, loop_head);
  }
  return res;
}

/**
 * Compute the end value, that be reached from the iteration.
 *
 * @param *loop_start       The loop's start value.
 * @param *loop_end         The loop's end, that "cmp" node have as parameter.
 * @param *loop_step        The value, with that induction variable is (in|de)cremented.
 * @param *is_endlessloop   A integer to set of 1, if the loop is endless in mathematic mean.
 * @param *is_nonentered_loop A integer to set of 1, if the loop is dead, that mean the loop can't be trodden.
 * @param *info             A struct, that contain loop information.
 */
static void compute_loop_end(tarval *loop_start, tarval *loop_end, tarval *loop_step,
                             int *is_endlessloop, int *is_nonentered_loop, induct_var_info *info)
{
  ir_node *cmp_out;
  pn_Cmp pnc;
  tarval *tarval_one, *diff;
  tarval_one = get_tarval_one(get_tarval_mode(loop_step));

  /* a compare should only have one Proj, but CSE might disturb that ... */
  assert(get_irn_n_outs(info->cmp) == 1);
  cmp_out = get_irn_out(info->cmp, 0);
  pnc     = get_Proj_proj(cmp_out);
  if (tarval_is_negative(loop_start) == tarval_is_negative(loop_end))
    diff = tarval_abs(tarval_sub(loop_end, loop_start));
  else
    diff = tarval_add(tarval_abs(loop_end),tarval_abs(loop_start));
  diff = tarval_mod(diff, loop_step);

  if (is_Add(info->op) && !tarval_is_negative(loop_step)) {
    /* iterator < loop_end*/
    if (pnc == pn_Cmp_Lt) {
      /* A loop that can't be entered as  for(i = 10; i < 5; i++) */
      if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq ||
        tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt){
        *is_nonentered_loop = 1;
        return;
      }
      if (diff == get_tarval_null(get_tarval_mode(loop_start)))
        diff = loop_step;
      info->l_itervar_phi->loop_iter_end = tarval_sub(loop_end, diff);
    } else
      if(pnc == pn_Cmp_Lg){
        /* A loop that can't be entered as  for(i = 5; i != 5; i++) */
        if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq){
          *is_nonentered_loop = 1;
          return;
        }
        if(tarval_cmp(loop_start, tarval_sub(get_mode_max(get_tarval_mode(loop_step)), loop_step)) == pn_Cmp_Gt){
          info->l_itervar_phi->flags |= loop_downto_loop;
          loop_step = tarval_sub(loop_start, tarval_add(loop_step, loop_start));
          info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, loop_step);
        }else
          info->l_itervar_phi->loop_iter_end = tarval_sub(loop_end, loop_step);
      }else
        /* iterator <= loop_end*/
        if(pnc == pn_Cmp_Le){
          /* A loop that can't be entered as  for(i = 10; i <= 5; i++) */
          if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt){
            *is_nonentered_loop = 1;
            return;
          }
          /* A endless loop as  for(i = 10; i <= 0xFFFFFFFF; i++) */
          if (tarval_cmp(loop_end, get_mode_max(get_tarval_mode(loop_start))) == pn_Cmp_Eq){
            info->l_itervar_phi->flags |= loop_is_endless;
            return;
          }
          if (tarval_cmp(loop_end, tarval_sub(get_mode_max(get_tarval_mode(loop_start)), tarval_one)) == pn_Cmp_Eq)
            if(tarval_cmp(loop_step, tarval_one) == pn_Cmp_Gt)
              info->l_itervar_phi->flags |= loop_end_false;
            info->l_itervar_phi->loop_iter_end = tarval_sub(loop_end, diff);
        }else{
          /* iterator > loop_end*/
          if(pnc == pn_Cmp_Gt){
            /* A endless loop in mathematic mean that iterate until 0xFFFFFFFF as  for(i = 10; i > 5; i++) */
            if(tarval_cmp(loop_start, tarval_sub(get_mode_max(get_tarval_mode(loop_step)), loop_step)) == pn_Cmp_Gt){
              if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt ){
                info->l_itervar_phi->flags |= loop_downto_loop;
                loop_step = tarval_sub(loop_start, tarval_add(loop_step, loop_start));
                diff = tarval_sub(loop_start, loop_end);
                diff = tarval_mod(diff, loop_step);
                if(diff == get_tarval_null(get_tarval_mode(loop_start)))
                  diff = loop_step;
                info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, diff);
              }else
                /* A loop that can't be entered as  for(i = 4; i > 5; i++) */
                *is_nonentered_loop = 1;
            }else
              if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt )
                *is_endlessloop = 1;
              else
                /* A loop that can't be entered as  for(i = 4; i > 5; i++) */
                *is_nonentered_loop = 1;
              /* iterator >= loop_end*/
          }else{
            /* Test if the cmp is after the incrementation and add to loop start one.*/
            if(get_irn_link(info->cmp_const) != NULL)
              loop_start = tarval_add(loop_start, loop_step);
            /* A endless loop in mathematic mean that iterate until 0xFFFFFFFF as  for(i = 10; i > 5; i++) */
            if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq ||
              tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt)
              *is_endlessloop = 1;
            else
              /* A loop that can't be entered as  for(i = 4; i >= 5; i++) */
              *is_nonentered_loop = 1;
          }
        }
  }else
    /* The loop iterate downto.*/
    if(is_Add(info->op) && tarval_is_negative(loop_step)){
      info->l_itervar_phi->flags |= loop_downto_loop;
      /* iterator > loop_end*/
      if(pnc == pn_Cmp_Gt){
        /* A loop that can't be entered as  for(i = 4; i > 5; i = i+(-1)) */
        if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq ||
          tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt ){
          *is_nonentered_loop = 1;
          return;
        }
        if(diff == get_tarval_null(get_tarval_mode(loop_start)))
          diff = tarval_abs(loop_step);
        info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, diff);
      }else
        if(pnc == pn_Cmp_Lg){
          /* A loop that can't be entered as  for(i = 5; i != 5; i--) */
          if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq)
            *is_nonentered_loop = 1;

          info->l_itervar_phi->loop_iter_end = tarval_sub(loop_end, loop_step);
        }else
          /* iterator >= loop_end*/
          if(pnc == pn_Cmp_Ge){
            /* Test if the cmp is after the incrementation and add to loop start the loop step.*/
            if(get_irn_link(info->cmp_const) != NULL)
              loop_start = tarval_add(loop_start, loop_step);
            /* A loop that can't be entered as  for(i = 4; i >= 5; i--) */
            if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt){
              *is_nonentered_loop = 1;
              return;
            }
            /* A endless loop as  for(i = 10; i >=(int)0x0; i--) */
            if (tarval_cmp(loop_end, get_mode_min(get_tarval_mode(loop_start))) == pn_Cmp_Eq){
              info->l_itervar_phi->flags |= loop_is_endless;
              return;
            }
            if (tarval_cmp(loop_end, tarval_add(get_mode_min(get_tarval_mode(loop_start)), tarval_one)) == pn_Cmp_Eq)
              if(tarval_cmp(tarval_abs(loop_step), tarval_one) == pn_Cmp_Gt)
                info->l_itervar_phi->flags |= loop_end_false;
              info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, diff);;
          } else
            /* iterator < loop_end*/
            if(pnc == pn_Cmp_Lt){
              /* A loop that can't be entered as  for(i = 6; i < 5; i--) */
              if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt ||
                tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq)
                *is_nonentered_loop = 1;
              else
                /* A endless loop in mathematic mean that iterate until 0xFFFFFFFF as  for(i = 4; i < 5; i--) */
                *is_endlessloop = 1;
            }else{
              /* A loop that can't be entered as  for(i = 6; i <= 5; i = i+(-1)) */
              if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Gt)
                *is_nonentered_loop = 1;
              else
                /* A endless loop in mathematic mean that iterate until 0xFFFFFFFF as  for(i = 4; i < 5; i--) */
                *is_endlessloop = 1;
            }
    }else
      if (is_Sub(info->op) && !tarval_is_negative(loop_step)){
        info->l_itervar_phi->flags |= loop_downto_loop;
        if(pnc == pn_Cmp_Lg){
          /* A loop that can't be entered as  for(i = 5; i != 5; i++) */
          if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq)
            *is_nonentered_loop = 1;

          info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, loop_step);
        }else
          if(pnc == pn_Cmp_Gt){
            /*A loop that can't be entered as  for(i = 10; i < 5; i++)*/
            if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq ||
              tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt){
              *is_nonentered_loop = 1;
              return;
            }
            if(diff == get_tarval_null(get_tarval_mode(loop_start)))
              diff = loop_step;
            info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, diff);
          }else
            /* iterator <= loop_end*/
            if(pnc == pn_Cmp_Ge){
              /* A loop that can't be entered as  for(i = 10; i <= 5; i++) */
              if (tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt){
                *is_nonentered_loop = 1;
                return;
              }
              /* A endless loop as  for((unsigned)i = 10; i >= 0x0; i--) */
              if (tarval_cmp(loop_end, get_mode_min(get_tarval_mode(loop_start))) == pn_Cmp_Eq){
                info->l_itervar_phi->flags |= loop_is_endless;
                return;
              }
              if (tarval_cmp(loop_end, tarval_add(get_mode_min(get_tarval_mode(loop_start)), tarval_one)) == pn_Cmp_Eq)
                if(tarval_cmp(loop_step, tarval_one) == pn_Cmp_Gt)
                  info->l_itervar_phi->flags |= loop_end_false;
                info->l_itervar_phi->loop_iter_end = tarval_add(loop_end, diff);
            }else{
              /* iterator < loop_end*/
              if(pnc == pn_Cmp_Lt){
                /* A endless loop in mathematic mean that iterate until 0x0 as  for(i = 5; i <= 10; i--) */
                if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt )
                  *is_endlessloop = 1;
                else
                  /* A loop that can't be entered as  for(i = 4; i > 5; i++) */
                  *is_nonentered_loop = 1;
                /* iterator >= loop_end*/
              }else{
                /* Test if the cmp is after the (in|de)crementation and sub to loop start one.*/
                if(get_irn_link(info->cmp_const) != NULL)
                  loop_start = tarval_sub(loop_start, loop_step);
                /* A endless loop in mathematic mean that iterate until 0x0 as  for(i = 5; i <= 10; i--) */
                if(tarval_cmp(loop_start, loop_end) == pn_Cmp_Eq ||
                  tarval_cmp(loop_start, loop_end) == pn_Cmp_Lt)
                  *is_endlessloop = 1;
                else
                  /* A loop that can't be entered as  for(i = 4; i >= 5; i++) */
                  *is_nonentered_loop = 1;
              }
            }
      }
}

int is_do_loop(induct_var_info *info)
{
  ir_node *cmp_pred_0, *cmp_pred_1;
  cmp_pred_0 = get_irn_n(info->cmp, 0);
  cmp_pred_1 = get_irn_n(info->cmp, 1);
  if(get_irn_op(cmp_pred_0) == op_Phi ||
     get_irn_op(cmp_pred_1) == op_Phi)
    return 0;
  else
    return 1;
}


/**
 * set the loop end if the loop couldn't be computed from the function "compute_loop_end",
 * the loop is endless in mathematic mean or the loop is dead.
 * @param is_endlessloop   A integer, that is set to 1, if the loop is endlees in mathematic mean.
 * @param is_nonentered_loop A integer, that is  set to 1, if the loop is dead, that mean the loop can't be trodden.
 * @param *info             A struct, that contain loop information.
 */
static void set_loop_end(int is_endlessloop, int is_nonentered_loop, induct_var_info *info)
{

  if (is_endlessloop) {
    if (info->l_itervar_phi->flags & loop_downto_loop)
      info->l_itervar_phi->loop_iter_end = get_mode_min(get_tarval_mode(get_Const_tarval(info->cmp_const)));
    else
      info->l_itervar_phi->loop_iter_end = get_mode_max(get_tarval_mode(get_Const_tarval(info->cmp_const)));
  } else if (is_nonentered_loop || info->l_itervar_phi->loop_iter_end == NULL)
    info->l_itervar_phi->loop_iter_end = get_Const_tarval(info->cmp_const);
}

/**
 * Walker: checks every phi node for a induction variable and
 * compute some loop information.
 *
 * @param n        An IR node.
 * @param env      Free environment pointer.
 */
static void set_loop_info(ir_node *n, void *env)
{
  tarval *loop_end;
  induct_var_info info;
  int is_endlessloop = 0 , is_nonentered_loop = 0;
  (void) env;

  /* The IR node must be a induction variable. */
  if (get_irn_op(n) != op_Phi)
    return;

  info.itervar_phi = n;

  /* Just induction variables are tested. */
  if (!is_induction_variable(&info))
    return;
  /* If the loop haven't a "cmp" node to break the iteration,
   * then this loop can't be a count loop.*/
  if (info.cmp == NULL) {
    return;
  } else {
    set_irn_link(info.cmp, NODE_VISITED);
    if(!is_count_loop(info.itervar_phi, get_loop_node(info.l_itervar_phi, 0)))
      return;
  }

  /* The loop start, end and iteration step muss be constants. */
  if (get_irn_op(info.increment) != op_Const   ||
      get_irn_op(info.init) != op_Const        ||
      get_irn_op(info.cmp_const) != op_Const) {
    return;
  }
  if(is_do_loop(&info))
     info.l_itervar_phi->flags |= do_loop;

  info.l_itervar_phi->loop_iter_start= get_Const_tarval(info.init);
  loop_end  = get_Const_tarval(info.cmp_const);
  info.l_itervar_phi->loop_iter_increment = get_Const_tarval(info.increment);
  info.l_itervar_phi->loop_iter_variable  = info.itervar_phi;

  compute_loop_end(info.l_itervar_phi->loop_iter_start, loop_end,  info.l_itervar_phi->loop_iter_increment,
			  &is_endlessloop, &is_nonentered_loop, &info);
  if(is_endlessloop)
    info.l_itervar_phi->flags |= loop_wrap_around;

  if(is_nonentered_loop){
    if(info.l_itervar_phi->flags & do_loop)
      info.l_itervar_phi->flags |= once;
    else
      info.l_itervar_phi->flags |= loop_is_dead;
  }
  /* The loop is a count loop and the information is computed.*/
  info.l_itervar_phi->flags              |= loop_is_count_loop;

  set_loop_end(is_endlessloop, is_nonentered_loop, &info);

}

/* Compute additional loop information
 *
 * @param *irg  The current ir graph.
 */
void compute_loop_info(ir_graph *irg)
{
  /* Call algorithm that computes the out edges */
  compute_irg_outs(irg);

  /* Call algorithm that computes the loop information */
  construct_cf_backedges(irg);

  /* -- Search expressions that can be optimized -- */
  irg_walk_graph(irg, NULL, set_loop_info, NULL);
}
