/*
 * Project:     libFIRM
 * File name:   ir/opt/proc_cloning.c
 * Purpose:     procedure cloning
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file proc_cloning.c
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <string.h>

#include "tv.h"
#include "set.h"
#include "entity.h"
#include "irprog_t.h"
#include "hashptr.h"
#include "irgwalk.h"
#include "proc_cloning.h"
#include "analyze_irg_args.h"
#include "irprintf.h"

/* A macro to iterate sets.*/
#define ITERATE_SET(set_entrys, entry) for(entry = set_first(set_entrys); entry; entry = set_next(set_entrys))

/**
 * This struct contains the information triple for a Call, which we need to
 * decide if this function must be cloned.
 */
typedef struct _entry {
  struct triple {
    entity        *ent;   /**< The entity of our Call. */
    int           pos;    /**< Position of a constant argument of our Call. */
    tarval        *tv;    /**< The tarval of this constant argument. */
  } t;                    /**< The heuristic triple. */

  float         weight;   /**< The estimated weight of this triple. */
  struct _entry *next;    /**< used for linking and sorting */
} entry_t;

/**
 * Compare two triples.
 *
 * @return 0 if they are identically
 */
static int entry_cmp(const void *elt, const void *key, size_t size)
{
  const entry_t *c1 = elt;
  const entry_t *c2 = key;

  return (c1->t.ent != c2->t.ent) || (c1->t.pos != c2->t.pos) || (c1->t.tv != c2->t.tv);
}

/**
 * Hash a element of typ entry_t
 *
 * @param name    A string (the name of the method)
 * @param length  The length of this string
 * @param pos     A integer value.
 */
static int hash_entry(const entry_t *entry)
{
  return HASH_PTR(entry->t.ent) ^ HASH_PTR(entry->t.tv) ^ (entry->t.pos * 9);
}

/**
 * Collect all calls in a ir_graph
 * to a set.
 *
 * @param call   A ir_node to be checked.
 * @param env    The set where we will collect the calls.
 */
static void collect_irg_calls(ir_node *call, void *env)
{
  ir_node *call_ptr;
  entity *call_ent;
  set *entrys;
  entry_t key, *entry;
  ir_node *call_param;
  int i;


  /* We collect just "Call" nodes*/
  if (get_irn_op(call) != op_Call)
    return;

  call_ptr = get_Call_ptr(call);

  /* Call pointer must be a symconst*/
  if (op_SymConst != get_irn_op(call_ptr))
    return;
  /* Call pointer must be the address of an entity.*/
  if (get_SymConst_kind(call_ptr) != symconst_addr_ent)
    return;

  entrys = env;
  call_ent = get_SymConst_entity(call_ptr);

  /* In this for loop we collect the calls, that have
     an constant parameter. */
  for (i = get_Call_n_params(call) - 1; i >= 0; i--) {
    call_param = get_Call_param(call, i);
    if (is_Const(call_param)) {
      /* we have found a Call to collect and we save the informations,
         which we need.*/
      key.t.ent  = call_ent;
      key.t.pos  = i;
      key.t.tv   = get_Const_tarval(call_param);
      key.weight = 0.0f;

      /* We insert our information in the set, where we collect the calls.*/
      entry     = set_insert(entrys, &key, sizeof(key), hash_entry(&key));

      /* we found one more */
      entry->weight += 1.0f;
    }
  }
}

/*
 * Do the procedure cloning. Evaluate a heuristic weight for every
 * call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 */
void proc_cloning(float threshold)
{
  set *set_entrys;
  entry_t *entry, *p, *heavy_uses = NULL;
  ir_graph *irg;
  int i;

  set_entrys  = new_set(entry_cmp, 8);
  entry       = NULL;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);
    irg_walk_graph(irg, collect_irg_calls, NULL, set_entrys);
  }

  /* We iterate the set and arrange the element of the set in a plist'
     The elements are arranged dependent of their value descending.*/
  ITERATE_SET(set_entrys, entry) {
    entry->weight *= get_method_param_weight(entry->t.ent, entry->t.pos);

    /*
     * Do not put entry with a weight < threshold in the list
     */
    if (entry->weight < threshold)
      continue;

    /* put entry in the heavy uses list */
    entry->next = NULL;
    if (! heavy_uses)
      heavy_uses = entry;
    else {
      if (entry->weight >= heavy_uses->weight) {
        entry->next = heavy_uses;
        heavy_uses  = entry;
      }
      else {
        for (p = heavy_uses; p->next; p = p->next) {
          if (entry->weight >= p->next->weight) {
	          entry->next = p->next;
            p->next     = entry;
	          break;
          }
        }
        if (! p->next)
          p->next = entry;
      }
    }
  }

  /* Print some informations about the list. */
  for (entry = heavy_uses; entry; entry = entry->next) {
    printf("\nweight: is %f\n", entry->weight);
    ir_printf("Call for Method %E\n", entry->t.ent);
    printf("Position %i\n", entry->t.pos);
    ir_printf("Value %T\n", entry->t.tv);
  }
}
