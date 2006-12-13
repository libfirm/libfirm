/*
 * Project:     libFIRM
 * File name:   ir/debug/dbginfo.c
 * Purpose:     Implements the Firm interface to debug information.
 * Author:      Goetz Lindenmaier
 * Modified by: Michael Beck
 * Created:     2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "dbginfo_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "entity_t.h"

void
default_dbg_info_merge_pair(ir_node *nw, ir_node *old, dbg_action info) {
  dbg_info *old_db = get_irn_dbg_info(old);
  if (old_db)
    set_irn_dbg_info(nw, old_db);
}

void
default_dbg_info_merge_sets(ir_node **new_nodes, int n_new_nodes,
            ir_node **old_nodes, int n_old_nodes,
            dbg_action info) {
}

merge_pair_func *__dbg_info_merge_pair = default_dbg_info_merge_pair;

merge_sets_func *__dbg_info_merge_sets = default_dbg_info_merge_sets;

snprint_dbg_func *__dbg_info_snprint   = (snprint_dbg_func *)0;

void dbg_init( merge_pair_func *mpf, merge_sets_func *msf, snprint_dbg_func *snprint_dbg )
{
  __dbg_info_merge_pair = mpf ? mpf : default_dbg_info_merge_pair;
  __dbg_info_merge_sets = msf ? msf : default_dbg_info_merge_sets;
  __dbg_info_snprint    = snprint_dbg;
}


void
set_irn_dbg_info(ir_node *n, struct dbg_info* db) {
  n->dbi = db;
}

struct dbg_info *
get_irn_dbg_info(const ir_node *n) {
  return n->dbi;
}


/* Routines to access the field of an entity containing the
   debugging information. */
void set_entity_dbg_info(ir_entity *ent, dbg_info* db) {
  ent->dbi = db;
}

dbg_info *get_entity_dbg_info(ir_entity *ent) {
  return ent->dbi;
}

/* Routines to access the field of a type containing the
   debugging information. */
void set_type_dbg_info(ir_type *tp, dbg_info* db) {
  tp->dbi = db;
}

dbg_info *get_type_dbg_info(ir_type *tp) {
  return tp->dbi;
}

/*
 * Converts a debug_action into a string.
 */
const char *dbg_action_2_str(dbg_action a) {
#define CASE(a) case a: return #a

  switch (a) {
  CASE(dbg_error);
  CASE(dbg_opt_ssa);
  CASE(dbg_opt_auxnode);
  CASE(dbg_const_eval);
  CASE(dbg_opt_cse);
  CASE(dbg_straightening);
  CASE(dbg_if_simplification);
  CASE(dbg_algebraic_simplification);
  CASE(dbg_write_after_write);
  CASE(dbg_write_after_read);
  CASE(dbg_read_after_write);
  CASE(dbg_read_after_read);
  CASE(dbg_read_a_const);
  CASE(dbg_rem_poly_call);
  CASE(dbg_dead_code);
  CASE(dbg_opt_confirm);
  CASE(dbg_backend);
  default:
    if (a <= dbg_max)
      return "string conversion not implemented";
    else
      assert(0);
    return NULL;
  }
#undef CASE
}
