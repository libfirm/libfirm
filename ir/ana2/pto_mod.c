/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_mod.c
   Purpose:     Load/Store Transfer Functions
   Author:      Florian
   Modified by:
   Created:     Fri Nov 26 17:29:49 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/*
  pto_mod: Load/Store Transfer Functions
*/

# include "pto_mod.h"

# include "xmalloc.h"

# include "pto_debug.h"
# include "pto_name.h"

/* Local Defines: */

/* Local Data Types: */

/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */


/* ===================================================
   Exported Implementation:
   =================================================== */
/* Perform the given store; return nonzero iff any involved values change */
int mod_store (ir_node *store, entity *ent,
                pto_t *ptr_pto, pto_t *val_pto)
{
  int change = 0;

  /* foreach descr in ptr_pto, add val_pto->values to descr.ent */

  qset_t *ptos = ptr_pto->values;

  desc_t *desc = (desc_t*) qset_start (ptos);

  while (NULL != desc) {
    qset_t *entry = get_entry (desc, ent);

    change |= qset_insert_all (entry, val_pto->values);

    desc = (desc_t*) qset_next (ptos);
  }

  return (change);
}

/* Perform the given load; return nonzero iff any involved values change */
int mod_load  (ir_node *load, entity *ent,
                pto_t *ptr_pto)
{
  int change = 0;
  pto_t *res = get_node_pto (load);
  /* todo: for each descr in ptr_pto, add descr.ent to res */

  qset_t *ptos = ptr_pto->values;
  desc_t *desc = (desc_t*) qset_start (ptos);

  while (NULL != desc) {
    qset_t *entry = get_entry (desc, ent);

    change |= qset_insert_all (res->values, entry);

    desc = (desc_t*) qset_next (ptos);
  }

  return (change);
}



/*
  $Log$
  Revision 1.2  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.1  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration


*/
