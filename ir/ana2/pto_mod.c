/* -*- c -*- */

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
 * @brief     Load/Store Transfer Functions
 * @author    Florian
 * @date      Fri Nov 26 17:29:49 CET 2004
 * @version   $Id$
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
int mod_store (ir_node *store, ir_entity *ent,
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
int mod_load  (ir_node *load, ir_entity *ent,
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
  Revision 1.3  2006/12/13 19:46:47  beck
  rename type entity into ir_entity

  Revision 1.2  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.1  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration


*/
