/*
 * Project:     libFIRM
 * File name:   ir/tr/typegmod.c
 * Purpose:     Functionality to modify the type graph.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

# include "typegmod.h"
# include "type_t.h"
# include "tpop_t.h"
# include "irmode.h"

INLINE void exchange_types(type *old_type, type *new_type) {
  /* Deallocate datastructures not directly contained in the
     old type.  We must do this now as it is the latest point
     where we know the original kind of type.
     */
  free_type_attrs(old_type);

  /* @@@@
     Things to deal with:
     * After exchange_types the type has two entries in the list of
       all types in irp.  So far this is fine for the walker.
       Maybe it's better to remove the id entry and shrink the list.
       Does this conflict with the walker?  Might a type be left out
       during the walk?
     * Deallocation:  if the Id is removed from the list it will eventually
       disappear in a memory leak.  When is impossible to determine so we
       need to hold it in a separate list for deallocation.
  */

  /* Exchange the types */
  old_type->type_op = type_id;
  old_type->mode = (ir_mode *) new_type;
}

INLINE type *skip_tid(type *tp) {
  /* @@@ implement the self cycle killing trick of skip_id(ir_node *) */
  while (tp->type_op == type_id)
    tp = (type *) tp->mode;
  return tp;
}
