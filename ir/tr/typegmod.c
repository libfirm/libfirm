/* Copyright (C) 2001 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
*/

# include "typegmod_t.h"
# include "type_t.h"
# include "tpop_t.h"
# include "irmode.h"

inline void exchange_types(type *old_type, type *new_type) {
  int i;
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
     * Deallocation:  if the Id is removed from the list it will eventualle
       disappear in a memory leak.  When is impossible to determine so we
       need to hold it in a seperate list for deallocation.
  */

  /* Exchange the types */
  old_type->type_op = type_id;
  old_type->mode = (ir_mode *) new_type;
}

inline type *skip_tid(type *tp) {
  while (tp->type_op == type_id)
    tp = (type *) tp->mode;
  return tp;
}
