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
     old type */
  /* @@@@ */

  /* Remove old type from type list.  Will this confuscate the
     iterators? */
  /* @@@ */

  /* Ev. add to a list of id types for later deallocation. */
  /* @@@ */

  /* Exchange the types */
  old_type->type_op = type_id;
  old_type->mode = (ir_mode *) new_type;
}

inline type *skip_tid(type *tp) {
  while (tp->type_op == type_id)
    tp = (type *) tp->mode;
  return tp;
}
