/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** firm_common.c:
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "firm_common_t.h"
#include "irgraph.h"

/* returns the kind of the thing */
firm_kind
get_kind (void *firm_thing) {
  assert (firm_thing);
  return *(firm_kind *)firm_thing;
}


const char* print_firm_kind(void *firm_thing) {
  assert (firm_thing);
  switch (*(firm_kind *)firm_thing) {
    case k_entity: { return "k_enitity"; } break;
    case k_type: { return "k_type"; } break;
#if 0
    case k_type_class: { return "k_type_class"; } break;
    case k_type_strct: { return "k_type_strct:"; } break;
    case k_type_method: { return "k_type_method:"; } break;
    case k_type_union: { return "k_type_union"; } break;
    case k_type_array: { return "k_type_array"; } break;
    case k_type_enumeration: { return "k_type_enumeration"; } break;
    case k_type_pointer: { return "k_type_pointer"; } break;
    case k_type_primitive: { return "k_type_primitive"; } break;
#endif
    case k_ir_node: { return "k_ir_node"; } break;
  default: break;
  }
  return "";
}
