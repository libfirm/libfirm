/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer &
*          Goetz Lindenmaier
*
* firm_common.c:
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "firm_common_t.h"
#include "irgraph.h"

/* returns the kind of the thing */
firm_kind
get_kind (const void *firm_thing) {
  assert (firm_thing);
  return *(firm_kind *)firm_thing;
}


const char* print_firm_kind(void *firm_thing) {
  assert (firm_thing);
  switch (*(firm_kind *)firm_thing) {
    case k_entity  : { return "k_entity"  ;} break;
    case k_type    : { return "k_type"    ;} break;
    case k_ir_graph: { return "k_ir_graph";} break;
    case k_ir_node : { return "k_ir_node" ;} break;
    case k_ir_mode : { return "k_ir_mode" ;} break;
    case k_ir_op   : { return "k_ir_op"   ;} break;
    case k_tarval  : { return "k_tarval"  ;} break;
    case k_ir_loop : { return "k_ir_loop" ;} break;
  default: break;
  }
  return "";
}
