/*
 * Project:     libFIRM
 * File name:   ir/common/firm_common.c
 * Purpose:
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


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
  if (! firm_thing)
    return "(NULL)";

  switch (*(firm_kind *)firm_thing) {
  case k_entity                 : return "k_entity";
  case k_type                   : return "k_type";
  case k_ir_graph               : return "k_ir_graph";
  case k_ir_node                : return "k_ir_node";
  case k_ir_mode                : return "k_ir_mode";
  case k_ir_op                  : return "k_ir_op";
  case k_tarval                 : return "k_tarval";
  case k_ir_loop                : return "k_ir_loop";
  case k_ir_compound_graph_path : return "k_ir_compound_graph_path";
  default: return "";
  }
}

/*
 * identify a firm thing
 */
void firm_identify_thing(void *X)
{
  firm_kind *p = X;

  if (! p) {
    printf("(NULL)\n");
    return;
  }

  switch (*p) {
  case k_BAD:
    printf("BAD: (%p)\n", X);
    break;
  case k_entity:
    printf("entity: %s: %ld (%p)\n", get_entity_name(X), get_entity_nr(X), X);
    break;
  case k_type:
    printf("type: %s %s: %ld (%p)\n", get_type_tpop_name(X), get_type_name(X), get_type_nr(X), X);
    break;
  case k_ir_graph:
    printf("graph: %s: %ld (%p)\n", get_entity_name(get_irg_ent(X)), get_irg_graph_nr(X), X);
    break;
  case k_ir_node:
    printf("irnode: %s%s %ld (%p)\n", get_irn_opname(X), get_mode_name(get_irn_mode(X)), get_irn_node_nr(X), X);
    break;
  case k_ir_mode:
    printf("mode %s: (%p)\n", get_mode_name(X),X);
    break;
  case k_tarval:
    printf("tarval : "); tarval_printf(X); printf(" (%p)\n", X);
    break;
  case k_ir_loop:
    printf("loop: with depth %d: (%p)\n", get_loop_depth(X), X);
    break;
  case k_ir_op:
  case k_ir_compound_graph_path:
  default:
    printf("Cannot identify thing at (%p).\n", X);
  }
}
