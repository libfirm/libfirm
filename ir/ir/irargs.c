/*
 * Project:     libFIRM
 * File name:   ir/ir/irargs.c
 * Purpose:     Support for libcore IR object output.
 * Author:      Sebastian Hack
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <ctype.h>
#include <libcore/xprintf.h>

#include "firm_common.h"
#include "irnode.h"
#include "entity.h"
#include "irloop.h"

/**
 * identify a firm object type
 */
static int firm_get_arg_type(const arg_occ_t *occ) {
  /* Firm objects are always pointer */
  return arg_type_ptr;
}

/**
 * emit a Firm object
 */
static int firm_emit(appendable_t *app, const arg_occ_t *occ, const arg_value_t *arg)
{
#define A(s)    occ->hash ? s " ": ""

  void *X = arg->v_ptr;
  firm_kind *obj = X;

  int i, n;
  ir_node *block;
  char buf[256];
  char tv[256];

  buf[0] = '\0';

  if (! X)
    strncpy(buf, "(null)", sizeof(buf));
  else {
    switch (*obj) {
    case k_BAD:
      snprintf(buf, sizeof(buf), "BAD[%p]", X);
      break;
    case k_entity:
      snprintf(buf, sizeof(buf), "%s%s[%ld]", A("ent"),
          isupper(occ->conversion) ? get_entity_ld_name(X): get_entity_name(X),
          get_entity_nr(X));
      break;
    case k_type:
      snprintf(buf, sizeof(buf), "%s%s:%s[%ld]", A("type"), get_type_tpop_name(X), get_type_name(X), get_type_nr(X));
      break;
    case k_ir_graph:
      snprintf(buf, sizeof(buf), "%s%s[%ld]", A("irg"), get_entity_name(get_irg_entity(X)), get_irg_graph_nr(X));
      break;
    case k_ir_node:
      switch (occ->conversion) {
      case 'B':
        block = is_no_Block(X) ? get_nodes_block(X) : X;
        snprintf(buf, sizeof(buf), "%s%s%s[%ld]", A("irn"), get_irn_opname(block),
            get_mode_name(get_irn_mode(block)), get_irn_node_nr(block));
        break;
      case 'N':
        snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(X));
        break;
      default:
        snprintf(buf, sizeof(buf), "%s%s%s[%ld]", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)), get_irn_node_nr(X));
      }
      break;
    case k_ir_mode:
      snprintf(buf, sizeof(buf), "%s%s", A("mode"), get_mode_name(X));
      break;
    case k_tarval:
      tarval_snprintf(tv, sizeof(tv), X);
      snprintf(buf, sizeof(buf), "%s%s", A("tv"), tv);
      break;
    case k_ir_loop:
      snprintf(buf, sizeof(buf), "ldepth[%d]", get_loop_depth(X));
      break;
    case k_ir_op:
      snprintf(buf, sizeof(buf), "%s%s", A("op"), get_op_name(X));
      break;
    case k_ir_compound_graph_path:
      strncat(buf, A("cgp"), sizeof(buf));

      n = get_compound_graph_path_length(X);
      for (i = 0; i < n; ++i) {
        entity *ent = get_compound_graph_path_node(X, i);
        strncat(buf, get_entity_name(ent), sizeof(buf));
        if (i < n - 1)
          strncat(buf, ".", sizeof(buf));
      }
      break;
    default:
      snprintf(buf, sizeof(buf), "UNKWN[%p]", X);
    }
  }
  return arg_append(app, occ, buf, strlen(buf));

#undef A
}

/**
 * emit an ident
 */
static int firm_emit_ident(appendable_t *app, const arg_occ_t *occ, const arg_value_t *arg)
{
  ident *id = (ident *)arg->v_ptr;
  const char *p = id ? get_id_str(id) : "(null)";

  return arg_append(app, occ, p, strlen(p));
}


arg_env_t *firm_get_arg_env(void)
{
#define X(name, letter) {"firm:" name, letter}

  static arg_env_t *env = NULL;

  static arg_handler_t firm_handler  = { firm_get_arg_type, firm_emit };
  static arg_handler_t ident_handler = { firm_get_arg_type, firm_emit_ident };

  static struct {
    const char *name;
    char letter;
  } args[] = {
    X("type",      't'),
    X("entity",    'e'),
    X("entity_ld", 'E'),
    X("tarval",    'T'),
    X("irn",       'n'),
    X("op",        'O'),
    X("irn_nr",    'N'),
    X("mode",      'm'),
    X("block",     'B'),
  };

  int i;

  if(env == NULL) {
    env = arg_new_env();
    arg_add_std(env);

    for (i = 0; i < sizeof(args)/sizeof(args[0]); ++i)
      arg_register(env, args[i].name, args[i].letter, &firm_handler);

    arg_register(env, "ident", 'I', &ident_handler);
  }

  return env;
}
