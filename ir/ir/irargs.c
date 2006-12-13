/*
 * Project:     libFIRM
 * File name:   ir/ir/irargs.c
 * Purpose:     Support for libcore IR object output.
 * Author:      Sebastian Hack
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef WITH_LIBCORE

#include "bitset.h"

#include <ctype.h>
#include <libcore/lc_printf.h>

#include "firm_common.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "irloop_t.h"
#include "tv_t.h"
#include "dbginfo_t.h"

/**
 * identify a firm object type
 */
static int firm_get_arg_type(const lc_arg_occ_t *occ) {
  /* Firm objects are always pointer */
  return lc_arg_type_ptr;
}

static int firm_get_arg_type_int(const lc_arg_occ_t *occ) {
  return lc_arg_type_int;
}


static int bitset_get_arg_type(const lc_arg_occ_t *occ) {
  return lc_arg_type_ptr;
}

static int bitset_emit(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
  int res = 2;
  bitset_t *b = arg->v_ptr;
  bitset_pos_t p;
  char buf[32];
  const char *prefix = "";

  lc_arg_append(app, occ, "[", 1);
  bitset_foreach(b, p) {
    int n;

    n = snprintf(buf, sizeof(buf), "%s%d", prefix, (int) p);
    lc_arg_append(app, occ, buf, n);
    prefix = ", ";
    res += n;
  }
  lc_arg_append(app, occ, "]", 1);

  return res;
}

/**
 * emit an opaque Firm dbg_info object
 */
static int firm_emit_dbg(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
  char buf[1024];
  ir_node *irn = arg->v_ptr;
  dbg_info *dbg = get_irn_dbg_info(irn);

  buf[0] = '\0';
  if (dbg && __dbg_info_snprint) {
    if (__dbg_info_snprint(buf, sizeof(buf), dbg) <= 0)
      buf[0] = '\0';
  }
  return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Beware: do not set the entity ld_name
 */
static const char *get_entity_ld_name_ex(ir_entity *ent) {
  if (ent->ld_name)
    return get_entity_ld_name(ent);
  return get_entity_name(ent);
}

/**
 * emit a Firm object
 */
static int firm_emit(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
#define A(s)    occ->flag_hash ? s " ": ""

  void *X = arg->v_ptr;
  firm_kind *obj = X;
  int i, n;
  ir_node *block;
  char add[64];
  char buf[256];
  char tv_buf[256];
  ir_entity *ent;

  buf[0] = '\0';
  add[0] = '\0';

  if (! X)
    strncpy(buf, "(null)", sizeof(buf));
  else {
    switch (*obj) {
    case k_BAD:
      snprintf(buf, sizeof(buf), "BAD");
      snprintf(add, sizeof(add), "[%p]", X);
      break;
    case k_entity:
      snprintf(buf, sizeof(buf), "%s%s", A("ent"),
          isupper(occ->conversion) ? get_entity_ld_name_ex(X): get_entity_name(X));
      snprintf(add, sizeof(add), "[%ld]", get_entity_nr(X));
      break;
    case k_type:
      snprintf(buf, sizeof(buf), "%s%s:%s", A("type"), get_type_tpop_name(X), get_type_name(X));
      snprintf(add, sizeof(add), "[%ld]", get_type_nr(X));
      break;
    case k_ir_graph:
      if (X == get_const_code_irg())
        snprintf(buf, sizeof(buf), "%s<ConstCodeIrg>", A("irg"));
      else
        snprintf(buf, sizeof(buf), "%s%s", A("irg"), get_entity_name(get_irg_entity(X)));
      snprintf(add, sizeof(add), "[%ld]", get_irg_graph_nr(X));
      break;
    case k_ir_node:
      switch (occ->conversion) {
      case 'B':
        block = is_no_Block(X) ? get_nodes_block(X) : X;
        snprintf(buf, sizeof(buf), "%s%s%s", A("irn"), get_irn_opname(block),
            get_mode_name(get_irn_mode(block)));
        snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(block));
        break;
      case 'N':
        snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(X));
        break;
      default:
        if (is_Const(X)) {
          tarval *tv = get_Const_tarval(X);
          if (tv)
            tarval_snprintf(tv_buf, sizeof(tv_buf), tv);
          else
            strncpy(tv_buf, "(NULL)", sizeof(tv_buf));
          snprintf(buf, sizeof(buf), "%s%s%s<%s>", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)), tv_buf);
        }
        else
          snprintf(buf, sizeof(buf), "%s%s%s", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)));
        snprintf(add, sizeof(add), "[%ld:%d]", get_irn_node_nr(X), get_irn_idx(X));
      }
      break;
    case k_ir_mode:
      snprintf(buf, sizeof(buf), "%s%s", A("mode"), get_mode_name(X));
      break;
    case k_tarval:
      tarval_snprintf(tv_buf, sizeof(tv_buf), X);
      snprintf(buf, sizeof(buf), "%s%s", A("tv"), tv_buf);
      break;
    case k_ir_loop:
      snprintf(buf, sizeof(buf), "loop[%d:%d]", get_loop_loop_nr(X), get_loop_depth(X));
      break;
    case k_ir_op:
      snprintf(buf, sizeof(buf), "%s%s", A("op"), get_op_name(X));
      break;
    case k_ir_compound_graph_path:
      n = get_compound_graph_path_length(X);

      for (i = 0; i < n; ++i) {
        ent = get_compound_graph_path_node(X, i);

        strncat(buf, ".", sizeof(buf));
        strncat(buf, get_entity_name(ent), sizeof(buf));
        if (is_Array_type(get_entity_owner(ent))) {
          snprintf(add, sizeof(add), "[%d]",
            get_compound_graph_path_array_index(X, i));
          strncat(buf, add, sizeof(buf));
        }
      }
      add[0] = '\0';
      break;
    case k_ir_extblk:
      snprintf(buf, sizeof(buf), "ExtBlock");
      snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(get_extbb_leader(X)));
      break;

    default:
      snprintf(buf, sizeof(buf), "UNKWN");
      snprintf(add, sizeof(add), "[%p]", X);
    }
  }

  if (occ->flag_plus)
  	strncat(buf, add, sizeof(buf));

  return lc_arg_append(app, occ, buf, strlen(buf));

#undef A
}

/**
 * emit an ident
 */
static int firm_emit_ident(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
  ident *id = (ident *)arg->v_ptr;
  const char *p = id ? get_id_str(id) : "(null)";

  return lc_arg_append(app, occ, p, strlen(p));
}

/**
 * Emit indent.
 */
static int firm_emit_indent(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	int i;
	int amount = arg->v_int * (occ->width > 0 ? occ->width : 1);

	for(i = 0; i < amount; ++i)
		lc_appendable_chadd(app, ' ');

	return amount;
}

/**
 * Emit pnc.
 */
static int firm_emit_pnc(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
  int value = arg->v_int;
  const char *p = get_pnc_string(value);

  return lc_arg_append(app, occ, p, strlen(p));
}

lc_arg_env_t *firm_get_arg_env(void)
{
#define X(name, letter) {"firm:" name, letter}

  static lc_arg_env_t *env = NULL;

  static lc_arg_handler_t firm_handler   = { firm_get_arg_type, firm_emit };
  static lc_arg_handler_t ident_handler  = { firm_get_arg_type, firm_emit_ident };
  static lc_arg_handler_t indent_handler = { firm_get_arg_type_int, firm_emit_indent };
  static lc_arg_handler_t pnc_handler    = { firm_get_arg_type_int, firm_emit_pnc };
  static lc_arg_handler_t bitset_handler = { bitset_get_arg_type, bitset_emit };
  static lc_arg_handler_t debug_handler  = { firm_get_arg_type, firm_emit_dbg };

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
    X("cg_path",   'P'),
  };

  int i;

  if(env == NULL) {
    env = lc_arg_new_env();
    lc_arg_add_std(env);

    lc_arg_register(env, "firm", 'F', &firm_handler);
    for (i = 0; i < sizeof(args)/sizeof(args[0]); ++i)
      lc_arg_register(env, args[i].name, args[i].letter, &firm_handler);

    lc_arg_register(env, "firm:ident",    'I', &ident_handler);
    lc_arg_register(env, "firm:indent",   '>', &indent_handler);
    lc_arg_register(env, "firm:dbg_info", 'G', &debug_handler);
    lc_arg_register(env, "firm:bitset",   'B', &bitset_handler);
    lc_arg_register(env, "firm:pnc",      '=', &pnc_handler);
  }

  return env;
}

#endif /* WITH_LIBCORE */
