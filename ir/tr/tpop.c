/*
 * Project:     libFIRM
 * File name:   ir/tr/tpop.c
 * Purpose:     Opcode of types.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

# include "xmalloc.h"
# include "tpop_t.h"
# include "type_t.h"

tp_op *type_class;         tp_op *get_tpop_class      (void) { return type_class;       }
tp_op *type_struct;        tp_op *get_tpop_struct     (void) { return type_struct;      }
tp_op *type_method;        tp_op *get_tpop_method     (void) { return type_method;      }
tp_op *type_union;         tp_op *get_tpop_union      (void) { return type_union;       }
tp_op *type_array;         tp_op *get_tpop_array      (void) { return type_array;       }
tp_op *type_enumeration;   tp_op *get_tpop_enumeration(void) { return type_enumeration; }
tp_op *type_pointer;       tp_op *get_tpop_pointer    (void) { return type_pointer;     }
tp_op *type_primitive;     tp_op *get_tpop_primitive  (void) { return type_primitive;   }
tp_op *type_id;            tp_op *get_tpop_id         (void) { return type_id;          }
tp_op *tpop_none;          tp_op *get_tpop_none       (void) { return tpop_none;        }
tp_op *tpop_unknown;       tp_op *get_tpop_unknown    (void) { return tpop_unknown;     }

tp_op *
new_tpop (tp_opcode code, ident *name, unsigned flags, size_t attr_size)
{
  tp_op *res;

  res = xmalloc(sizeof(*res));
  res->code = code;
  res->name = name;
  res->attr_size = attr_size;
  return res;
}

INLINE void
free_tpop(tp_op *tpop) {
  free(tpop);
}

#define C     TP_OP_FLAG_COMPOUND
#define ID(s) new_id_from_chars(s, sizeof(s) - 1)
void
init_tpop(void)
{
  type_class       = new_tpop(tpo_class      , ID("class"),       C, sizeof (cls_attr));
  type_struct      = new_tpop(tpo_struct     , ID("struct"),      C, sizeof (stc_attr));
  type_method      = new_tpop(tpo_method     , ID("method"),      0, sizeof (mtd_attr));
  type_union       = new_tpop(tpo_union      , ID("union"),       C, sizeof (uni_attr));
  type_array       = new_tpop(tpo_array      , ID("array"),       C, sizeof (arr_attr));
  type_enumeration = new_tpop(tpo_enumeration, ID("enumeration"), 0, sizeof (enm_attr));
  type_pointer     = new_tpop(tpo_pointer    , ID("pointer"),     0, sizeof (ptr_attr));
  type_primitive   = new_tpop(tpo_primitive  , ID("primitive"),   0, /* sizeof (pri_attr) */ 0);
  type_id          = new_tpop(tpo_id         , ID("type_id"),     0, /* sizeof (id_attr)  */ 0);
  tpop_none        = new_tpop(tpo_none       , ID("tpop_none"),   0, /* sizeof (non_attr) */ 0);
  tpop_unknown     = new_tpop(tpo_unknown    , ID("tpop_unknown"),0, /* sizeof (ukn_attr) */ 0);
}
#undef ID
#undef C

/* Finalize the tpop module.
 * Frees all type opcodes.  */
void finish_tpop(void) {
  free_tpop(type_class      ); type_class       = NULL;
  free_tpop(type_struct     ); type_struct      = NULL;
  free_tpop(type_method     ); type_method      = NULL;
  free_tpop(type_union      ); type_union       = NULL;
  free_tpop(type_array      ); type_array       = NULL;
  free_tpop(type_enumeration); type_enumeration = NULL;
  free_tpop(type_pointer    ); type_pointer     = NULL;
  free_tpop(type_primitive  ); type_primitive   = NULL;
  free_tpop(type_id         ); type_id          = NULL;
  free_tpop(tpop_none       ); tpop_none        = NULL;
  free_tpop(tpop_unknown    ); tpop_unknown     = NULL;
}

/* Returns the string for the tp_opcode. */
const char  *get_tpop_name(const tp_op *op) {
  return get_id_str(op->name);
}

tp_opcode (get_tpop_code)(const tp_op *op){
  return __get_tpop_code(op);
}

ident *(get_tpop_ident)(const tp_op *op){
  return __get_tpop_ident(op);
}

/* returns the attribute size of the operator. */
int (get_tpop_attr_size)(const tp_op *op) {
  return __get_tpop_attr_size(op);
}
