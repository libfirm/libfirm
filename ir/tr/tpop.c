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
# include <config.h>
#endif

# include "xmalloc.h"
# include "tpop_t.h"
# include "type_t.h"

tp_op *type_class;         tp_op *get_type_class      () { return type_class;       }
tp_op *type_struct;        tp_op *get_type_struct     () { return type_struct;      }
tp_op *type_method;        tp_op *get_type_method     () { return type_method;      }
tp_op *type_union;         tp_op *get_type_union      () { return type_union;       }
tp_op *type_array;         tp_op *get_type_array      () { return type_array;       }
tp_op *type_enumeration;   tp_op *get_type_enumeration() { return type_enumeration; }
tp_op *type_pointer;       tp_op *get_type_pointer    () { return type_pointer;     }
tp_op *type_primitive;     tp_op *get_type_primitive  () { return type_primitive;   }
tp_op *type_id;            tp_op *get_type_id         () { return type_id;          }

tp_op *
new_tpop (tp_opcode code, ident *name, size_t attr_size)
{
  tp_op *res;

  res = (tp_op *) xmalloc (sizeof (tp_op));
  res->code = code;
  res->name = name;
  res->attr_size = attr_size;
  return res;
}

void
init_tpop(void)
{
  type_class       = new_tpop (tpo_class      , id_from_str("class"      , 5), sizeof (cls_attr));
  type_struct      = new_tpop (tpo_struct     , id_from_str("struct"     , 6), sizeof (stc_attr));
  type_method      = new_tpop (tpo_method     , id_from_str("method"     , 6), sizeof (mtd_attr));
  type_union       = new_tpop (tpo_union      , id_from_str("union"      , 5), sizeof (uni_attr));
  type_array       = new_tpop (tpo_array      , id_from_str("array"      , 5), sizeof (arr_attr));
  type_enumeration = new_tpop (tpo_enumeration, id_from_str("enumeration",11), sizeof (enm_attr));
  type_pointer     = new_tpop (tpo_pointer    , id_from_str("pointer"    , 7), sizeof (ptr_attr));
  type_primitive   = new_tpop (tpo_primitive  , id_from_str("primitive"  , 9), /* sizeof (pri_attr) */ 0);
  type_id          = new_tpop (tpo_id         , id_from_str("type_id"    , 7), /* sizeof (id_attr)  */ 0);
}

/* Returns the string for the tp_opcode. */
const char  *get_tpop_name      (tp_op *op) {
  return get_id_str(op->name);
}

tp_opcode get_tpop_code (tp_op *op){
  return op->code;
}

ident *get_tpop_ident(tp_op *op){
  return op->name;
}

/* returns the attribute size of the operator. */
int get_tpop_attr_size (tp_op *op) {
  return op->attr_size;
}
