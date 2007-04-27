/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/*
 * Project:     libFIRM
 * File name:   ir/tr/tpop_t.h
 * Purpose:     Opcode of types -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 */
#ifndef _TPOP_T_H_
#define _TPOP_T_H_

#include <stdlib.h>

#include "firm_types.h"
#include "tpop.h"
#include "irmode.h"

/**
 * @file tpop_t.h
 *
 * This file contains the datatypes hidden in tpop.h.
 *
 * @author Goetz Lindenmaier
 * @see  tpop.h
 */

/** A function called to free attributes of a type. */
typedef void (*free_attrs_func)(ir_type *tp);

/** A function called to free owned entities of a type. */
typedef void (*free_entities_func)(ir_type *tp);

/** A function called to free all automatic allocated entities of a type. */
typedef void (*free_auto_entities_func)(ir_type *tp);

/** A function called to set the mode of a type. */
typedef void (*set_type_mode_func)(ir_type *tp, ir_mode *m);

/** A function called to set the size of a type in bits */
typedef void (*set_type_size_func)(ir_type *tp, int size);

/** A function called to get the number of compound members */
typedef int (*get_n_members_func)(const ir_type *tp);

/** A function called to get the pos'th compound member */
typedef ir_entity *(*get_member_func)(const ir_type *tp, int pos);

typedef int (*get_member_index_func)(const ir_type *tp, ir_entity *member);

/** A function called to insert an entity into the type */
typedef void (*insert_entity_func)(ir_type *tp, ir_entity *member);

/**
 * tp_op operations.
 */
typedef struct _tp_op_ops {
  free_attrs_func         free_attrs;         /**< called to free the attributes of a type */
  free_entities_func      free_entities;      /**< called to free the owned entities of a type */
  free_auto_entities_func free_auto_entities; /**< called to free the automatic allocated entities of a type */
  set_type_mode_func      set_type_mode;      /**< called to set a ir_mode of a type */
  set_type_size_func      set_type_size;      /**< called to set the bit size of a type */
  get_n_members_func      get_n_members;      /**< called to return the number of compound members */
  get_member_func         get_member;         /**< called to get the pos'th compound member */
  get_member_index_func   get_member_index;   /**< called to get the index of a compound member */
} tp_op_ops;

/** possible flags for a type opcode */
enum tp_op_flags_t {
  TP_OP_FLAG_COMPOUND = 1   /**< is a compound type */
};

/** The type opcode */
struct tp_op {
  tp_opcode code;                     /**< the tpop code */
  ident     *name;                    /**< the name of the type opcode */
  size_t    attr_size;                /**< the attribute size for a type of this opcode */
  unsigned  flags;                    /**< flags for this opcode */
  tp_op_ops ops;                      /**< tp_op operations */
};

/**
 *   Returns a new type opcode.
 *
 *   Allocates a new tp_op struct and initializes it's fields with
 *   the passed values.  This function is only to be used during
 *   initialization of the library.
 *
 *   @param code        the enum for this type opcode.
 *   @param name        an ident for the name of the type opcode.
 *   @param flags       additional flags
 *   @param attr_size   the size of the attributes necessary for a type with
 *                      this opcode
 *   @param ops         the tp_op operations for this type
 *   @return A new type opcode.
 */
tp_op *new_tpop (tp_opcode code, ident *name, unsigned flags, size_t attr_size,
                 const tp_op_ops *ops);

/**
 * Free a tpop datastructure.
 */
void free_tpop(tp_op *tpop);

/**
 *   Initialize the tpop module.
 *
 *   Must be called during the initialization of the library. Allocates
 *   opcodes and sets the globals that are external visible as specified
 *   in tpop.h.
 *   Allocates opcodes for classes, struct, method, union, array,
 *   enumeration, pointer and primitive and sets the according values.
 */
void init_tpop (void);

/**
 *  Finalize the tpop module.
 *
 *  Frees all type opcodes.
 */
void finish_tpop(void);

/**
 *   Returns the size of the attribute to this kind
 *   of type.
 *
 *   Internal feature.
 *
 *   @param op  The type opcode to get the size for.
 *   @return The size of the attribute of types with this opcode.
 *
 */
int get_tpop_attr_size (const tp_op *op);


/* ---------------- *
 * inline functions *
 * -----------------*/

static INLINE tp_opcode
_get_tpop_code(const tp_op *op) {
  return op->code;
}

static INLINE ident *
_get_tpop_ident(const tp_op *op){
  return op->name;
}

static INLINE size_t
_get_tpop_attr_size(const tp_op *op) {
  return op->attr_size;
}

#define get_tpop_code(op)      _get_tpop_code(op)
#define get_tpop_ident(op)     _get_tpop_ident(op)
#define get_tpop_attr_size(op) _get_tpop_attr_size(op)

#endif /* _TPOP_T_H_ */
