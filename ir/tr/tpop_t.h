/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Opcode of types -- private header.
 * @author  Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_TR_TPOP_T_H
#define FIRM_TR_TPOP_T_H

#include <stdlib.h>

#include "firm_types.h"
#include "typerep.h"
#include "irmode.h"

#define get_tpop_code(op)      _get_tpop_code(op)
#define get_tpop_ident(op)     _get_tpop_ident(op)

/** A function called to free attributes of a type. */
typedef void (*free_attrs_func)(ir_type *tp);

/** A function called to free owned entities of a type. */
typedef void (*free_entities_func)(ir_type *tp);

/** A function called to free all automatic allocated entities of a type. */
typedef void (*free_auto_entities_func)(ir_type *tp);

/** A function called to set the mode of a type. */
typedef void (*set_type_mode_func)(ir_type *tp, ir_mode *m);

/** A function called to set the size of a type in bytes. */
typedef void (*set_type_size_func)(ir_type *tp, unsigned size);

/** A function called to get the number of compound members */
typedef size_t (*get_n_members_func)(const ir_type *tp);

/** A function called to get the pos'th compound member */
typedef ir_entity *(*get_member_func)(const ir_type *tp, size_t pos);

typedef size_t (*get_member_index_func)(const ir_type *tp, ir_entity *member);

/** A function called to insert an entity into the type */
typedef void (*insert_entity_func)(ir_type *tp, ir_entity *member);

/**
 * tp_op operations.
 */
typedef struct tp_op_ops {
	free_attrs_func         free_attrs;         /**< Called to free the attributes of a type. */
	free_entities_func      free_entities;      /**< Called to free the owned entities of a type. */
	free_auto_entities_func free_auto_entities; /**< Called to free the automatic allocated entities of a type. */
	set_type_mode_func      set_type_mode;      /**< Called to set a ir_mode of a type. */
	set_type_size_func      set_type_size;      /**< Called to set the byte size of a type. */
	get_n_members_func      get_n_members;      /**< Called to return the number of compound members. */
	get_member_func         get_member;         /**< Called to get the pos'th compound member. */
	get_member_index_func   get_member_index;   /**< Called to get the index of a compound member. */
} tp_op_ops;

/** possible flags for a type opcode */
enum tp_op_flags_t {
	TP_OP_FLAG_COMPOUND = 1   /**< is a compound type */
};

/** The type opcode. */
struct tp_op {
	tp_opcode code;                     /**< The tpop code. */
	ident     *name;                    /**< The name of the type opcode. */
	size_t    attr_size;                /**< The attribute size for a type of this opcode. */
	unsigned  flags;                    /**< Flags for this opcode. */
	tp_op_ops ops;                      /**< tp_op operations. */
};

/**
 * Returns a new type opcode.
 *
 * Allocates a new tp_op struct and initializes its fields with
 * the passed values.  This function is only to be used during
 * initialization of the library.
 *
 * @param code        the enum for this type opcode.
 * @param name        an ident for the name of the type opcode.
 * @param flags       additional flags
 * @param attr_size   the size of the attributes necessary for a type with
 *                    this opcode
 * @param ops         the tp_op operations for this type
 * @return A new type opcode.
 */
const tp_op *new_tpop (tp_opcode code, ident *name, unsigned flags, size_t attr_size, const tp_op_ops *ops);

/**
 * Free a tpop data structure.
 */
void free_tpop(const tp_op *tpop);

/**
 * Initialize the tpop module.
 *
 * Must be called during the initialization of the library. Allocates
 * opcodes and sets the globals that are external visible as specified
 * in tpop.h.
 * Allocates opcodes for classes, struct, method, union, array,
 * pointer and primitive and sets the according values.
 */
void init_tpop(void);

/**
 * Finalize the tpop module.
 *
 * Frees all type opcodes.
 */
void finish_tpop(void);

/**
 * Returns the size of the attribute to this kind
 * of type.
 *
 * Internal feature.
 *
 * @param op  The type opcode to get the size for.
 * @return The size of the attribute of types with this opcode.
 */
static inline size_t get_tpop_attr_size(const tp_op *op)
{
	return op->attr_size;
}

/* ---------------- *
 * inline functions *
 * -----------------*/

static inline tp_opcode _get_tpop_code(const tp_op *op)
{
	return op->code;
}

static inline ident *_get_tpop_ident(const tp_op *op)
{
	return op->name;
}

#endif /* FIRM_TR_TPOP_T_H */
