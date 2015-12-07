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

#define get_tpop_code(op)      _get_tpop_code(op)
#define get_tpop_ident(op)     _get_tpop_ident(op)

/** The type opcode. */
struct tp_op {
	tp_opcode code;      /**< The tpop code. */
	ident     *name;     /**< The name of the type opcode. */
	size_t    attr_size; /**< The attribute size for a type of this opcode. */
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
 * @param attr_size   the size of the attributes necessary for a type with
 *                    this opcode
 * @return A new type opcode.
 */
tp_op const *new_tpop(tp_opcode code, ident *name, size_t attr_size);

/**
 * Free a tpop data structure.
 */
void free_tpop(tp_op const *tpop);

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
static inline size_t get_tpop_attr_size(tp_op const *op)
{
	return op->attr_size;
}

static inline tp_opcode _get_tpop_code(tp_op const *op)
{
	return op->code;
}

static inline ident *_get_tpop_ident(tp_op const *op)
{
	return op->name;
}

#endif
