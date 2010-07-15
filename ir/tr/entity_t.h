/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @file
 * @brief   Representation of all program known entities -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_TR_ENTITY_T_H
#define FIRM_TR_ENTITY_T_H

#include <assert.h>

#include "typerep.h"
#include "type_t.h"
#include "ident.h"
#include "compound_path.h"

typedef struct ir_initializer_base_t {
	ir_initializer_kind_t kind;
} ir_initializer_base_t;

/**
 * An compound initializer.
 */
typedef struct ir_initializer_compound_t {
	ir_initializer_base_t  base;
	unsigned               n_initializers;
	ir_initializer_t      *initializers[1];
} ir_initializer_compound_t;

/**
 * An initializer containing an ir_node,
 */
typedef struct ir_initializer_const_t {
	ir_initializer_base_t  base;
	ir_node               *value;
} ir_initializer_const_t ;

/**
 * An initializer containing a tarval.
 */
typedef struct ir_initializer_tarval_t {
	ir_initializer_base_t  base;
	tarval                *value;
} ir_initializer_tarval_t ;

union ir_initializer_t {
	ir_initializer_kind_t      kind;
	ir_initializer_base_t      base;
	ir_initializer_compound_t  compound;
	ir_initializer_const_t     consti;
	ir_initializer_tarval_t    tarval;
};

/** The attributes for compound entities. */
typedef struct compound_ent_attr {
	ir_node **values;     /**< constant values of compound entities. */
	compound_graph_path **val_paths;
	                     /**< paths corresponding to constant values. */
} compound_ent_attr;

/** The attributes for methods. */
typedef struct method_ent_attr {
	ir_graph *irg;                 /**< The corresponding irg if known.
	                                    The ir_graph constructor automatically sets this field. */
	unsigned irg_add_properties;   /**< Additional graph properties can be
	                                    stored in a entity if no irg is available. */

	unsigned vtable_number;        /**< For a dynamically called method, the number assigned
	                                    in the virtual function table. */

	ptr_access_kind *param_access; /**< the parameter access */
	unsigned *param_weight;        /**< The weight of method's parameters. Parameters
	                                    with a high weight are good candidates for procedure cloning. */
} method_ent_attr;

/** additional attributes for code entities */
typedef struct code_ent_attr {
	ir_label_t  label;       /** label of the basic block */
} code_ent_attr;


/**
 * An abstract data type to represent program entities.
 *
 * @see  ir_type
 */
struct ir_entity {
	firm_kind kind;       /**< The dynamic type tag for entity. */
	ident *name;          /**< The name of this entity. */
	ident *ld_name;       /**< Unique name of this entity, i.e., the mangled
	                           name.  If the field is read before written a default
	                           mangling is applies.  The name of the owner is prepended
	                           to the name of the entity, separated by a underscore.
	                           E.g.,  for a class `A' with field `a' this
	                           is the ident for `A_a'. */
	ir_type *type;        /**< The type of this entity, e.g., a method type, a
	                           basic type of the language or a class itself. */
	ir_type *owner;       /**< The compound type (e.g. class type) this entity belongs to. */
	unsigned linkage:10;        /**< Specifies linkage type */
	unsigned volatility:1;      /**< Specifies volatility of entities content.*/
	unsigned aligned:1;         /**< Specifies alignment of entities content. */
	unsigned usage:4;           /**< flag indicating usage types of this entity,
	                                 see ir_entity_usage. */
	unsigned compiler_gen:1;    /**< If set, this entity was compiler generated.
	                             */
	unsigned visibility:3;      /**< @deprecated */
	unsigned allocation:3;      /**< @deprecated */
	unsigned peculiarity:3;     /**< @deprecated */
	unsigned final:1;           /**< @deprecated */
	int offset;                 /**< Offset in bytes for this entity. Fixed
	                                 when layout of owner is determined. */
	unsigned alignment;         /**< entity alignment in bytes */
	unsigned char offset_bit_remainder;
	                            /**< If the entity is a bit field, this is the
	                                 offset of the start of the bit field
	                                 within the byte specified by offset. */
	ir_visited_t visit;         /**< visited counter for walks of the type
	                                 information. */
	struct dbg_info *dbi;       /**< A pointer to information for debug support.
	                             */
	void *link;                 /**< To store some intermediate information. */
	ir_type *repr_class;        /**< If this entity represents a class info, the
	                                 associated class. */

	/* ------------- fields for entities owned by a class type ---------------*/

	ir_entity **overwrites;     /**< A list of entities this entity overwrites.
	                             */
	ir_entity **overwrittenby;  /**< A list of entities that overwrite this
	                                 entity. */

	/* ------------- fields for atomic entities  --------------- */
	ir_initializer_t *initializer; /**< entity initializer */
	union {
		/* ------------- fields for compound entities -------------- */
		compound_ent_attr cmpd_attr;
		/* ------------- fields for method entities ---------------- */
		method_ent_attr   mtd_attr;
		/* fields for code entities */
		code_ent_attr     code_attr;
	} attr; /**< type specific attributes */

	/* ------------- fields for analyses ---------------*/

#ifdef DEBUG_libfirm
	long nr;             /**< A unique node number for each node to make output
	                          readable. */
#endif
};

/** Initialize the entity module. */
void ir_init_entity(void);
/** Cleanup entity module */
void ir_finish_entity(void);

/* ----------------------- inline functions ------------------------ */
static inline int _is_entity(const void *thing)
{
	return get_kind(thing) == k_entity;
}

static inline const char *_get_entity_name(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return get_id_str(get_entity_ident(ent));
}

static inline ident *_get_entity_ident(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->name;
}

static inline void _set_entity_ident(ir_entity *ent, ident *id)
{
	assert(ent && ent->kind == k_entity);
	ent->name = id;
}

static inline ir_type *_get_entity_owner(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->owner;
}

static inline ident *_get_entity_ld_ident(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	if (ent->ld_name == NULL)
		return ent->name;
	return ent->ld_name;
}

static inline void _set_entity_ld_ident(ir_entity *ent, ident *ld_ident)
{
	assert(ent && ent->kind == k_entity);
	ent->ld_name = ld_ident;
}

static inline const char *_get_entity_ld_name(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return get_id_str(get_entity_ld_ident(ent));
}

static inline ir_type *_get_entity_type(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->type;
}

static inline void _set_entity_type(ir_entity *ent, ir_type *type)
{
	assert(ent && ent->kind == k_entity);
	ent->type = type;
}

static inline ir_linkage _get_entity_linkage(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->linkage;
}

static inline ir_volatility _get_entity_volatility(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->volatility;
}

static inline void _set_entity_volatility(ir_entity *ent, ir_volatility vol)
{
	assert(ent && ent->kind == k_entity);
	ent->volatility = vol;
}

static inline unsigned _get_entity_alignment(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->alignment;
}

static inline void _set_entity_alignment(ir_entity *ent, unsigned alignment)
{
	assert(ent && ent->kind == k_entity);
	ent->alignment = alignment;
}

static inline ir_align _get_entity_aligned(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->aligned;
}

static inline void _set_entity_aligned(ir_entity *ent, ir_align a)
{
	assert(ent && ent->kind == k_entity);
	ent->aligned = a;
}

static inline int _is_entity_compiler_generated(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->compiler_gen;
}

static inline void _set_entity_compiler_generated(ir_entity *ent, int flag)
{
	assert(ent && ent->kind == k_entity);
	ent->compiler_gen = flag ? 1 : 0;
}

static inline ir_entity_usage _get_entity_usage(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->usage;
}

static inline void _set_entity_usage(ir_entity *ent, ir_entity_usage state)
{
	assert(ent && ent->kind == k_entity);
	ent->usage = state;
}

static inline int _get_entity_offset(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->offset;
}

static inline void _set_entity_offset(ir_entity *ent, int offset)
{
	assert(ent && ent->kind == k_entity);
	ent->offset = offset;
}

static inline unsigned char _get_entity_offset_bits_remainder(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->offset_bit_remainder;
}

static inline void _set_entity_offset_bits_remainder(ir_entity *ent, unsigned char offset)
{
	assert(ent && ent->kind == k_entity);
	ent->offset_bit_remainder = offset;
}

static inline void *_get_entity_link(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->link;
}

static inline void _set_entity_link(ir_entity *ent, void *l)
{
	assert(ent && ent->kind == k_entity);
	ent->link = l;
}

static inline ir_graph *_get_entity_irg(const ir_entity *ent)
{
	ir_graph *irg;
	assert(ent && ent->kind == k_entity);
	if (!is_Method_type(ent->type) || ent == unknown_entity) {
		return NULL;
	}

	return ent->attr.mtd_attr.irg;
}

static inline ir_visited_t _get_entity_visited(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->visit;
}

static inline void _set_entity_visited(ir_entity *ent, ir_visited_t num)
{
	assert(ent && ent->kind == k_entity);
	ent->visit = num;
}

static inline void _mark_entity_visited(ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	ent->visit = firm_type_visited;
}

static inline int _entity_visited(const ir_entity *ent)
{
	return _get_entity_visited(ent) >= firm_type_visited;
}

static inline int _entity_not_visited(const ir_entity *ent)
{
	return _get_entity_visited(ent) < firm_type_visited;
}

static inline ir_type *_get_entity_repr_class(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return ent->repr_class;
}

static inline dbg_info *_get_entity_dbg_info(const ir_entity *ent)
{
	return ent->dbi;
}

static inline void _set_entity_dbg_info(ir_entity *ent, dbg_info *db)
{
	ent->dbi = db;
}

#define is_entity(thing)                         _is_entity(thing)
#define get_entity_name(ent)                     _get_entity_name(ent)
#define get_entity_ident(ent)                    _get_entity_ident(ent)
#define set_entity_ident(ent, id)                _set_entity_ident(ent, id)
#define get_entity_owner(ent)                    _get_entity_owner(ent)
#define get_entity_ld_ident(ent)                 _get_entity_ld_ident(ent)
#define set_entity_ld_ident(ent, ld_ident)       _set_entity_ld_ident(ent, ld_ident)
#define get_entity_ld_name(ent)                  _get_entity_ld_name(ent)
#define get_entity_type(ent)                     _get_entity_type(ent)
#define set_entity_type(ent, type)               _set_entity_type(ent, type)
#define get_entity_linkage(ent)                  _get_entity_linkage(ent)
#define get_entity_volatility(ent)               _get_entity_volatility(ent)
#define set_entity_volatility(ent, vol)          _set_entity_volatility(ent, vol)
#define set_entity_alignment(ent, alignment)     _set_entity_alignment(ent, alignment)
#define get_entity_alignment(ent)                _get_entity_alignment(ent)
#define get_entity_align(ent)                    _get_entity_align(ent)
#define set_entity_align(ent, a)                 _set_entity_align(ent, a)
#define is_entity_compiler_generated(ent)        _is_entity_compiler_generated(ent)
#define set_entity_compiler_generated(ent, flag) _set_entity_compiler_generated(ent, flag)
#define get_entity_usage(ent)                    _get_entity_usage(ent)
#define set_entity_usage(ent, flags)             _set_entity_usage(ent, flags)
#define get_entity_offset(ent)                   _get_entity_offset(ent)
#define set_entity_offset(ent, offset)           _set_entity_offset(ent, offset)
#define get_entity_offset_bits_remainder(ent)    _get_entity_offset_bits_remainder(ent)
#define set_entity_offset_bits_remainder(ent, o) _set_entity_offset_bits_remainder(ent, o)
#define get_entity_link(ent)                     _get_entity_link(ent)
#define set_entity_link(ent, l)                  _set_entity_link(ent, l)
#define get_entity_irg(ent)                      _get_entity_irg(ent)
#define get_entity_visited(ent)                  _get_entity_visited(ent)
#define set_entity_visited(ent, num)             _set_entity_visited(ent, num)
#define mark_entity_visited(ent)                 _mark_entity_visited(ent)
#define entity_visited(ent)                      _entity_visited(ent)
#define entity_not_visited(ent)                  _entity_not_visited(ent)
#define get_entity_repr_class(ent)               _get_entity_repr_class(ent)
#define get_entity_dbg_info(ent)                 _get_entity_dbg_info(ent)
#define set_entity_dbg_info(ent, db)             _set_entity_dbg_info(ent, db)

#endif
