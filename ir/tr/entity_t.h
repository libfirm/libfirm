/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 */
#ifndef FIRM_TR_ENTITY_T_H
#define FIRM_TR_ENTITY_T_H

#include <assert.h>
#include <stdbool.h>

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
	size_t                 n_initializers;
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
	ir_tarval             *value;
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
	mtp_additional_properties irg_add_properties;   /**< Additional graph properties can be
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

typedef struct parameter_ent_attr {
	/**< parameters might be compounds too */
	compound_ent_attr  cmpd_attr;

	size_t   number; /**< corresponding parameter number */
	ir_mode *doubleword_low_mode;/**< entity is a lowered doubleword parameter,
								so additional stores because of calling
								convention are correctly performed.
	                            Matze: This is a hack. In an ideal
	                            wor^H^H^Hlibfirm we would first establish
	                            calling conventions and then perform doubleword
	                            lowering...) */
} parameter_ent_attr;

typedef enum ir_entity_kind {
	IR_ENTITY_NORMAL,
	IR_ENTITY_METHOD,
	IR_ENTITY_COMPOUND_MEMBER,
	IR_ENTITY_PARAMETER,
	IR_ENTITY_LABEL,
	IR_ENTITY_UNKNOWN,
} ir_entity_kind;

/**
 * An abstract data type to represent program entities.
 */
struct ir_entity {
	firm_kind kind;          /**< The dynamic type tag for entity. */
	ident *name;             /**< The name of this entity. */
	ident *ld_name;          /**< Unique name of this entity, i.e., the mangled
	                              name. May be NULL to indicate that a default
	                              mangling based on the name should happen */
	ir_type *type;           /**< The type of this entity */
	ir_type *owner;          /**< The compound type (e.g. class type) this
							      entity belongs to. */
	unsigned entity_kind:3;  /**< entity kind */
	unsigned linkage:10;     /**< Specifies linkage type */
	unsigned volatility:1;   /**< Specifies volatility of entities content.*/
	unsigned aligned:1;      /**< Specifies alignment of entities content. */
	unsigned usage:4;        /**< flag indicating usage types of this entity,
	                              see ir_entity_usage. */
	unsigned compiler_gen:1; /**< If set, this entity was compiler generated.
	                          */
	unsigned visibility:3;   /**< @deprecated */
	unsigned allocation:3;   /**< @deprecated */
	unsigned peculiarity:3;  /**< @deprecated */
	unsigned final:1;        /**< @deprecated */
	unsigned offset_bit_remainder:8;
	                         /**< If the entity is a bit field, this is the
	                              offset of the start of the bit field
	                              within the byte specified by offset. */
	int offset;              /**< Offset in bytes for this entity. Fixed
	                              when layout of owner is determined. */
	unsigned alignment;      /**< entity alignment in bytes */
	ir_visited_t visit;      /**< visited counter for walks of the type
	                              information. */
	struct dbg_info *dbi;    /**< A pointer to information for debug support. */
	void *link;              /**< To store some intermediate information. */
	ir_type *repr_class;     /**< If this entity represents a class info, the
	                              associated class. */

	ir_entity **overwrites;  /**< A list of entities this entity overwrites. */
	ir_entity **overwrittenby; /**< A list of entities that overwrite this
	                                entity. */

	ir_initializer_t *initializer; /**< entity initializer */
#ifdef DEBUG_libfirm
	long nr;             /**< A unique node number for each node to make output
	                          readable. */
#endif

	union {
		/* ------------- fields for compound entities -------------- */
		compound_ent_attr  cmpd_attr;
		/* ------------- fields for method entities ---------------- */
		method_ent_attr    mtd_attr;
		/* fields for code entities */
		code_ent_attr      code_attr;
		/** parameter number for parameter entities */
		parameter_ent_attr parameter;
	} attr; /**< type specific attributes */
};

/** Initialize the entity module. */
void ir_init_entity(void);
/** Cleanup entity module */
void ir_finish_entity(void);

/**
 * Creates an entity corresponding to the start address of a basic block
 * (the basic block is marked with a label id).
 */
ir_entity *new_label_entity(ir_label_t label);

/**
 * Like new_label_entity() but with debug information.
 */
ir_entity *new_d_label_entity(ir_label_t label, dbg_info *dbgi);

void set_entity_irg(ir_entity *ent, ir_graph *irg);

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

static inline ir_linkage _get_entity_linkage(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return (ir_linkage) ent->linkage;
}

static inline ir_volatility _get_entity_volatility(const ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	return (ir_volatility) ent->volatility;
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
	return (ir_align) ent->aligned;
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
	return (ir_entity_usage) ent->usage;
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

static inline int _is_parameter_entity(const ir_entity *entity)
{
	return entity->entity_kind == IR_ENTITY_PARAMETER;
}

static inline size_t _get_entity_parameter_number(const ir_entity *entity)
{
	assert(entity->entity_kind == IR_ENTITY_PARAMETER);
	return entity->attr.parameter.number;
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
#define is_parameter_entity(ent)                 _is_parameter_entity(ent)
#define get_entity_parameter_number(ent)         _get_entity_parameter_number(ent)
#define get_entity_visited(ent)                  _get_entity_visited(ent)
#define set_entity_visited(ent, num)             _set_entity_visited(ent, num)
#define mark_entity_visited(ent)                 _mark_entity_visited(ent)
#define entity_visited(ent)                      _entity_visited(ent)
#define entity_not_visited(ent)                  _entity_not_visited(ent)
#define get_entity_repr_class(ent)               _get_entity_repr_class(ent)
#define get_entity_dbg_info(ent)                 _get_entity_dbg_info(ent)
#define set_entity_dbg_info(ent, db)             _set_entity_dbg_info(ent, db)

#endif
