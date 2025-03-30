/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of all program known entities -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_TR_ENTITY_T_H
#define FIRM_TR_ENTITY_T_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>

#include "compiler.h"
#include "ident.h"
#include "panic.h"
#include "type_t.h"
#include "typerep.h"

#define get_entity_name(ent)                 _get_entity_name(ent)
#define get_entity_ident(ent)                _get_entity_ident(ent)
#define set_entity_ident(ent, id)            _set_entity_ident(ent, id)
#define get_entity_owner(ent)                _get_entity_owner(ent)
#define get_entity_ld_ident(ent)             _get_entity_ld_ident(ent)
#define get_entity_ld_name(ent)              _get_entity_ld_name(ent)
#define get_entity_type(ent)                 _get_entity_type(ent)
#define get_entity_linkage(ent)              _get_entity_linkage(ent)
#define get_entity_volatility(ent)           _get_entity_volatility(ent)
#define set_entity_volatility(ent, vol)      _set_entity_volatility(ent, vol)
#define set_entity_alignment(ent, alignment) _set_entity_alignment(ent, alignment)
#define get_entity_alignment(ent)            _get_entity_alignment(ent)
#define get_entity_aligned(ent)              _get_entity_aligned(ent)
#define set_entity_aligned(ent, a)           _set_entity_aligned(ent, a)
#define get_entity_usage(ent)                _get_entity_usage(ent)
#define set_entity_usage(ent, flags)         _set_entity_usage(ent, flags)
#define get_entity_initializer(ent)          _get_entity_initializer(ent)
#define get_entity_offset(ent)               _get_entity_offset(ent)
#define set_entity_offset(ent, offset)       _set_entity_offset(ent, offset)
#define get_entity_bitfield_offset(ent)      _get_entity_bitfield_offset(ent)
#define set_entity_bitfield_offset(ent, o)   _set_entity_bitfield_offset(ent, o)
#define get_entity_bitfield_size(ent)        _get_entity_bitfield_size(ent)
#define set_entity_bitfield_size(ent, s)     _set_entity_bitfield_size(ent, s)
#define get_entity_link(ent)                 _get_entity_link(ent)
#define set_entity_link(ent, l)              _set_entity_link(ent, l)
#define get_entity_irg(ent)                  _get_entity_irg(ent)
#define get_entity_linktime_irg(ent)         _get_entity_linktime_irg(ent)
#define is_parameter_entity(ent)             _is_parameter_entity(ent)
#define get_entity_parameter_number(ent)     _get_entity_parameter_number(ent)
#define get_entity_visited(ent)              _get_entity_visited(ent)
#define set_entity_visited(ent, num)         _set_entity_visited(ent, num)
#define mark_entity_visited(ent)             _mark_entity_visited(ent)
#define entity_visited(ent)                  _entity_visited(ent)
#define entity_not_visited(ent)              _entity_not_visited(ent)
#define get_entity_dbg_info(ent)             _get_entity_dbg_info(ent)
#define set_entity_dbg_info(ent, db)         _set_entity_dbg_info(ent, db)

#define INVALID_OFFSET    INT_MAX

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

typedef struct global_ent_attr {
	void const *jit_addr;
	/**< Additional graph properties for methods+alias */
	mtp_additional_properties properties;
} global_ent_attr;

typedef struct normal_ent_attr {
	global_ent_attr   base;
	ir_initializer_t *initializer; /**< entity initializer */
} normal_ent_attr;

/** The attributes for methods. */
typedef struct method_ent_attr {
	global_ent_attr           base;
	ir_graph *irg;                 /**< The corresponding irg if known.
	                                    The ir_graph constructor automatically sets this field. */

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

typedef struct compound_member_ent_attr {
	int offset;               /**< Offset in bytes for this entity. Fixed
	                               when layout of owner is determined. */
	uint16_t bitfield_offset; /**< for bitfields: offset in bits from base */
	uint16_t bitfield_size;   /**< for bitfields: size of entity in bits,
	                               0 if entity is not a bitfield. */
} compound_member_ent_attr;

typedef struct parameter_ent_attr {
	compound_member_ent_attr  base; /**< a parameter is also a compound_member
	                                     of the frame type. */
	size_t   number; /**< corresponding parameter number */
	bool     is_lowered_doubleword; /**< entity is a lowered doubleword parameter,
	                                     so additional stores because of calling
	                                     convention are correctly performed.
	                                     Matze: This is a hack. In an ideal
	                                     wor^H^H^Hlibfirm we would first establish
	                                     calling conventions and then perform doubleword
	                                     lowering...) */
} parameter_ent_attr;

typedef struct alias_ent_attr {
	global_ent_attr  base;
	ir_entity       *aliased;
} alias_ent_attr;

typedef struct spillslot_ent_attr {
	compound_member_ent_attr base;
	unsigned                 size;
} spillslot_ent_attr;

typedef enum ir_entity_kind {
	IR_ENTITY_ALIAS,
	IR_ENTITY_COMPOUND_MEMBER,
	IR_ENTITY_LABEL,
	IR_ENTITY_METHOD,
	IR_ENTITY_NORMAL,
	IR_ENTITY_PARAMETER,
	IR_ENTITY_SPILLSLOT,
	IR_ENTITY_UNKNOWN,
} ir_entity_kind;

/**
 * An abstract data type to represent program entities.
 */
struct ir_entity {
	firm_kind firm_tag;      /**< The dynamic type tag for entity. */
	ident *name;             /**< The name of this entity. */
	ident *ld_name;          /**< Unique name of this entity, i.e., the mangled
	                              name. May be NULL to indicate that a default
	                              mangling based on the name should happen */
	ir_type *type;           /**< The type of this entity */
	ir_type *owner;          /**< The compound type (e.g. class type) this
	                              entity belongs to. */
	ENUMBF(ir_entity_kind)  kind:3;        /**< entity kind */
	ENUMBF(ir_linkage)      linkage:7;     /**< Linkage type */
	ENUMBF(ir_volatility)   volatility:1;  /**< Volatility of entity content.*/
	ENUMBF(ir_align)        aligned:1;     /**< Alignment of entity content. */
	ENUMBF(ir_entity_usage) usage:4;       /**< Usage type of entity */
	ENUMBF(ir_visibility)   visibility:3;  /**< Visibility of entity. */
	ENUMBF(unsigned)        alignment:12;  /**< entity alignment in bytes */
	ir_visited_t visit;      /**< visited counter for walks of the type
	                              information. */
	dbg_info *dbi;           /**< A pointer to information for debug support. */
	void *link;              /**< To store some intermediate information. */

	ir_entity **overwrites;  /**< A list of entities this entity overwrites. */
	ir_entity **overwrittenby; /**< A list of entities that overwrite this
	                                entity. */
	long nr;                 /**< A unique number for each entity. */

	union {
		/** attributes for normal entities */
		normal_ent_attr          normal;
		/** attributes for method entities */
		method_ent_attr          mtd_attr;
		/** fields for code entities */
		code_ent_attr            code_attr;
		/** compound member attributes */
		compound_member_ent_attr compound_member;
		/** parameter number for parameter entities */
		parameter_ent_attr       parameter;
		/** alias attributes */
		alias_ent_attr           alias;
		/** properties shared by global entities */
		global_ent_attr          global;
		/** properties of spill slot entities */
		spillslot_ent_attr        spillslot;
	} attr; /**< type specific attributes */
};

/** Initialize the entity module. */
void ir_init_entity(ir_prog *irp);

/**
 * Creates an entity corresponding to the start address of a basic block
 * (the basic block is marked with a label id).
 */
ir_entity *new_label_entity(ir_label_t label);

ir_entity *new_spillslot(ir_type *frame, unsigned size, unsigned po2align);

void set_entity_irg(ir_entity *ent, ir_graph *irg);

/* ----------------------- inline functions ------------------------ */
static inline bool is_entity(const void *thing)
{
	return get_kind(thing) == k_entity;
}

static inline ir_entity_kind get_entity_kind(const ir_entity *entity)
{
	return (ir_entity_kind)entity->kind;
}

static inline ident *_get_entity_ident(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->name;
}

static inline const char *_get_entity_name(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return get_id_str(get_entity_ident(ent));
}

static inline void _set_entity_ident(ir_entity *ent, ident *id)
{
	assert(ent->firm_tag == k_entity);
	ent->name = id;
}

static inline ir_type *_get_entity_owner(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->owner;
}

static inline ident *_get_entity_ld_ident(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	if (ent->ld_name)
		return ent->ld_name;
	return ent->name;
}

static inline const char *_get_entity_ld_name(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return get_id_str(get_entity_ld_ident(ent));
}

static inline ir_type *_get_entity_type(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	assert(ent->kind != IR_ENTITY_SPILLSLOT);
	return ent->type;
}

static inline ir_linkage _get_entity_linkage(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return (ir_linkage) ent->linkage;
}

static inline ir_volatility _get_entity_volatility(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return (ir_volatility) ent->volatility;
}

static inline void _set_entity_volatility(ir_entity *ent, ir_volatility vol)
{
	assert(ent->firm_tag == k_entity);
	ent->volatility = vol;
}

static inline unsigned _get_entity_alignment(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->alignment;
}

static inline void _set_entity_alignment(ir_entity *ent, unsigned alignment)
{
	assert(ent->firm_tag == k_entity);
	ent->alignment = alignment;
}

static inline ir_align _get_entity_aligned(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return (ir_align) ent->aligned;
}

static inline void _set_entity_aligned(ir_entity *ent, ir_align a)
{
	assert(ent->firm_tag == k_entity);
	ent->aligned = a;
}

static inline ir_entity_usage _get_entity_usage(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return (ir_entity_usage) ent->usage;
}

static inline void _set_entity_usage(ir_entity *ent, ir_entity_usage state)
{
	assert(ent->firm_tag == k_entity);
	ent->usage = state;
}

static inline bool is_entity_compound_member(const ir_entity *entity)
{
	return entity->kind == IR_ENTITY_COMPOUND_MEMBER
	    || entity->kind == IR_ENTITY_PARAMETER
	    || entity->kind == IR_ENTITY_SPILLSLOT;
}

static inline ir_initializer_t *_get_entity_initializer(ir_entity const *const ent)
{
	assert(ent->kind == IR_ENTITY_NORMAL);
	return ent->attr.normal.initializer;
}

static inline int _get_entity_offset(const ir_entity *ent)
{
	assert(is_entity_compound_member(ent));
	return ent->attr.compound_member.offset;
}

static inline void _set_entity_offset(ir_entity *ent, int offset)
{
	assert(is_entity_compound_member(ent));
	ent->attr.compound_member.offset = offset;
}

static inline unsigned _get_entity_bitfield_offset(const ir_entity *ent)
{
	assert(is_entity_compound_member(ent));
	return ent->attr.compound_member.bitfield_offset;
}

static inline void _set_entity_bitfield_offset(ir_entity *ent, unsigned offset)
{
	assert(is_entity_compound_member(ent));
	ent->attr.compound_member.bitfield_offset = offset;
}

static inline unsigned _get_entity_bitfield_size(const ir_entity *entity)
{
	assert(is_entity_compound_member(entity));
	return entity->attr.compound_member.bitfield_size;
}

static inline void _set_entity_bitfield_size(ir_entity *entity, unsigned size)
{
	assert(is_entity_compound_member(entity));
	entity->attr.compound_member.bitfield_size = size;
}

static inline void *_get_entity_link(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->link;
}

static inline void _set_entity_link(ir_entity *ent, void *l)
{
	assert(ent->firm_tag == k_entity);
	ent->link = l;
}

static inline ir_graph *_get_entity_irg(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	assert(ent->kind == IR_ENTITY_METHOD);
	return ent->attr.mtd_attr.irg;
}

static inline ir_graph *_get_entity_linktime_irg(const ir_entity *entity)
{
	/* weak entities might get replaced by non-weak entities at linktime
	 * so we can't return a definite graph. */
	if (get_entity_linkage(entity) & IR_LINKAGE_WEAK)
		return NULL;
	/* only method entities have an irg field (alias etc. does not) */
	if (entity->kind != IR_ENTITY_METHOD)
		return NULL;
	return get_entity_irg(entity);
}

static inline ir_visited_t _get_entity_visited(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->visit;
}

static inline void _set_entity_visited(ir_entity *ent, ir_visited_t num)
{
	assert(ent->firm_tag == k_entity);
	ent->visit = num;
}

static inline void _mark_entity_visited(ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
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
	return entity->kind == IR_ENTITY_PARAMETER;
}

static inline size_t _get_entity_parameter_number(const ir_entity *entity)
{
	assert(is_parameter_entity(entity));
	return entity->attr.parameter.number;
}

static inline dbg_info *_get_entity_dbg_info(const ir_entity *ent)
{
	return ent->dbi;
}

static inline void _set_entity_dbg_info(ir_entity *ent, dbg_info *db)
{
	ent->dbi = db;
}

static inline bool is_global_entity(ir_entity const *const entity)
{
	switch (entity->kind) {
	case IR_ENTITY_ALIAS:
	case IR_ENTITY_METHOD:
	case IR_ENTITY_NORMAL:
		return true;
	case IR_ENTITY_COMPOUND_MEMBER:
	case IR_ENTITY_LABEL:
	case IR_ENTITY_PARAMETER:
	case IR_ENTITY_UNKNOWN:
	case IR_ENTITY_SPILLSLOT:
		return false;
	}
	panic("Invalid entity");
}

#endif
