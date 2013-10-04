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

#include "typerep.h"
#include "type_t.h"
#include "ident.h"

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
#define get_entity_bitfield_offset(ent)          _get_entity_bitfield_offset(ent)
#define set_entity_bitfield_offset(ent, o)       _set_entity_bitfield_offset(ent, o)
#define get_entity_bitfield_size(ent)            _get_entity_bitfield_size(ent)
#define set_entity_bitfield_size(ent, s)         _set_entity_bitfield_size(ent, s)
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
#define get_entity_dbg_info(ent)                 _get_entity_dbg_info(ent)
#define set_entity_dbg_info(ent, db)             _set_entity_dbg_info(ent, db)

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

/** The attributes for methods. */
typedef struct method_ent_attr {
	ir_graph *irg;                 /**< The corresponding irg if known.
	                                    The ir_graph constructor automatically sets this field. */
	mtp_additional_properties properties;   /**< Additional graph properties can be
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

typedef struct compound_member_ent_attr {
	int offset;               /**< Offset in bytes for this entity. Fixed
	                               when layout of owner is determined. */
	unsigned bitfield_offset; /**< for bitfields: offset in bits from base */
	unsigned bitfield_size;   /**< for bitfields: size of entity in bits,
	                               0 if entity is not a bitfield. */
} compound_member_ent_attr;

typedef struct parameter_ent_attr {
	compound_member_ent_attr  base; /**< a parameter is also a compound_member
	                                     of the frame type. */
	size_t   number; /**< corresponding parameter number */
	ir_mode *doubleword_low_mode;/**< entity is a lowered doubleword parameter,
								so additional stores because of calling
								convention are correctly performed.
	                            Matze: This is a hack. In an ideal
	                            wor^H^H^Hlibfirm we would first establish
	                            calling conventions and then perform doubleword
	                            lowering...) */
} parameter_ent_attr;

typedef struct alias_ent_attr {
	mtp_additional_properties properties;
	ir_entity                *aliased;
} alias_ent_attr;

typedef struct got_ent_attr {
	ir_entity *referenced;
} got_ent_attr;

typedef enum ir_entity_kind {
	IR_ENTITY_ALIAS,
	IR_ENTITY_COMPOUND_MEMBER,
	IR_ENTITY_LABEL,
	IR_ENTITY_METHOD,
	IR_ENTITY_NORMAL,
	IR_ENTITY_PARAMETER,
	IR_ENTITY_UNKNOWN,
	IR_ENTITY_GOTENTRY,
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
	unsigned alignment;      /**< entity alignment in bytes */
	ir_visited_t visit;      /**< visited counter for walks of the type
	                              information. */
	struct dbg_info *dbi;    /**< A pointer to information for debug support. */
	void *link;              /**< To store some intermediate information. */

	ir_entity **overwrites;  /**< A list of entities this entity overwrites. */
	ir_entity **overwrittenby; /**< A list of entities that overwrite this
	                                entity. */

	ir_initializer_t *initializer; /**< entity initializer */
#ifdef DEBUG_libfirm
	long nr;             /**< A unique node number for each node to make output
	                          readable. */
#endif

	union {
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
		/** got entry attributes */
		got_ent_attr             got;
	} attr; /**< type specific attributes */
};

/** Initialize the entity module. */
void ir_init_entity(ir_prog *irp);
/** Cleanup entity module */
void ir_finish_entity(ir_prog *irp);

/**
 * Creates an entity corresponding to the start address of a basic block
 * (the basic block is marked with a label id).
 */
ir_entity *new_label_entity(ir_label_t label);

/**
 * Create an entity representing an entry in the global offset table used for
 * position independent code (PIC) code.
 */
ir_entity *new_got_entry_entity(ir_entity *reference);

void set_entity_irg(ir_entity *ent, ir_graph *irg);

/* ----------------------- inline functions ------------------------ */
static inline int _is_entity(const void *thing)
{
	return get_kind(thing) == k_entity;
}

static inline ir_entity_kind get_entity_kind(const ir_entity *entity)
{
	return (ir_entity_kind)entity->entity_kind;
}

static inline ident *_get_entity_ident(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->name;
}

static inline const char *_get_entity_name(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return get_id_str(get_entity_ident(ent));
}

static inline void _set_entity_ident(ir_entity *ent, ident *id)
{
	assert(ent->kind == k_entity);
	ent->name = id;
}

static inline ir_type *_get_entity_owner(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->owner;
}

static inline ident *_get_entity_ld_ident(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	if (ent->ld_name == NULL)
		return ent->name;
	return ent->ld_name;
}

static inline void _set_entity_ld_ident(ir_entity *ent, ident *ld_ident)
{
	assert(ent->kind == k_entity);
	ent->ld_name = ld_ident;
}

static inline const char *_get_entity_ld_name(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return get_id_str(get_entity_ld_ident(ent));
}

static inline ir_type *_get_entity_type(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->type;
}

static inline ir_linkage _get_entity_linkage(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return (ir_linkage) ent->linkage;
}

static inline ir_volatility _get_entity_volatility(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return (ir_volatility) ent->volatility;
}

static inline void _set_entity_volatility(ir_entity *ent, ir_volatility vol)
{
	assert(ent->kind == k_entity);
	ent->volatility = vol;
}

static inline unsigned _get_entity_alignment(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->alignment;
}

static inline void _set_entity_alignment(ir_entity *ent, unsigned alignment)
{
	assert(ent->kind == k_entity);
	ent->alignment = alignment;
}

static inline ir_align _get_entity_aligned(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return (ir_align) ent->aligned;
}

static inline void _set_entity_aligned(ir_entity *ent, ir_align a)
{
	assert(ent->kind == k_entity);
	ent->aligned = a;
}

static inline int _is_entity_compiler_generated(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->compiler_gen;
}

static inline void _set_entity_compiler_generated(ir_entity *ent, int flag)
{
	assert(ent->kind == k_entity);
	ent->compiler_gen = flag ? 1 : 0;
}

static inline ir_entity_usage _get_entity_usage(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return (ir_entity_usage) ent->usage;
}

static inline void _set_entity_usage(ir_entity *ent, ir_entity_usage state)
{
	assert(ent->kind == k_entity);
	ent->usage = state;
}

static inline bool is_entity_compound_member(const ir_entity *entity)
{
	return entity->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	    || entity->entity_kind == IR_ENTITY_PARAMETER;
}

static inline int _get_entity_offset(const ir_entity *ent)
{
	assert(ent->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || ent->entity_kind == IR_ENTITY_PARAMETER);
	return ent->attr.compound_member.offset;
}

static inline void _set_entity_offset(ir_entity *ent, int offset)
{
	assert(ent->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || ent->entity_kind == IR_ENTITY_PARAMETER);
	ent->attr.compound_member.offset = offset;
}

static inline unsigned _get_entity_bitfield_offset(const ir_entity *ent)
{
	assert(ent->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || ent->entity_kind == IR_ENTITY_PARAMETER);
	return ent->attr.compound_member.bitfield_offset;
}

static inline void _set_entity_bitfield_offset(ir_entity *ent, unsigned offset)
{
	assert(ent->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || ent->entity_kind == IR_ENTITY_PARAMETER);
	ent->attr.compound_member.bitfield_offset = offset;
}

static inline unsigned _get_entity_bitfield_size(const ir_entity *entity)
{
	assert(entity->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || entity->entity_kind == IR_ENTITY_PARAMETER);
	return entity->attr.compound_member.bitfield_size;
}

static inline void _set_entity_bitfield_size(ir_entity *entity, unsigned size)
{
	assert(entity->entity_kind == IR_ENTITY_COMPOUND_MEMBER
	       || entity->entity_kind == IR_ENTITY_PARAMETER);
	entity->attr.compound_member.bitfield_size = size;
}

static inline void *_get_entity_link(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->link;
}

static inline void _set_entity_link(ir_entity *ent, void *l)
{
	assert(ent->kind == k_entity);
	ent->link = l;
}

static inline ir_graph *_get_entity_irg(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	if (!is_Method_type(ent->type) || is_unknown_entity(ent)) {
		return NULL;
	}

	return ent->attr.mtd_attr.irg;
}

static inline ir_visited_t _get_entity_visited(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
	return ent->visit;
}

static inline void _set_entity_visited(ir_entity *ent, ir_visited_t num)
{
	assert(ent->kind == k_entity);
	ent->visit = num;
}

static inline void _mark_entity_visited(ir_entity *ent)
{
	assert(ent->kind == k_entity);
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

static inline dbg_info *_get_entity_dbg_info(const ir_entity *ent)
{
	return ent->dbi;
}

static inline void _set_entity_dbg_info(ir_entity *ent, dbg_info *db)
{
	ent->dbi = db;
}

#endif
