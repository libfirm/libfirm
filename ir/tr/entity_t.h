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
 * @file     entity_t.h
 * @brief   Representation of all program known entities -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_TR_ENTITY_T_H
#define FIRM_TR_ENTITY_T_H

#include <assert.h>

#include "firm_common_t.h"
#include "firm_config.h"

#include "typerep.h"
#include "type_t.h"
#include "ident.h"
#include "pseudo_irg.h"

/** A path in a compound graph. */
struct compound_graph_path {
	firm_kind kind;       /**< The dynamic type tag for compound graph path. */
	ir_type *tp;          /**< The type this path belongs to. */
	int len;              /**< The length of the path. */
	struct tuple {
		int       index;    /**< Array index.  To compute position of array elements */
		ir_entity *node;    /**< The accessed entity. */
	} list[1];            /**< List of entity/index tuple of length len to express the
	                           access path. */
};

/** The attributes for atomic entities. */
typedef struct atomic_ent_attr {
	ir_node *value;            /**< value if entity is not of variability uninitialized.
	                             Only for atomic entities. */
} atomic_ent_attr;

/** The attributes for compound entities. */
typedef struct compound_ent_attr {
	ir_node **values;     /**< constant values of compound entities. Only available if
	                           variability not uninitialized.  Must be set for variability constant. */
	compound_graph_path **val_paths;
	                     /**< paths corresponding to constant values. Only available if
	                          variability not uninitialized.  Must be set for variability constant. */
} compound_ent_attr;

/** A reserved value for "not yet set". */
#define VTABLE_NUM_NOT_SET ((unsigned)(-1))

/** The attributes for methods. */
typedef struct method_ent_attr {
	ir_graph *irg;                 /**< The corresponding irg if known.
	                                    The ir_graph constructor automatically sets this field. */
	unsigned irg_add_properties;   /**< Additional graph properties can be
	                                    stored in a entity if no irg is available. */

	unsigned vtable_number;        /**< For a dynamically called method, the number assigned
	                                    in the virtual function table. */

	ptr_access_kind *param_access; /**< the parameter access */
	float *param_weight;           /**< The weight of method's parameters. Parameters
	                                    with a high weight are good for procedure cloning. */
	ir_img_section section;        /**< The code section where this method should be placed */
} method_ent_attr;


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
	ir_allocation allocation:3;             /**< Distinguishes static and dynamically allocated
	                                             entities and some further cases. */
	ir_visibility visibility:3;             /**< Specifies visibility to external program fragments. */
	ir_variability variability:3;           /**< Specifies variability of entities content. */
	ir_volatility volatility:2;             /**< Specifies volatility of entities content. */
	ir_stickyness stickyness:2;             /**< Specifies whether this entity is sticky.  */
	ir_peculiarity peculiarity:3;           /**< The peculiarity of this entity. */
	ir_address_taken_state address_taken:3; /**< A flag that can be set to mark address taken entities. */
	unsigned final:1;              /**< If set, this entity cannot be overridden. */
	unsigned compiler_gen:1;       /**< If set, this entity was compiler generated. */
	unsigned backend_marked:1;     /**< If set, this entity was marked by the backend for emission. */
	int offset;                    /**< Offset in bytes for this entity.  Fixed when layout
	                                    of owner is determined. */
	unsigned char offset_bit_remainder;
	                               /**< If the entity is a bit field, this is the offset of
	                                    the start of the bit field within the byte specified
	                                    by offset. */
	unsigned long visit;           /**< visited counter for walks of the type information. */
	struct dbg_info *dbi;          /**< A pointer to information for debug support. */
	void *link;                    /**< To store some intermediate information. */
	ir_type *repr_class;           /**< If this entity represents a class info, the associated class. */

	/* ------------- fields for entities owned by a class type ---------------*/

	ir_entity **overwrites;     /**< A list of entities this entity overwrites. */
	ir_entity **overwrittenby;  /**< A list of entities that overwrite this entity.  */

	/* ------------- fields for atomic entities  --------------- */
	ir_node *value;          /**< value if entity is not of variability uninitialized.
	                              Only for atomic entities. */
	union {
		/* ------------- fields for compound entities -------------- */
		compound_ent_attr cmpd_attr;
		/* ------------- fields for method entities ---------------- */
		method_ent_attr   mtd_attr;
	} attr; /**< type specific attributes */

	/* ------------- fields for analyses ---------------*/

#ifdef DEBUG_libfirm
	long nr;             /**< A unique node number for each node to make output readable. */
# endif /* DEBUG_libfirm */
};

/** Initialize the entity module. */
void firm_init_entity(void);


/* ----------------------- inline functions ------------------------ */
static INLINE int
_is_entity(const void *thing) {
	return get_kind(thing) == k_entity;
}

static INLINE const char *
_get_entity_name(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return get_id_str(get_entity_ident(ent));
}

static INLINE ident *
_get_entity_ident(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->name;
}

static INLINE void
_set_entity_ident(ir_entity *ent, ident *id) {
	assert(ent && ent->kind == k_entity);
	ent->name = id;
}

static INLINE ir_type *
_get_entity_owner(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->owner = skip_tid(ent->owner);
}

static INLINE ident *
_get_entity_ld_ident(ir_entity *ent)
{
	assert(ent && ent->kind == k_entity);
	if (ent->ld_name == NULL)
		ent->ld_name = mangle_entity(ent);
	return ent->ld_name;
}

static INLINE void
_set_entity_ld_ident(ir_entity *ent, ident *ld_ident) {
	assert(ent && ent->kind == k_entity);
	ent->ld_name = ld_ident;
}

static INLINE const char *
_get_entity_ld_name(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return get_id_str(get_entity_ld_ident(ent));
}

static INLINE ir_type *
_get_entity_type(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->type = skip_tid(ent->type);
}

static INLINE void
_set_entity_type(ir_entity *ent, ir_type *type) {
	assert(ent && ent->kind == k_entity);
	ent->type = type;
}

static INLINE ir_allocation
_get_entity_allocation(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->allocation;
}

static INLINE void
_set_entity_allocation(ir_entity *ent, ir_allocation al) {
	assert(ent && ent->kind == k_entity);
	ent->allocation = al;
}

static INLINE ir_visibility
_get_entity_visibility(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->visibility;
}

static INLINE ir_variability
_get_entity_variability(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->variability;
}

static INLINE ir_volatility
_get_entity_volatility(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->volatility;
}

static INLINE void
_set_entity_volatility(ir_entity *ent, ir_volatility vol) {
	assert(ent && ent->kind == k_entity);
	ent->volatility = vol;
}

static INLINE ir_peculiarity
_get_entity_peculiarity(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->peculiarity;
}

/**
 * @todo Why peculiarity only for methods?
 *       Good question.  Originally, there were only description and
 *       existent.  The thought was, what sense does it make to
 *       describe a field?  With inherited the situation changed.  So
 *       I removed the assertion.  GL, 28.2.05
 */
static INLINE void
_set_entity_peculiarity(ir_entity *ent, ir_peculiarity pec) {
	assert(ent && ent->kind == k_entity);
	/* @@@ why peculiarity only for methods? */
	//assert(is_Method_type(ent->type));

	ent->peculiarity = pec;
}

static INLINE ir_stickyness
_get_entity_stickyness(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->stickyness;
}

static INLINE void
_set_entity_stickyness(ir_entity *ent, ir_stickyness stickyness) {
	assert(ent && ent->kind == k_entity);
	ent->stickyness = stickyness;
}

static INLINE int
_is_entity_final(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return (int)ent->final;
}

static INLINE void
_set_entity_final(ir_entity *ent, int final) {
	assert(ent && ent->kind == k_entity);
	ent->final = final ? 1 : 0;
}

static INLINE int
_is_entity_compiler_generated(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->compiler_gen;
}

static INLINE void
_set_entity_compiler_generated(ir_entity *ent, int flag) {
	assert(ent && ent->kind == k_entity);
	ent->compiler_gen = flag ? 1 : 0;
}

static INLINE int
_is_entity_backend_marked(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->backend_marked;
}

static INLINE void
_set_entity_backend_marked(ir_entity *ent, int flag) {
	assert(ent && ent->kind == k_entity);
	ent->backend_marked = flag ? 1 : 0;
}

static INLINE ir_address_taken_state
_get_entity_address_taken(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->address_taken;
}

static INLINE void
_set_entity_address_taken(ir_entity *ent, ir_address_taken_state state) {
	assert(ent && ent->kind == k_entity);
	assert(state == ir_address_not_taken ||
			state == ir_address_taken_unknown ||
			state == ir_address_taken);
	ent->address_taken = state;
}

static INLINE int
_get_entity_offset(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->offset;
}

static INLINE void
_set_entity_offset(ir_entity *ent, int offset) {
	assert(ent && ent->kind == k_entity);
	ent->offset = offset;
}

static INLINE unsigned char
_get_entity_offset_bits_remainder(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->offset_bit_remainder;
}

static INLINE void
_set_entity_offset_bits_remainder(ir_entity *ent, unsigned char offset) {
	assert(ent && ent->kind == k_entity);
	ent->offset_bit_remainder = offset;
}

static INLINE void *
_get_entity_link(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->link;
}

static INLINE void
_set_entity_link(ir_entity *ent, void *l) {
	assert(ent && ent->kind == k_entity);
	ent->link = l;
}

static INLINE ir_graph *
_get_entity_irg(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	assert(ent == unknown_entity || is_Method_type(ent->type));
	if (!get_visit_pseudo_irgs() && ent->attr.mtd_attr.irg
		&& is_pseudo_ir_graph(ent->attr.mtd_attr.irg))
		return NULL;
	return ent->attr.mtd_attr.irg;
}

static INLINE unsigned long
_get_entity_visited(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->visit;
}

static INLINE void
_set_entity_visited(ir_entity *ent, unsigned long num) {
	assert(ent && ent->kind == k_entity);
	ent->visit = num;
}

static INLINE void
_mark_entity_visited(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	ent->visit = firm_type_visited;
}

static INLINE int
_entity_visited(ir_entity *ent) {
	return _get_entity_visited(ent) >= firm_type_visited;
}

static INLINE int
_entity_not_visited(ir_entity *ent) {
	return _get_entity_visited(ent) < firm_type_visited;
}

static INLINE ir_type *
_get_entity_repr_class(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	return ent->repr_class;
}

static INLINE dbg_info *
_get_entity_dbg_info(const ir_entity *ent) {
	return ent->dbi;
}

static INLINE void
_set_entity_dbg_info(ir_entity *ent, dbg_info *db) {
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
#define get_entity_allocation(ent)               _get_entity_allocation(ent)
#define set_entity_allocation(ent, al)           _set_entity_allocation(ent, al)
#define get_entity_visibility(ent)               _get_entity_visibility(ent)
#define get_entity_variability(ent)              _get_entity_variability(ent)
#define get_entity_volatility(ent)               _get_entity_volatility(ent)
#define set_entity_volatility(ent, vol)          _set_entity_volatility(ent, vol)
#define get_entity_peculiarity(ent)              _get_entity_peculiarity(ent)
#define set_entity_peculiarity(ent, pec)         _set_entity_peculiarity(ent, pec)
#define get_entity_stickyness(ent)               _get_entity_stickyness(ent)
#define set_entity_stickyness(ent, stickyness)   _set_entity_stickyness(ent, stickyness)
#define is_entity_final(ent)                     _is_entity_final(ent)
#define set_entity_final(ent, final)             _set_entity_final(ent, final)
#define is_entity_compiler_generated(ent)        _is_entity_compiler_generated(ent)
#define set_entity_compiler_generated(ent, flag) _set_entity_compiler_generated(ent, flag)
#define is_entity_backend_marked(ent)            _is_entity_backend_marked(ent)
#define set_entity_backend_marked(ent, flag)     _set_entity_backend_marked(ent, flag)
#define get_entity_address_taken(ent)            _get_entity_address_taken(ent)
#define set_entity_address_taken(ent, flag)      _set_entity_address_taken(ent, flag)
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


#endif /* FIRM_TR_ENTITY_T_H */
