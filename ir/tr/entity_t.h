/*
 * Project:     libFIRM
 * File name:   ir/tr/entity_t.h
 * Purpose:     Representation of all program known entities -- private header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file entity_t.h
 *
 * entity.h:  entities represent all program known objects.
 *
 * @author Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 *
 *  An entity is the representation of program known objects in Firm.
 *  The primary concept of entities is to represent members of complex
 *  types, i.e., fields and methods of classes.  As not all programming
 *  language model all variables and methods as members of some class,
 *  the concept of entities is extended to cover also local and global
 *  variables, and arbitrary procedures.
 *
 *  An entity always specifies the type of the object it represents and
 *  the type of the object it is a part of, the owner of the entity.
 *  Originally this is the type of the class of which the entity is a
 *  member.
 *  The owner of local variables is the procedure they are defined in.
 *  The owner of global variables and procedures visible in the whole
 *  program is a universally defined class type "GlobalType".  The owner
 *  of procedures defined in the scope of an other procedure is the
 *  enclosing procedure.
 */

# ifndef _ENTITY_T_H_
# define _ENTITY_T_H_

#include "firm_common_t.h"
#include "firm_config.h"

# include "entity.h"
# include "typegmod.h"
# include "mangle.h"
# include "pseudo_irg.h"


/** A path in a compund graph. */
struct compound_graph_path {
  firm_kind kind;       /**< dynamic type tag for compound graph path. */
  type *tp;             /**< The type this path belongs to. */
  int len;              /**< length of the path */
  int *arr_indicees;    /**< List of array indeces.  To compute position of
			   array elements */
  entity *nodes[1];     /**< List of entities of length len to express the
			   access path. */
};

/** the type of an entity */
struct entity {
  firm_kind kind;       /**< dynamic type tag for entity. */
  ident *name;          /**< name of this entity */
  ident *ld_name;       /**< Unique name of this entity, i.e., the mangled
                           name.  If the field is read before written a default
                           mangling is applies.  The name of the owner is prepended
                           to the name of the entity, separated by a underscore.
                           E.g.,  for a class `A' with field `a' this
                           is the ident for `A_a'. */
  type *type;           /**< The type of this entity, e.g., a method type, a
                           basic type of the language or a class itself */
  type *owner;          /**< The compound type (e.g. class type) this entity belongs to. */
  ent_allocation allocation;  /**< Distinguishes static and dynamically allocated
                 entities and some further cases. */
  ent_visibility visibility;  /**< Specifies visibility to external program
                 fragments */
  ent_variability variability;  /**< Specifies variability of entities content */
  ent_volatility volatility;    /**< Specifies volatility of entities content */
  ent_stickyness stickyness;    /**< Specifies whether this entity is sticky  */
  int  offset;          /**< Offset in bits for this entity.  Fixed when layout
               of owner is determined.  */
  void *link;           /**< To store some intermediate information */
  unsigned long visit;  /**< visited counter for walks of the type information */
  struct dbg_info* dbi;    /**< A pointer to information for debug support. */

  /* ------------- fields for atomic entities  ---------------*/

  ir_node *value;            /**< value if entity is not of variability uninitialized.
                               Only for atomic entities. */

  /* ------------- fields for compound entities ---------------*/

  ir_node **values;     /**< constant values of compound entities. Only available if
               variablility not uninitialized.  Must be set for variability constant
                           */
  compound_graph_path **val_paths; /**< paths corresponding to constant values. Only available if
                      variablility not uninitialized.  Must be set for variability constant */

  /* ------------- fields for entities owned by a class type ---------------*/

  entity **overwrites;     /**< A list of entities this entity overwrites. */
  entity **overwrittenby;  /**< A list of entities that overwrite this entity.  */

  /* ------------- fields for methods ---------------*/

  enum peculiarity peculiarity;
  ir_graph *irg;        /**< If (type == method_type) this is the corresponding irg.
               The ir_graph constructor automatically sets this field.
               Yes, it must be here. */

  /* ------------- fields for analyses ---------------*/


#ifdef DEBUG_libfirm
  int nr;             /**< a unique node number for each node to make output
                           readable. */
# endif /* DEBUG_libfirm */
};



/* ----------------------- inline functions ------------------------ */
static INLINE int
_is_entity(const void *thing) {
  return get_kind(thing) == k_entity;
}

static INLINE const char *
_get_entity_name(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return get_id_str(get_entity_ident(ent));
}

static INLINE ident *
_get_entity_ident(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->name;
}

static INLINE type *
_get_entity_owner(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->owner = skip_tid(ent->owner);
}

static INLINE ident *
_get_entity_ld_ident(entity *ent)
{
  assert(ent && ent->kind == k_entity);
  if (ent->ld_name == NULL)
    ent->ld_name = mangle_entity(ent);
  return ent->ld_name;
}

static INLINE void
_set_entity_ld_ident(entity *ent, ident *ld_ident) {
  assert(ent && ent->kind == k_entity);
  ent->ld_name = ld_ident;
}

static INLINE const char *
_get_entity_ld_name(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return get_id_str(get_entity_ld_ident(ent));
}

static INLINE type *
_get_entity_type(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->type = skip_tid(ent->type);
}

static INLINE void
_set_entity_type(entity *ent, type *type) {
  assert(ent && ent->kind == k_entity);
  ent->type = type;
}

static INLINE ent_allocation
_get_entity_allocation(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->allocation;
}

static INLINE void
_set_entity_allocation(entity *ent, ent_allocation al) {
  assert(ent && ent->kind == k_entity);
  ent->allocation = al;
}

static INLINE ent_visibility
_get_entity_visibility(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->visibility;
}

static INLINE ent_variability
_get_entity_variability(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->variability;
}

static INLINE ent_volatility
_get_entity_volatility(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->volatility;
}

static INLINE void
_set_entity_volatility(entity *ent, ent_volatility vol) {
  assert(ent && ent->kind == k_entity);
  ent->volatility = vol;
}

static INLINE peculiarity
_get_entity_peculiarity(const entity *ent) {
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
_set_entity_peculiarity(entity *ent, peculiarity pec) {
  assert(ent && ent->kind == k_entity);
  /* @@@ why peculiarity only for methods? */
  //assert(is_Method_type(ent->type));

  ent->peculiarity = pec;
}

static INLINE ent_stickyness
_get_entity_stickyness(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->stickyness;
}

static INLINE void
_set_entity_stickyness(entity *ent, ent_stickyness stickyness)
{
  assert(ent && ent->kind == k_entity);
  ent->stickyness = stickyness;
}

static INLINE int
_get_entity_offset_bits(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->offset;
}

static INLINE int
_get_entity_offset_bytes(const entity *ent) {
  int bits = _get_entity_offset_bits(ent);

  if (bits & 7) return -1;
  return bits >> 3;
}

static INLINE void
_set_entity_offset_bits(entity *ent, int offset) {
  assert(ent && ent->kind == k_entity);
  ent->offset = offset;
}

static INLINE void
_set_entity_offset_bytes(entity *ent, int offset) {
  _set_entity_offset_bits(ent, offset * 8);
}

static INLINE void *
_get_entity_link(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->link;
}

static INLINE void
_set_entity_link(entity *ent, void *l) {
  assert(ent && ent->kind == k_entity);
  ent->link = l;
}

static INLINE ir_graph *
_get_entity_irg(const entity *ent) {
  assert(ent && ent->kind == k_entity);
  assert(ent == unknown_entity || is_Method_type(ent->type));
  if (!get_visit_pseudo_irgs() && ent->irg && is_pseudo_ir_graph(ent->irg))
    return NULL;
  return ent->irg;
}

#define is_entity(thing)                        _is_entity(thing)
#define get_entity_name(ent)                    _get_entity_name(ent)
#define get_entity_ident(ent)                   _get_entity_ident(ent)
#define get_entity_owner(ent)                   _get_entity_owner(ent)
#define get_entity_ld_ident(ent)                _get_entity_ld_ident(ent)
#define set_entity_ld_ident(ent, ld_ident)      _set_entity_ld_ident(ent, ld_ident)
#define get_entity_ld_name(ent)                 _get_entity_ld_name(ent)
#define get_entity_type(ent)                    _get_entity_type(ent)
#define set_entity_type(ent, type)              _set_entity_type(ent, type)
#define get_entity_allocation(ent)              _get_entity_allocation(ent)
#define set_entity_allocation(ent, al)          _set_entity_allocation(ent, al)
#define get_entity_visibility(ent)              _get_entity_visibility(ent)
#define get_entity_variability(ent)             _get_entity_variability(ent)
#define get_entity_volatility(ent)              _get_entity_volatility(ent)
#define set_entity_volatility(ent, vol)         _set_entity_volatility(ent, vol)
#define get_entity_peculiarity(ent)             _get_entity_peculiarity(ent)
#define set_entity_peculiarity(ent, pec)        _set_entity_peculiarity(ent, pec)
#define get_entity_stickyness(ent)              _get_entity_stickyness(ent)
#define set_entity_stickyness(ent, stickyness)  _set_entity_stickyness(ent, stickyness)
#define get_entity_offset_bits(ent)             _get_entity_offset_bits(ent)
#define get_entity_offset_bytes(ent)            _get_entity_offset_bytes(ent)
#define set_entity_offset_bits(ent, offset)     _set_entity_offset_bits(ent, offset)
#define set_entity_offset_bytes(ent, offset)    _set_entity_offset_bytes(ent, offset)
#define get_entity_link(ent)                    _get_entity_link(ent)
#define set_entity_link(ent, l)                 _set_entity_link(ent, l)
#define get_entity_irg(ent)                     _get_entity_irg(ent)

# endif /* _ENTITY_T_H_ */
