/*
*  Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
*  All rights reserved.
*
*  Authors: Martin Trapp, Christian Schaefer,
*           Goetz Lindenmaier
*
*  entity.h:  entities represent all program known objects.
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
*
*  In detail the datastructure entity has the following fields:
*
*  ident *name     Name of this entity as specified in the source code.
*                  Only unequivocal in conjuction with scope.
*  ident *ld_name  Unique name of this entity, i.e., the mangled
*                  name.  E.g., for a class `A' with field `a' this
*                  is the ident for `A_a'.
*  type *type      The type of this entity, e.g., a method type, a
*                  basic type of the language or a class itself.
*  type *owner;    The class this entity belongs to.  In case of local
* 	    variables the method they are defined in.
*  int offset;     Offset in byte for this entity.  Fixed when layout
* 	    of owner is determined.
*  ir_graph *irg;  If (type == method_type) this is the corresponding irg.
* 	    The ir_graph constructor automatically sets this field.
*                  If (type != method_type) access of this field will cause
*                  an assertion.
*/

/* $Id$ */

# ifndef _ENTITY_H_
# define _ENTITY_H_

# include "ident.h"
# include "type.h"
# include "dbginfo.h"

/*******************************************************************/
/** general                                                       **/
/*******************************************************************/

/** initalize entity module */
void init_entity (void);

/*******************************************************************/
/** ENTITY                                                        **/
/*******************************************************************/

/* to resolve recursion between entity.h and irgraph.h */
#ifndef _IR_GRAPH_TYPEDEF_
#define _IR_GRAPH_TYPEDEF_
typedef struct ir_graph ir_graph;
#endif

/**
 *
 *   An abstract data type to represent program entites.
 *
 *   @param owner      A compound type this entity is a part of.
 *   @param type       The type of this entity.
 *   @param name       The string that represents this entity in the source program.
 *   @param allocation A flag saying whether the entity is dynamically or statically
 *              allocated (values: dynamic_allocated,  static_allocated,
 *              automatic_allocated).
 *   @param visibility A flag indicating the visibility of this entity (values: local,
 *              external_visible,  external_allocated)
 *   @param variability A flag indicating the variability of this entity (values:
 *              uninitialized, initalized, part_constant, constant)
 *   @param volatility @@@
 *   @param offset     The offset of the entity within the compound object.  Only set
 *              if the owner in the state "layout_fixed".
 *   @param overwrites A list of entities overwritten by this entity.  This list is only
 *              existent if the owner of this entity is a class.  The members in
 *              this list must be entities of super classes.
 *   @param overwrittenby A list of entities that overwrite this entity.  This list is only
 *              existent if the owner of this entity is a class.  The members in
 *              this list must be entities of sub classes.
 *   @param link       A void* to associate some additional information with the entity.
 *   @param irg        If the entity is a method this is the ir graph that represents the
 *              code of the method.
 *   @param peculiarity The peculiarity of the entity.  If the entity is a method this
 *              indicates whether the entity represents
 *              a real method or whether it only exists to describe an interface.
 *              In that case there nowhere exists code for this entity and this entity
 *              is never dynamically used in the code.
 *              Values: description, existent.  Default: existent.
 *   @param visited   visited flag.  Master flag is type_visited.
 *
 *  @param These fields can only be accessed via access functions.
 *
 * @see  type
 */

/* to resolve recursion between entity.h and type.h */
#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct entity entity;
#endif


/** Creates a new entity.
   Automatically inserts the entity as a member of owner.
   Entity is automatic_allocated and uninitialize except if the type
   is type_method, then it is static_allocated and constant.  The constant
   value is a pointer to the method.
   Visibility is local, offset -1, and it is not volatile. */
entity     *new_entity (type *owner, ident *name, type *tp);
entity     *new_d_entity (type *owner, ident *name, type *tp, dbg_info *db);
/** Copies the entity if the new_owner is different from the
   owner of the old entity.  Else returns the old entity.
   Automatically inserts the new entity as a member of owner.
   Resets the overwrites/overwritten_by fields. */
entity     *copy_entity_own (entity *old, type *new_owner);
/** Copies the entity if the new_name is different from the
   name of the old entity.  Else returns the old entity.
   Automatically inserts the new entity as a member of owner.
   The mangled name ld_name is set to NULL. */
entity     *copy_entity_name (entity *old, ident *new_name);
/** Frees the entity.  The owner will still contain the pointer to this
   entity, as well as all other references! */
void        free_entity (entity *ent);

/** manipulate fields of entity **/
const char *get_entity_name     (entity *ent);
ident      *get_entity_ident    (entity *ent);

/* returns the mangled name of the entity.  If the mangled name is
   set it returns the existing name.  Else it generates a name
   with mangle_entity() and remembers this new name internally. */
ident      *get_entity_ld_ident (entity *ent);
void        set_entity_ld_ident (entity *ent, ident *ld_ident);
const char *get_entity_ld_name (entity *end);

type       *get_entity_owner (entity *ent);
/* Sets the owner field in entity to owner.  Don't forget to add
   ent to owner!! */
void        set_entity_owner (entity *ent, type *owner);
INLINE void assert_legal_owner_of_ent(type *owner);

type     *get_entity_type (entity *ent);
void      set_entity_type (entity *ent, type *tp);

typedef enum {
  automatic_allocated, /**< The entity is allocated during runtime, implicitly
		  	  as component of a compound type.   This is the default. */
  parameter_allocated, /**< The entity is a parameter.  It is also automatic allocated.
			  We distinguish the allocation of paramters from the allocation
			  of local variables as their placement depends on the calling
			  conventions. */
  dynamic_allocated,   /**< The entity is allocated during runtime, explicitly
			  by an Alloc node. */
  static_allocated     /**< The entity is allocated statically.  We can use a
                          SymConst(?) as address of the entity. */
} ent_allocation;

ent_allocation get_entity_allocation (entity *ent);
void           set_entity_allocation (entity *ent, ent_allocation al);

/* This enumeration flags the visibility of entities.  This is necessary
   for partial compilation. */
typedef enum {
  local,              /**< The entity is only visible locally.  This is the default. */
  external_visible,   /**< The entity is visible to other external program parts, but
			 it is defined here.  It may not be optimized away.  The entity must
		         be static_allocated. */
  external_allocated  /**< The entity is defined and allocated externally.  This compilation
			 must not allocate memory for this entity. The entity must
		         be static_allocated.  This can also be an external defined
			 method. */
} ent_visibility;

ent_visibility get_entity_visibility (entity *ent);
void           set_entity_visibility (entity *ent, ent_visibility vis);

/** This enumeration flags the variability of entities. */
typedef enum {
  uninitialized,    /**< The content of the entity is completely unknown. */
  initialized,      /**< After allocation the entity is initalized with the
		       value given somewhere in the entity. */
  part_constant,    /**< For entities of compound types.  Some members of the entity
		       are constant.  The others are uninitialized.  Those members
		       given a value for are constant. */
  constant          /**< The entity is constant. */
} ent_variability;

ent_variability get_entity_variability (entity *ent);
void            set_entity_variability (entity *ent, ent_variability var);

/** This enumeration flags the volatility of entities. */
typedef enum {
  non_volatile,    /**< The entity is not volatile */
  is_volatile      /**< The entity is volatile */
} ent_volatility;

ent_volatility get_entity_volatility (entity *ent);
void           set_entity_volatility (entity *ent, ent_volatility vol);

/* Only set if layout = fixed. */
int       get_entity_offset (entity *ent);
void      set_entity_offset (entity *ent, int offset);

/* A link to store intermediate information */
void*   get_entity_link(entity *ent);
void    set_entity_link(entity *ent, void *l);

/** Fields of method entities **/
/* The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg.
   Only entities of peculiarity "existent" can have a corresponding irg,
   else the field is fixed to NULL.  (Get returns NULL, set asserts.) */
ir_graph *get_entity_irg(entity *ent);
void      set_entity_irg(entity *ent, ir_graph *irg);

/* For the definition of enumeration peculiarity see type.h */
peculiarity get_entity_peculiarity (entity *ent);
void        set_entity_peculiarity (entity *ent, peculiarity pec);

/** Representation of constant values of entites **/
/* Set current_ir_graph to get_const_code_irg() to generate a constant
   expression. */
/* Copies a firm subgraph that complies to the restrictions for
   constant expressions to current_block in current_ir_graph. */
ir_node *copy_const_value(ir_node *n);

/* Set has no effect for entities of type method. */
ir_node *get_atomic_ent_value(entity *ent);
void     set_atomic_ent_value(entity *ent, ir_node *val);

/* A value of a compound entity is a pair of a value and the corresponding
   member of the compound. */
void     add_compound_ent_value(entity *ent, ir_node *val, entity *member);
int      get_compound_ent_n_values(entity *ent);
ir_node *get_compound_ent_value(entity *ent, int pos);
entity  *get_compound_ent_value_member(entity *ent, int pos);
void     set_compound_ent_value(entity *ent, ir_node *val, entity *member, int pos);
void     remove_compound_ent_value(entity *ent, entity *value_ent);
/* Inits the entity ent witch must be of a one dimensional
   array type with the values given in the values array.
   The array must have a lower and an upper bound.  Keeps the
   order of values. Does not test whether the number of values
   fits into the given array size.  Does not test whether the
   values have the proper mode for the array. */
void set_array_entity_values(entity *ent, tarval **values, int num_vals);

/** Fields of entities with a class type as owner **/
/* Overwrites is a field that specifies that an access to the overwritten
   entity in the supertype must use this entity.  It's a list as with
   multiple inheritance several enitites can be overwritten.  This field
   is mostly useful for method entities.
   If a Sel node selects an entity that is overwritten by other entities it
   must return a pointer to the entity of the dynamic type of the pointer
   that is passed to it.  Lowering of the Sel node must assure this.
   Overwrittenby is the inverse of overwrites.  Both add routines add
   both relations, they only differ in the order of arguments. */
void    add_entity_overwrites   (entity *ent, entity *overwritten);
int     get_entity_n_overwrites (entity *ent);
int     get_entity_overwrites_index(entity *ent, entity *overwritten);
entity *get_entity_overwrites   (entity *ent, int pos);
void    set_entity_overwrites   (entity *ent, int pos, entity *overwritten);
void    remove_entity_overwrites(entity *ent, entity *overwritten);

void    add_entity_overwrittenby   (entity *ent, entity *overwrites);
int     get_entity_n_overwrittenby (entity *ent);
int     get_entity_overwrittenby_index(entity *ent, entity *overwrites);
entity *get_entity_overwrittenby   (entity *ent, int pos);
void    set_entity_overwrittenby   (entity *ent, int pos, entity *overwrites);
void    remove_entity_overwrittenby(entity *ent, entity *overwrites);

/**
 *   Checks whether a pointer points to an entity.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is an entity, else false
 */
int is_entity (void *thing);

/** Returns true if the type of the entity is a primitive, pointer
   enumeration or method type. */
int is_atomic_entity(entity *ent);
/** Returns true if the type of the entity is a class, structure,
   array or union type. */
int is_compound_entity(entity *ent);

/** Returns true if ent1 and ent2 have are equal except for their owner.
   Two entities are equal if
    - they have the same type (the same C-struct)
    - ...?
*/
bool equal_entity(entity *ent1, entity *ent2);


unsigned long get_entity_visited(entity *ent);
void        set_entity_visited(entity *ent, unsigned long num);
/** Sets visited field in entity to entity_visited. */
void        mark_entity_visited(entity *ent);
bool        entity_visited(entity *ent);
bool        entity_not_visited(entity *ent);




# endif /* _ENTITY_H_ */
