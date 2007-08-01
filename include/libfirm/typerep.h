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
#ifndef FIRM_TYPEREP_H
#define FIRM_TYPEREP_H

#include "firm_types.h"

/**
 * @page entity       Entity representation
 *
 * An entity is the representation of program known objects in Firm.
 * The primary concept of entities is to represent members of complex
 * types, i.e., fields and methods of classes.  As not all programming
 * language model all variables and methods as members of some class,
 * the concept of entities is extended to cover also local and global
 * variables, and arbitrary procedures.
 *
 * An entity always specifies the type of the object it represents and
 * the type of the object it is a part of, the owner of the entity.
 * Originally this is the type of the class of which the entity is a
 * member.
 * The owner of local variables is the procedure they are defined in.
 * The owner of global variables and procedures visible in the whole
 * program is a universally defined class type "GlobalType".  The owner
 * of procedures defined in the scope of an other procedure is the
 * enclosing procedure.
 *
 * The type ir_entity is an abstract data type to represent program entities.
 * If contains the following attributes:
 *
 *   - owner:      A compound type this entity is a part of.
 *   - type:       The type of this entity.
 *   - name:       The string that represents this entity in the source program. *   - allocation: A flag saying whether the entity is dynamically or statically *                 allocated (values: dynamic_allocated,  static_allocated,
 *                 automatic_allocated).
 *   - visibility: A flag indicating the visibility of this entity (values: local,
 *                 external_visible,  external_allocated)
 *   - variability: A flag indicating the variability of this entity (values:
 *                  uninitialized, initialized, part_constant, constant)
 *   - volatility: @@@
 *   - offset:     The offset of the entity within the compound object in bytes.  Only set
 *                 if the owner in the state "layout_fixed".
 *   - offset_bits_remainder:   The offset bit remainder of a bitfield entity (in a compound)
 *                 in bits.  Only set if the owner in the state "layout_fixed".
 *   - overwrites: A list of entities overwritten by this entity.  This list is only
 *                 existent if the owner of this entity is a class.  The members in
 *                 this list must be entities of super classes.
 *   - overwrittenby: A list of entities that overwrite this entity.  This list is only
 *                 existent if the owner of this entity is a class.  The members in
 *                 this list must be entities of sub classes.
 *   - link:       A void* to associate some additional information with the entity.
 *   - irg:        If the entity is a method this is the ir graph that represents the
 *                 code of the method.
 *   - peculiarity: The peculiarity of the entity.  If the entity is a method this
 *                 indicates whether the entity represents
 *                 a real method or whether it only exists to describe an interface.
 *                 In that case there nowhere exists code for this entity and this entity
 *                 is never dynamically used in the code.
 *                 Values: description, existent.  Default: existent.
 *   - visited:    visited flag.  Master flag is type_visited.
 *
 * These fields can only be accessed via access functions.
 *
 * @see  ir_type, ir_entity
 */

/** This enumeration flags the visibility of entities and types.
 *
 * This is necessary for partial compilation.
 * We rely on the ordering of the flags.
 */
typedef enum {
  visibility_local,              /**< The entity is only visible locally.  This is the default for
                                      entities.
                                      The type is only visible locally.  All instances are allocated
                                      locally, and no pointer to entities of this type are passed
                                      out of this compilation unit. */
  visibility_external_visible,   /**< The entity is visible to other external program parts, but
                                      it is defined here.  It may not be optimized away.  The entity must
                                      be static_allocated.
                                      For types:  entities of this type can be accessed externally.  No
                                      instances of this type are allocated externally.  */
  visibility_external_allocated  /**< The entity is defined and allocated externally.  This compilation
                                      must not allocate memory for this entity. The entity must
                                      be static_allocated.  This can also be an external defined
                                      method.
                                      For types:  entities of this type are allocated and accessed from
                                      external code.  Default for types.  */
} ir_visibility;

/** This enumeration flags the peculiarity of entities and types. */
typedef enum {
  peculiarity_description,     /**< Represents only a description.  The entity/type is never
                            allocated, no code/data exists for this entity/type.
                        @@@ eventually rename to descriptive (adjective as the others!)*/
  peculiarity_inherited,       /**< Describes explicitly that other entities are
                            inherited to the owner of this entity.
                            Overwrites must refer to at least one other
                            entity.  If this is a method entity there exists
                            no irg for this entity, only for one of the
                            overwritten ones.
                        Only for entity. */
  peculiarity_existent         /**< The entity/type (can) exist.
                    @@@ eventually rename to 'real' i.e., 'echt'
                        This serves better as opposition to description _and_ inherited.*/
} ir_peculiarity;

/**
 * Additional method type properties:
 * Tell about special properties of a method type. Some
 * of these may be discovered by analyses.
 */
typedef enum {
  mtp_no_property        = 0x00000000, /**< no additional properties, default */
  mtp_property_const     = 0x00000001, /**< This method did not access memory and calculates
                                         its return values solely from its parameters.
                                         GCC: __attribute__((const)). */
  mtp_property_pure      = 0x00000002, /**< This method did NOT write to memory and calculates
                                         its return values solely from its parameters and
                                         the memory they points to (or global vars).
                                         GCC: __attribute__((pure)). */
  mtp_property_noreturn  = 0x00000004, /**< This method did not return due to an aborting system
                                         call.
                                         GCC: __attribute__((noreturn)). */
  mtp_property_nothrow   = 0x00000008, /**< This method cannot throw an exception.
                                         GCC: __attribute__((nothrow)). */
  mtp_property_naked     = 0x00000010, /**< This method is naked.
                                         GCC: __attribute__((naked)). */
  mtp_property_malloc    = 0x00000020, /**< This method returns newly allocate memory.
                                         GCC: __attribute__((malloc)). */
  mtp_property_intrinsic = 0x00000040, /**< This method is intrinsic. It is expected that
                                         a lowering phase will remove all calls to it. */
  mtp_property_runtime   = 0x00000080, /**< This method represents a runtime routine. */
  mtp_property_private   = 0x00000100, /**< All method invocations are known, the backend is free to
									        optimize the call in any possible way. */
  mtp_property_inherited = (1<<31)     /**< Internal. Used only in irg's, means property is
                                         inherited from type. */
} mtp_additional_property;

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * Entity is automatic_allocated and uninitialized except if the type
 * is type_method, then it is static_allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
ir_entity     *new_entity(ir_type *owner, ident *name, ir_type *tp);

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * The entity is automatic allocated and uninitialized except if the type
 * is type_method, then it is static allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
ir_entity     *new_d_entity(ir_type *owner, ident *name, ir_type *tp, dbg_info *db);

/**
 * Copies the entity if the new_owner is different from the
 * owner of the old entity,  else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * Resets the overwrites/overwritten_by fields.
 * Keeps the old atomic value.
 *   @@@ Maybe we should change this.  If peculiarity of a method
 *       is existent, we should add a new SymConst that points to
 *       itself and not to the origin.  Right now we have to change
 *       the peculiarity and then set a new atomic value by hand.
 */
ir_entity     *copy_entity_own(ir_entity *old, ir_type *new_owner);

/**
 * Copies the entity if the new_name is different from the
 * name of the old entity, else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * The mangled name ld_name is set to NULL.
 * Overwrites relation is copied from old.
 */
ir_entity     *copy_entity_name(ir_entity *old, ident *new_name);

/**
 * Frees the entity.
 *
 * The owner will still contain the pointer to this
 * entity, as well as all other references!
 */
void        free_entity(ir_entity *ent);

/** Returns the name of an entity. */
const char *get_entity_name(const ir_entity *ent);

/** Returns the ident of an entity. */
ident      *get_entity_ident(const ir_entity *ent);

/** Sets the ident of the entity. */
void        set_entity_ident(ir_entity *ent, ident *id);

/** Returns the mangled name of the entity.
 *
 * If the mangled name is set it returns the existing name.
 * Else it generates a name with mangle_entity()
 * and remembers this new name internally.
 */
ident      *get_entity_ld_ident(ir_entity *ent);

/** Sets the mangled name of the entity. */
void        set_entity_ld_ident(ir_entity *ent, ident *ld_ident);

/** Returns the mangled name of the entity as a string. */
const char *get_entity_ld_name(ir_entity *ent);

/** Returns the owner of the entity. */
ir_type    *get_entity_owner(ir_entity *ent);

/** Sets the owner field in entity to owner.  Don't forget to add
   ent to owner!! */
void        set_entity_owner(ir_entity *ent, ir_type *owner);

/** Returns the type of an entity. */
ir_type  *get_entity_type(ir_entity *ent);

/** Sets the type of an entity. */
void      set_entity_type(ir_entity *ent, ir_type *tp);

/** The allocation type. */
typedef enum {
  allocation_automatic, /**< The entity is allocated during runtime, implicitly
                             as component of a compound type.   This is the default. */
  allocation_parameter, /**< The entity is a parameter.  It is also automatic allocated.
                             We distinguish the allocation of parameters from the allocation
                             of local variables as their placement depends on the calling
                             conventions. */
  allocation_dynamic,   /**< The entity is allocated during runtime, explicitly
                             by an Alloc node. */
  allocation_static     /**< The entity is allocated statically.  We can use a
                             Const as address of the entity.  This is the default for methods. */
} ir_allocation;

/** Returns the allocation type of an entity. */
ir_allocation get_entity_allocation(const ir_entity *ent);

/** Sets the allocation type of an entity. */
void           set_entity_allocation(ir_entity *ent, ir_allocation al);

/** Return the name of the allocation type. */
const char *get_allocation_name(ir_allocation vis);

/** Returns the visibility of an entity. */
ir_visibility get_entity_visibility(const ir_entity *ent);

/** Sets the visibility of an entity. */
void       set_entity_visibility(ir_entity *ent, ir_visibility vis);

/** Return the name of the visibility */
const char *get_visibility_name(ir_visibility vis);

/** This enumeration flags the variability of entities. */
typedef enum {
  variability_uninitialized,    /**< The content of the entity is completely unknown. Default. */
  variability_initialized,      /**< After allocation the entity is initialized with the
                                     value given somewhere in the entity. */
  variability_part_constant,    /**< For entities of compound types.
                                     The members of the entity are mixed constant,
                                     initialized or uninitialized. */
  variability_constant          /**< The entity is constant. */
} ir_variability;

/** Returns the variability of an entity. */
ir_variability get_entity_variability(const ir_entity *ent);

/** Sets the variability of an entity. */
void           set_entity_variability(ir_entity *ent, ir_variability var);

/** Return the name of the variability. */
const char *get_variability_name(ir_variability var);

/** This enumeration flags the volatility of entities. */
typedef enum {
  volatility_non_volatile,    /**< The entity is not volatile. Default. */
  volatility_is_volatile      /**< The entity is volatile */
} ir_volatility;

/** Returns the volatility of an entity. */
ir_volatility get_entity_volatility(const ir_entity *ent);

/** Sets the volatility of an entity. */
void          set_entity_volatility(ir_entity *ent, ir_volatility vol);

/** Return the name of the volatility. */
const char *get_volatility_name(ir_volatility var);

/** This enumeration flags the stickyness of an entity. */
typedef enum {
  stickyness_unsticky,          /**< The entity can be removed from
                                   the program, unless contraindicated
                                   by other attributes. Default. */
  stickyness_sticky             /**< The entity must remain in the
                                   program in any case. */
} ir_stickyness;

/** Get the entity's stickyness. */
ir_stickyness get_entity_stickyness(const ir_entity *ent);

/** Set the entity's stickyness. */
void          set_entity_stickyness(ir_entity *ent, ir_stickyness stickyness);

/** Returns the offset of an entity (in a compound) in bytes. Only set if layout = fixed. */
int       get_entity_offset(const ir_entity *ent);

/** Sets the offset of an entity (in a compound) in bytes. */
void      set_entity_offset(ir_entity *ent, int offset);

/** Returns the offset bit remainder of a bitfield entity (in a compound) in bits. Only set if layout = fixed. */
unsigned char get_entity_offset_bits_remainder(const ir_entity *ent);

/** Sets the offset bit remainder of a bitfield entity (in a compound) in bits. */
void      set_entity_offset_bits_remainder(ir_entity *ent, unsigned char offset);

/** Returns the stored intermediate information. */
void *get_entity_link(const ir_entity *ent);

/** Stores new intermediate information. */
void set_entity_link(ir_entity *ent, void *l);

/* -- Fields of method entities -- */
/** The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg.
   Only entities of peculiarity "existent" can have a corresponding irg,
   else the field is fixed to NULL.  (Get returns NULL, set asserts.) */
ir_graph *get_entity_irg(const ir_entity *ent);
void      set_entity_irg(ir_entity *ent, ir_graph *irg);

/** Gets the entity vtable number. */
unsigned get_entity_vtable_number(const ir_entity *ent);

/** Sets the entity vtable number. */
void     set_entity_vtable_number(ir_entity *ent, unsigned vtable_number);

/** Return the peculiarity of an entity. */
ir_peculiarity get_entity_peculiarity(const ir_entity *ent);

/** Sets the peculiarity of an entity. */
void           set_entity_peculiarity(ir_entity *ent, ir_peculiarity pec);

/** Checks if an entity cannot be overridden anymore. */
int is_entity_final(const ir_entity *ent);

/** Sets/resets the final flag of an entity. */
void set_entity_final(ir_entity *ent, int final);

/** Checks if an entity is compiler generated. */
int is_entity_compiler_generated(const ir_entity *ent);

/** Sets/resets the compiler generated flag. */
void set_entity_compiler_generated(ir_entity *ent, int flag);

/** Checks if an entity is marked by the backend. */
int is_entity_backend_marked(const ir_entity *ent);

/** Sets/resets the backend marker flag. */
void set_entity_backend_marked(ir_entity *ent, int flag);

/**
 * The state of the address_taken flag.
 */
typedef enum {
	ir_address_not_taken     = 0,  /**< The address is NOT taken. */
	ir_address_taken_unknown = 1,  /**< The state of the address taken flag is unknown. */
	ir_address_taken         = 2   /**< The address IS taken. */
} ir_address_taken_state;

/** Return the state of the address taken flag of an entity. */
ir_address_taken_state get_entity_address_taken(const ir_entity *ent);

/** Sets/resets the state of the address taken flag of an entity. */
void set_entity_address_taken(ir_entity *ent, ir_address_taken_state flag);

/** Return the name of the address_taken state. */
const char *get_address_taken_state_name(ir_address_taken_state state);

/**
 * Returns the debug information of an entity.
 *
 * @param ent The entity.
 */
dbg_info *get_entity_dbg_info(const ir_entity *ent);

/**
 * Sets the debug information of an entity.
 *
 * @param ent The entity.
 * @param db  The debug info.
 */
void set_entity_dbg_info(ir_entity *ent, dbg_info *db);

/* -- Representation of constant values of entities -- */
/**
 * Returns true if the the node is representable as code on
 * const_code_irg.
 *
 * @deprecated This function is not used by libFirm and stays here
 *             only as a helper for the old Jack frontend.
 */
int      is_irn_const_expression(ir_node *n);

/**
 * Copies a Firm subgraph that complies to the restrictions for
 * constant expressions to current_block in current_ir_graph.
 *
 * @param dbg  debug info for all newly created nodes
 * @param n    the node
 *
 * Set current_ir_graph to get_const_code_irg() to generate a constant
 * expression.
 */
ir_node *copy_const_value(dbg_info *dbg, ir_node *n);

/* Set has no effect for existent entities of type method. */
ir_node *get_atomic_ent_value(ir_entity *ent);
void     set_atomic_ent_value(ir_entity *ent, ir_node *val);

/** Creates a new compound graph path. */
compound_graph_path *new_compound_graph_path(ir_type *tp, int length);

/** Returns non-zero if an object is a compound graph path */
int     is_compound_graph_path(const void *thing);

/** Frees a graph path object */
void    free_compound_graph_path (compound_graph_path *gr);

/** Returns the length of a graph path */
int     get_compound_graph_path_length(const compound_graph_path *gr);

ir_entity *get_compound_graph_path_node(const compound_graph_path *gr, int pos);
void    set_compound_graph_path_node(compound_graph_path *gr, int pos, ir_entity *node);
int     get_compound_graph_path_array_index(const compound_graph_path *gr, int pos);
void    set_compound_graph_path_array_index(compound_graph_path *gr, int pos, int index);

/** Checks whether the path up to pos is correct. If the path contains a NULL,
 *  assumes the path is not complete and returns non-zero. */
int is_proper_compound_graph_path(compound_graph_path *gr, int pos);

/* A value of a compound entity is a pair of a value and the description of the
   corresponding access path to the member of the compound.  */
void     add_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path);
void     set_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path, int pos);
/** Returns the number of constant values needed to initialize the entity.
 *
 *  Asserts if the entity has variability_uninitialized.
 * */
int      get_compound_ent_n_values(ir_entity *ent);
/** Returns a constant value given the position. */
ir_node *get_compound_ent_value(ir_entity *ent, int pos);
/** Returns the access path for value at position pos. */
compound_graph_path *get_compound_ent_value_path(ir_entity *ent, int pos);
/** Returns a constant value given the access path.
 *  The path must contain array indices for all array element entities. */
ir_node *get_compound_ent_value_by_path(ir_entity *ent, compound_graph_path *path);

/** Removes all constant entries where the path ends at value_ent. Does not
   free the memory of the paths.  (The same path might be used for several
   constant entities. */
void     remove_compound_ent_value(ir_entity *ent, ir_entity *value_ent);

/* Some languages support only trivial access paths, i.e., the member is a
   direct, atomic member of the constant entities type. In this case the
   corresponding entity can be accessed directly.  The following functions
   allow direct access. */

/** Generates a Path with length 1.
    Beware: Has a bad runtime for array elements (O(|array|) and should be
    avoided there. Use add_compound_ent_value_w_path() instead and create
    the path manually. */
void     add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member);

/** Returns the last member in the path */
ir_entity  *get_compound_ent_value_member(ir_entity *ent, int pos);

/** Sets the path at pos 0 */
void     set_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member, int pos);

/** Initializes the entity ent which must be of a one dimensional
   array type with the values given in the values array.
   The array must have a lower and an upper bound.  Keeps the
   order of values. Does not test whether the number of values
   fits into the given array size.  Does not test whether the
   values have the proper mode for the array. */
void set_array_entity_values(ir_entity *ent, tarval **values, int num_vals);

/**
 * Return the offset in bits from the last byte address.
 *
 * This requires that the layout of all concerned types is fixed.
 *
 * @param ent Any entity of compound type with at least pos initialization values.
 * @param pos The position of the value for which the offset is requested.
 */
int get_compound_ent_value_offset_bit_remainder(ir_entity *ent, int pos);

/** Return the overall offset of value at position pos in bytes.
 *
 * This requires that the layout of all concerned types is fixed.
 * Asserts if bit offset is not byte aligned.
 *
 * @param ent Any entity of compound type with at least pos initialization values.
 * @param pos The position of the value for which the offset is requested.
 */
int  get_compound_ent_value_offset_bytes(ir_entity *ent, int pos);

/* --- Fields of entities with a class type as owner --- */
/* Overwrites is a field that specifies that an access to the overwritten
   entity in the supertype must use this entity.  It's a list as with
   multiple inheritance several entities can be overwritten.  This field
   is mostly useful for method entities.
   If a Sel node selects an entity that is overwritten by other entities it
   must return a pointer to the entity of the dynamic type of the pointer
   that is passed to it.  Lowering of the Sel node must assure this.
   Overwrittenby is the inverse of overwrites.  Both add routines add
   both relations, they only differ in the order of arguments. */
void    add_entity_overwrites   (ir_entity *ent, ir_entity *overwritten);
int     get_entity_n_overwrites (ir_entity *ent);
int     get_entity_overwrites_index(ir_entity *ent, ir_entity *overwritten);
ir_entity *get_entity_overwrites   (ir_entity *ent, int pos);
void    set_entity_overwrites   (ir_entity *ent, int pos, ir_entity *overwritten);
void    remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten);

void    add_entity_overwrittenby   (ir_entity *ent, ir_entity *overwrites);
int     get_entity_n_overwrittenby (ir_entity *ent);
int     get_entity_overwrittenby_index(ir_entity *ent, ir_entity *overwrites);
ir_entity *get_entity_overwrittenby   (ir_entity *ent, int pos);
void    set_entity_overwrittenby   (ir_entity *ent, int pos, ir_entity *overwrites);
void    remove_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites);

/**
 *   Checks whether a pointer points to an entity.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is an entity, else false
 */
int is_entity (const void *thing);

/** Returns true if the type of the entity is a primitive, pointer
 * enumeration or method type.
 *
 * @Note This is a different classification than from is_primitive_type().
 */
int is_atomic_entity(ir_entity *ent);
/** Returns true if the type of the entity is a class, structure,
   array or union type. */
int is_compound_entity(ir_entity *ent);
/** Returns true if the type of the entity is a Method type. */
int is_method_entity(ir_entity *ent);

/** Returns non-zero if ent1 and ent2 have are equal except for their owner.
   Two entities are equal if
    - they have the same type (the same C-struct)
    - ...?
*/
int equal_entity(ir_entity *ent1, ir_entity *ent2);

/** Outputs a unique number for this entity if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
long get_entity_nr(const ir_entity *ent);

/** Returns the entities visited count. */
unsigned long get_entity_visited(ir_entity *ent);

/** Sets the entities visited count. */
void        set_entity_visited(ir_entity *ent, unsigned long num);

/** Sets visited field in entity to entity_visited. */
void        mark_entity_visited(ir_entity *ent);

/** Returns true if this entity was visited. */
int        entity_visited(ir_entity *ent);

/** Returns true if this entity was not visited. */
int        entity_not_visited(ir_entity *ent);

/**
 * Returns the mask of the additional entity properties.
 * The properties are automatically inherited from the irg if available
 * or from the method type if they were not set using
 * set_entity_additional_properties() or
 * set_entity_additional_property().
 */
unsigned get_entity_additional_properties(ir_entity *ent);

/** Sets the mask of the additional graph properties. */
void set_entity_additional_properties(ir_entity *ent, unsigned property_mask);

/** Sets one additional graph property. */
void set_entity_additional_property(ir_entity *ent, mtp_additional_property flag);

/** Returns the class type that this type info entity represents or NULL
    if ent is no type info entity. */
ir_type *get_entity_repr_class(const ir_entity *ent);

/**
 * @page unknown_entity  The Unknown entity
 *
 *  This entity is an auxiliary entity dedicated to support analyses.
 *
 *  The unknown entity represents that there could be an entity, but it is not
 *  known.  This entity can be used to initialize fields before an analysis (not known
 *  yet) or to represent the top of a lattice (could not be determined).  There exists
 *  exactly one entity unknown. This entity has as owner and as type the unknown type. It is
 *  allocated when initializing the entity module.
 *
 *  The entity can take the role of any entity, also methods.  It returns default
 *  values in these cases.
 *
 *  The following values are set:
 *
 * - name          = "unknown_entity"
 * - ld_name       = "unknown_entity"
 * - owner         = unknown_type
 * - type          = unknown_type
 * - allocation    = allocation_automatic
 * - visibility    = visibility_external_allocated
 * - offset        = -1
 * - variability   = variability_uninitialized
 * - value         = SymConst(unknown_entity)
 * - values        = NULL
 * - val_paths     = NULL
 * - peculiarity   = peculiarity_existent
 * - volatility    = volatility_non_volatile
 * - stickyness    = stickyness_unsticky
 * - ld_name       = NULL
 * - overwrites    = NULL
 * - overwrittenby = NULL
 * - irg           = NULL
 * - link          = NULL
 */
/* A variable that contains the only unknown entity. */
extern ir_entity *unknown_entity;

/** Returns the @link unknown_entity unknown entity @endlink. */
ir_entity *get_unknown_entity(void);

/** Encodes how a pointer parameter is accessed. */
typedef enum acc_bits {
  ptr_access_none  = 0,                                 /**< no access */
  ptr_access_read  = 1,                                 /**< read access */
  ptr_access_write = 2,                                 /**< write access */
  ptr_access_rw    = ptr_access_read|ptr_access_write,  /**< read AND write access */
  ptr_access_store = 4,                                 /**< the pointer is stored */
  ptr_access_all   = ptr_access_rw|ptr_access_store     /**< all possible access */
} ptr_access_kind;

#define IS_READ(a)     ((a) & ptr_access_read)
#define IS_WRITTEN(a)  ((a) & ptr_access_write)
#define IS_STORED(a)   ((a) & ptr_access_store)

/**
 * Supported image sections.
 * Currently only methods can be placed in different sections.
 */
typedef enum {
  section_text,           /**< The code segment. This is the default for methods. */
  section_constructors    /**< The constructor section. */
} ir_img_section;

/** Returns the section of a method. */
ir_img_section get_method_img_section(const ir_entity *method);

/** Sets the section of a method. */
void set_method_img_section(ir_entity *method, ir_img_section section);




/**
 * @page tyop  type operations
 *  This module specifies the kinds of types available in firm.
 *
 *  They are called type opcodes. These include classes, structs, methods, unions,
 *  arrays, enumerations, pointers and primitive types.
 *  Special types with own opcodes are the id type, a type representing an unknown
 *  type and a type used to specify that something has no type.
 *
 *  @see type.h
 */

/**
 *  An enum for the type kinds.
 *  For each type kind exists a typecode to identify it.
 */
typedef enum {
	tpo_uninitialized = 0,   /* not a type opcode */
	tpo_class,               /**< A class type. */
	tpo_struct,              /**< A struct type. */
	tpo_method,              /**< A method type. */
	tpo_union,               /**< An union type. */
	tpo_array,               /**< An array type. */
	tpo_enumeration,         /**< An enumeration type. */
	tpo_pointer,             /**< A pointer type. */
	tpo_primitive,           /**< A primitive type. */
	tpo_id,                  /**< Special Id tag used for type replacement. */
	tpo_none,                /**< Special type for the None type. */
	tpo_unknown,             /**< Special code for the Unknown type. */
	tpo_max                  /* not a type opcode */
} tp_opcode;

/**
 * A structure containing information about a kind of type.
 * A structure containing information about a kind of type.  So far
 * this is only the kind name, an enum for case-switching and some
 * internal values.
 *
 * @see  get_tpop_name(), get_tpop_code(), get_tpop_ident()
 */
typedef struct tp_op tp_op;


/**
 * Returns the string for the type opcode.
 *
 * @param op  The type opcode to get the string from.
 * @return a string.  (@todo Null terminated?)
 */
const char *get_tpop_name (const tp_op *op);

/**
 * Returns an enum for the type opcode.
 *
 * @param op   The type opcode to get the enum from.
 * @return the enum.
 */
tp_opcode get_tpop_code (const tp_op *op);

/**
 * Returns the ident for the type opcode.
 *
 * @param op   The type opcode to get the ident from.
 * @return The ident.
 */
ident *get_tpop_ident (const tp_op *op);

/**
 * This type opcode marks that the corresponding type is a class type.
 *
 * Consequently the type refers to supertypes, subtypes and entities.
 * Entities can be any fields, but also methods.
 * @@@ value class or not???
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_class;
tp_op *get_tpop_class(void);

/**
 * This type opcode marks that the corresponding type is a compound type
 * as a struct in C.
 *
 * Consequently the type refers to a list of entities
 * which may not be methods (but pointers to methods).
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_struct;
tp_op *get_tpop_struct(void);

/**
 * This type opcode marks that the corresponding type is a method type.
 *
 * Consequently it refers to a list of arguments and results.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_method;
tp_op *get_tpop_method(void);

/**
 * This type opcode marks that the corresponding type is a union type.
 *
 * Consequently it refers to a list of unioned types.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_union;
tp_op *get_tpop_union(void);

/**
 * This type opcode marks that the corresponding type is an array type.
 *
 * Consequently it contains a list of dimensions (lower and upper bounds)
 * and an element type.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_array;
tp_op *get_tpop_array(void);

/**
 * This type opcode marks that the corresponding type is an enumeration type.
 *
 * Consequently it contains a list of idents for the enumeration identifiers
 * and a list of target values that are the constants used to implement
 * the enumerators.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_enumeration;
tp_op *get_tpop_enumeration(void);

/**
 * This type opcode marks that the corresponding type is a pointer type.
 *
 * It contains a reference to the type the pointer points to.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_pointer;
tp_op *get_tpop_pointer(void);

/**
 * This type opcode marks that the corresponding type is a primitive type.
 *
 * Primitive types are types that are directly mapped to target machine
 * modes.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_primitive;
tp_op *get_tpop_primitive(void);

/**
 * This type opcode is an auxiliary opcode dedicated to support transformations
 * of the type structure.
 *
 * If a type is changed to another type with another
 * opcode the new type will be allocated with new memory.  All nodes refering
 * to the old type need to be changed to refer to the new one.  This is simplified
 * by turning the old type into an id type that merely forwards to the new type
 * that now replaces the old one.
 * type_ids should never be visible out of the type module.  All access routines
 * should automatically check for type_id and eventually follow the forward in
 * type_id.  Two types are exchanged by a call to exchange_types.
 * If a type_id is visible externally report this as bug.  If it is assured that
 * this never happens this extern variable can be moved to tpop_t.h.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
extern tp_op *type_id;
tp_op *get_tpop_id(void);

/**
 * This type opcode is an auxiliary opcode dedicated to support type analyses.
 *
 * Types with this opcode represents that there is no type.
 * The type can be used to initialize fields of the type* that actually can not
 * contain a type or that are initialized for an analysis. There exists exactly
 * one type with this opcode.
 */
extern tp_op *tpop_none;
tp_op *get_tpop_none(void);

/**
 * This type opcode is an auxiliary opcode dedicated to support type analyses.
 *
 * Types with this opcode represents that there could be a type, but it is not
 * known.  This type can be used to initialize fields before an analysis (not known
 * yet) or to represent the top of a lattice (could not be determined).  There exists
 * exactly one type with this opcode.
 */
extern tp_op *tpop_unknown;
tp_op *get_tpop_unknown(void);

/* ----------------------------------------------------------------------- */
/* Classify pairs of types/entities in the inheritance relations.          */
/* ----------------------------------------------------------------------- */

/** Returns true if low is subclass of high.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
int is_SubClass_of(ir_type *low, ir_type *high);

/** Subclass check for pointers to classes.
 *
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and subclasses, returns true, else false.  Can also be called with
 *  two class types.  */
int is_SubClass_ptr_of(ir_type *low, ir_type *high);

/** Returns true if high is superclass of low.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
#define is_SuperClass_of(high, low) is_SubClass_of(low, high)

/** Superclass check for pointers to classes.
 *
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and superclasses, returns true, else false.  Can also be called with
 *  two class types.  */
#define is_SuperClass_ptr_of(low, high) is_SubClass_ptr_of(high, low)

/** Returns true if high is (transitive) overwritten by low.
 *
 *  Returns false if high == low. */
int is_overwritten_by(ir_entity *high, ir_entity *low);

/** Resolve polymorphism in the inheritance relation.
 *
 *  Returns the dynamically referenced entity if the static entity and the
 *  dynamic type are given.
 *  Searches downwards in overwritten tree. */
ir_entity *resolve_ent_polymorphy(ir_type *dynamic_class, ir_entity* static_ent);

/* ----------------------------------------------------------------------- */
/* Resolve implicit inheritance.                                           */
/* ----------------------------------------------------------------------- */

/** Default name mangling for inherited entities.
 *
 *  Returns an ident that consists of the name of type followed by an
 *  underscore and the name (not ld_name) of the entity. */
ident *default_mangle_inherited_name(ir_entity *ent, ir_type *clss);

/** Type of argument functions for inheritance resolver.
 *
 * @param ent     The entity in the super type that will be overwritten
 *                by the newly generated entity, for which this name is
 *                used.
 * @param clss    The class type in which the new entity will be placed.
 */
typedef ident *mangle_inherited_name_func(ir_entity *ent, ir_type *clss);

/** Resolve implicit inheritance.
 *
 *  Resolves the implicit inheritance supplied by firm.  Firm defines,
 *  that each entity that is not overwritten in a subclass is
 *  inherited to this subclass without change implicitly.  This
 *  function generates entities that explicitly represent this
 *  inheritance.  It generates for each entity overwriting entities in
 *  all subclasses of the owner of the entity, if the entity is not
 *  overwritten in that subclass.
 *
 *  The name of the new entity is generated with the function passed.
 *  If the function is NULL, the default_mangle_inherited_name() is
 *  used.
 *
 *  This function was moved here from firmlower 3/2005.
 */
void resolve_inheritance(mangle_inherited_name_func *mfunc);


/* ----------------------------------------------------------------------- */
/* The transitive closure of the subclass/superclass and                   */
/* overwrites/overwrittenby relation.                                      */
/*                                                                         */
/* A walk over the ir (O(#types+#entities)) computes the transitive        */
/* closure.  Adding a new type/entity or changing the basic relations in   */
/* some other way invalidates the transitive closure, i.e., it is not      */
/* updated by the basic functions.                                         */
/*                                                                         */
/* The transitive edges are held in a set, not in an array as the          */
/* underlying relation.                                                    */
/*                                                                         */
/* Do the sets contain the node itself?  I assume NOT!                     */
/* ----------------------------------------------------------------------- */

/** The state of the transitive closure.
 *
 *  @todo: we could manage the state for each relation separately.  Invalidating
 *  the entity relations does not mean invalidating the class relation. */
typedef enum {
	inh_transitive_closure_none,       /**<  Closure is not computed, can not be accessed. */
	inh_transitive_closure_valid,      /**<  Closure computed and valid. */
	inh_transitive_closure_invalid,    /**<  Closure invalid, but can be accessed. */
	inh_transitive_closure_max         /**<  Invalid value. */
} inh_transitive_closure_state;

void                         set_irp_inh_transitive_closure_state(inh_transitive_closure_state s);
void                         invalidate_irp_inh_transitive_closure_state(void);
inh_transitive_closure_state get_irp_inh_transitive_closure_state(void);


/** Compute transitive closure of the subclass/superclass and
 * overwrites/overwrittenby relation.
 *
 * This function walks over the ir (O(#types+#entities)) to compute the
 * transitive closure.    */
void compute_inh_transitive_closure(void);

/** Free memory occupied by the transitive closure information. */
void free_inh_transitive_closure(void);


/* - subtype ------------------------------------------------------------- */

/** Iterate over all transitive subtypes. */
ir_type *get_class_trans_subtype_first(ir_type *tp);
ir_type *get_class_trans_subtype_next (ir_type *tp);
int   is_class_trans_subtype (ir_type *tp, ir_type *subtp);

/* - supertype ----------------------------------------------------------- */

/** Iterate over all transitive supertypes. */
ir_type *get_class_trans_supertype_first(ir_type *tp);
ir_type *get_class_trans_supertype_next (ir_type *tp);

/* - overwrittenby ------------------------------------------------------- */

/** Iterate over all entities that transitive overwrite this entities. */
ir_entity *get_entity_trans_overwrittenby_first(ir_entity *ent);
ir_entity *get_entity_trans_overwrittenby_next (ir_entity *ent);

/* - overwrites ---------------------------------------------------------- */

/** Iterate over all transitive overwritten entities. */
ir_entity *get_entity_trans_overwrites_first(ir_entity *ent);
ir_entity *get_entity_trans_overwrites_next (ir_entity *ent);


/* ----------------------------------------------------------------------- */
/** The state of Cast operations that cast class types or pointers to class
 *  types.
 *
 * The state expresses, how far Cast operations conform with the class
 * hierarchy.
 *
 *   class A {}
 *   class B1 extends A {}
 *   class B2 extends A {}
 *   class C  extends B1 {}
 * normalized:  Cast operations conform with the inheritance relation.
 *   I.e., the type of the operand of a Cast is either a super= or a sub-
 *   type of the type casted to. Example: (A)((B2) (new C())).
 * transitive:  Cast operations conform with the transitive inheritance
 *   relation. Example: (A)(new C()).
 * any:  Cast operations do not conform with the transitive inheritance
 *   relation.  Example: (B2)(new B1())
 *
 *  @see: tropt.h
 */
/* ----------------------------------------------------------------------- */

/** Flags for class cast state.
 *
 * The state in irp is always smaller or equal to the state of any
 * irg.
 *
 * We rely on the ordering of the enum. */
typedef enum {
	ir_class_casts_any        = 0, /**< There are class casts that do not cast in conformance with
	                                    the class hierarchy.  @@@ So far this does not happen in Firm. */
	ir_class_casts_transitive = 1, /**< Class casts conform to transitive inheritance edges. Default. */
	ir_class_casts_normalized = 2, /**< Class casts conform to inheritance edges. */
	ir_class_casts_state_max
} ir_class_cast_state;
char *get_class_cast_state_string(ir_class_cast_state s);

void                set_irg_class_cast_state(ir_graph *irg, ir_class_cast_state s);
ir_class_cast_state get_irg_class_cast_state(ir_graph *irg);
void                set_irp_class_cast_state(ir_class_cast_state s);
ir_class_cast_state get_irp_class_cast_state(void);

/** Verify the class cast state of an irg.
 *
 *  Asserts if state is to high, outputs warning if state is to low
 *  and firm verbosity is set.
 */
void verify_irg_class_cast_state(ir_graph *irg);

/**
 * possible trvrfy() error codes
 */
enum trvrfy_error_codes {
	no_error = 0,                      /**< no error */
	error_ent_not_cont,                /**< overwritten entity not in superclass */
	error_null_mem,                    /**< compound contains NULL member */
	error_const_on_wrong_irg,          /**< constant placed on wrong IRG */
	error_existent_entity_without_irg, /**< Method entities with pecularity_exist must have an irg */
	error_wrong_ent_overwrites,        /**< number of entity overwrites exceeds number of class overwrites */
	error_inherited_ent_without_const, /**< inherited method entity not pointing to existent entity */
	error_glob_ent_allocation,         /**< wrong allocation of a global entity */
	error_ent_const_mode,              /**< Mode of constant in entity did not match entities type. */
	error_ent_wrong_owner              /**< Mode of constant in entity did not match entities type. */
};

/**
 * Checks a type.
 *
 * @return
 *  0   if no error encountered
 */
int check_type(ir_type *tp);

/**
 * Check an entity. Currently, we check only if initialized constants
 * are build on the const irg graph.
 *
 * @return
 *  0   if no error encountered
 *  != 0    a trvrfy_error_codes code
 */
int check_entity(ir_entity *ent);

/**
 * Walks the type information and performs a set of sanity checks.
 *
 * Currently, the following checks are executed:
 * - values of initialized entities must be allocated on the constant IRG
 * - class types: doesn't have NULL members
 * - class types: all overwrites are existent in the super type
 *
 * @return
 *    0 if graph is correct
 *    else error code.
 */
int tr_vrfy(void);

/**
 * If NDEBUG is defined performs nothing, else calls the tr_vrfy() function.
 */
#ifdef NDEBUG
#define TR_VRFY()	0
#else
#define TR_VRFY()	tr_vrfy()
#endif

/**
 *
 * @file typegmod.h
 *  This module supplies routines that support changing the type graph.
 */

/** Replaces one type by the other.
 *
 *  Old type is replaced by new_type.  All references to old_type
 *  now point to new_type.  The memory for the old type is destroyed,
 *  but still used.  Therefore it is not freed.
 *  All referenced to this memory will be lost after a certain while.
 *  An exception is the list of types in irp (irprog.h).
 *  In the future there might be a routine to recover the memory, but
 *  this will be at considerable runtime cost.
 *
 *  @param old_type  - The old type that shall be replaced by the new type.
 *  @param new_type  - The new type that will replace old_type.
 *
 */
void exchange_types(ir_type *old_type, ir_type *new_type);

/** Skip id types until a useful type is reached.
 *
 *  @param tp - A type of arbitrary kind.
 *
 *  @return
 *    tp if it is not an id type.
 *    If tp is an id type returns the real type it stands for.
 */
ir_type *skip_tid(ir_type *tp);

/**
 * @page type   representation of types
 *
 *  Datastructure to hold type information.
 *
 *  This module supplies a datastructure to represent all types
 *  known in the compiled program.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.  Finally it specifies some auxiliary types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  A type is described by a set of attributes.  Some of
 *  these attributes are common to all types, others depend on the
 *  kind of the type.
 *
 *  Types are different from the modes defined in irmode:  Types are
 *  on the level of the programming language, modes at the level of
 *  the target processor.
 *
 *  @see  tpop.h
 */

#include "typerep.h"

/** Frees all entities associated with a type.
 *  Does not free the array entity.
 *  Warning: ensure these entities are not referenced anywhere else.
 */
void        free_type_entities(ir_type *tp);

/** Frees the memory used by the type.
 *
 * Removes the type from the type list. Does not free the entities
 * belonging to the type, except for the array element entity.  Does
 * not free if tp is "none" or "unknown".  Frees entities in value
 * param subtypes of method types!!! Make sure these are not
 * referenced any more.  Further make sure there is no pointer type
 * that refers to this type.                           */
void        free_type(ir_type *tp);

const tp_op*get_type_tpop(const ir_type *tp);
ident*      get_type_tpop_nameid(const ir_type *tp);
const char* get_type_tpop_name(const ir_type *tp);
tp_opcode   get_type_tpop_code(const ir_type *tp);

ident*      get_type_ident(const ir_type *tp);
void        set_type_ident(ir_type *tp, ident* id);
const char* get_type_name(const ir_type *tp);

/** The visibility of a type.
 *
 *  The visibility of a type indicates, whether entities of this type
 *  are accessed or allocated in external code.
 *
 *  An entity of a type is allocated in external code, if the external
 *  code declares a variable of this type, or dynamically allocates
 *  an entity of this type.  If the external code declares a (compound)
 *  type, that contains entities of this type, the visibility also
 *  must be external_allocated.
 *
 *  The visibility must be higher than that of all entities, if the
 *  type is a compound.  Here it is questionable, what happens with
 *  static entities.  If these are accessed external by direct reference,
 *  (a static call to a method, that is also in the dispatch table)
 *  it should not affect the visibility of the type.
 *
 *
 * @@@ Do we need a visibility for types?
 * I change the layout of types radically when doing type splitting.
 * I need to know, which fields of classes are accessed in the RTS,
 * e.g., [_length.  I may not move [_length to the split part.
 * The layout though, is a property of the type.
 *
 * One could also think of changing the mode of a type ...
 *
 * But, we could also output macros to access the fields, e.g.,
 *  ACCESS_[_length (X)   X->length              // conventional
 *  ACCESS_[_length (X)   X->_split_ref->length  // with type splitting
 *
 * For now I implement this function, that returns the visibility
 * based on the visibility of the entities of a compound ...
 *
 * This function returns visibility_external_visible if one or more
 * entities of a compound type have visibility_external_visible.
 * Entities of types are never visibility_external_allocated (right?).
 * Else returns visibility_local.
 */
ir_visibility get_type_visibility(const ir_type *tp);
void          set_type_visibility(ir_type *tp, ir_visibility v);



/** The state of the type layout. */
typedef enum {
  layout_undefined,    /**< The layout of this type is not defined.
                            Address computation to access fields is not
                            possible, fields must be accessed by Sel
                            nodes.  Enumeration constants might be undefined.
                            This is the default value except for
                            pointer, primitive and method types. */
  layout_fixed         /**< The layout is fixed, all component/member entities
                            have an offset assigned.  Size of the type is known.
                            Arrays can be accessed by explicit address
                            computation.  Enumeration constants must be defined.
                            Default for pointer, primitive and method types. */
} type_state;

/** Returns a human readable string for the enum entry. */
const char *get_type_state_name(type_state s);

/** Returns the type layout state of a type. */
type_state  get_type_state(const ir_type *tp);

/** Sets the type layout state of a type.
 *
 * For primitives, pointer and method types the layout is always fixed.
 * This call is legal but has no effect.
 */
void        set_type_state(ir_type *tp, type_state state);

/** Returns the mode of a type.
 *
 * Returns NULL for all non atomic types.
 */
ir_mode*    get_type_mode(const ir_type *tp);

/** Sets the mode of a type.
 *
 * Only has an effect on primitive, enumeration and pointer types.
 */
void        set_type_mode(ir_type *tp, ir_mode* m);

/** Returns the size of a type in bytes, returns -1 if the size is NOT
 *  a byte size, i.e. not dividable by 8. */
int         get_type_size_bytes(const ir_type *tp);

/** Returns the size of a type in bits. */
int         get_type_size_bits(const ir_type *tp);

/** Sets the size of a type in bytes.
 *
 * For primitive, enumeration, pointer and method types the size
 * is always fixed. This call is legal but has no effect.
 */
void        set_type_size_bytes(ir_type *tp, int size);

/** Sets the size of a type in bits.
 *
 * For primitive, enumeration, pointer and method types the size
 * is always fixed. This call is legal but has no effect.
 */
void        set_type_size_bits(ir_type *tp, int size);

/** Returns the alignment of a type in bytes.
 *
 *  Returns -1 if the alignment is NOT
 *  a byte size, i.e. not dividable by 8. Calls get_type_alignment_bits(). */
int         get_type_alignment_bytes(ir_type *tp);

/** Returns the alignment of a type in bits.
 *
 *  If the alignment of a type is
 *  not set, it is calculated here according to the following rules:
 *  -#.) if a type has a mode, the alignment is the mode size.
 *  -#.) compound types have the alignment of there biggest member.
 *  -#.) array types have the alignment of there element type.
 *  -#.) method types return 0 here.
 *  -#.) all other types return 8 here (i.e. aligned at byte).
 */
int         get_type_alignment_bits(ir_type *tp);

/** Sets the alignment of a type in bytes. */
void        set_type_alignment_bytes(ir_type *tp, int size);

/** Sets the alignment of a type in bits.
 *
 * For method types the alignment is always fixed.
 * This call is legal but has no effect.
 */
void        set_type_alignment_bits(ir_type *tp, int size);

/** Returns the visited count of a type. */
unsigned long get_type_visited(const ir_type *tp);
/** Sets the visited count of a type to num. */
void          set_type_visited(ir_type *tp, unsigned long num);
/** Sets visited field in type to type_visited. */
void          mark_type_visited(ir_type *tp);
/** Returns non-zero if the type is already visited */
int           type_visited(const ir_type *tp);
/** Returns non-zero if the type is not yet visited */
int           type_not_visited(const ir_type *tp);

/** Returns the associated link field of a type. */
void*         get_type_link(const ir_type *tp);
/** Sets the associated link field of a type. */
void          set_type_link(ir_type *tp, void *l);

/**
 * Visited flag to traverse the type information.
 *
 * Increase this flag by one before traversing the type information
 * using inc_master_type_visited().
 * Mark type nodes as visited by mark_type_visited(ir_type).
 * Check whether node was already visited by type_visited(ir_type)
 * and type_not_visited(ir_type).
 * Or use the function to walk all types.
 *
 * @see  typewalk
 */
void          set_master_type_visited(unsigned long val);
unsigned long get_master_type_visited(void);
void          inc_master_type_visited(void);

/**
 * Sets the debug information of a type.
 *
 * @param tp  The type.
 * @param db  The debug info.
 */
void set_type_dbg_info(ir_type *tp, dbg_info *db);

/**
 * Returns the debug information of a type.
 *
 * @param tp  The type.
 */
dbg_info *get_type_dbg_info(const ir_type *tp);

/**
 * Checks whether a pointer points to a type.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a type, else false
 */
int is_type(const void *thing);

/**
 *   Checks whether two types are structurally equal.
 *
 *   @param typ1  the first type
 *   @param typ2  the second type
 *
 *   @return
 *    true if the types are equal, else false.
 *
 *   Types are equal if :
 *    - they are the same type kind
 *    - they have the same name
 *    - they have the same mode (if applicable)
 *    - they have the same type_state and, ev., the same size
 *    - they are class types and have:
 *      - the same members (see same_entity in entity.h)
 *      - the same supertypes -- the C-pointers are compared --> no recursive call.
 *      - the same number of subtypes.  Subtypes are not compared,
 *        as this could cause a cyclic test.
 *      - the same peculiarity
 *    - they are structure types and have the same members
 *    - they are method types and have
 *      - the same parameter types
 *      - the same result types
 *    - they are union types and have the same members
 *    - they are array types and have
 *      - the same number of dimensions
 *      - the same dimension bounds
 *      - the same dimension order
 *      - the same element type
 *    - they are enumeration types and have the same enumerator names
 *    - they are pointer types and have the identical points_to type
 *      (i.e., the same C-struct to represent the type, type_id is skipped.
 *       This is to avoid endless recursions; with pointer types cyclic
 *       type graphs are possible.)
 */
int equal_type(ir_type *typ1, ir_type *typ2);

/**
 *   Checks whether two types are structural comparable.
 *
 *   @param st pointer type
 *   @param lt pointer type
 *
 *   @return
 *    true if type st is smaller than type lt, i.e. whenever
 *    lt is expected a st can be used.
 *    This is true if
 *    - they are the same type kind
 *    - mode(st) < mode (lt)  (if applicable)
 *    - they are class types and st is (transitive) subtype of lt,
 *    - they are structure types and
 *       - the members of st have exactly one counterpart in lt with the same name,
 *       - the counterpart has a bigger type.
 *    - they are method types and have
 *      - the same number of parameter and result types,
 *      - the parameter types of st are smaller than those of lt,
 *      - the result types of st are smaller than those of lt
 *    - they are union types and have the members of st have exactly one
 *      @return counterpart in lt and the type is smaller
 *    - they are array types and have
 *      - the same number of dimensions
 *      - all bounds of lt are bound of st
 *      - the same dimension order
 *      - the same element type
 *      @return or
 *      - the element type of st is smaller than that of lt
 *      - the element types have the same size and fixed layout.
 *    - they are enumeration types and have the same enumerator names
 *    - they are pointer types and have the points_to type of st is
 *      @return smaller than the points_to type of lt.
 *
 */
int smaller_type(ir_type *st, ir_type *lt);

/**
 *  @page class_type    Representation of a class type
 *
 *  If the type opcode is set to type_class the type represents class
 *  types.  A list of fields and methods is associated with a class.
 *  Further a class can inherit from and bequest to other classes.
 *
 *  The following attributes are private to this type kind:
 *  - member:     All entities belonging to this class.  This are method entities
 *                which have type_method or fields that can have any of the
 *                following type kinds: type_class, type_struct, type_union,
 *                type_array, type_enumeration, type_pointer, type_primitive.
 *
 *  The following two are dynamic lists that can be grown with an "add_" function,
 *  but not shrinked:
 *
 *  - subtypes:    A list of direct subclasses.
 *
 *  - supertypes:  A list of direct superclasses.
 *
 *  - peculiarity: The peculiarity of this class.  If the class is of peculiarity
 *                 "description" it only is a description of requirements to a class,
 *                 as, e.g., a Java interface.  The class will never be allocated.
 *                 Peculiarity inherited is only possible for entities.  An entity
 *                 is of peculiarity inherited if the compiler generated the entity
 *                 to explicitly resolve inheritance.  An inherited method entity has
 *                 no value for irg.
 *                 Values: description, existent, inherited.  Default: existent.
 *
 *  - type_info:   An entity representing the type information of this class.
 *                 This entity can be of arbitrari type, Firm did not use it yet.
 *                 It allows to express the coupling of a type with an entity
 *                 representing this type.  This information is useful for lowering
 *                 of InstOf and TypeChk nodes.  Default: NULL
 *
 *  - vtable_size: The size of this class virtual function table.
 *                 Default:  0
 *
 *  - final:       A final class is always a leaf in the class hierarchy.  Final
 *                 classes cannot be super classes of other ones.  As this information
 *                 can only be computed in whole world compilations, we allow to
 *                 set this flag.  It is used in optimizations if get_opt_closed_world()
 *                 is false.  Default:  false
 *
 *  - interface:   The class represents an interface.  This flag can be set to distinguish
 *                 between interfaces, abstract classes and other classes that all may
 *                 have the peculiarity peculiarity_description.  Depending on this flag
 *                 the lowering might do different actions.  Default:  false
 *
 *  - abstract :   The class represents an abstract class.  This flag can be set to distinguish
 *                 between interfaces, abstract classes and other classes that all may
 *                 have the peculiarity peculiarity_description.  Depending on this flag
 *                 the lowering might do different actions.  Default:  false
 */

/** Creates a new class type. */
ir_type *new_type_class (ident *name);

/** Creates a new class type with debug information. */
ir_type *new_d_type_class (ident *name, dbg_info *db);

/* --- manipulate private fields of class type  --- */

/** Adds the entity as member of the class.  */
void add_class_member   (ir_type *clss, ir_entity *member);

/** Returns the number of members of this class. */
int get_class_n_members (const ir_type *clss);

/** Returns the member at position pos, 0 <= pos < n_member */
ir_entity *get_class_member   (const ir_type *clss, int pos);

/** Returns index of mem in clss, -1 if not contained. */
int get_class_member_index(const ir_type *clss, ir_entity *mem);

/** Finds the member with name 'name'. If several members with the same
 *  name returns one of them.  Returns NULL if no member found. */
ir_entity *get_class_member_by_name(ir_type *clss, ident *name);

/** Overwrites the member at position pos, 0 <= pos < n_member with
 *  the passed entity. */
void set_class_member   (ir_type *clss, ir_entity *member, int pos);

/** Replaces complete member list in class type by the list passed.
 *
 *  Copies the list passed. This function is necessary to reduce the number of members.
 *  members is an array of entities, num the size of this array.  Sets all
 *  owners of the members passed to clss. */
void set_class_members  (ir_type *clss, ir_entity *members[], int arity);

/** Finds member in the list of members and removes it.
 *
 *  Shrinks the member list, so iterate from the end!!!
 *  Does not deallocate the entity.  */
void remove_class_member(ir_type *clss, ir_entity *member);


/** Adds subtype as subtype to clss.
 *
 *  Checks whether clss is a supertype of subtype.  If not
 *  adds also clss as supertype to subtype.  */
void    add_class_subtype   (ir_type *clss, ir_type *subtype);

/** Returns the number of subtypes */
int     get_class_n_subtypes (const ir_type *clss);

/** Gets the subtype at position pos, 0 <= pos < n_subtype. */
ir_type *get_class_subtype   (ir_type *clss, int pos);

/** Returns the index to access subclass as subtype of class.
 *
 *  If subclass is no direct subtype of class returns -1.
 */
int get_class_subtype_index(ir_type *clss, const ir_type *subclass);

/** Sets the subtype at position pos, 0 <= pos < n_subtype.
 *
 *  Does not set the corresponding supertype relation for subtype: this might
 *  be a different position! */
void    set_class_subtype   (ir_type *clss, ir_type *subtype, int pos);

/** Finds subtype in the list of subtypes and removes it  */
void    remove_class_subtype(ir_type *clss, ir_type *subtype);

/* Convenience macros */
#define add_class_derived_type(clss, drvtype)       add_class_subtype(clss, drvtype)
#define get_class_n_derived_types(clss)             get_class_n_subtypes(clss)
#define get_class_derived_type(clss, pos)           get_class_subtype(clss, pos)
#define get_class_derived_type_index(clss, drvtype) get_class_subtype_index(clss, drvtype)
#define set_class_derived_type(clss, drvtype, pos)  set_class_subtype(clss, drvtype, pos)
#define remove_class_derived_type(clss, drvtype)    remove_class_subtype(clss, drvtype)

/** Adds supertype as supertype to class.
 *
 *  Checks whether clss is a subtype of supertype.  If not
 *  adds also clss as subtype to supertype.  */
void    add_class_supertype   (ir_type *clss, ir_type *supertype);

/** Returns the number of supertypes */
int     get_class_n_supertypes (const ir_type *clss);

/** Returns the index to access superclass as supertype of class.
 *
 *  If superclass is no direct supertype of class returns -1.
 */
int     get_class_supertype_index(ir_type *clss, ir_type *super_clss);

/** Gets the supertype at position pos,  0 <= pos < n_supertype. */
ir_type *get_class_supertype   (ir_type *clss, int pos);

/** Sets the supertype at position pos, 0 <= pos < n_supertype.
 *
 *  Does not set the corresponding subtype relation for supertype: this might
 *  be at a different position! */
void    set_class_supertype   (ir_type *clss, ir_type *supertype, int pos);

/** Finds supertype in the list of supertypes and removes it */
void    remove_class_supertype(ir_type *clss, ir_type *supertype);

/** Convenience macro */
#define add_class_base_type(clss, basetype)  add_class_supertype(clss, basetype)
#define get_class_n_base_types(clss)  get_class_n_supertypes(clss)
#define get_class_base_type_index(clss, base_clss) get_class_supertype_index(clss, base_clss)
#define get_class_base_type(clss, pos)  get_class_supertype(clss, pos)
#define set_class_base_type(clss, basetype, pos) set_class_supertype(clss, basetype, pos)
#define remove_class_base_type(clss, basetype)  remove_class_supertype(clss, basetype)

/** Convenience macro */
#define add_class_base_type(clss, basetype)        add_class_supertype(clss, basetype)
#define get_class_n_base_types(clss)               get_class_n_supertypes(clss)
#define get_class_base_type_index(clss, base_clss) get_class_supertype_index(clss, base_clss)
#define get_class_base_type(clss, pos)             get_class_supertype(clss, pos)
#define set_class_base_type(clss, basetype, pos)   set_class_supertype(clss, basetype, pos)
#define remove_class_base_type(clss, basetype)     remove_class_supertype(clss, basetype)

/** Returns a human readable string for a peculiarity. */
const char *get_peculiarity_name(ir_peculiarity p);

/** Returns the peculiarity of the class. */
ir_peculiarity get_class_peculiarity (const ir_type *clss);
/** Sets the peculiarity of the class. */
void           set_class_peculiarity (ir_type *clss, ir_peculiarity pec);

/** Returns the type info entity of a class. */
ir_entity *get_class_type_info(const ir_type *clss);

/** Set a type info entity for the class. */
void set_class_type_info(ir_type *clss, ir_entity *ent);

/** Returns the size of the virtual function table. */
unsigned get_class_vtable_size(const ir_type *clss);

/** Sets a new size of the virtual function table. */
void set_class_vtable_size(ir_type *clss, unsigned size);

/** Returns non-zero if a class is final. */
int is_class_final(const ir_type *clss);

/** Sets the class final flag. */
void set_class_final(ir_type *clss, int flag);

/** Return non-zero if a class is an interface */
int is_class_interface(const ir_type *clss);

/** Sets the class interface flag. */
void set_class_interface(ir_type *clss, int flag);

/** Return non-zero if a class is an abstract class. */
int is_class_abstract(const ir_type *clss);

/** Sets the class abstract flag. */
void set_class_abstract(ir_type *clss, int flag);

/** Set and get a class' dfn --
   @todo This is an undocumented field, subject to change! */
void set_class_dfn (ir_type *clss, int dfn);
int  get_class_dfn (const ir_type *clss);

/** Returns true if a type is a class type. */
int is_Class_type(const ir_type *clss);

/**
 *  @page struct_type   Representation of a struct type
 *
 *  A struct type represents aggregate types that consist of a list
 *  of fields.
 *
 *  The following attributes are private to this type kind:
 *  - member:  All entities belonging to this class.  This are the fields
 *             that can have any of the following types:  type_class,
 *             type_struct, type_union, type_array, type_enumeration,
 *             type_pointer, type_primitive.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 */
/** Creates a new type struct */
ir_type *new_type_struct (ident *name);
/** Creates a new type struct with debug information. */
ir_type *new_d_type_struct (ident *name, dbg_info* db);

/* --- manipulate private fields of struct --- */

/** Adds the entity as member of the struct.  */
void add_struct_member   (ir_type *strct, ir_entity *member);

/** Returns the number of members of this struct. */
int get_struct_n_members (const ir_type *strct);

/** Returns the member at position pos, 0 <= pos < n_member */
ir_entity *get_struct_member   (const ir_type *strct, int pos);

/** Returns index of member in strct, -1 if not contained. */
int get_struct_member_index(const ir_type *strct, ir_entity *member);

/** Overwrites the member at position pos, 0 <= pos < n_member with
   the passed entity. */
void set_struct_member   (ir_type *strct, int pos, ir_entity *member);

/** Finds member in the list of members and removes it. */
void remove_struct_member (ir_type *strct, ir_entity *member);

/** Returns true if a type is a struct type. */
int is_Struct_type(const ir_type *strct);

/**
 * @page method_type    Representation of a method type
 *
 * A method type represents a method, function or procedure type.
 * It contains a list of the parameter and result types, as these
 * are part of the type description.  These lists should not
 * be changed by a optimization, as a change creates a new method
 * type.  Therefore optimizations should allocated new method types.
 * The set_ routines are only for construction by a frontend.
 *
 * - n_params:   Number of parameters to the procedure.
 *               A procedure in FIRM has only call by value parameters.
 *
 * - param_type: A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth element
 *               in the parameter tuple that is a result of the start node.
 *               (See ircons.h for more information.)
 *
 * - value_param_ents
 *               A list of entities (whose owner is a struct private to the
 *               method type) that represent parameters passed by value.
 *
 * - n_res:      The number of results of the method.  In general, procedures
 *               have zero results, functions one.
 *
 * - res_type:   A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth input to
 *               Return nodes.  (See ircons.h for more information.)
 *
 * - value_res_ents
 *               A list of entities (whose owner is a struct private to the
 *               method type) that represent results passed by value.
 */

/* These macros define the suffixes for the types and entities used
   to represent value parameters / results. */
#define VALUE_PARAMS_SUFFIX  "val_param"
#define VALUE_RESS_SUFFIX    "val_res"

/** Create a new method type.
 *
 * @param name      the name (ident) of this type
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
ir_type *new_type_method (ident *name, int n_param, int n_res);

/** Create a new method type with debug information.
 *
 * @param name      the name (ident) of this type
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 * @param db        user defined debug information
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
ir_type *new_d_type_method (ident *name, int n_param, int n_res, dbg_info* db);

/* -- manipulate private fields of method. -- */

/** Returns the number of parameters of this method. */
int   get_method_n_params  (const ir_type *method);

/** Returns the type of the parameter at position pos of a method. */
ir_type *get_method_param_type(ir_type *method, int pos);
/** Sets the type of the parameter at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_param_type(ir_type *method, int pos, ir_type *tp);
/** Returns an entity that represents the copied value argument.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
ir_entity *get_method_value_param_ent(ir_type *method, int pos);
/**
 * Returns a type that represents the copied value arguments if one
 * was allocated, else NULL.
 */
ir_type *get_method_value_param_type(const ir_type *method);
/** Returns an ident representing the parameters name. Returns NULL if not set.
    For debug support only. */
ident *get_method_param_ident(ir_type *method, int pos);
/** Returns a string representing the parameters name. Returns NULL if not set.
    For debug support only. */
const char *get_method_param_name(ir_type *method, int pos);
/** Sets an ident representing the parameters name. For debug support only. */
void set_method_param_ident(ir_type *method, int pos, ident *id);

/** Returns the number of results of a method type. */
int   get_method_n_ress   (const ir_type *method);
/** Returns the return type of a method type at position pos. */
ir_type *get_method_res_type(ir_type *method, int pos);
/** Sets the type of the result at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_res_type(ir_type *method, int pos, ir_type *tp);
/** Returns an entity that represents the copied value result.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
ir_entity *get_method_value_res_ent(ir_type *method, int pos);

/**
 * Returns a type that represents the copied value results.
 */
ir_type *get_method_value_res_type(const ir_type *method);

/**
 * This enum flags the variadicity of methods (methods with a
 * variable amount of arguments (e.g. C's printf). Default is
 * non_variadic.
 */
typedef enum variadicity {
  variadicity_non_variadic, /**< non variadic */
  variadicity_variadic      /**< variadic */
} variadicity;

/** Returns the null-terminated name of this variadicity. */
const char *get_variadicity_name(variadicity vari);

/** Returns the variadicity of a method. */
variadicity get_method_variadicity(const ir_type *method);

/** Sets the variadicity of a method. */
void set_method_variadicity(ir_type *method, variadicity vari);

/**
 * Returns the first variadic parameter index of a type.
 * If this index was NOT set, the index of the last parameter
 * of the method type plus one is returned for variadic functions.
 * Non-variadic function types always return -1 here.
 */
int get_method_first_variadic_param_index(const ir_type *method);

/**
 * Sets the first variadic parameter index. This allows to specify
 * a complete call type (containing the type of all parameters)
 * but still have the knowledge, which parameter must be passed as
 * variadic one.
 */
void set_method_first_variadic_param_index(ir_type *method, int index);

/** Returns the mask of the additional graph properties. */
unsigned get_method_additional_properties(const ir_type *method);

/** Sets the mask of the additional graph properties. */
void set_method_additional_properties(ir_type *method, unsigned property_mask);

/** Sets one additional graph property. */
void set_method_additional_property(ir_type *method, mtp_additional_property flag);

/**
 * Calling conventions: lower 24 bits are the number of register parameters,
 * upper 8 encode the calling conventions.
 */
typedef enum {
  cc_reg_param        = 0x01000000, /**< Transmit parameters in registers, else the stack is used.
                                         This flag may be set as default on some architectures. */
  cc_last_on_top      = 0x02000000, /**< The last non-register parameter is transmitted on top of
                                         the stack. This is equivalent to the pascal
                                         calling convention. If this flag is not set, the first
                                         non-register parameter is used (stdcall or cdecl
                                         calling convention) */
  cc_callee_clear_stk = 0x04000000, /**< The callee clears the stack. This forbids variadic
                                         function calls (stdcall). */
  cc_this_call        = 0x08000000, /**< The first parameter is a this pointer and is transmitted
                                         in a special way. */

  cc_bits             = (0xFF << 24)  /**< the calling convention bits */
} calling_convention;

/* some often used cases: made as defines because firmjni cannot handle two
   equal enum values. */

/** cdecl calling convention */
#define cc_cdecl_set    (0)
/** stdcall calling convention */
#define cc_stdcall_set  cc_callee_clear_stk
/** fastcall calling convention */
#define cc_fastcall_set (cc_reg_param|cc_callee_clear_stk)

/** Returns the default calling convention for method types. */
unsigned get_default_cc_mask(void);

/**
 * check for the CDECL calling convention
 */
#define IS_CDECL(cc_mask)     (((cc_mask) & cc_bits) == cc_cdecl_set)

/**
 * check for the STDCALL calling convention
 */
#define IS_STDCALL(cc_mask)   (((cc_mask) & cc_bits) == cc_stdcall_set)

/**
 * check for the FASTCALL calling convention
 */
#define IS_FASTCALL(cc_mask)  (((cc_mask) & cc_bits) == cc_fastcall_set)

/**
 * Sets the CDECL convention bits.
 */
#define SET_CDECL(cc_mask)    (((cc_mask) & ~cc_bits) | cc_cdecl_set)

/**
 * Set. the STDCALL convention bits.
 */
#define SET_STDCALL(cc_mask)  (((cc_mask) & ~cc_bits) | cc_stdcall_set)

/**
 * Sets the FASTCALL convention bits.
 */
#define SET_FASTCALL(cc_mask) (((cc_mask) & ~cc_bits) | cc_fastcall_set)

/** Returns the calling convention of an entities graph. */
unsigned get_method_calling_convention(const ir_type *method);

/** Sets the calling convention of an entities graph. */
void set_method_calling_convention(ir_type *method, unsigned cc_mask);

/** Returns the number of registers parameters, 0 means default. */
unsigned get_method_n_regparams(ir_type *method);

/** Sets the number of registers parameters, 0 means default. */
void set_method_n_regparams(ir_type *method, unsigned n_regs);

/** Returns true if a type is a method type. */
int   is_Method_type     (const ir_type *method);

/**
 *   @page union_type   Representation of a union (variant) type.
 *
 *   The union type represents union types.  Note that this representation
 *   resembles the C union type.  For tagged variant types like in Pascal or Modula
 *   a combination of a struct and a union type must be used.
 *
 *   - n_types:     Number of unioned types.
 *   - members:     Entities for unioned types.  Fixed length array.
 *                  This is a dynamic list that can be grown with an "add_" function,
 *                  but not shrinked.
 */
/** Creates a new type union. */
ir_type   *new_type_union (ident *name);

/** Creates a new type union with debug information. */
ir_type   *new_d_type_union (ident *name, dbg_info* db);

/* --- manipulate private fields of struct --- */

/** Returns the number of unioned types of this union */
int     get_union_n_members      (const ir_type *uni);

/** Adds a new entity to a union type */
void    add_union_member (ir_type *uni, ir_entity *member);

/** Returns the entity at position pos of a union */
ir_entity *get_union_member (const ir_type *uni, int pos);

/** Returns index of member in uni, -1 if not contained. */
int     get_union_member_index(const ir_type *uni, ir_entity *member);

/** Overwrites a entity at position pos in a union type. */
void    set_union_member (ir_type *uni, int pos, ir_entity *member);

/** Finds member in the list of members and removes it. */
void    remove_union_member (ir_type *uni, ir_entity *member);

/** Returns true if a type is a union type. */
int     is_Union_type          (const ir_type *uni);

/**
 * @page array_type Representation of an array type
 *
 * The array type represents rectangular multi dimensional arrays.
 * The constants representing the bounds must be allocated to
 * get_const_code_irg() by setting current_ir_graph accordingly.
 *
 * - n_dimensions:    Number of array dimensions.
 * - *lower_bound:    Lower bounds of dimensions.  Usually all 0.
 * - *upper_bound:    Upper bounds or dimensions.
 * - *element_type:   The type of the array elements.
 * - *element_ent:    An entity for the array elements to be used for
 *                      element selection with Sel.
 * @todo
 *   Do we need several entities?  One might want
 *   to select a dimension and not a single element in case of multi
 *   dimensional arrays.
 */

/** Create a new type array.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 */
ir_type *new_type_array         (ident *name, int n_dimensions,
                  ir_type *element_type);

/** Create a new type array with debug information.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 * A legal array type must have at least one dimension set.
 */
ir_type *new_d_type_array         (ident *name, int n_dimensions,
                  ir_type *element_type, dbg_info* db);

/* --- manipulate private fields of array type --- */

/** Returns the number of array dimensions of this type. */
int   get_array_n_dimensions (const ir_type *array);

/**
 * Allocates Const nodes of mode_Is for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
void  set_array_bounds_int   (ir_type *array, int dimension, int lower_bound,
                                                          int upper_bound);
/**
 * Sets the bounds for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
void  set_array_bounds       (ir_type *array, int dimension, ir_node *lower_bound,
                                                          ir_node *upper_bound);
/** Sets the lower bound for one array dimension, i.e. [lower,upper[ */
void  set_array_lower_bound  (ir_type *array, int dimension, ir_node *lower_bound);

/** Allocates Const nodes of mode_Is for the lower bound of an array
    dimension, i.e. [lower,upper[ */
void  set_array_lower_bound_int (ir_type *array, int dimension, int lower_bound);

/** Sets the upper bound for one array dimension, i.e. [lower,upper[ */
void  set_array_upper_bound  (ir_type *array, int dimension, ir_node *upper_bound);

/** Allocates Const nodes of mode_Is for the upper bound of an array
    dimension, i.e. [lower,upper[. */
void  set_array_upper_bound_int (ir_type *array, int dimension, int upper_bound);

/** Returns true if lower bound != Unknown. */
int       has_array_lower_bound     (const ir_type *array, int dimension);
/** Returns the lower bound of an array. */
ir_node * get_array_lower_bound     (const ir_type *array, int dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
long      get_array_lower_bound_int (const ir_type *array, int dimension);
/** returns true if lower bound != Unknown */
int       has_array_upper_bound     (const ir_type *array, int dimension);
/** Returns the upper bound of an array. */
ir_node * get_array_upper_bound     (const ir_type *array, int dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
long      get_array_upper_bound_int (const ir_type *array, int dimension);

/** Sets an array dimension to a specific order. */
void set_array_order (ir_type *array, int dimension, int order);

/** Returns the order of an array dimension. */
int  get_array_order (const ir_type *array, int dimension);

/** Find the array dimension that is placed at order order. */
int find_array_dimension(const ir_type *array, int order);

/** Sets the array element type. */
void  set_array_element_type (ir_type *array, ir_type* tp);

/** Gets the array element type. */
ir_type *get_array_element_type (ir_type *array);

/** Sets the array element entity. */
void  set_array_element_entity (ir_type *array, ir_entity *ent);

/** Get the array element entity. */
ir_entity *get_array_element_entity (const ir_type *array);

/** Returns true if a type is an array type. */
int    is_Array_type(const ir_type *array);

/**
 * @page enumeration_type   Representation of an enumeration type
 *
 * Enumeration types need not necessarily be represented explicitly
 * by Firm types, as the frontend can lower them to integer constants as
 * well.  For debugging purposes or similar tasks this information is useful.
 * The type state layout_fixed is set, if all enumeration constant have
 * there tarvals assigned.  Until then
 *
 * - *const:        The target values representing the constants used to
 *                  represent individual enumerations.
 */

/** Create a new type enumeration -- set the enumerators independently. */
ir_type   *new_type_enumeration(ident *name, int n_enums);

/** Create a new type enumeration with debug information -- set the enumerators independently. */
ir_type   *new_d_type_enumeration(ident *name, int n_enums, dbg_info *db);

/* --- manipulate fields of enumeration type. --- */

/** Set an enumeration constant to a enumeration type at a given position. */
void set_enumeration_const(ir_type *enumeration, int pos, ident *nameid, tarval *con);

/** Returns the number of enumeration values of this enumeration */
int     get_enumeration_n_enums(const ir_type *enumeration);

/** Returns the enumeration constant at a given position. */
ir_enum_const *get_enumeration_const(const ir_type *enumeration, int pos);

/** Returns the enumeration type owner of an enumeration constant. */
ir_type *get_enumeration_owner(const ir_enum_const *enum_cnst);

/** Sets the enumeration constant value. */
void    set_enumeration_value(ir_enum_const *enum_cnst, tarval *con);

/** Returns the enumeration constant value. */
tarval *get_enumeration_value(const ir_enum_const *enum_cnst);

/** Assign an ident to an enumeration constant. */
void    set_enumeration_nameid(ir_enum_const *enum_cnst, ident *id);

/** Returns the assigned ident of an enumeration constant. */
ident  *get_enumeration_nameid(const ir_enum_const *enum_cnst);

/** Returns the assigned name of an enumeration constant. */
const char *get_enumeration_name(const ir_enum_const *enum_cnst);

/** Returns true if a type is a enumeration type. */
int     is_Enumeration_type(const ir_type *enumeration);

/**
 * @page pointer_type   Representation of a pointer type
 *
 * The mode of the pointer type must be a reference mode.
 *
 * Pointer types:
 * - points_to:      The type of the entity this pointer points to.
 */

/** Creates a new type pointer. */
ir_type *new_type_pointer           (ident *name, ir_type *points_to, ir_mode *ptr_mode);

/** Creates a new type pointer with debug information. */
ir_type *new_d_type_pointer         (ident *name, ir_type *points_to, ir_mode *ptr_mode, dbg_info* db);

/* --- manipulate fields of type_pointer --- */

/** Sets the type to which a pointer points to. */
void  set_pointer_points_to_type (ir_type *pointer, ir_type *tp);

/** Returns the type to which a pointer points to. */
ir_type *get_pointer_points_to_type (ir_type *pointer);

/** Returns true if a type is a pointer type. */
int   is_Pointer_type            (const ir_type *pointer);

/** Returns the first pointer type that has as points_to tp.
 *  Not efficient: O(#types).
 *  If not found returns firm_unknown_type. */
ir_type *find_pointer_type_to_type (ir_type *tp);

/**
 * @page primitive_type Representation of a primitive type
 *
 * Primitive types are types that represent atomic data values that
 * map directly to modes.  They don't have private attributes.  The
 * important information they carry is held in the common mode field.
 */
/** Creates a new primitive type. */
ir_type *new_type_primitive(ident *name, ir_mode *mode);

/** Creates a new primitive type with debug information. */
ir_type *new_d_type_primitive(ident *name, ir_mode *mode, dbg_info* db);

/** Returns true if a type is a primitive type. */
int  is_Primitive_type(const ir_type *primitive);

/** Return the base type of a primitive (bitfield) type or NULL if none. */
ir_type *get_primitive_base_type(ir_type *tp);

/** Sets the base type of a primitive (bitfield) type. */
void set_primitive_base_type(ir_type *tp, ir_type *base_tp);

/**
 * @page none_type The None type
 *
 *  This type is an auxiliary type dedicated to support type analyses.
 *
 *  The none type represents that there is no type.  The type can be used to
 *  initialize fields of type* that actually can not contain a type or that
 *  are initialized for an analysis. There exists exactly one type none.
 *  This type is not on the type list in ir_prog. It is
 *  allocated when initializing the type module.
 *
 *  The following values are set:
 *    - mode:  mode_BAD
 *    - name:  "type_none"
 *    - state: layout_fixed
 *    - size:  0
 */
/** A variable that contains the only none type. */
extern ir_type *firm_none_type;

/** Returns the none type. */
ir_type *get_none_type(void);

/**
 * @page unknown_type  The Unknown type
 *
 *  This type is an auxiliary type dedicated to support type analyses.
 *
 *  The unknown type represents that there could be a type, but it is not
 *  known.  This type can be used to initialize fields before an analysis (not known
 *  yet) or to represent the top of a lattice (could not be determined).  There exists
 *  exactly one type unknown. This type is not on the type list in ir_prog.  It is
 *  allocated when initializing the type module.
 *
 *  The following values are set:
 *    - mode:  mode_ANY
 *    - name:  "type_unknown"
 *    - state: layout_fixed
 *    - size:  0
 */
/** A variable that contains the only unknown type. */
extern ir_type *firm_unknown_type;

/** Returns the unknown type. */
ir_type *get_unknown_type(void);


/**
 *  Checks whether a type is atomic.
 *  @param tp   any type
 *  @return true if type is primitive, pointer or enumeration
 */
int is_atomic_type(const ir_type *tp);

/* --- Support for compound types --- */

/**
 * Gets the number of elements in a Firm compound type.
 *
 * This is just a comfortability function, because structs and
 * classes can often be treated be the same code, but they have
 * different access functions to their members.
 *
 * @param tp  The type (must be struct, union or class).
 *
 * @return Number of members in the compound type.
 */
int get_compound_n_members(const ir_type *tp);

/**
 * Gets the member of a Firm compound type at position pos.
 *
 * @param tp  The type (must be struct, union or class).
 * @param pos The number of the member.
 *
 * @return The member entity at position pos.
 *
 * @see get_compound_n_members() for justification of existence.
 */
ir_entity *get_compound_member(const ir_type *tp, int pos);

/** Returns index of member in tp, -1 if not contained. */
int     get_compound_member_index(const ir_type *tp, ir_entity *member);

/**
 * Checks whether a type is a compound type.
 *
 * @param tp - any type
 *
 * @return true if the type is class, structure, union or array type.
 */
int is_compound_type(const ir_type *tp);

/**
 * Checks, whether a type is a frame type.
 */
int is_frame_type(const ir_type *tp);

/**
 * Checks, whether a type is a value parameter type.
 */
int is_value_param_type(const ir_type *tp);

/**
 * Checks, whether a type is a lowered type.
 */
int is_lowered_type(const ir_type *tp);

/**
 * Makes a new frame type. Frame types are class types,
 * so all class access functions work.
 * Frame types are not in the global list of types.
 */
ir_type *new_type_frame(ident *name);

/**
 * Sets a lowered type for a type. This sets both associations
 * and marks lowered_type as a "lowered" one.
 */
void set_lowered_type(ir_type *tp, ir_type *lowered_type);

/**
 * Gets the lowered/unlowered type of a type or NULL if this type
 * has no lowered/unlowered one.
 */
ir_type *get_associated_type(const ir_type *tp);

/**
 * Allocate an area of size bytes aligned at alignment
 * at the start or the end of a frame type.
 * The frame type must already have a fixed layout.
 *
 * @param frame_type a frame type
 * @param size       the size of the entity
 * @param alignment  the alignment of the entity
 * @param at_start   if true, put the area at the frame type's start, else at end
 *
 * @return the entity representing the area
 */
ir_entity *frame_alloc_area(ir_type *frame_type, int size, int alignment, int at_start);

/*-----------------------------------------------------------------*/
/** Debug aides                                                   **/
/*-----------------------------------------------------------------*/

/**
 *  Outputs a unique number for this type if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
long get_type_nr(const ir_type *tp);

/* ------------------------------------------------------------------------ */

/**  Type for a function that compares two types.
 *
 *   @param tp1  The first type to compare.
 *   @param tp2  The second type to compare.
 */
typedef int (compare_types_func_t)(const void *tp1, const void *tp2);

/** Compares two types by their name.
 *
 * Compares the opcode and the name of the types. If these are
 * equal returns 0, else non-zero.
 */
int compare_names (const void *tp1, const void *tp2);

/** Compares two types strict.
 *
 * returns 0 if tp1 == tp2, else non-zero
 */
int compare_strict (const void *tp1, const void *tp2);

/* ------------------------------------------------------------------------ */

/**  Type for a function that computes a hash value for a type.
 *
 *   @param tp The type to compute a hash for.
 */
typedef int (hash_types_func_t)(ir_type *tp);

/** Computes a hash value by the type name.
 *
 * Uses the name of the type and the type opcode to compute the hash.
 */
int firm_hash_name (ir_type *tp);

/* ------------------------------------------------------------------------ */

/** Finalize type construction.
 *
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.
 *
 * If it finds a type identical to tp it returns this type.  It turns
 * tp into the Id type.  All places formerly pointing to tp will now
 * point to the found type.  All entities of tp now refer to the found
 * type as their owner, but they are not a member of this type.  This
 * is invalid firm -- the entities must be replaced by entities of the
 * found type.  The Id type will be removed from the representation
 * automatically, but within an unknown time span.  It occupies memory
 * for this time.
 *
 * @param tp     The type to mature.
 */
ir_type *    mature_type(ir_type *tp);

/** Finalize type construction.
 *
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.
 *
 * If it finds a type identical to tp it returns this type.  It frees
 * type tp and all its entities.
 *
 * @param tp     The type to mature.
 */
ir_type *    mature_type_free(ir_type *tp);

/** Finalize type construction.
 *
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.
 *
 * If it find a type identical to tp it returns this type.  It frees
 * the entities and turns the type into an Id type.  All places
 * formerly pointing to tp will now point to the found type.  The Id
 * type will be removed from the representation automatically, but
 * within an unknown time span.  It occupies memory for this time.
 *
 * @param tp     The type to mature.
 */
ir_type *    mature_type_free_entities(ir_type *tp);

/**
 * The interface type for the type identify module;
 */
typedef struct _type_identify_if_t {
	compare_types_func_t *cmp;    /**< The function that should be used to compare two types.
	                                   If NULL, compare_strict() will be used. */
	hash_types_func_t *hash;      /**< The function that should be used to calculate a hash
	                                   value of a type. If NULL, hash_name() will be used. */
} type_identify_if_t;

/**
 * Initialise the type identifier module.
 *
 * @param ti_if    The interface functions for this module.
 *
 * If the parameter ti_if is NULL, the default functions compare_strict() and
 * firm_hash_name() will be used.
 */
void init_type_identify(type_identify_if_t *ti_if);

/** A data type to treat types and entities as the same. */
typedef union {
  ir_type   *typ;   /**< points to a type */
  ir_entity *ent;   /**< points to an entity */
} type_or_ent;

/** Type of argument functions for type walkers.
 *
 * @param tore    points to the visited type or entity
 * @param env     free environment pointer
 */
typedef void type_walk_func(type_or_ent *tore, void *env);

/**  The class walk function
 *
 * @param clss    points to the visited class
 * @param env     free environment pointer
 */
typedef void class_walk_func(ir_type *clss, void *env);

/** Touches every type and entity in unspecified order.  If new
 *  types/entities are created during the traversal these will
 *  be visited, too.
 *  Does not touch frame types or types for value params ... */
void type_walk(type_walk_func *pre, type_walk_func *post, void *env);

/** Walks over all type information reachable from an ir graph.
 *
 *  Walks over all type information reachable from irg, i.e., starts a
 *  type walk at the irgs entity, the irgs frame type and all types and
 *  entities that are attributes to firm nodes. */
void type_walk_irg(ir_graph *irg, type_walk_func *pre, type_walk_func *post,
                   void *env);

/**
    Touches every class in specified order:
    - first the super class
    - second the class itself
    - third the sub classes.  If new classes are created
    during the traversal these will be visited, too.

    @todo should be named class-walk

    @deprecated will be removed?
*/
void type_walk_super2sub(type_walk_func *pre, type_walk_func *post, void *env);

/** Walker for class types in inheritance order.
 *
 *  Touches every class in specified order:
 *   - first the super class
 *   - second the class itself
 *   If new classes are created during the traversal these
 *   will be visited, too.
 * Starts the walk at arbitrary classes.
 * Executes pre when first visiting a class.  Executes post after
 * visiting all superclasses.
 *
 * The arguments pre, post, env may be NULL. */
void type_walk_super(type_walk_func *pre, type_walk_func *post, void *env);

/** Same as type_walk_super2sub, but visits only class types.
   Executes pre for a class if all superclasses have been visited.
   Then iterates to subclasses.  Executes post after return from
   subclass.
   Does not visit global type, frame types.

   @bug ?? something is wrong with this.
*/
void class_walk_super2sub(class_walk_func *pre, class_walk_func *post,
                          void *env);

/**
 * the entity walk function.  A function type for entity walkers.
 *
 * @param ent     points to the visited entity
 * @param env     free environment pointer
 */
typedef void entity_walk_func(ir_entity *ent, void *env);

/**
 * Walks over all entities in the type.
 *
 * @param tp    the type
 * @param doit  the entity walker function
 * @param env   environment, will be passed to the walker function
 */
void walk_types_entities(ir_type *tp, entity_walk_func *doit, void *env);

#endif
