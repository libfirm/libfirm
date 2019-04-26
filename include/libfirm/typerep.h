/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief Declarations for functions and data structures to represent types
 */
#ifndef FIRM_TYPEREP_H
#define FIRM_TYPEREP_H

#include <stddef.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup ir_entity Entities
 *
 * An entity is the Firm representation of an object known to the program.
 * The primary concept of entities is to represent members of complex
 * types, i.e., fields and methods of classes.  As not all programming
 * languages model all variables and methods as members of some class,
 * the concept of entities is extended to cover also local and global
 * variables, and arbitrary procedures.
 *
 * An entity always specifies the type of the object it represents and
 * the type of the object it is a part of, the "owner" of the entity.
 * Originally this is the type of the class of which the entity is a
 * member.
 * The owner of local variables is the procedure they are defined in.
 * The owner of global variables and procedures visible in the whole
 * program is a universally defined class type "GlobalType".  The owner
 * of procedures defined in the scope of an other procedure is the
 * enclosing procedure.
 *
 * The type ir_entity is an abstract data type to represent program entities.
 * It contains the following attributes:
 *
 *   - owner:      A compound type this entity is a part of.
 *   - type:       The type of this entity.
 *   - name:       The string that represents this entity in the source program
 *   - linkage:    A flag indicating how the linker treats a symbol
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
 *   - visited:    visited flag.  Master flag is type_visited.
 *
 * These fields can only be accessed via access functions.
 *
 * Overwrites is a field that specifies that an access to the overwritten
 * entity in the supertype must use this entity.  It's a list as with
 * multiple inheritance several entities can be overwritten.  This field
 * is mostly useful for method entities.
 * If a Sel node selects an entity that is overwritten by other entities it
 * must return a pointer to the entity of the dynamic type of the pointer
 * that is passed to it.  Lowering of the Sel node must assure this.
 * Overwrittenby is the inverse of overwrites.  Both add_entity_overwrites()
 * and add_entity_overwrittenby() update both lists, they only differ in the
 * order of arguments.
 *
 * @{
 */

/**
 * Visibility classes for entities.
 */
typedef enum {
	/**
	 * The entity is visible across compilation units. It might have an
	 * initializer/graph.
	 * Note that entities with visibility_external without initializer are
	 * assumed to be defined in another compilation unit (not like C variables
	 * which are considered 'uninitialized' in this case).
	 */
	ir_visibility_external,
	/**
	 * The entity is visible across compilation units, but not across modules.
	 * This is equivalent to __attribute__((visibility("hidden"))) in gcc.
	 */
	ir_visibility_external_private,
	/**
	 * The entity is visible across compilation units and modules and cannot be
	 * overridden by other modules.
	 * Equivalent to __attribute__((visible("protected"))) in gcc.
	 */
	ir_visibility_external_protected,
	/**
	 * The entity is local to the compilation unit.
	 * A local entity is not visible in other compilation units.
	 * Note that the entity might still be accessed indirectly from other units
	 * through pointers.
	 */
	ir_visibility_local,
	/**
	 * This has the same semantic as visibility_local. Additionally the symbol
	 * is completely hidden from the linker (it only appears in the assembly).
	 * While visibility_local is probably still visible to debuggers,
	 * visibility_private symbols aren't and probably won't appear in the object
	 * files
	 */
	ir_visibility_private,
} ir_visibility;

/**
 * linkage specifies how the linker treats symbols
 */
typedef enum ir_linkage {
	IR_LINKAGE_DEFAULT         = 0,
	/**
	 * A symbol whose definition won't change in a program.
	 * Optimization might replace loads from this entity with constants.
	 * Also most linkers put such data in a constant segment which is shared
	 * between multiple running instances of the same application.
	 */
	IR_LINKAGE_CONSTANT        = 1 << 0,
	/**
	 * The entity is a weak symbol.
	 * A weak symbol is overridden by a non-weak symbol if one exists.
	 * Most linkers only support the IR_LINKAGE_WEAK in combination with
	 * IR_LINKAGE_MERGE.
	 */
	IR_LINKAGE_WEAK            = 1 << 1,
	/**
	 * The entity may be removed when it isn't referenced anywhere in the
	 * compilation unit even if it is exported (non-local).
	 * Typically used for C++ instantiated template code (,,COMDAT'' section).
	 */
	IR_LINKAGE_GARBAGE_COLLECT = 1 << 2,
	/**
	 * The linker will try to merge entities with same name from different
	 * compilation units. This is the usual behaviour for global variables
	 * without explicit initialisation in C (``COMMON'' symbols). It's also
	 * typically used in C++ for instantiated template code (,,COMDAT'' section)
	 */
	IR_LINKAGE_MERGE           = 1 << 3,
	/**
	 * Some entity uses are potentially hidden from the compiler.
	 * (For example because they happen in an asm("") statement. This flag
	 *  should be set for __attribute__((used)) in C code).
	 * Setting this flag prohibits that the compiler making assumptions about
	 * read/write behaviour to global variables or changing calling conventions
	 * from cdecl to fastcall.
	 */
	IR_LINKAGE_HIDDEN_USER     = 1 << 4,
	/**
	 * Do not generate code even if the entity has a graph or
	 * initialization data attached.  The graph/data is only used for
	 * inlining.  Otherwise the entity is regarded as a declaration of
	 * an externally defined entity.
	 * This linkage flag can be used to implement C99 "inline" or GNU89
	 * "extern inline".
	 */
	IR_LINKAGE_NO_CODEGEN      = 1 << 5,
	/**
	 * The entity does not need to have an address that is different from other
	 * entities. When this property is set the entity may be safely merged with
	 * entities with the same content.
	 */
	IR_LINKAGE_NO_IDENTITY     = 1 << 6,
} ir_linkage;
ENUM_BITSET(ir_linkage)

/**
 * Returns the visibility class of an entity
 */
FIRM_API ir_visibility get_entity_visibility(const ir_entity *entity);

/**
 * Sets visibility class of an entity
 */
FIRM_API void set_entity_visibility(ir_entity *entity, ir_visibility visibility);

/**
 * Returns 1 if the entity is visible outside the current compilation unit
 * or to unknown callers (like asm statements).
 * If invisible, the entity might still be accessible indirectly through pointers.
 * This is a convenience function and does the same as
 * get_entity_visibility(entity) != ir_visibility_local ||
 * (get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER)
 */
FIRM_API int entity_is_externally_visible(const ir_entity *entity);

/**
 * Returns 1 if the entity has a definition (initializer) in the current
 * compilation unit. Note that this function returns false if
 * IR_LINKAGE_NO_CODEGEN is set even if a graph is present.
 */
FIRM_API int entity_has_definition(const ir_entity *entity);

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * Entity is automatic_allocated and uninitialized except if the type
 * is type_method, then it is static_allocated and constant.
 * Visibility is external, offset -1, and it is not volatile.
 */
FIRM_API ir_entity *new_entity(ir_type *owner, ident *name, ir_type *tp);

/**
 * Creates a new global entity.
 */
FIRM_API ir_entity *new_global_entity(ir_type *segment, ident *ld_name,
                                      ir_type *type, ir_visibility visibility,
                                      ir_linkage linkage);

/**
 * Creates a new entity corresponding to a function parameter.
 * The owner must be an ir_graph's frame_type.
 * @sa get_irg_frame_type()
 */
FIRM_API ir_entity *new_parameter_entity(ir_type *owner, size_t pos,
                                         ir_type *type);

/**
 * Creates a new entity aliasing another entity.
 * An aliasing entity is a logically separate entity with its own name,
 * but instead of having a definition the linker will merge the name with
 * the definition of the aliased entity.
 * In gcc this feature is known as __attribute__((alias())).
 *
 * @param owner  owning type (must be a global segment)
 * @param name   name of the entity
 * @param alias  entity that is aliased
 * @param type   type of the aliasing entity, should but need not be the same
 *               type as the one of the aliased entity
 * @param visibility  visibility of the entity
 * @return       the newly created entity
 */
FIRM_API ir_entity *new_alias_entity(ir_type *owner, ident *name,
                                     ir_entity *alias, ir_type *type,
                                     ir_visibility visibility);

/**
 * Sets the aliased entity of an alias entity.
 */
FIRM_API void set_entity_alias(ir_entity *alias, ir_entity *aliased);

/**
 * Returns the entity aliased by an alias entity.
 */
FIRM_API ir_entity *get_entity_alias(const ir_entity *alias);

/**
 * Check an entity.
 *
 * @return non-zero if no errors were found
 */
FIRM_API int check_entity(const ir_entity *ent);

/**
 * Create a new entity with attributes copied from an existing entity.
 *
 * Does not copy the overwrites, overwritten_by, visited and usage fields, sets
 * a new name and inserts the entity into @p owner.
 */
FIRM_API ir_entity *clone_entity(ir_entity const *old, ident *name,
                                 ir_type *owner);

/**
 * Frees the entity.
 *
 * The owner will still contain the pointer to this
 * entity, as well as all other references!
 */
FIRM_API void free_entity(ir_entity *ent);

/** Returns the name of an entity. */
FIRM_API const char *get_entity_name(const ir_entity *ent);

/** Returns the ident of an entity. */
FIRM_API ident *get_entity_ident(const ir_entity *ent);

/** Sets the ident of the entity. */
FIRM_API void set_entity_ident(ir_entity *ent, ident *id);

/** Returns the mangled name of the entity.
 *
 * If the mangled name is set it returns the existing name.
 * Else it generates a name with mangle_entity()
 * and remembers this new name internally.
 */
FIRM_API ident *get_entity_ld_ident(const ir_entity *ent);

/** Sets the mangled name of the entity. */
FIRM_API void set_entity_ld_ident(ir_entity *ent, ident *ld_ident);

/** Returns the mangled name of the entity as a string. */
FIRM_API const char *get_entity_ld_name(const ir_entity *ent);

/** returns 1 if the entity has an ld_ident set explicitely */
FIRM_API int entity_has_ld_ident(const ir_entity *entity);

/** Returns the owner of the entity. */
FIRM_API ir_type *get_entity_owner(const ir_entity *ent);

/**
 * Sets the owner field in entity to owner.
 * Automatically removes entity from old owner type and adds it to the new
 * one.
 */
FIRM_API void set_entity_owner(ir_entity *ent, ir_type *owner);

/** Returns the type of an entity. */
FIRM_API ir_type *get_entity_type(const ir_entity *ent);

/** Sets the type of an entity. */
FIRM_API void set_entity_type(ir_entity *ent, ir_type *tp);

/** Returns the linkage of an entity. */
FIRM_API ir_linkage get_entity_linkage(const ir_entity *entity);

/** Sets the linkage flags of entity @p entity to @p linkage. */
FIRM_API void set_entity_linkage(ir_entity *entity, ir_linkage linkage);
/** Adds linkage flags @p linkage to entity @p entity. */
FIRM_API void add_entity_linkage(ir_entity *entity, ir_linkage linkage);
/** Remove linkage flags @p linkage from entity @p entity. */
FIRM_API void remove_entity_linkage(ir_entity *entity, ir_linkage linkage);

/**
 * Returns the volatility of an entity.
 * @deprecated
 */
FIRM_API ir_volatility get_entity_volatility(const ir_entity *ent);

/**
 * Sets the volatility of an entity.
 * @deprecated
 */
FIRM_API void set_entity_volatility(ir_entity *ent, ir_volatility vol);

/** Returns the name of the volatility. */
FIRM_API const char *get_volatility_name(ir_volatility var);

/** Returns alignment of entity in bytes */
FIRM_API unsigned get_entity_alignment(const ir_entity *entity);

/** Allows you to override the type alignment for an entity.
 * @param entity      the entity
 * @param alignment   alignment in bytes
 */
FIRM_API void set_entity_alignment(ir_entity *entity, unsigned alignment);

/**
 * Returns indication whether entity is aligned in memory.
 * @deprecated
 */
FIRM_API ir_align get_entity_aligned(const ir_entity *ent);

/**
 * Sets indication whether entity is aligned in memory
 * @deprecated
 */
FIRM_API void set_entity_aligned(ir_entity *ent, ir_align a);

/** Returns the name of the alignment. */
FIRM_API const char *get_align_name(ir_align a);

/**
 * Returns the offset of an entity (in a compound) in bytes. Only set if
 * the layout of the entity's owner has been fixed.
 * @sa get_type_state()
 */
FIRM_API int get_entity_offset(const ir_entity *entity);

/** Sets the offset of an entity (in a compound) in bytes. */
FIRM_API void set_entity_offset(ir_entity *entity, int offset);

/** For bitfields, returns the offset in bits to the bitfield base. */
FIRM_API unsigned get_entity_bitfield_offset(const ir_entity *entity);

/** Sets the offset in bits to the base for a bitfield. */
FIRM_API void set_entity_bitfield_offset(ir_entity *entity, unsigned offset);

/** Sets the size in bits for a bitfield. 0 means not a bitfield. */
FIRM_API void set_entity_bitfield_size(ir_entity *entity, unsigned size);

/** Returns the size in bits for a bitfield, 0 if entity is not a bitfield. */
FIRM_API unsigned get_entity_bitfield_size(const ir_entity *entity);

/** Returns the stored intermediate information. */
FIRM_API void *get_entity_link(const ir_entity *ent);

/** Stores new intermediate information. */
FIRM_API void set_entity_link(ir_entity *ent, void *l);

/**
 * Return the method graph of a method entity.
 * @warning If the entity has IR_LINKAGE_WEAK, then this is not necessarily the final code
 *          bound to the entity. If you are writing an analysis use
 *          get_entity_linktime_irg()!
 */
FIRM_API ir_graph *get_entity_irg(const ir_entity *ent);

/**
 * Return the method graph the method entity points to after linking.
 * This is different to get_entity_irg() in case of weak symbols where this
 * function returns NULL because the code may be replaced by a non-weak symbol
 * after linking.
 */
FIRM_API ir_graph *get_entity_linktime_irg(const ir_entity *ent);

/** A reserved value for "not yet set". */
#define IR_VTABLE_NUM_NOT_SET ((unsigned)(-1))

/** Returns the entity vtable number. */
FIRM_API unsigned get_entity_vtable_number(const ir_entity *ent);

/** Sets the entity vtable number. */
FIRM_API void set_entity_vtable_number(ir_entity *ent, unsigned vtable_number);

/** Sets label number of an entity with code type */
FIRM_API void set_entity_label(ir_entity *ent, ir_label_t label);
/** Returns label number of an entity with code type */
FIRM_API ir_label_t get_entity_label(const ir_entity *ent);

/**
 * Bitfield type indicating the way an entity is used.
 */
typedef enum {
	ir_usage_none             = 0,      /**< This entity is unused. */
	ir_usage_address_taken    = 1 << 0, /**< The address of this entity was taken. */
	ir_usage_write            = 1 << 1, /**< The entity was written to. */
	ir_usage_read             = 1 << 2, /**< The entity was read. */
	ir_usage_reinterpret_cast = 1 << 3, /**< The entity was read but with a wrong mode
	                                         (an implicit reinterpret cast) */
	/** Unknown access */
	ir_usage_unknown
		= ir_usage_address_taken | ir_usage_write | ir_usage_read
		| ir_usage_reinterpret_cast
} ir_entity_usage;

/** Returns the entity usage */
FIRM_API ir_entity_usage get_entity_usage(const ir_entity *ent);

/** Sets/resets the state of the address taken flag of an entity. */
FIRM_API void set_entity_usage(ir_entity *ent, ir_entity_usage flag);

/**
 * Returns the debug information of an entity.
 *
 * @param ent The entity.
 */
FIRM_API dbg_info *get_entity_dbg_info(const ir_entity *ent);

/**
 * Sets the debug information of an entity.
 *
 * @param ent The entity.
 * @param db  The debug info.
 */
FIRM_API void set_entity_dbg_info(ir_entity *ent, dbg_info *db);

/**
 * Sepcial parameter number which can be used for parameter entities to
 * indicate the first non-declared parameter in a procedure with variable
 * arguments.
 * Starting from this address you can walk the stack to find further
 * parameters.
 */
#define IR_VA_START_PARAMETER_NUMBER  ((size_t)-1)

/**
 * returns true if a given entity is a parameter_entity representing the
 * address of a function parameter
 */
FIRM_API int is_parameter_entity(const ir_entity *entity);

/**
 * Returns the parameter number of the parameter which @p entity represents.
 */
FIRM_API size_t get_entity_parameter_number(const ir_entity *entity);

/**
 * Sets the parameter number of the parameter which @p entity represents.
 */
FIRM_API void set_entity_parameter_number(ir_entity *entity, size_t n);

/** @defgroup ir_initializer  Entity Initializers
 * @{
 */

/** the kind (type) of an initializer */
typedef enum ir_initializer_kind_t {
	/** initializer containing an ir_node from the const-code irg */
	IR_INITIALIZER_CONST,
	/** initializer containing a tarval */
	IR_INITIALIZER_TARVAL,
	/** initializes type with default values (usually 0) */
	IR_INITIALIZER_NULL,
	/** list of initializers used to initialize a compound or array type */
	IR_INITIALIZER_COMPOUND
} ir_initializer_kind_t;

/** Returns the kind of an initializer */
FIRM_API ir_initializer_kind_t get_initializer_kind(const ir_initializer_t *initializer);

/** Returns the name of the initializer kind. */
FIRM_API const char *get_initializer_kind_name(ir_initializer_kind_t ini);

/**
 * Returns the null initializer (there is only one instance of it in a program )
 */
FIRM_API ir_initializer_t *get_initializer_null(void);

/**
 * Creates an initializer containing a reference to a node on the const-code
 * irg.
 */
FIRM_API ir_initializer_t *create_initializer_const(ir_node *value);

/** Creates an initializer containing a single tarval value */
FIRM_API ir_initializer_t *create_initializer_tarval(ir_tarval *tv);

/** Returns the value contained in a const initializer */
FIRM_API ir_node *get_initializer_const_value(const ir_initializer_t *initializer);

/** Returns the value contained in a tarval initializer */
FIRM_API ir_tarval *get_initializer_tarval_value(const ir_initializer_t *initialzier);

/** Creates a compound initializer which holds @p n_entries entries */
FIRM_API ir_initializer_t *create_initializer_compound(size_t n_entries);

/** Returns the number of entries in a compound initializer */
FIRM_API size_t get_initializer_compound_n_entries(const ir_initializer_t *initializer);

/** Sets entry with index @p index to the initializer @p value */
FIRM_API void set_initializer_compound_value(ir_initializer_t *initializer,
                                             size_t index,
                                             ir_initializer_t *value);

/** Returns the value with index @p index of a compound initializer */
FIRM_API ir_initializer_t *get_initializer_compound_value(
		const ir_initializer_t *initializer, size_t index);

/** @} */

/** Sets the initializer of an entity. */
FIRM_API void set_entity_initializer(ir_entity *entity, ir_initializer_t *initializer);

/** Returns the initializer of an entity. */
FIRM_API ir_initializer_t *get_entity_initializer(const ir_entity *entity);

/** Adds entity @p ent to the list of entities that overwrite @p overwritten. */
FIRM_API void add_entity_overwrites(ir_entity *ent, ir_entity *overwritten);
/** Returns the number of entities in the list of entities that overwrite
 * entity @p ent. */
FIRM_API size_t get_entity_n_overwrites(const ir_entity *ent);
/** Returns index of @p overwritten in list of entities overwriting entity
 * @p ent. */
FIRM_API size_t get_entity_overwrites_index(const ir_entity *ent,
                                            ir_entity *overwritten);
/** Returns entry @p pos in list of entities overwriting entity @p ent. */
FIRM_API ir_entity *get_entity_overwrites(const ir_entity *ent, size_t pos);
/** Sets entry @p pos in list of entities overwriting entity @p ent. */
FIRM_API void set_entity_overwrites(ir_entity *ent, size_t pos,
                                    ir_entity *overwritten);
/** Remove @p overwritten from list of entities overwriting entity @p ent. */
FIRM_API void remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten);

/** Returns number of entities overwritten by @p ent. */
FIRM_API size_t get_entity_n_overwrittenby(const ir_entity *ent);
/** Returns index of @p overwrites in list of entities overwritten by entity
 * @p ent. */
FIRM_API size_t get_entity_overwrittenby_index(const ir_entity *ent,
                                               ir_entity *overwrites);
/** Return entry @p pos in list of entities overwritten by entity @p ent. */
FIRM_API ir_entity *get_entity_overwrittenby(const ir_entity *ent, size_t pos);
/** Sets entry @p pos in list of entities overwritten by entity @p ent. */
FIRM_API void set_entity_overwrittenby(ir_entity *ent, size_t pos,
                                       ir_entity *overwrites);
/** Removes entry @p overwrites in list of entities overwritten by @p ent. */
FIRM_API void remove_entity_overwrittenby(ir_entity *ent,
                                          ir_entity *overwrites);

/** Returns true if the type of the entity is a class, structure,
   array or union type. */
FIRM_API int is_compound_entity(const ir_entity *ent);
/** Returns true if the entity is a method entity. */
FIRM_API int is_method_entity(const ir_entity *ent);
/** Returns true if the entity is an alias entity. */
FIRM_API int is_alias_entity(const ir_entity *ent);

/** Outputs a unique number for this entity. */
FIRM_API long get_entity_nr(const ir_entity *ent);

/** Returns the entities visited counter.
 * @see @ref visited_counters */
FIRM_API ir_visited_t get_entity_visited(const ir_entity *ent);

/** Sets the entities visited counter.
 * @see @ref visited_counters */
FIRM_API void set_entity_visited(ir_entity *ent, ir_visited_t num);

/** Marks entity as visited.
 * @see @ref visited_counters */
FIRM_API void mark_entity_visited(ir_entity *ent);

/** Returns true if this entity was visited.
 * @see @ref visited_counters */
FIRM_API int entity_visited(const ir_entity *ent);

/** Returns true if this entity was not visited.
 * @see @ref visited_counters */
FIRM_API int entity_not_visited(const ir_entity *ent);

/** Return true if this entity can be queried for additional properties.
 *
 * This is possible for method and alias entities.
 */
FIRM_API int entity_has_additional_properties(const ir_entity *entity);

/**
 * Returns the mask of the additional entity properties.
 * The properties are automatically inherited from the irg if available
 * or from the method type if they were not set using
 * set_entity_additional_properties() or
 * set_entity_additional_property().
 */
FIRM_API mtp_additional_properties get_entity_additional_properties(const ir_entity *ent);

/** Sets the mask of the additional graph properties. */
FIRM_API void set_entity_additional_properties(ir_entity *ent,
                                               mtp_additional_properties prop);

/** Sets additional graph properties. */
FIRM_API void add_entity_additional_properties(ir_entity *ent,
                                               mtp_additional_properties flag);

/**
 * @page unknown_entity  The Unknown entity
 *
 *  This entity is an auxiliary entity dedicated to support analyses.
 *
 *  The unknown entity represents that there could be an entity, but it is not
 *  known.  This entity can be used to initialize fields before an analysis (not
 *  known yet) or to represent the top of a lattice (could not be determined).
 *  There exists exactly one unknown entity. This entity has as type the unknown
 *  type and its owner is the dummy type. It is allocated when initializing the
 *  entity module.
 */

/** Returns the @link unknown_entity unknown entity @endlink. */
FIRM_API ir_entity *get_unknown_entity(void);

/** Tests whether entity @p entity is (the) unknown entity.
 * @returns 1 if it is the unknown entity, 0 otherwise */
FIRM_API int is_unknown_entity(const ir_entity *entity);

/** @} */

/** Encodes how a pointer parameter is accessed. */
typedef enum ptr_access_kind {
	ptr_access_none  = 0,                                 /**< no access */
	ptr_access_read  = 1,                                 /**< read access */
	ptr_access_write = 2,                                 /**< write access */
	ptr_access_rw    = ptr_access_read|ptr_access_write,  /**< read AND write access */
	ptr_access_store = 4,                                 /**< the pointer is stored */
	ptr_access_all   = ptr_access_rw|ptr_access_store     /**< all possible access */
} ptr_access_kind;
ENUM_BITSET(ptr_access_kind)

/**
 * @defgroup ir_type Type System
 *
 *  Datastructure to hold type information.
 *
 *  This module supplies a data structure to represent all types
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
 * @{
 */

/**
 *  An enum for the type kinds.
 *  For each type kind exists a typecode to identify it.
 */
typedef enum tp_opcode {
	tpo_uninitialized = 0,   /* not a type opcode */
	tpo_struct,              /**< A struct type. */
	tpo_union,               /**< A union type. */
	tpo_class,               /**< A class type. */
	tpo_segment,             /**< A segment type. */
	tpo_method,              /**< A method type. */
	tpo_array,               /**< An array type. */
	tpo_pointer,             /**< A pointer type. */
	tpo_primitive,           /**< A primitive type. */
	tpo_code,                /**< a piece of code (a basic block) */
	tpo_unknown,             /**< Special code for the Unknown type. */
	tpo_last = tpo_unknown   /* not a type opcode */
} tp_opcode;

/**
 * Returns the name of the type opcode @p opcode.
 */
FIRM_API const char *get_type_opcode_name(tp_opcode opcode);

/** Returns true if low is subclass of high.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
FIRM_API int is_SubClass_of(const ir_type *low, const ir_type *high);

/** Subclass check for pointers to classes.
 *
 *  Dereferences both types the same number of times (as much as possible).
 *  If the remaining types are both class types and subclasses, returns
 *  true, else false.  Can also be called with two class types.
 *  @sa is_SubClass_of()
 */
FIRM_API int is_SubClass_ptr_of(ir_type *low, ir_type *high);

/** Returns true if high is superclass of low.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
#define is_SuperClass_of(high, low) is_SubClass_of(low, high)

/** Superclass check for pointers to classes.
 *
 *  Dereferences both types the same number of times (as much as possible).
 *  If the remaining types are both class types and superclasses, returns
 *  true, else false.  Can also be called with two class types.  */
#define is_SuperClass_ptr_of(low, high) is_SubClass_ptr_of(high, low)

/** Returns true if high is (transitively) overwritten by low.
 *
 *  Returns false if high == low. */
FIRM_API int is_overwritten_by(ir_entity *high, ir_entity *low);

/** Resolve polymorphism in the inheritance relation.
 *
 *  Returns the dynamically referenced entity if the static entity and the
 *  dynamic type are given.
 *  Searches downwards in overwritten tree. */
FIRM_API ir_entity *resolve_ent_polymorphy(ir_type *dynamic_class,
                                           ir_entity* static_ent);

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

/**
 * The state of the transitive closure.
 */
typedef enum {
	inh_transitive_closure_none,       /**<  Closure is not computed, can not be accessed. */
	inh_transitive_closure_valid,      /**<  Closure computed and valid. */
	inh_transitive_closure_invalid,    /**<  Closure invalid, but can be accessed. */
	inh_transitive_closure_max         /**<  Invalid value. */
} inh_transitive_closure_state;

/** Sets the transitive closure of sub/superclass state for the
 * whole program. */
FIRM_API void set_irp_inh_transitive_closure_state(inh_transitive_closure_state s);
/** Sets the transitive closure of sub/superclass state for the
 * whole program to #inh_transitive_closure_invalid */
FIRM_API void invalidate_irp_inh_transitive_closure_state(void);
/** Returns the transitive closure of sub/superclass state for the
 * whole program. */
FIRM_API inh_transitive_closure_state get_irp_inh_transitive_closure_state(void);

/** Compute transitive closure of the subclass/superclass and
 * overwrites/overwrittenby relation.
 *
 * This function walks over the ir (O(\#types+\#entities)) to compute the
 * transitive closure.    */
FIRM_API void compute_inh_transitive_closure(void);

/** Free memory occupied by the transitive closure information. */
FIRM_API void free_inh_transitive_closure(void);

/** Start iteration over all transitive subtypes of @p tp */
FIRM_API ir_type *get_class_trans_subtype_first(const ir_type *tp);
/**
 * Returns next type in a subtype iteration started by
 * get_class_trans_subtype_first()
 */
FIRM_API ir_type *get_class_trans_subtype_next(const ir_type *tp);
/**
 * Check if @p subtp is a subtype of @p tp. This function checks the full
 * transitive closure of the subtype relation and not just direct subtyping.
 * @return 1 if it is a subtype, 0 otherwise
 */
FIRM_API int is_class_trans_subtype(const ir_type *tp, const ir_type *subtp);

/** Start iteration over all transitive supertypes of @p tp */
FIRM_API ir_type *get_class_trans_supertype_first(const ir_type *tp);
/**
 * Returns next type in a supertype iteration started by
 * get_class_trans_supertype_first()
 */
FIRM_API ir_type *get_class_trans_supertype_next(const ir_type *tp);

/** Start iteration over all entities that transitive overwrite entity @p ent.*/
FIRM_API ir_entity *get_entity_trans_overwrittenby_first(const ir_entity *ent);
/**
 * Returns next entity in a overwrittenby iteration started by
 * get_entity_trans_overwrittenby_first()
 */
FIRM_API ir_entity *get_entity_trans_overwrittenby_next(const ir_entity *ent);

/** Start iteration over all transitive overwritten entities, overwritten by
 * entity @p ent */
FIRM_API ir_entity *get_entity_trans_overwrites_first(const ir_entity *ent);
/**
 * Returns next entity in a overwrites iteration started by
 * get_entity_trans_overwrites_first()
 */
FIRM_API ir_entity *get_entity_trans_overwrites_next(const ir_entity *ent);


/**
 * Checks a type for memory corruption, dangling pointers and consistency.
 *
 * @return non-zero if no errors were found
 */
FIRM_API int check_type(const ir_type *tp);

/**
 * Walks the type information and performs a set of sanity checks.
 *
 * @return 0 in case of an error
 */
FIRM_API int tr_verify(void);

/**
 * Frees the memory used by the type.
 *
 * Removes the type from the type list and frees all entities
 * belonging to the type.
 */
FIRM_API void free_type(ir_type *tp);

/** Returns opcode of type @p type */
FIRM_API tp_opcode get_type_opcode(const ir_type *type);

/**
 * construct a string representing the type.
 * This uses the info retrieved by the type_dbg_info if available.
 * Otherwise it tries to create an approximate textual representation of the
 * type.
 * Keep in mind that this representation is not unique for each type,
 * might abstract away some details. The main intention of this is creating
 * human redable strings giving an idea of the type.
 */
FIRM_API void ir_print_type(char *buffer, size_t buffer_size,
                            const ir_type *tp);

/** The state of the type layout. */
typedef enum {
	layout_undefined,    /**< The layout of this type is not defined.
	                          Address computation to access fields is not
	                          possible, fields must be accessed by Sel
	                          nodes.
	                          This is the default value except for
	                          pointer, primitive and method types. */
	layout_fixed         /**< The layout is fixed, all component/member entities
	                          have an offset assigned. Size of the type is
	                          known. Arrays can be accessed by explicit address
	                          computation.
	                          Default for pointer, primitive and method types.
	                          */
} ir_type_state;

/** Returns a human readable string for the enum entry. */
FIRM_API const char *get_type_state_name(ir_type_state s);

/** Returns the type layout state of a type. */
FIRM_API ir_type_state get_type_state(const ir_type *tp);

/** Sets the type layout state of a type.
 *
 * For primitives, pointer and method types the layout is always fixed.
 * This call is legal but has no effect.
 */
FIRM_API void set_type_state(ir_type *tp, ir_type_state state);

/** Returns the mode of a type.
 *
 * Returns NULL for all non atomic types.
 */
FIRM_API ir_mode *get_type_mode(const ir_type *tp);

/** Returns the size of a type in bytes. */
FIRM_API unsigned get_type_size(const ir_type *tp);

/** Sets the size of a type in bytes.
 *
 * For primitive, pointer and method types the size is always fixed.
 * This call is legal but has no effect.
 */
FIRM_API void set_type_size(ir_type *tp, unsigned size);

/** Returns the alignment of a type in bytes. */
FIRM_API unsigned get_type_alignment(const ir_type *tp);

/** Sets the alignment of a type in bytes. */
FIRM_API void set_type_alignment(ir_type *tp, unsigned align);

/** Returns the visited counter of a type.
 * @see @ref visited_counters */
FIRM_API ir_visited_t get_type_visited(const ir_type *tp);
/** Sets the visited counter of a type to num.
 * @see @ref visited_counters */
FIRM_API void set_type_visited(ir_type *tp, ir_visited_t num);
/** Sets visited field in type to type_visited.
 * @see @ref visited_counters */
FIRM_API void mark_type_visited(ir_type *tp);
/** Returns non-zero if the type is already visited
 * @see @ref visited_counters */
FIRM_API int type_visited(const ir_type *tp);

/** Returns the associated link field of a type. */
FIRM_API void *get_type_link(const ir_type *tp);
/** Sets the associated link field of a type. */
FIRM_API void set_type_link(ir_type *tp, void *l);

/** Increments type visited reference counter by one.
 * @see @ref visited_counters, mark_type_visited(), type_visited() */
FIRM_API void         inc_master_type_visited(void);
/** Sets type visited reference counter.
 * @see @ref visited_counters */
FIRM_API void         set_master_type_visited(ir_visited_t val);
/** Returns type visited reference counter.
 * @see @ref visited_counters */
FIRM_API ir_visited_t get_master_type_visited(void);

/**
 * Sets the debug information of a type.
 *
 * @param tp  The type.
 * @param db  The debug info.
 */
FIRM_API void set_type_dbg_info(ir_type *tp, type_dbg_info *db);

/**
 * Returns the debug information of a type.
 *
 * @param tp  The type.
 */
FIRM_API type_dbg_info *get_type_dbg_info(const ir_type *tp);

/**
 *  Outputs a unique number for this type if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
FIRM_API long get_type_nr(const ir_type *tp);

/**
 * @ingroup compound_type
 * @defgroup class_type Class
 *
 *  If the type opcode is set to type_class the type represents class
 *  types.  A list of fields and methods is associated with a class.
 *  Furthermore, a class can inherit from and bequest to other classes.
 *
 *  The following attributes are private to this type kind:
 *  - member:     All entities belonging to this class.  These are method entities
 *                which have type kind type_method or fields that can have any of the
 *                following type kinds: type_class, type_struct, type_union,
 *                type_array, type_pointer, type_primitive.
 *
 *  The following two are dynamic lists that can be grown with add_class_subtype() and
 *  add_class_supertype() respectively but not shrunk (remove_class_subtype() and
 *  remove_class_supertype() will not free memory):
 *
 *  - subtypes:    A list of direct subclasses.
 *
 *  - supertypes:  A list of direct superclasses.
 *
 * @{
 */

/** Creates a new class type. */
FIRM_API ir_type *new_type_class(ident *name);

/** Returns the number of members of this class. */
FIRM_API size_t get_class_n_members(const ir_type *clss);

/** Returns the member at position pos, 0 <= pos < n_member */
FIRM_API ir_entity *get_class_member(const ir_type *clss, size_t pos);

/**
 * Special index returned when get_class_member_index() cannot find a member.
 * This index is never used for actual members.
 */
#define INVALID_MEMBER_INDEX ((size_t)-1)

/** Returns index of mem in clss, INVALID_MEMBER_INDEX if not contained. */
FIRM_API size_t get_class_member_index(ir_type const *clss,
                                       ir_entity const *mem);

/** Adds subtype as subtype to clss.
 *
 *  Checks whether clss is a supertype of subtype.  If not
 *  adds also clss as supertype to subtype.  */
FIRM_API void add_class_subtype(ir_type *clss, ir_type *subtype);

/** Returns the number of subtypes */
FIRM_API size_t get_class_n_subtypes(const ir_type *clss);

/** Returns the subtype at position pos, 0 <= pos < n_subtype. */
FIRM_API ir_type *get_class_subtype(const ir_type *clss, size_t pos);

/** Returns the index to access subclass as subtype of class.
 *
 *  If subclass is no direct subtype of class returns -1.
 */
FIRM_API size_t get_class_subtype_index(const ir_type *clss,
                                        const ir_type *subclass);

/** Sets the subtype at position pos, 0 <= pos < n_subtype.
 *
 *  Does not set the corresponding supertype relation for subtype: this might
 *  be a different position! */
FIRM_API void set_class_subtype(ir_type *clss, ir_type *subtype, size_t pos);

/** Finds subtype in the list of subtypes and removes it  */
FIRM_API void remove_class_subtype(ir_type *clss, ir_type *subtype);

/** Adds supertype as supertype to class.
 *
 *  Checks whether clss is a subtype of supertype.  If not
 *  adds also clss as subtype to supertype.  */
FIRM_API void add_class_supertype(ir_type *clss, ir_type *supertype);

/** Returns the number of supertypes */
FIRM_API size_t get_class_n_supertypes(const ir_type *clss);

/** Returns the index to access superclass as supertype of class.
 *
 *  If superclass is no direct supertype of class returns -1.
 */
FIRM_API size_t get_class_supertype_index(const ir_type *clss,
                                          const ir_type *super_clss);

/** Returns the supertype at position pos,  0 <= pos < n_supertype. */
FIRM_API ir_type *get_class_supertype(const ir_type *clss, size_t pos);

/** Sets the supertype at position pos, 0 <= pos < n_supertype.
 *
 *  Does not set the corresponding subtype relation for supertype: this might
 *  be at a different position! */
FIRM_API void set_class_supertype(ir_type *clss, ir_type *supertype, size_t pos);

/** Finds supertype in the list of supertypes and removes it */
FIRM_API void remove_class_supertype(ir_type *clss, ir_type *supertype);

/** Returns true if a type is a class type. */
FIRM_API int is_Class_type(const ir_type *clss);

/** @} */

/** @ingroup compound_type
 * @defgroup struct_type Struct
 *
 *  A struct type represents aggregate types that consist of a list
 *  of fields.
 *
 *  The following attributes are private to this type kind:
 *  - member:  All entities belonging to this class.  This are the fields
 *             that can have any of the following types:  type_class,
 *             type_struct, type_union, type_array, type_pointer,
 *             type_primitive.
 * @{
 */

/** Creates a new type struct */
FIRM_API ir_type *new_type_struct(ident *name);

/** Returns the number of members of this struct. */
FIRM_API size_t get_struct_n_members(const ir_type *strct);

/** Returns the member at position pos, pos < n_member */
FIRM_API ir_entity *get_struct_member(const ir_type *strct, size_t pos);

/** Returns index of member in strct, -1 if not contained. */
FIRM_API size_t get_struct_member_index(ir_type const *strct,
                                        ir_entity const *member);

/** Returns true if a type is a struct type. */
FIRM_API int is_Struct_type(const ir_type *strct);

/** @} */

/**
 * @ingroup compound_type
 * @defgroup union_type  Union
 *
 *   The union type represents union types.  Note that this representation
 *   resembles the C union type.  For tagged variant types like in Pascal or
 *   Modula, a combination of a struct and a union type must be used.
 *
 *   - n_types:     Number of alternatives.
 *   - members:     Entities for the alternatives.
 * @{
 */
/** Creates a new type union. */
FIRM_API ir_type *new_type_union(ident *name);

/** Returns the number of unioned types of this union */
FIRM_API size_t get_union_n_members(const ir_type *uni);

/** Returns the entity at position pos of a union */
FIRM_API ir_entity *get_union_member(const ir_type *uni, size_t pos);

/** Returns index of member in uni, -1 if not contained. */
FIRM_API size_t get_union_member_index(ir_type const *uni,
                                       ir_entity const *member);

/** Returns true if a type is a union type. */
FIRM_API int is_Union_type(const ir_type *uni);

/** @} */

/**
 * @defgroup method_type    Method
 *
 * A method type represents a method, function or procedure type.
 * It contains a list of the parameter and result types, as these
 * are part of the type description.  These lists should not
 * be changed by an optimization, as a change creates a new method
 * type.  Therefore optimizations should allocate new method types.
 * set_method_param_type() and set_method_res_type() are only for
 * construction by a frontend.
 *
 * - n_params:   Number of parameters to the procedure.
 *               A procedure in FIRM has only call by value parameters.
 *
 * - param_type: A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth element
 *               in the parameter tuple that is a result of the start node.
 *               (See ircons.h for more information.)
 *
 * - value_param_ents:
 *               A list of entities (whose owner is a struct private to the
 *               method type) that represent parameters passed by value.
 *
 * - n_res:      The number of results of the method.  In general, procedures
 *               have zero results, functions one.
 *
 * - res_type:   A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth input to
 *               Return nodes.  (See ircons.h for more information.)
 * @{
 */

/** Create a new method type.
 *
 * @param n_param        the number of parameters
 * @param n_res          the number of results
 * @param is_variadic    whether the function is variadic
 * @param cc_mask        the calling convention
 * @param property_mask  the mask of the additional graph properties
 *
 * The arrays for the parameter and result types are not populated by
 * the constructor.
 */
FIRM_API ir_type *new_type_method(size_t n_param, size_t n_res, int is_variadic, unsigned cc_mask, mtp_additional_properties property_mask);

/** Returns the number of parameters of this method. */
FIRM_API size_t get_method_n_params(const ir_type *method);

/** Returns the type of the parameter at position pos of a method. */
FIRM_API ir_type *get_method_param_type(const ir_type *method, size_t pos);
/** Sets the type of the parameter at position pos of a method.
 * Note: does not change the corresponding parameter entities (if there are any)
 */
FIRM_API void set_method_param_type(ir_type *method, size_t pos, ir_type *tp);
/** Returns the number of results of a method type. */
FIRM_API size_t get_method_n_ress(const ir_type *method);
/** Returns the return type of a method type at position pos. */
FIRM_API ir_type *get_method_res_type(const ir_type *method, size_t pos);
/** Sets the type of the result at position pos of a method. */
FIRM_API void set_method_res_type(ir_type *method, size_t pos, ir_type *tp);

/** Returns the variadicity of a method. */
FIRM_API int is_method_variadic(ir_type const *method);

/** Returns the mask of the additional graph properties. */
FIRM_API mtp_additional_properties get_method_additional_properties(const ir_type *method);

/**
 * Calling conventions: lower 24 bits are the number of register parameters,
 * upper 8 encode the calling conventions.
 */
typedef enum {
	cc_reg_param           = 0x01000000, /**< Transmit parameters in registers, else the stack is used.
	                                          This flag may be set as default on some architectures. */
	cc_last_on_top         = 0x02000000, /**< The last non-register parameter is transmitted on top of
	                                          the stack. This is equivalent to the pascal
	                                          calling convention. If this flag is not set, the first
	                                          non-register parameter is used (stdcall or cdecl
	                                          calling convention) */
	cc_callee_clear_stk    = 0x04000000, /**< The callee clears the stack. This forbids variadic
	                                          function calls (stdcall). */
	cc_this_call           = 0x08000000, /**< The first parameter is a this pointer and is transmitted
	                                          in a special way. */
	cc_compound_ret        = 0x10000000, /**< The method returns a compound type. */
	cc_frame_on_caller_stk = 0x20000000, /**< The method did not allocate an own stack frame, instead the
	                                          caller must reserve size on its own stack. */
	cc_fpreg_param         = 0x40000000, /**< Transmit floating point parameters in registers, else the stack is used. */
} calling_convention;

/**< The calling convention bits. */
#define cc_bits (0xFF000000)

/** cdecl calling convention */
#define cc_cdecl_set    (0)
/** stdcall calling convention */
#define cc_stdcall_set  cc_callee_clear_stk
/** fastcall calling convention */
#define cc_fastcall_set (cc_reg_param|cc_callee_clear_stk)

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
 * Sets the STDCALL convention bits.
 */
#define SET_STDCALL(cc_mask)  (((cc_mask) & ~cc_bits) | cc_stdcall_set)

/**
 * Sets the FASTCALL convention bits.
 */
#define SET_FASTCALL(cc_mask) (((cc_mask) & ~cc_bits) | cc_fastcall_set)

/** Returns the calling convention of an entities graph. */
FIRM_API unsigned get_method_calling_convention(const ir_type *method);

/** Returns the number of registers parameters, 0 means default. */
FIRM_API unsigned get_method_n_regparams(ir_type *method);

/** Returns true if a type is a method type. */
FIRM_API int is_Method_type(const ir_type *method);

/** @} */

/**
 * @defgroup array_type  Array
 *
 * The array type represents linear arrangement of objects of the same type.
 *
 * - size:           The number of elements in the array.
 * - element_type:   The type of the array elements.
 * @{
 */

/**
 * Create a new array type with @p n_elements of type @p element_type.
 *
 * 0 elements designates an array of unknown length.
 */
FIRM_API ir_type *new_type_array(ir_type *element_type, unsigned n_elements);

/** Returns the size (number of elements) of an array. */
FIRM_API unsigned get_array_size(const ir_type *array);

/** Returns the array element type. */
FIRM_API ir_type *get_array_element_type(const ir_type *array);

/** Returns true if a type is an array type. */
FIRM_API int is_Array_type(const ir_type *array);

/** @} */

/**
 * @defgroup pointer_type   Pointer
 *
 * Pointer types:
 * - points_to:      The type this pointer points to.
 * @{
 */

/** Creates a new type pointer. */
FIRM_API ir_type *new_type_pointer(ir_type *points_to);

/** Sets the type to which a pointer points to. */
FIRM_API void set_pointer_points_to_type(ir_type *pointer, ir_type *tp);

/** Returns the type to which a pointer points to. */
FIRM_API ir_type *get_pointer_points_to_type(const ir_type *pointer);

/** Returns true if a type is a pointer type. */
FIRM_API int is_Pointer_type(const ir_type *pointer);

/** @} */

/**
 * @defgroup primitive_type Primitive
 *
 * Primitive types are types that represent atomic data values that
 * map directly to modes.  They don't have private attributes.  The
 * important information they carry is held in the common mode field.
 * @{
 */
/** Creates a new primitive type. */
FIRM_API ir_type *new_type_primitive(ir_mode *mode);

/** Returns true if a type is a primitive type. */
FIRM_API int is_Primitive_type(const ir_type *primitive);

/** @} */

/** @defgroup code_type Code
 * @{
 */
/** Returns the code type. */
FIRM_API ir_type *get_code_type(void);
/**
 * Checks whether a type is a code type.
 */
FIRM_API int is_code_type(const ir_type *tp);
/** @} */

/**
 * @defgroup unknown_type  Unknown
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
 * @{
 */
/** Returns the unknown type. */
FIRM_API ir_type *get_unknown_type(void);
/** Checks whether type @p type is the unknown type */
FIRM_API int is_unknown_type(const ir_type *type);
/** @} */

/**
 *  Checks whether a type is atomic.
 *  @param tp   any type
 *  @return true if type is primitive or pointer
 */
FIRM_API int is_atomic_type(const ir_type *tp);

/**
 * @defgroup compound_type Compound
 *
 * These functions are common to classes, structs and unions,
 * collectively known as "compound types".
 *
 * Note that there is no function to add a member to a compound
 * type. Instead, create an entity with the compound type as its owner
 * or use set_entity_owner() to make it a member of the compound type.
 *
 * @{
 */

/**
 * Returns the identifier of a compound type
 */
FIRM_API ident *get_compound_ident(const ir_type *tp);

/** Returns compound identifier as c-string */
FIRM_API const char *get_compound_name(const ir_type *tp);

/**
 * Returns the number of elements in a Firm compound type.
 *
 * This is just a comfortability function, because structs and
 * classes can often be treated be the same code, but they have
 * different access functions to their members.
 *
 * @param tp  The type (must be struct, union or class).
 *
 * @return Number of members in the compound type.
 */
FIRM_API size_t get_compound_n_members(const ir_type *tp);

/**
 * Returns the member of a Firm compound type at position pos.
 *
 * @param tp  The type (must be struct, union or class).
 * @param pos The number of the member.
 *
 * @return The member entity at position pos.
 */
FIRM_API ir_entity *get_compound_member(const ir_type *tp, size_t pos);

/** Returns index of member in tp, -1 if not contained. */
FIRM_API size_t get_compound_member_index(ir_type const *tp,
                                          ir_entity const *member);

/** Remove a member from a compound type. */
FIRM_API void remove_compound_member(ir_type *compound, ir_entity *entity);

/**
 * Layout members of a compound type in the default way (as determined
 * by the target ABI). The compound type may not contain bitfield
 * members, in which case it must be laid out by hand.
 */
FIRM_API void default_layout_compound_type(ir_type *tp);

/**
 * Checks whether a type is a compound type.
 *
 * @param tp - any type
 *
 * @return true if the type is class, structure, union or segment type.
 */
FIRM_API int is_compound_type(const ir_type *tp);

/** @} */

/** @defgroup frame_type  Frame
 * @{
 */

/**
 * Makes a new frame type. Frame types are class types,
 * so all class access functions work.
 * Frame types are not in the global list of types.
 */
FIRM_API ir_type *new_type_frame(void);

/**
 * Checks, whether a type is a frame type.
 */
FIRM_API int is_frame_type(const ir_type *tp);

/**
 * Makes a clone of a frame type.
 * Sets entity links from old frame entities to new ones and
 * vice versa.
 */
FIRM_API ir_type *clone_frame_type(ir_type *type);

/** @} */

/** @defgroup segment_type Segment
 *
 * Segment types represent segments in the object file.
 * @{
 */

/** Checks, whether a type is a frame type. */
FIRM_API int is_segment_type(const ir_type *tp);

/** @} */

/**
 * @defgroup trwalk Traversing
 * @{
 */

/**  Type for a function that compares two types.
 *
 *   @param tp1  The first type to compare.
 *   @param tp2  The second type to compare.
 */
typedef int (compare_types_func_t)(const void *tp1, const void *tp2);

/** Type of argument functions for type walkers.
 *
 * @param type    points to the visited type, either this or entity is non-null
 * @param entity  points to the visited entity, either this or type is non-null
 * @param env     free environment pointer
 */
typedef void type_walk_func(ir_type *type, ir_entity *entity, void *env);

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
FIRM_API void type_walk(type_walk_func *pre, type_walk_func *post, void *env);

/** Walks over all type information reachable from an ir graph.
 *
 *  Walks over all type information reachable from irg, i.e., starts a
 *  type walk at the irgs entity, the irgs frame type and all types and
 *  entities that are attributes to firm nodes. */
FIRM_API void type_walk_irg(ir_graph *irg, type_walk_func *pre,
                            type_walk_func *post, void *env);

/**
 * Touches every class in specified order:
 *    - first the super class
 *    - second the class itself
 *    - third the sub classes.  If new classes are created
 *    during the traversal these will be visited, too.
 *
 *    @deprecated will be removed?
 */
FIRM_API void type_walk_super2sub(type_walk_func *pre, type_walk_func *post,
                                  void *env);

/**
 * Walker for class types in inheritance order.
 *
 * Touches every class in specified order:
 *   - first the super class
 *   - second the class itself
 *   If new classes are created during the traversal these
 *   will be visited, too.
 * Starts the walk at arbitrary classes.
 * Executes pre when first visiting a class.  Executes post after
 * visiting all superclasses.
 *
 * The arguments pre, post, env may be NULL. */
FIRM_API void type_walk_super(type_walk_func *pre, type_walk_func *post,
                              void *env);

/**
 * Same as type_walk_super2sub, but visits only class types.
 * Executes pre for a class if all superclasses have been visited.
 * Then iterates to subclasses.  Executes post after return from
 * subclass.
 * Does not visit global type, frame types.
 */
FIRM_API void class_walk_super2sub(class_walk_func *pre, class_walk_func *post,
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
FIRM_API void walk_types_entities(ir_type *tp, entity_walk_func *doit,
                                  void *env);

/** @} */

/** @} */

#include "end.h"

#endif
