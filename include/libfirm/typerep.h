/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief Declarations for functions and datastructures to represent types
 */
#ifndef FIRM_TYPEREP_H
#define FIRM_TYPEREP_H

#include <stdlib.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup ir_entity Entities
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
 * Overwrittenby is the inverse of overwrites.  Both add routines add
 * both relations, they only differ in the order of arguments.
 *
 * @{
 */

/**
 * Visibility classed for entities.
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
	 * Optimisation might replace loads from this entity with constants.
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
	 * Do not generate code even if the entity has a graph attached. The graph
	 * is only used for inlining. Otherwise the entity is regarded as a
	 * declaration of an externally defined entity.
	 * This linkage flag can be used to implement C99 "inline" or GNU89
	 * "extern inline".
	 */
	IR_LINKAGE_NO_CODEGEN      = 1 << 5,
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
 * (The entity might still be accessible indirectly through pointers)
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
 * is type_method, then it is static_allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
FIRM_API ir_entity *new_entity(ir_type *owner, ident *name, ir_type *tp);

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * The entity is automatic allocated and uninitialized except if the type
 * is type_method, then it is static allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
FIRM_API ir_entity *new_d_entity(ir_type *owner, ident *name, ir_type *tp,
                                 dbg_info *db);

/**
 * Creates a new entity corresponding to a function parameter.
 * This must be created on an irgs frame_type
 */
FIRM_API ir_entity *new_parameter_entity(ir_type *owner, size_t pos,
                                         ir_type *type);

/**
 * Like new_parameter_entity() but with debug information.
 */
FIRM_API ir_entity *new_d_parameter_entity(ir_type *owner, size_t pos,
                                           ir_type *type, dbg_info *dbgi);

/**
 * Check an entity. Currently, we check only if initialized constants
 * are build on the const irg graph.
 *
 * @return non-zero if no errors were found
 */
FIRM_API int check_entity(const ir_entity *ent);

/**
 * Copies the entity if the new_owner is different from the
 * owner of the old entity,  else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * Resets the overwrites/overwritten_by fields.
 * Keeps the old atomic value.
 */
FIRM_API ir_entity *copy_entity_own(ir_entity *old, ir_type *new_owner);

/**
 * Copies the entity if the new_name is different from the
 * name of the old entity, else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * The mangled name ld_name is set to NULL.
 * Overwrites relation is copied from old.
 */
FIRM_API ir_entity *copy_entity_name(ir_entity *old, ident *new_name);

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

/** Returns the offset of an entity (in a compound) in bytes. Only set if
 * layout = fixed. */
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
 * The entity knows the corresponding irg if the entity is a method.
 * This allows to get from a Call to the called irg.
 */
FIRM_API ir_graph *get_entity_irg(const ir_entity *ent);

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

/** Checks if an entity is compiler generated. */
FIRM_API int is_entity_compiler_generated(const ir_entity *ent);

/** Sets/resets the compiler generated flag. */
FIRM_API void set_entity_compiler_generated(ir_entity *ent, int flag);

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
 * We assumes that all additional parameters for variable parameters are on the
 * stack. Starting from this address you can walk the stack to find all other
 * parameters.
 */
#define IR_VA_START_PARAMETER_NUMBER  ((size_t)-1)

/**
 * returns true if a given entity is a parameter_entity representing the
 * address of a function parameter
 */
FIRM_API int is_parameter_entity(const ir_entity *entity);

/**
 * returns number of parameter a parameter entitiy represents
 */
FIRM_API size_t get_entity_parameter_number(const ir_entity *entity);

/**
 * set number of parameter an entity represents
 */
FIRM_API void set_entity_parameter_number(ir_entity *entity, size_t n);

/** Returns initial value of entity with atomic type @p ent. */
FIRM_API ir_node *get_atomic_ent_value(const ir_entity *ent);
/** Sets initial value of entity with atomic type @p ent to node @p val.
 * @note @p val must be a node in the const_code graph */
FIRM_API void set_atomic_ent_value(ir_entity *ent, ir_node *val);

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
	/** list of initializers used to initializer a compound or array type */
	IR_INITIALIZER_COMPOUND
} ir_initializer_kind_t;

/** Returns kind of an initializer */
FIRM_API ir_initializer_kind_t get_initializer_kind(const ir_initializer_t *initializer);

/** Returns the name of the initializer kind. */
FIRM_API const char *get_initializer_kind_name(ir_initializer_kind_t ini);

/**
 * Returns the null initializer (there's only one instance of it in a program )
 */
FIRM_API ir_initializer_t *get_initializer_null(void);

/**
 * Creates an initializer containing a reference to a node on the const-code
 * irg.
 */
FIRM_API ir_initializer_t *create_initializer_const(ir_node *value);

/** Creates an initializer containing a single tarval value */
FIRM_API ir_initializer_t *create_initializer_tarval(ir_tarval *tv);

/** Returns value contained in a const initializer */
FIRM_API ir_node *get_initializer_const_value(const ir_initializer_t *initializer);

/** Returns value contained in a tarval initializer */
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

/** Sets the new style initializers of an entity. */
FIRM_API void set_entity_initializer(ir_entity *entity, ir_initializer_t *initializer);

/** Returns true, if an entity has new style initializers. */
FIRM_API int has_entity_initializer(const ir_entity *entity);

/** Returns the new style initializers of an entity. */
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

/**
 *   Checks whether a pointer points to an entity.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is an entity, else false
 */
FIRM_API int is_entity(const void *thing);

/** Returns true if the type of the entity is a primitive, pointer
 * or method type.
 *
 * @note This is a different classification than from is_primitive_type().
 */
FIRM_API int is_atomic_entity(const ir_entity *ent);
/** Returns true if the type of the entity is a class, structure,
   array or union type. */
FIRM_API int is_compound_entity(const ir_entity *ent);
/** Returns true if the type of the entity is a Method type. */
FIRM_API int is_method_entity(const ir_entity *ent);

/** Outputs a unique number for this entity if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
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
 * - offset        = -1
 * - value         = SymConst(unknown_entity)
 * - values        = NULL
 * - val_paths     = NULL
 * - volatility    = volatility_non_volatile
 * - stickyness    = stickyness_unsticky
 * - ld_name       = NULL
 * - overwrites    = NULL
 * - overwrittenby = NULL
 * - irg           = NULL
 * - link          = NULL
 */

/** Returns the @link unknown_entity unknown entity @endlink. */
FIRM_API ir_entity *get_unknown_entity(void);

/** Tests whether entity @p entity is (the) unknown entity.
 * @returns 1 if it is the unknown entity, 0 otherwise */
FIRM_API int is_unknown_entity(const ir_entity *entity);

/** @deprecated */
typedef enum {
	allocation_automatic,
	allocation_parameter,
	allocation_dynamic,
	allocation_static
} ir_allocation;
/** @deprecated */
FIRM_API ir_allocation get_entity_allocation(const ir_entity *ent);
/** @deprecated */
FIRM_API void set_entity_allocation(ir_entity *ent, ir_allocation al);

/** @deprecated */
typedef enum {
	peculiarity_existent,
	peculiarity_description,
	peculiarity_inherited
} ir_peculiarity;
/** @deprecated */
FIRM_API ir_peculiarity get_entity_peculiarity(const ir_entity *ent);
/** @deprecated */
FIRM_API void set_entity_peculiarity(ir_entity *ent, ir_peculiarity pec);

/** @deprecated */
FIRM_API int is_entity_final(const ir_entity *ent);
/** @deprecated */
FIRM_API void set_entity_final(ir_entity *ent, int final);

/** @deprecated */
FIRM_API ir_peculiarity get_class_peculiarity(const ir_type *clss);
/** @deprecated */
FIRM_API void set_class_peculiarity(ir_type *clss, ir_peculiarity pec);

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
 * @{
 */

/**
 * @defgroup tp_op  Type Opcodes
 *  This module specifies the kinds of types available in firm.
 *
 *  They are called type opcodes. These include classes, structs, methods,
 *  unions, arrays, pointers and primitive types.
 *  Special types with own opcodes are the id type, a type representing an
 *  unknown type and a type used to specify that something has no type.
 *
 * @{
 */

/**
 *  An enum for the type kinds.
 *  For each type kind exists a typecode to identify it.
 */
typedef enum tp_opcode {
	tpo_uninitialized = 0,   /* not a type opcode */
	tpo_class,               /**< A class type. */
	tpo_struct,              /**< A struct type. */
	tpo_method,              /**< A method type. */
	tpo_union,               /**< An union type. */
	tpo_array,               /**< An array type. */
	tpo_pointer,             /**< A pointer type. */
	tpo_primitive,           /**< A primitive type. */
	tpo_code,                /**< a piece of code (a basic block) */
	tpo_unknown,             /**< Special code for the Unknown type. */
	tpo_last = tpo_unknown   /* not a type opcode */
} tp_opcode;

/**
 * A structure containing information about a kind of type.
 * A structure containing information about a kind of type.  So far
 * this is only the kind name, an enum for case-switching and some
 * internal values.
 *
 * @see  get_tpop_name(), get_tpop_code()
 */
typedef struct tp_op tp_op;


/**
 * Returns the string for the type opcode.
 *
 * @param op  The type opcode to get the string from.
 * @return    a string.
 */
FIRM_API const char *get_tpop_name(const tp_op *op);

/**
 * Returns an enum for the type opcode.
 *
 * @param op   The type opcode to get the enum from.
 * @return the enum.
 */
FIRM_API tp_opcode get_tpop_code(const tp_op *op);

/** @} */

/** Returns true if low is subclass of high.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
FIRM_API int is_SubClass_of(ir_type *low, ir_type *high);

/** Subclass check for pointers to classes.
 *
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and subclasses, returns true, else false.  Can also be called with
 *  two class types.  */
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
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and superclasses, returns true, else false.  Can also be called with
 *  two class types.  */
#define is_SuperClass_ptr_of(low, high) is_SubClass_ptr_of(high, low)

/** Returns true if high is (transitive) overwritten by low.
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

/** Default name mangling for inherited entities.
 *
 *  Returns an ident that consists of the name of type followed by an
 *  underscore and the name (not ld_name) of the entity. */
FIRM_API ident *default_mangle_inherited_name(const ir_entity *ent,
                                              const ir_type *clss);

/** Type of argument functions for inheritance resolver.
 *
 * @param ent     The entity in the super type that will be overwritten
 *                by the newly generated entity, for which this name is
 *                used.
 * @param clss    The class type in which the new entity will be placed.
 */
typedef ident *mangle_inherited_name_func(const ir_entity *ent,
                                          const ir_type *clss);

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
 */
FIRM_API void resolve_inheritance(mangle_inherited_name_func *mfunc);


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
 * Checks a type.
 *
 * @return non-zero if no errors were found
 */
FIRM_API int check_type(const ir_type *tp);

/**
 * Walks the type information and performs a set of sanity checks.
 *
 * Currently, the following checks are executed:
 * - values of initialized entities must be allocated on the constant IRG
 * - class types: doesn't have NULL members
 * - class types: all overwrites are existent in the super type
 *
 * @return 0 if no error encountered
 */
FIRM_API int tr_verify(void);

/**
 * Frees the memory used by the type.
 *
 * Removes the type from the type list and frees all entities
 * belonging to the type.
 */
FIRM_API void free_type(ir_type *tp);

/** Returns type opcode of type @p tp */
FIRM_API const tp_op *get_type_tpop(const ir_type *tp);
/** Returns name identifier of type opcode of type @p tp */
FIRM_API ident *get_type_tpop_nameid(const ir_type *tp);
/** Returns name of type opcode of type @p tp */
FIRM_API const char *get_type_tpop_name(const ir_type *tp);
/** Returns opcode of type opcode of type @p tp */
FIRM_API tp_opcode get_type_tpop_code(const ir_type *tp);

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

/** Sets the mode of a type.
 *
 * Only has an effect on primitive and pointer types.
 */
FIRM_API void set_type_mode(ir_type *tp, ir_mode* m);

/** Returns the size of a type in bytes. */
FIRM_API unsigned get_type_size_bytes(const ir_type *tp);

/** Sets the size of a type in bytes.
 *
 * For primitive, pointer and method types the size is always fixed.
 * This call is legal but has no effect.
 */
FIRM_API void set_type_size_bytes(ir_type *tp, unsigned size);

/** Returns the alignment of a type in bytes. */
FIRM_API unsigned get_type_alignment_bytes(ir_type *tp);

/** Sets the alignment of a type in bytes.
 *
 *  If the alignment of a type is
 *  not set, it is calculated here according to the following rules:
 *  -#.) if a type has a mode, the alignment is the mode size.
 *  -#.) compound types have the alignment of their biggest member.
 *  -#.) array types have the alignment of their element type.
 *  -#.) method types return 0 here.
 *  -#.) all other types return 1 here (i.e. aligned at byte).
 */
FIRM_API void set_type_alignment_bytes(ir_type *tp, unsigned align);

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
/** Returns non-zero if the type is not yet visited
 * @see @ref visited_counters */
FIRM_API int type_not_visited(const ir_type *tp);

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
 * Checks whether a pointer points to a type.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a type, else false
 */
FIRM_API int is_type(const void *thing);

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
 *  Further a class can inherit from and bequest to other classes.
 *
 *  The following attributes are private to this type kind:
 *  - member:     All entities belonging to this class.  This are method entities
 *                which have type_method or fields that can have any of the
 *                following type kinds: type_class, type_struct, type_union,
 *                type_array, type_pointer, type_primitive.
 *
 *  The following two are dynamic lists that can be grown with an "add_" function,
 *  but not shrinked:
 *
 *  - subtypes:    A list of direct subclasses.
 *
 *  - supertypes:  A list of direct superclasses.
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
 * @{
 */

/** Creates a new class type. */
FIRM_API ir_type *new_type_class(ident *name);

/** Creates a new class type with debug information. */
FIRM_API ir_type *new_d_type_class(ident *name, type_dbg_info *db);

/** Returns identifier of the class type */
FIRM_API ident *get_class_ident(const ir_type *clss);

/** Returns identifier of the class type */
FIRM_API const char *get_class_name(const ir_type *clss);

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
FIRM_API size_t get_class_member_index(const ir_type *clss, ir_entity *mem);

/** Finds the member with name 'name'. If several members with the same
 *  name returns one of them.  Returns NULL if no member found. */
FIRM_API ir_entity *get_class_member_by_name(ir_type *clss, ident *name);

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

/** Returns the size of the virtual function table. */
FIRM_API unsigned get_class_vtable_size(const ir_type *clss);

/** Sets a new size of the virtual function table. */
FIRM_API void set_class_vtable_size(ir_type *clss, unsigned size);

/** Returns non-zero if a class is final. */
FIRM_API int is_class_final(const ir_type *clss);

/** Sets the class final flag. */
FIRM_API void set_class_final(ir_type *clss, int flag);

/** Returns non-zero if a class is an interface */
FIRM_API int is_class_interface(const ir_type *clss);

/** Sets the class interface flag. */
FIRM_API void set_class_interface(ir_type *clss, int flag);

/** Returns non-zero if a class is an abstract class. */
FIRM_API int is_class_abstract(const ir_type *clss);

/** Sets the class abstract flag. */
FIRM_API void set_class_abstract(ir_type *clss, int flag);

/** Returns true if a type is a class type. */
FIRM_API int is_Class_type(const ir_type *clss);

/**
 * This type opcode marks that the corresponding type is a class type.
 *
 * Consequently the type refers to supertypes, subtypes and entities.
 * Entities can be any fields, but also methods.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_class;
/** Returns type opcode for class type. @see type_class */
FIRM_API const tp_op *get_tpop_class(void);

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
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 * @{
 */

/** Creates a new type struct */
FIRM_API ir_type *new_type_struct(ident *name);
/** Creates a new type struct with debug information. */
FIRM_API ir_type *new_d_type_struct(ident *name, type_dbg_info* db);

/** Returns struct identifier */
FIRM_API ident *get_struct_ident(const ir_type *strct);

/** Returns struct identifier as c-string*/
FIRM_API const char *get_struct_name(const ir_type *strct);

/** Returns the number of members of this struct. */
FIRM_API size_t get_struct_n_members(const ir_type *strct);

/** Returns the member at position pos, pos < n_member */
FIRM_API ir_entity *get_struct_member(const ir_type *strct, size_t pos);

/** Returns index of member in strct, -1 if not contained. */
FIRM_API size_t get_struct_member_index(const ir_type *strct, ir_entity *member);

/** Returns true if a type is a struct type. */
FIRM_API int is_Struct_type(const ir_type *strct);

/**
 * This type opcode marks that the corresponding type is a compound type
 * as a struct in C.
 *
 * Consequently the type refers to a list of entities
 * which may not be methods (but pointers to methods).
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_struct;
/** Returns type opcode for struct type. @see type_struct */
FIRM_API const tp_op *get_tpop_struct(void);

/** @} */

/**
 * @ingroup compound_type
 * @defgroup union_type  Union
 *
 *   The union type represents union types.  Note that this representation
 *   resembles the C union type.  For tagged variant types like in Pascal or
 *   Modula a combination of a struct and a union type must be used.
 *
 *   - n_types:     Number of unioned types.
 *   - members:     Entities for unioned types.  Fixed length array.
 *                  This is a dynamic list that can be grown with an "add_"
 *                  function, but not shrinked.
 * @{
 */
/** Creates a new type union. */
FIRM_API ir_type *new_type_union(ident *name);

/** Creates a new type union with debug information. */
FIRM_API ir_type *new_d_type_union(ident *name, type_dbg_info* db);


/** Returns union identifier */
FIRM_API ident *get_union_ident(const ir_type *uni);

/** Returns union identifier as c-string */
FIRM_API const char *get_union_name(const ir_type *uni);

/** Returns the number of unioned types of this union */
FIRM_API size_t get_union_n_members(const ir_type *uni);

/** Returns the entity at position pos of a union */
FIRM_API ir_entity *get_union_member(const ir_type *uni, size_t pos);

/** Returns index of member in uni, -1 if not contained. */
FIRM_API size_t get_union_member_index(const ir_type *uni, ir_entity *member);

/** Returns true if a type is a union type. */
FIRM_API int is_Union_type(const ir_type *uni);

/**
 * This type opcode marks that the corresponding type is a union type.
 *
 * Consequently it refers to a list of unioned types.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_union;
/** Returns type opcode for union type. @see type_union */
FIRM_API const tp_op *get_tpop_union(void);

/** @} */

/**
 * @defgroup method_type    Method
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
 * @{
 */

/** Create a new method type.
 *
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
FIRM_API ir_type *new_type_method(size_t n_param, size_t n_res);

/** Create a new method type with debug information.
 *
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 * @param db        user defined debug information
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
FIRM_API ir_type *new_d_type_method(size_t n_param, size_t n_res,
                                    type_dbg_info *db);

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

/**
 * This enum flags the variadicity of methods (methods with a
 * variable amount of arguments (e.g. C's printf). Default is
 * non_variadic.
 */
typedef enum ir_variadicity {
	variadicity_non_variadic, /**< non variadic */
	variadicity_variadic      /**< variadic */
} ir_variadicity;

/** Returns the null-terminated name of this variadicity. */
FIRM_API const char *get_variadicity_name(ir_variadicity vari);

/** Returns the variadicity of a method. */
FIRM_API ir_variadicity get_method_variadicity(const ir_type *method);

/** Sets the variadicity of a method. */
FIRM_API void set_method_variadicity(ir_type *method, ir_variadicity vari);

/** Returns the mask of the additional graph properties. */
FIRM_API mtp_additional_properties get_method_additional_properties(const ir_type *method);

/** Sets the mask of the additional graph properties. */
FIRM_API void set_method_additional_properties(ir_type *method,
                                               mtp_additional_properties property_mask);

/** Sets one additional graph property. */
FIRM_API void add_method_additional_properties(ir_type *method,
                                               mtp_additional_properties flag);

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
	cc_bits                = (0xFF << 24)/**< The calling convention bits. */
} calling_convention;

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

/** Sets the calling convention of an entities graph. */
FIRM_API void set_method_calling_convention(ir_type *method, unsigned cc_mask);

/** Returns the number of registers parameters, 0 means default. */
FIRM_API unsigned get_method_n_regparams(ir_type *method);

/** Sets the number of registers parameters, 0 means default. */
FIRM_API void set_method_n_regparams(ir_type *method, unsigned n_regs);

/** Returns true if a type is a method type. */
FIRM_API int is_Method_type(const ir_type *method);

/**
 * This type opcode marks that the corresponding type is a method type.
 *
 * Consequently it refers to a list of arguments and results.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_method;
/** Returns type opcode for method type @see type_method */
FIRM_API const tp_op *get_tpop_method(void);

/** @} */

/**
 * @defgroup array_type  Array
 *
 * The array type represents rectangular multi dimensional arrays.
 * The constants representing the bounds must be allocated to
 * get_const_code_irg().
 *
 * - n_dimensions:    Number of array dimensions.
 * - *lower_bound:    Lower bounds of dimensions.  Usually all 0.
 * - *upper_bound:    Upper bounds or dimensions.
 * - *element_type:   The type of the array elements.
 * - *element_ent:    An entity for the array elements to be used for
 *                      element selection with Sel.
 * @{
 */

/** Create a new type array.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Sets dimension sizes after call to constructor with set_* routines.
 */
FIRM_API ir_type *new_type_array(size_t n_dims, ir_type *element_type);

/** Create a new type array with debug information.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Sets dimension sizes after call to constructor with set_* routines.
 * A legal array type must have at least one dimension set.
 */
FIRM_API ir_type *new_d_type_array(size_t n_dims, ir_type *element_type,
                                   type_dbg_info* db);


/** Returns the number of array dimensions of this type. */
FIRM_API size_t get_array_n_dimensions(const ir_type *array);

/**
 * Allocates Const nodes of mode_Is for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
FIRM_API void set_array_bounds_int(ir_type *array, size_t dimension,
                                   int lower_bound, int upper_bound);
/**
 * Sets the bounds for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
FIRM_API void set_array_bounds(ir_type *array, size_t dimension,
                               ir_node *lower_bound, ir_node *upper_bound);
/** Sets the lower bound for one array dimension, i.e. [lower,upper[ */
FIRM_API void set_array_lower_bound(ir_type *array, size_t dimension,
                                    ir_node *lower_bound);

/** Allocates Const nodes of mode_Is for the lower bound of an array
    dimension, i.e. [lower,upper[ */
FIRM_API void set_array_lower_bound_int(ir_type *array, size_t dimension,
                                        int lower_bound);

/** Sets the upper bound for one array dimension, i.e. [lower,upper[ */
FIRM_API void set_array_upper_bound(ir_type *array, size_t dimension,
                                    ir_node *upper_bound);

/** Allocates Const nodes of mode_Is for the upper bound of an array
    dimension, i.e. [lower,upper[. */
FIRM_API void set_array_upper_bound_int(ir_type *array, size_t dimension,
                                        int upper_bound);

/** Returns true if lower bound != Unknown. */
FIRM_API int has_array_lower_bound(const ir_type *array, size_t dimension);
/** Returns the lower bound of an array. */
FIRM_API ir_node *get_array_lower_bound(const ir_type *array, size_t dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
FIRM_API long get_array_lower_bound_int(const ir_type *array, size_t dimension);
/** returns true if lower bound != Unknown */
FIRM_API int has_array_upper_bound(const ir_type *array, size_t dimension);
/** Returns the upper bound of an array. */
FIRM_API ir_node *get_array_upper_bound(const ir_type *array, size_t dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
FIRM_API long get_array_upper_bound_int(const ir_type *array, size_t dimension);

/** Sets an array dimension to a specific order. */
FIRM_API void set_array_order(ir_type *array, size_t dimension, size_t order);

/** Returns the order of an array dimension. */
FIRM_API size_t get_array_order(const ir_type *array, size_t dimension);

/** Find the array dimension that is placed at order order. */
FIRM_API size_t find_array_dimension(const ir_type *array, size_t order);

/** Sets the array element type. */
FIRM_API void set_array_element_type(ir_type *array, ir_type *tp);

/** Returns the array element type. */
FIRM_API ir_type *get_array_element_type(const ir_type *array);

/** Sets the array element entity. */
FIRM_API void set_array_element_entity(ir_type *array, ir_entity *ent);

/** Returns the array element entity. */
FIRM_API ir_entity *get_array_element_entity(const ir_type *array);

/**
 * Sets the array variable size flag.
 * If this flag is set then no upper/lower bounds need to be set and
 * get_type_size_bytes() returns -1
 */
FIRM_API void set_array_variable_size(ir_type *array, int variable_size_flag);

/**
 * Returns the array variable size flag.
 * @see set_array_variable_size()
 */
FIRM_API int is_array_variable_size(const ir_type *array);

/** Returns true if a type is an array type. */
FIRM_API int is_Array_type(const ir_type *array);

/**
 * This type opcode marks that the corresponding type is an array type.
 *
 * Consequently it contains a list of dimensions (lower and upper bounds)
 * and an element type.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_array;
/** Returns type opcode for array type. @see type_array */
FIRM_API const tp_op *get_tpop_array(void);

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

/** Creates a new type pointer with debug information. */
FIRM_API ir_type *new_d_type_pointer(ir_type *points_to, type_dbg_info* db);


/** Sets the type to which a pointer points to. */
FIRM_API void set_pointer_points_to_type(ir_type *pointer, ir_type *tp);

/** Returns the type to which a pointer points to. */
FIRM_API ir_type *get_pointer_points_to_type(const ir_type *pointer);

/** Returns true if a type is a pointer type. */
FIRM_API int is_Pointer_type(const ir_type *pointer);

/** Returns the first pointer type that has as points_to tp.
 *  Not efficient: O(\#types).
 *  If not found returns firm_unknown_type. */
FIRM_API ir_type *find_pointer_type_to_type(ir_type *tp);

/**
 * This type opcode marks that the corresponding type is a pointer type.
 *
 * It contains a reference to the type the pointer points to.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_pointer;
/** Returns type opcode for pointer type. @see type_pointer */
FIRM_API const tp_op *get_tpop_pointer(void);

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

/** Creates a new primitive type with debug information. */
FIRM_API ir_type *new_d_type_primitive(ir_mode *mode, type_dbg_info* db);

/** Returns true if a type is a primitive type. */
FIRM_API int is_Primitive_type(const ir_type *primitive);

/**
 * This type opcode marks that the corresponding type is a primitive type.
 *
 * Primitive types are types that are directly mapped to target machine
 * modes.
 * This struct is dynamically allocated but constant for the lifetime
 * of the library.
 */
FIRM_API const tp_op *type_primitive;
/** Returns type opcode for primitive type. @see type_primitive */
FIRM_API const tp_op *get_tpop_primitive(void);

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
/**
 * The code type is used to mark pieces of code (basic blocks)
 */
FIRM_API const tp_op *tpop_code;
/** Returns type opcode for code type. @see tpop_code */
FIRM_API const tp_op *get_tpop_code_type(void);
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
/**
 * This type opcode is an auxiliary opcode dedicated to support type analyses.
 *
 * Types with this opcode represents that there could be a type, but it is not
 * known.  This type can be used to initialize fields before an analysis (not known
 * yet) or to represent the top of a lattice (could not be determined).  There exists
 * exactly one type with this opcode.
 */
FIRM_API const tp_op *tpop_unknown;
/** Returns type opcode for unknown type. @see tpop_unknown */
FIRM_API const tp_op *get_tpop_unknown(void);
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
FIRM_API size_t get_compound_member_index(const ir_type *tp, ir_entity *member);

/** Remove a member from a compound type. */
FIRM_API void remove_compound_member(ir_type *compound, ir_entity *entity);

/**
 * Sets the variable size flag of a compound type.
 * The last member of a variable size compound type may be an array type
 * without explicit size. So the get_type_size_bytes() of a variable size
 * compound type only returns a minimum size for the type (the size if the
 * last members size is 0)
 */
FIRM_API void set_compound_variable_size(ir_type *compound, int variable_size);

/**
 * Returns the variable size flag. @see set_compound_variable_size()
 */
FIRM_API int is_compound_variable_size(const ir_type *compound);

/**
 * layout members of a struct/union or class type in a default way.
 */
FIRM_API void default_layout_compound_type(ir_type *tp);

/**
 * Checks whether a type is a compound type.
 *
 * @param tp - any type
 *
 * @return true if the type is class, structure, union or array type.
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
 * Sets entity links from old frame entities to new onces and
 * vice versa.
 */
FIRM_API ir_type *clone_frame_type(ir_type *type);

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
FIRM_API ir_entity *frame_alloc_area(ir_type *frame_type, int size,
                                     unsigned alignment, int at_start);

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
typedef void type_walk_func(type_or_ent tore, void *env);

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
FIRM_API void type_walk_super(type_walk_func *pre, type_walk_func *post,
                              void *env);

/** Same as type_walk_super2sub, but visits only class types.
   Executes pre for a class if all superclasses have been visited.
   Then iterates to subclasses.  Executes post after return from
   subclass.
   Does not visit global type, frame types.
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

/**
 * If we have the closed world assumption, we can calculate the
 * finalization of classes and entities by inspecting the class hierarchy.
 * After this is done, all classes and entities that are not overridden
 * anymore have the final property set.
 */
FIRM_API void types_calc_finalization(void);

/** @deprecated */
FIRM_API ir_visibility get_type_visibility(const ir_type *tp);
/** @deprecated */
FIRM_API void set_type_visibility(ir_type *tp, ir_visibility v);

/** @} */

/** @} */

#include "end.h"

#endif
