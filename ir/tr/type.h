/*
 * (C) 2001 by Universitaet Karlsruhe
 */

/**
 *
 *  @file type.h
 *
 *  Datastructure to hold type information.
 *
 *  @author Goetz Lindenmaier
 *
 *  This module supplies a datastructure to represent all types
 *  known in the compiled program.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
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
 * @see  tpop.h
 */

/* $Id$ */

# ifndef _TYPE_H_
# define _TYPE_H_

# include "tpop.h"
# include "firm_common.h"
# include "ident.h"
# include "irmode.h"
# include <stdbool.h>
# include "dbginfo.h"


/* to resolve recursion between entity.h and type.h */
#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct entity entity;
#endif

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/**
 *  An abstract data type to represent types.
 *
 *  This is the abstract data type with which any type known in the
 *  compiled program can be represented.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  These are distinguished by a type opcode.
 *  A type is described by a set of attributes.  Some of these attributes
 *  are common to all types, others depend on the kind of the type.
 *
 *  The following describes the common attributes.  They can only be
 *  accessed by the functions given below.
 *
 *  The common fields are:
 *
 *  - firm_kind: A firm_kind tag containing k_type.  This is useful
 *               for dynamically checking whether a node is a type node.
 *  - type_op:   A tp_op specifying the kind of the type.
 *  - mode:      The mode to be used to represent the type on a machine.
 *  - name:      An identifier specifying the name of the type.  To be
 *               set by the frontend.
 *  - size:      The size of the type, i.e. an entity of this type will
 *               occupy size bytes in memory.  In several cases this is
 *               determined when fixing the layout of this type (class,
 *               struct, union, array, enumeration).
 *  - state:     The state of the type.  The state represents whether the
 *               layout of the type is undefined or fixed (values: layout_undefined
 *               or layout_fixed).  Compound types can have an undefined
 *               layout.  The layout of the basic types primitive and pointer
 *               is always layout_fixed.  If the layout of
 *               compound types is fixed all entities must have an offset
 *               and the size of the type must be set.
 *               A fixed layout for enumeration types means that each enumeration
 *               is associated with an implementation value.
 *  - visit:     A counter for walks of the type information.
 *  - link:      A void* to associate some additional information with the type.
 *
 *  These fields can only be accessed via access functions.
 *
 *  Depending on the value of @c type_op, i.e., depending on the kind of the
 *  type the adt contains further attributes.  These are documented below.
 *
 *  @see
 *
 *  @link class_type class @endlink, @link struct_type struct @endlink,
 *  @link method_type method @endlink, @link union_type union @endlink,
 *  @link array_type array @endlink, @link enumeration_type enumeration @endlink,
 *  @link pointer_type pointer @endlink, @link primitive_type primitive @endlink
 *
 *  @todo
 *  	mode maybe not global field??
 */
#ifndef _TYPE_TYPEDEF_
#define _TYPE_TYPEDEF_
typedef struct type type;
#endif

# include "type_or_entity.h"

/** Frees the memory used by the type.   Does not free the entities
   belonging to the type, except for the array element entity.  */
void        free_type(type *tp);

tp_op*      get_type_tpop(type *tp);
ident*      get_type_tpop_nameid(type *tp);
const char* get_type_tpop_name(type *tp);
tp_opcode   get_type_tpop_code(type *tp);

ident*      get_type_ident(type *tp);
void        set_type_ident(type *tp, ident* id);
const char* get_type_name(type *tp);

/** The state of a type layout. */
typedef enum {
  layout_undefined,    /**< The layout of this type is not defined.
			  Address computation to access fields is not
			  possible, fields must be accessed by Sel
			  nodes.  This is the default value except for
			  pointer, primitive and method types. */
  layout_fixed         /**< The layout is fixed, all component/member entities
			  have an offset assigned.  Size of the type is known.
			  Arrays can be accessed by explicit address
			  computation. Default for pointer, primitive ane method
			  types.  */
} type_state;

/** Returns the type layout state of a type. */
type_state  get_type_state(type *tp);

/** Sets the type layout state of a type.
 *
 * For primitives, pointer and method types the layout is always fixed.
 * This call is legal but has no effect.
 */
void        set_type_state(type *tp, type_state state);

/** Returns the mode of a type.
 *
 * Returns NULL for all non atomic types.
 */
ir_mode*    get_type_mode(type *tp);

/** Sets the mode of a type.
 *
 * Only has an effect on primitive and enumeration types.
 */
void        set_type_mode(type *tp, ir_mode* m);

/** Returns the size of a type. */
int         get_type_size(type *tp);

/** Sets the size of a type.
 *
 * For primitive, enumeration, pointer and method types the size
 * is always fixed. This call is legal but has no effect.
 */
void        set_type_size(type *tp, int size);


unsigned long get_type_visited(type *tp);
void          set_type_visited(type *tp, unsigned long num);
/* Sets visited field in type to type_visited. */
void          mark_type_visited(type *tp);
/* @@@ name clash!! bool          type_visited(type *tp); */
bool          type_not_visited(type *tp);

void*         get_type_link(type *tp);
void          set_type_link(type *tp, void *l);

/**
 * Visited flag to traverse the type information.
 *
 * Increase this flag by one before traversing the type information.
 * Mark type nodes as visited by set_type_visited(type, type_visited).
 * Check whether node was already visited by comparing get_type_visited(type)
 * and type_visited.
 * Or use the function to walk all types.
 *
 * @see  typewalk
 */
extern unsigned long type_visited;
void          set_master_type_visited(unsigned long val);
unsigned long get_master_type_visited(void);
void          inc_master_type_visited(void);

/**
 * Checks whether a pointer points to a type.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a type, else false
 */
int is_type            (void *thing);

/**
 *   Checks whether two types are structural equal.
 *
 *   @param st pointer type
 *   @param lt pointer type
 *
 *   @return
 *    true if the types are equal, else false.
 *    Types are equal if :
 *    - they are the same type kind
 *    - they have the same name
 *    - they have the same mode (if applicable)
 *    - they have the same type_state and, ev., the same size
 *    - they are class types and have
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
 *       This is to avoid endless recursions; with pointer types circlic
 *       type graphs are possible.)
 */
bool equal_type(type *tpy1, type *typ2);

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
bool smaller_type (type *st, type *lt);

/**
 *  @page class_type	Representation of a class type
 *
 *  If the type opcode is set to type_class the type represents class
 *  types.  A list of fields and methods is associated with a class.
 *  Further a class can inherit from and bequest to other classes.
 *  @@@ value class???
 *  The following attributes are private to this type kind:
 *  - member:     All entities belonging to this class.  This are methode entities
 *                which have type_method or fields that can have any of the
 *                following type kinds: type_class, type_struct, type_union,
 *                type_array, type_enumeration, type_pointer, type_primitive.
 *
 *  The following two are dynamic lists that can be grown with an "add_" function,
 *  but not shrinked:
 *
 *  - subtypes:   A list of direct subclasses.
 *
 *  - supertypes: A list of direct superclasses.
 *
 *  - peculiarity: The peculiarity of this class.  If the class is of peculiarity
 *                 "description" it only is a description of requirememts to a class,
 *                 as, e.g., a Java interface.  The class will never be allocated.
 *                 Peculiatity inherited is only possible for entities.  An entity
 *                 is of peculiarity inherited if the compiler generated the entity
 *                 to explicitly resolve inheritance.  An inherited method entity has
 *                 no value for irg.
 *                 Values: description, existent, inherited.  Default: existent.
 *
 */

/** Creates a new class type. */
type   *new_type_class (ident *name);

/** Creates a new class type with debug information. */
type   *new_d_type_class (ident *name, dbg_info *db);

/* --- manipulate private fields of class type  --- */

/** Adds the entity as member of the class.  */
void    add_class_member   (type *clss, entity *member);

/** Returns the number of members of this class. */
int     get_class_n_members (type *clss);

/** Returns the member at position pos, 0 <= pos < n_member */
entity *get_class_member   (type *clss, int pos);

/** Returns index of mem in clss, -1 if not contained. */
int     get_class_member_index(type *clss, entity *mem);

/** Overwrites the member at position pos, 0 <= pos < n_member with
   the passed entity. */
void    set_class_member   (type *clss, entity *member, int pos);

/** Replaces complete member list in class type by the list passed.
   Copies the list passed. This function is necessary to reduce the number of members.
   members is an array of entities, num the size of this array.  Sets all
   owners of the members passed to clss. */
void    set_class_members  (type *clss, entity *members[], int arity);

/** Finds member in the list of members and removes it.
   Shrinks the member list, so iterate from the end!!!
   Does not deallocate the entity.  */
void    remove_class_member(type *clss, entity *member);


/** Adds subtype as subtype to clss.
   Checks whether clss is a supertype of subtype.  If not
   adds also clss as supertype to subtype.  */
void    add_class_subtype   (type *clss, type *subtype);

/** Returns the number of subtypes */
int     get_class_n_subtypes (type *clss);

/** Gets the subtype at position pos, 0 <= pos < n_subtype. */
type   *get_class_subtype   (type *clss, int pos);

/** Sets the subtype at positioin pos, 0 <= pos < n_subtype.
   Does not set the corresponding supertype relation for subtype: this might
   be a different position! */
void    set_class_subtype   (type *clss, type *subtype, int pos);

/** Finds subtype in the list of subtypes and removes it  */
void    remove_class_subtype(type *clss, type *subtype);


/** Adds supertype as supertype to class.
   Checks whether clss is a subtype of supertype.  If not
   adds also clss as subtype to supertype.  */
void    add_class_supertype   (type *clss, type *supertype);

/** Returns the number of supertypes */
int     get_class_n_supertypes (type *clss);

/** Returns the index of an supertype in a type. */
int     get_class_supertype_index(type *clss, type *super_clss);

/** Gets the supertype at position pos,  0 <= pos < n_supertype. */
type   *get_class_supertype   (type *clss, int pos);

/** Sets the supertype at postition pos, 0 <= pos < n_subtype.
   Does not set the corresponding subtype relation for supertype: this might
   be a different position! */
void    set_class_supertype   (type *clss, type *supertype, int pos);

/** Finds supertype in the list of supertypes and removes it */
void    remove_class_supertype(type *clss, type *supertype);

/** This enumeration flags the peculiarity of entities and types. */
typedef enum peculiarity {
  description,     /**< Represents only a description.  The entity/type is never
	  	      allocated, no code/data exists for this entity/type. */
  inherited,       /**< Describes explicitly that other entities are
 		      inherited to the owner of this entity.
 		      Overwrites must refer to at least one other
 		      entity.  If this is a method entity there exists
 		      no irg for this entity, only for one of the
 		      overwritten ones. */
  existent         /**< The entity/type (can) exist. */
} peculiarity;

/* The peculiarity of the class.  The enumeration peculiarity is defined
   in entity.h */
INLINE peculiarity get_class_peculiarity (type *clss);
INLINE void        set_class_peculiarity (type *clss, peculiarity pec);

/* Set and get a class' dfn --
   @todo This is an undocumented field, subject to change! */
void set_class_dfn (type *clss, int dfn);
int  get_class_dfn (type *clss);

/** Returns true if a type is a class type. */
bool is_class_type(type *clss);

/** Returns true if low is subclass of high. */
bool is_subclass_of(type *low, type *high);

/**
 *  @page struct_type	Representation of a struct type
 *
 *  Type_strct represents aggregate types that consist of a list
 *  of fields.
 *  The following attributes are private to this type kind:
 *  - member:  All entities belonging to this class.  This are the fields
 *             that can have any of the following types:  type_class,
 *             type_struct, type_union, type_array, type_enumeration,
 *  	       type_pointer, type_primitive.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 */
/** Creates a new type struct */
type   *new_type_struct (ident *name);
/** Creates a new type struct with debug information. */
type   *new_d_type_struct (ident *name, dbg_info* db);

/* manipulate private fields of struct */
void    add_struct_member   (type *strct, entity *member);
int     get_struct_n_members (type *strct);
entity *get_struct_member   (type *strct, int pos);
void    set_struct_member   (type *strct, int pos, entity *member);

/** Finds member in the list of members and removes it. */
void    remove_struct_member (type *strct, entity *member);

/** Returns true if a type is a struct type. */
bool    is_struct_type(type *strct);

/**
 * @page method_type	Representation of a method type
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

/* These makros define the suffixes for the types and entities used
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
type *new_type_method (ident *name, int n_param, int n_res);

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
type *new_d_type_method (ident *name, int n_param, int n_res, dbg_info* db);

/* -- manipulate private fields of method. -- */

/** Returns the number of parameters of this method. */
int   get_method_n_params  (type *method);

/** Returns the type of the parameter at position pos of a method. */
type *get_method_param_type(type *method, int pos);
/** Sets the type of the parameter at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_param_type(type *method, int pos, type* tp);
/* Returns an entity that represents the copied value argument.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
entity *get_method_value_param_ent(type *method, int pos);

int   get_method_n_ress   (type *method);
type *get_method_res_type(type *method, int pos);
/** Sets the type of the result at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_res_type(type *method, int pos, type* tp);
/* Returns an entity that represents the copied value result.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
entity *get_method_value_res_ent(type *method, int pos);

/**
 * this enum flags the variadicity of methods (methods with a
 * variable amount of arguments (e.g. C's printf). Default is
 * non_variadic.
 */
typedef enum variadicity {
  non_variadic,		/**< non variadic */
  variadic		/**< variadic */
} variadicity;

/** Returns the variadicity of a method. */
variadicity get_method_variadicity(type *method);

/** Sets the variadicity of a method. */
void set_method_variadicity(type *method, variadicity vari);

/** Returns true if a type is a method type. */
bool  is_method_type     (type *method);

/**
 *   @page union_type	Representation of a union type.
 *
 *   The union type represents union types.
 *   - n_types:     Number of unioned types.
 *   - members:     Entities for unioned types.  Fixed length array.
 *                  This is a dynamic list that can be grown with an "add_" function,
 *                  but not shrinked.
 */
/** Creates a new type union. */
type   *new_type_union (ident *name);

/** Creates a new type union with debug information. */
type   *new_d_type_union (ident *name, dbg_info* db);

/* --- manipulate private fields of struct --- */

/** Returns the number of unioned types of this union */
int     get_union_n_members      (type *uni);

/** Adds a new entity to a union type */
void    add_union_member (type *uni, entity *member);

/** Returns the entity at position pos of a union */
entity *get_union_member (type *uni, int pos);

/** Overwrites a entity at position pos in a union type. */
void    set_union_member (type *uni, int pos, entity *member);

/** Finds member in the list of members and removes it. */
void    remove_union_member (type *uni, entity *member);

/** Returns true if a type is a union type. */
bool    is_union_type          (type *uni);

/**
 * @page array_type	Representation of an array type
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
 *   to select a dimension and not a single element in case of multidim arrays.
 */

/** Create a new type array.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 */
type *new_type_array         (ident *name, int n_dimensions,
			      type *element_type);

/** Create a new type array with debug information.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 */
type *new_d_type_array         (ident *name, int n_dimensions,
			      type *element_type, dbg_info* db);

/* --- manipulate private fields of array type --- */

/** Returns the number of array dimensions of this type. */
int   get_array_n_dimensions (type *array);

/** Allocates Const nodes of mode_I for the array dimensions */
void  set_array_bounds_int   (type *array, int dimension, int lower_bound,
                                                          int upper_bound);
void  set_array_bounds       (type *array, int dimension, ir_node *lower_bound,
                                                          ir_node *upper_bound);
void  set_array_lower_bound  (type *array, int dimension, ir_node *lower_bound);
void  set_array_lower_bound_int (type *array, int dimension, int lower_bound);
void  set_array_upper_bound  (type *array, int dimension, ir_node *upper_bound);
void  set_array_upper_bound_int (type *array, int dimension, int lower_bound);
/* returns true is lower bound != Unknown */
int       has_array_lower_bound  (type *array, int dimension);
ir_node * get_array_lower_bound  (type *array, int dimension);
int       has_array_upper_bound  (type *array, int dimension);
ir_node * get_array_upper_bound  (type *array, int dimension);

void set_array_order (type *array, int dimension, int order);
int  get_array_order (type *array, int dimension);

void  set_array_element_type (type *array, type *tp);
type *get_array_element_type (type *array);

void  set_array_element_entity (type *array, entity *ent);
entity *get_array_element_entity (type *array);

/** Returns true if a type is an array type. */
bool   is_array_type         (type *array);

/**
 * @page enumeration_type	Representation of an enumeration type
 *
 * Enumeration types need not necessarily be represented explicitly
 * by Firm types, as the frontend can lower them to integer constants as
 * well.  For debugging purposes or similar tasks this information is useful.
 *
 * - *enum:         The target values representing the constants used to
 *                  represent individual enumerations.
 * - *enum_nameid:  Idents containing the source program name of the enumeration
 *  	     constants
 */
/** Create a new type enumeration -- set the enumerators independently. */
type   *new_type_enumeration    (ident *name, int n_enums);

/** Create a new type enumeration with debug information -- set the enumerators independently. */
type   *new_d_type_enumeration    (ident *name, int n_enums, dbg_info* db);

/* --- manipulate fields of enumeration type. --- */

/** Returns the number of enumeration values of this enumeration */
int     get_enumeration_n_enums (type *enumeration);

/** Sets the enumeration value at a given position. */
void    set_enumeration_enum    (type *enumeration, int pos, tarval *con);

/** Returns the enumeration value at a given position. */
tarval *get_enumeration_enum    (type *enumeration, int pos);

/** Assign an ident to an enumeration value at a given position. */
void    set_enumeration_nameid  (type *enumeration, int pos, ident *id);

/** Returns the assigned ident of an enumeration value at a given position. */
ident  *get_enumeration_nameid  (type *enumeration, int pos);

/** Returns the assigned name of an enumeration value at a given position. */
const char *get_enumeration_name(type *enumeration, int pos);

/** Returns true if a type is a enumeration type. */
bool    is_enumeration_type     (type *enumeration);

/**
 * @page pointer_type	Representation of a pointer type
 *
 * The mode of the pointer type must be a mode_reference.
 *
 * Pointer types:
 * - points_to:      The type of the entity this pointer points to.
 */

/** Creates a new type pointer with mode mode_p. */
#define new_type_pointer(N, P) new_type_pointer_mode(N, P, mode_P)
//type *new_type_pointer           (ident *name, type *points_to);

/** Creates a new type pointer with given pointer mode. */
type *new_type_pointer_mode      (ident *name, type *points_to, ir_mode *ptr_mode);

/** Creates a new type pointer given pointer mode and with debug information. */
type *new_d_type_pointer         (ident *name, type *points_to, ir_mode *ptr_mode, dbg_info* db);

/* --- manipulate fields of type_pointer --- */

/** Sets the type to which a pointer points to. */
void  set_pointer_points_to_type (type *pointer, type *tp);

/** Returns the type to which a pointer points to. */
type *get_pointer_points_to_type (type *pointer);

/** Returns true if a type is a pointer type. */
bool  is_pointer_type            (type *pointer);

/**
 * @page primitive_type Representation of a primitive type
 *
 * Primitive types are types that represent indivisible data values that
 * map directly to modes.  They don't have a private attribute.  The
 * important information they carry is held in the common mode field.
*/
/** Creates a new primitive type. */
type *new_type_primitive (ident *name, ir_mode *mode);

/** Creates a new primitive type with debug information. */
type *new_d_type_primitive (ident *name, ir_mode *mode, dbg_info* db);

/** Returns true if a type is a primitive type. */
bool  is_primitive_type  (type *primitive);


/**
 *  Checks whether a type is atomic.
 *  @param tp - any type
 *  @return true if type is primitive, pointer or enumeration
 */
int is_atomic_type(type *tp);

/* --- Support for compound types --- */

/**
 * Gets the number of elements in a firm compound type.
 *
 * This is just a comforability function, because structs and
 * classes can often be treated be the same code, but they have
 * different access functions to their members.
 *
 * @param tp  The type (must be struct, union or class).
 *
 * @return Number of members in the compound type.
 */
int get_compound_n_members(type *tp);

/**
 * Gets the member of a firm compound type at position pos.
 *
 * @param tp  The type (must be struct, union or class).
 * @param pos The number of the member.
 *
 * @return The member entity at position pos.
 *
 * @see get_compound_n_members() for justifaction of existence.
 */
entity *get_compound_member(type *tp, int pos);

/**
 *  Checks whether a type is compound.
 *
 *  @param tp - any type
 *
 *  @return true if the type is class, structure, union or array type.
 */
int is_compound_type(type *tp);


/** Outputs a unique number for this type if libfirm is compiled for
   debugging, (configure with --enable-debug) else returns 0. */
INLINE long get_type_nr(type *tp);

# endif /* _TYPE_H_ */
