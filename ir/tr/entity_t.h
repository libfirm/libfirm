/*10 2002/03/19 13:08:33
*  Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
*  All rights reserved.
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

/* $Id$ */

# ifndef _ENTITY_T_H_
# define _ENTITY_T_H_

# include "entity.h"

/** A path in a compund graph. */
struct compound_graph_path {
  firm_kind kind;       /**< dynamic type tag for compound graph path. */
  type *tp;
  int len;
  entity *nodes[1];
};

/** the type of an entity */
struct entity {
  firm_kind kind;       /**< dynamic type tag for entity. */
  ident *name;          /**< name of this entity */
  ident *ld_name;       /**< Unique name of this entity, i.e., the mangled
                           name.  If the field is read before written a default
                           magling is applies.  The name of the owner is prepended
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
  int  offset;          /**< Offset in byte for this entity.  Fixed when layout
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
  compound_graph_path **val_paths;    /**< paths corresponding to constant values. Only available if
                          variablility not uninitialized.  Must be set for variability constant */

  /* ------------- fields for entities owned by a class type ---------------*/
  entity **overwrites;  /**< A list of entities this entity overwrites. */
  entity **overwrittenby;  /**< A list of entities that overwrite this entity.  */

  /* ------------- fields for methods ---------------*/
  enum peculiarity peculiarity;
  ir_graph *irg;        /**< If (type == method_type) this is the corresponding irg.
			   The ir_graph constructor automatically sets this field.
			   Yes, it must be here. */
#ifdef DEBUG_libfirm
  int nr;             /**< a unique node number for each node to make output
			      readable. */
#endif
};

INLINE long get_entity_nr(entity *ent);

# endif /* _ENTITY_T_H_ */
