/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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

/**
 * @file
 * @brief Deprecated way to initialize compound entites! (use ir_initializer
 *        stuff instead)
 * Declarations for functions and datastructures to represent compound
 * type initializers.
 */
#ifndef FIRM_COMPOUND_PATHS_H
#define FIRM_COMPOUND_PATHS_H

#include "firm_types.h"
#include "begin.h"

typedef struct compound_graph_path  compound_graph_path, *ir_compound_graph_path_ptr;

/**
 * @deprecated
 * Creates a new compound graph path of given length.
 */
FIRM_API compound_graph_path *new_compound_graph_path(ir_type *tp, int length);

/**
 * @deprecated
 * Returns non-zero if an object is a compound graph path
 */
FIRM_API int is_compound_graph_path(const void *thing);

/**
 * @deprecated
 * Frees a graph path object
 */
FIRM_API void free_compound_graph_path(compound_graph_path *gr);

/**
 * @deprecated
 * Returns the length of a graph path
 */
FIRM_API int get_compound_graph_path_length(const compound_graph_path *gr);

/**
 * @deprecated
 * Get the entity node of an compound graph path at position pos.
 */
ir_entity *get_compound_graph_path_node(const compound_graph_path *gr, int pos);

/**
 * @deprecated
 * Set the entity node of an compound graph path at position pos.
 */
FIRM_API void set_compound_graph_path_node(compound_graph_path *gr, int pos,
                                           ir_entity *node);

/**
 * @deprecated
 * Get the index of an compound graph path at position pos.
 */
FIRM_API int get_compound_graph_path_array_index(const compound_graph_path *gr, int pos);

/**
 * @deprecated
 * Set the index of an compound graph path at position pos.
 */
FIRM_API void set_compound_graph_path_array_index(compound_graph_path *gr, int pos, int index);

/**
 * @deprecated
 * Get the type of an compound graph path.
 */
FIRM_API ir_type *get_compound_graph_path_type(const compound_graph_path *gr);

/**
 * @deprecated
 * Checks whether the path up to pos is correct. If the path contains a NULL,
 *  assumes the path is not complete and returns non-zero.
 */
FIRM_API int is_proper_compound_graph_path(compound_graph_path *gr, int pos);

/**
 * @deprecated
 * A value of a compound entity is a pair of a value and the description of the
 * corresponding access path to the member of the compound.
 */
FIRM_API void add_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path);
FIRM_API void set_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path, int pos);

/**
 * @deprecated
 * Returns the access path for value at position pos.
 */
FIRM_API compound_graph_path *get_compound_ent_value_path(const ir_entity *ent, int pos);

/**
 * @deprecated
 * Returns a constant value given the access path.
 *  The path must contain array indices for all array element entities.
 */
FIRM_API ir_node *get_compound_ent_value_by_path(const ir_entity *ent,
                                                 compound_graph_path *path);

/**
 * @deprecated
 * Removes all constant entries where the path ends at value_ent. Does not
 * free the memory of the paths.  (The same path might be used for several
 * constant entities.
 */
FIRM_API void remove_compound_ent_value(ir_entity *ent, ir_entity *value_ent);

/**
 * @deprecated
 * Generates a Path with length 1.
 *  Beware: Has a bad runtime for array elements (O(|array|) and should be
 *  avoided there. Use add_compound_ent_value_w_path() instead and create
 *  the path manually.
 */
FIRM_API void add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member);

/**
 * @deprecated
 * Returns the last member in the path
 */
FIRM_API ir_entity *get_compound_ent_value_member(const ir_entity *ent, int pos);

/**
 * @deprecated
 * Sets the path at pos 0
 */
FIRM_API void set_compound_ent_value(ir_entity *ent, ir_node *val,
                                     ir_entity *member, int pos);

/**
 * @deprecated
 * Initializes the entity ent which must be of a one dimensional
 * array type with the values given in the values array.
 * The array must have a lower and an upper bound.  Keeps the
 * order of values. Does not test whether the number of values
 * fits into the given array size.  Does not test whether the
 * values have the proper mode for the array.
 */
FIRM_API void set_array_entity_values(ir_entity *ent, tarval **values, int num_vals);

/**
 * @deprecated
 * Return the offset in bits from the last byte address.
 *
 * This requires that the layout of all concerned types is fixed.
 *
 * @param ent Any entity of compound type with at least pos initialization values.
 * @param pos The position of the value for which the offset is requested.
 */
FIRM_API unsigned get_compound_ent_value_offset_bit_remainder(const ir_entity *ent, int pos);

/**
 * @deprecated
 * Return the overall offset of value at position pos in bytes.
 *
 * This requires that the layout of all concerned types is fixed.
 * Asserts if bit offset is not byte aligned.
 *
 * @param ent Any entity of compound type with at least pos initialization values.
 * @param pos The position of the value for which the offset is requested.
 */
FIRM_API unsigned get_compound_ent_value_offset_bytes(const ir_entity *ent, int pos);

/**
 * @deprecated
 * Returns the number of constant values needed to initialize the entity.
 * Asserts if the entity has variability_uninitialized.
 */
FIRM_API int get_compound_ent_n_values(const ir_entity *ent);

/**
 * @deprecated
 * Returns a constant value given the position.
 */
FIRM_API ir_node *get_compound_ent_value(const ir_entity *ent, int pos);

/**
 * @deprecated
 * return 1 if entity has a compound_graph-style initializer
 */
FIRM_API int entity_has_compound_ent_values(const ir_entity *entity);

#include "end.h"

#endif
