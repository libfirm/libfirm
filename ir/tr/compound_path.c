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
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>
#include <assert.h>

#include "firm_types.h"
#include "typerep.h"
#include "compound_path_t.h"
#include "xmalloc.h"
#include "type_t.h"
#include "entity_t.h"
#include "irgraph_t.h"
#include "ircons.h"

compound_graph_path *new_compound_graph_path(ir_type *tp, int length)
{
	compound_graph_path *res;

	assert(is_compound_type(tp) || is_Array_type(tp));
	assert(length > 0);

	res = xmalloc(sizeof(*res) + (length-1) * sizeof(res->list[0]));
	memset(res, 0, sizeof(*res) + (length-1) * sizeof(res->list[0]));
	res->kind = k_ir_compound_graph_path;
	res->tp   = tp;
	res->len  = length;

	return res;
}

void free_compound_graph_path(compound_graph_path *gr)
{
	assert(gr && is_compound_graph_path(gr));
	gr->kind = k_BAD;
	free(gr);
}

int is_compound_graph_path(const void *thing)
{
	return get_kind(thing) == k_ir_compound_graph_path;
}

int is_proper_compound_graph_path(compound_graph_path *gr, int pos)
{
	int i;
	ir_entity *node;
	ir_type *owner = gr->tp;

	for (i = 0; i <= pos; i++) {
		node = get_compound_graph_path_node(gr, i);
		if (node == NULL)
			/* Path not yet complete. */
			return 1;
		if (get_entity_owner(node) != owner)
			return 0;
		owner = get_entity_type(node);
	}
	if (pos == get_compound_graph_path_length(gr))
		if (!is_atomic_type(owner))
			return 0;
		return 1;
}

int get_compound_graph_path_length(const compound_graph_path *gr)
{
	assert(gr && is_compound_graph_path(gr));
	return gr->len;
}

ir_entity *get_compound_graph_path_node(const compound_graph_path *gr, int pos)
{
	assert(gr && is_compound_graph_path(gr));
	assert(pos >= 0 && pos < gr->len);
	return gr->list[pos].node;
}

void set_compound_graph_path_node(compound_graph_path *gr, int pos,
                                  ir_entity *node)
{
	assert(gr && is_compound_graph_path(gr));
	assert(pos >= 0 && pos < gr->len);
	assert(is_entity(node));
	gr->list[pos].node = node;
	assert(is_proper_compound_graph_path(gr, pos));
}

int get_compound_graph_path_array_index(const compound_graph_path *gr, int pos)
{
	assert(gr && is_compound_graph_path(gr));
	assert(pos >= 0 && pos < gr->len);
	return gr->list[pos].index;
}

void set_compound_graph_path_array_index(compound_graph_path *gr, int pos,
                                         int index)
{
	assert(gr && is_compound_graph_path(gr));
	assert(pos >= 0 && pos < gr->len);
	gr->list[pos].index = index;
}

ir_type *get_compound_graph_path_type(const compound_graph_path *gr)
{
	assert(gr && is_compound_graph_path(gr));
	return gr->tp;
}

static void allocate_values(ir_entity *ent)
{
	if (ent->attr.cmpd_attr.values == NULL) {
		ent->attr.cmpd_attr.values = NEW_ARR_F(ir_node*, 0);
		assert(ent->attr.cmpd_attr.val_paths == NULL);
		ent->attr.cmpd_attr.val_paths = NEW_ARR_F(compound_graph_path*, 0);
	}
}

void add_compound_ent_value_w_path(ir_entity *ent, ir_node *val,
                                   compound_graph_path *path)
{
	assert(is_compound_entity(ent));
	assert(is_compound_graph_path(path));
	allocate_values(ent);
	ARR_APP1(ir_node *, ent->attr.cmpd_attr.values, val);
	ARR_APP1(compound_graph_path *, ent->attr.cmpd_attr.val_paths, path);
}

void set_compound_ent_value_w_path(ir_entity *ent, ir_node *val,
                                   compound_graph_path *path, int pos)
{
	assert(is_compound_entity(ent));
	assert(is_compound_graph_path(path));
	assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.values));
	ent->attr.cmpd_attr.values[pos]    = val;
	ent->attr.cmpd_attr.val_paths[pos] = path;
}

compound_graph_path *get_compound_ent_value_path(const ir_entity *ent, int pos)
{
	assert(is_compound_entity(ent));
	assert(ent->initializer == NULL);
	assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.val_paths));
	return ent->attr.cmpd_attr.val_paths[pos];
}

/**
 * Returns non-zero, if two compound_graph_pathes are equal
 *
 * @param path1            the first path
 * @param path2            the second path
 */
static int equal_paths(compound_graph_path *path1, compound_graph_path *path2)
{
	int i;
	int len1 = get_compound_graph_path_length(path1);
	int len2 = get_compound_graph_path_length(path2);

	if (len2 != len1) return 0;

	for (i = 0; i < len1; i++) {
		ir_type *tp;
		ir_entity *node1 = get_compound_graph_path_node(path1, i);
		ir_entity *node2 = get_compound_graph_path_node(path2, i);

		if (node1 != node2) return 0;

		tp = get_entity_owner(node1);
		if (is_Array_type(tp)) {
			int index1 = get_compound_graph_path_array_index(path1, i);
			int index2 = get_compound_graph_path_array_index(path2, i);
			if (index1 != index2)
				return 0;
		}
	}
	return 1;
}

/**
 * Returns the position of a value with the given path.
 * The path must contain array indices for all array element entities.
 *
 * @todo  This implementation is very slow (O(number of initializers * |path|)
 *        and should be replaced when the new tree oriented
 *        value representation is finally implemented.
 */
static int get_compound_ent_pos_by_path(const ir_entity *ent,
                                        compound_graph_path *path)
{
	int i, n_paths = get_compound_ent_n_values(ent);

	for (i = 0; i < n_paths; i ++) {
		compound_graph_path *gr = get_compound_ent_value_path(ent, i);
		if (equal_paths(gr, path))
			return i;
	}
	return -1;
}

ir_node *get_compound_ent_value_by_path(const ir_entity *ent,
                                        compound_graph_path *path)
{
	int pos = get_compound_ent_pos_by_path(ent, path);
	if (pos >= 0)
		return get_compound_ent_value(ent, pos);
	return NULL;
}

void remove_compound_ent_value(ir_entity *ent, ir_entity *value_ent)
{
	int i, n;
	assert(is_compound_entity(ent));

	n = ARR_LEN(ent->attr.cmpd_attr.val_paths);
	for (i = 0; i < n; ++i) {
		compound_graph_path *path = ent->attr.cmpd_attr.val_paths[i];
		if (path->list[path->len-1].node == value_ent) {
			for (; i < n - 1; ++i) {
				ent->attr.cmpd_attr.val_paths[i] = ent->attr.cmpd_attr.val_paths[i+1];
				ent->attr.cmpd_attr.values[i]    = ent->attr.cmpd_attr.values[i+1];
			}
			ARR_SETLEN(ir_entity*, ent->attr.cmpd_attr.val_paths, n - 1);
			ARR_SETLEN(ir_node*,   ent->attr.cmpd_attr.values,    n - 1);
			break;
		}
	}
}

void add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member)
{
	compound_graph_path *path;
	ir_type *owner_tp = get_entity_owner(member);
	assert(is_compound_entity(ent));
	allocate_values(ent);
	path = new_compound_graph_path(get_entity_type(ent), 1);
	path->list[0].node = member;
	if (is_Array_type(owner_tp)) {
		int max;
		int i, n;

		assert(get_array_n_dimensions(owner_tp) == 1 && has_array_lower_bound(owner_tp, 0));
		max = get_array_lower_bound_int(owner_tp, 0) -1;
		for (i = 0, n = get_compound_ent_n_values(ent); i < n; ++i) {
			int index = get_compound_graph_path_array_index(get_compound_ent_value_path(ent, i), 0);
			if (index > max) {
				max = index;
			}
		}
		path->list[0].index = max + 1;
	}
	add_compound_ent_value_w_path(ent, val, path);
}

ir_entity *get_compound_ent_value_member(const ir_entity *ent, int pos)
{
	compound_graph_path *path;
	assert(is_compound_entity(ent));
	path = get_compound_ent_value_path(ent, pos);

	return get_compound_graph_path_node(path, get_compound_graph_path_length(path)-1);
}

void set_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member,
                            int pos)
{
	compound_graph_path *path;
	assert(is_compound_entity(ent));
	path = get_compound_ent_value_path(ent, pos);
	set_compound_graph_path_node(path, 0, member);
	set_compound_ent_value_w_path(ent, val, path, pos);
}

void set_array_entity_values(ir_entity *ent, tarval **values, int num_vals)
{
	int i;
	ir_type  *arrtp = get_entity_type(ent);
	ir_node  *val;
	ir_type  *elttp = get_array_element_type(arrtp);
	ir_graph *irg = get_const_code_irg();

	assert(is_Array_type(arrtp));
	assert(get_array_n_dimensions(arrtp) == 1);
	/* One bound is sufficient, the number of constant fields makes the
	   size. */
	assert(get_array_lower_bound (arrtp, 0) || get_array_upper_bound (arrtp, 0));

	for (i = 0; i < num_vals; i++) {
		val = new_r_Const_type(irg, values[i], elttp);
		add_compound_ent_value(ent, val, get_array_element_entity(arrtp));
		set_compound_graph_path_array_index(get_compound_ent_value_path(ent, i), 0, i);
	}
}

unsigned get_compound_ent_value_offset_bytes(const ir_entity *ent, int pos)
{
	compound_graph_path *path;
	int path_len, i;
	unsigned offset = 0;
	ir_type *curr_tp;

	assert(get_type_state(get_entity_type(ent)) == layout_fixed);

	path     = get_compound_ent_value_path(ent, pos);
	path_len = get_compound_graph_path_length(path);
	curr_tp  = path->tp;

	for (i = 0; i < path_len; ++i) {
		if (is_Array_type(curr_tp)) {
			ir_type *elem_type = get_array_element_type(curr_tp);
			unsigned size      = get_type_size_bytes(elem_type);
			unsigned align     = get_type_alignment_bytes(elem_type);
			int      idx;

			assert(size > 0);
			if (size % align > 0) {
				size += align - (size % align);
			}
			idx = get_compound_graph_path_array_index(path, i);
			assert(idx >= 0);
			offset += size * idx;
			curr_tp = elem_type;
		} else {
			ir_entity *node = get_compound_graph_path_node(path, i);
			offset += get_entity_offset(node);
			curr_tp = get_entity_type(node);
		}
	}

	return offset;
}

unsigned get_compound_ent_value_offset_bit_remainder(const ir_entity *ent,
                                                     int pos)
{
	compound_graph_path *path;
	int path_len;
	ir_entity *last_node;

	assert(get_type_state(get_entity_type(ent)) == layout_fixed);

	path      = get_compound_ent_value_path(ent, pos);
	path_len  = get_compound_graph_path_length(path);
	last_node = get_compound_graph_path_node(path, path_len - 1);

	if (last_node == NULL)
		return 0;

	return get_entity_offset_bits_remainder(last_node);
}

int get_compound_ent_n_values(const ir_entity *ent)
{
	assert(ent->initializer == NULL);
	assert(is_compound_entity(ent));
	allocate_values((ir_entity*) ent);
	return ARR_LEN(ent->attr.cmpd_attr.values);
}

ir_node *get_compound_ent_value(const ir_entity *ent, int pos)
{
	assert(is_compound_entity(ent));
	assert(ent->initializer == NULL);
	assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.values));
	return skip_Id(ent->attr.cmpd_attr.values[pos]);
}

int entity_has_compound_ent_values(const ir_entity *entity)
{
	if (!is_compound_entity(entity))
		return 0;

	return entity->attr.cmpd_attr.values != NULL;
}
