/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Loop datastructure and access functions -- private stuff.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 * @version  $Id: irloop_t.h 17143 2008-01-02 20:56:33Z beck $
 */
#include "config.h"

#include <string.h>
#include <stdlib.h>

#include "irloop_t.h"
#include "irprog_t.h"
#include "error.h"

void add_loop_son(ir_loop *loop, ir_loop *son)
{
	loop_element lson;
	assert(loop && loop->kind == k_ir_loop);
	assert(get_kind(son) == k_ir_loop);
	lson.son = son;
	ARR_APP1(loop_element, loop->children, lson);
	++loop->n_sons;
	loop->flags |= loop_outer_loop;
}

void add_loop_node(ir_loop *loop, ir_node *n)
{
	loop_element ln;
	ln.node = n;
	assert(loop && loop->kind == k_ir_loop);
	ARR_APP1(loop_element, loop->children, ln);
	loop->n_nodes++;
}

void add_loop_irg(ir_loop *loop, ir_graph *irg)
{
	loop_element ln;
	ln.irg = irg;
	assert(loop && loop->kind == k_ir_loop);
	ARR_APP1(loop_element, loop->children, ln);
	loop->n_nodes++;
}

/**
 * Mature all loops by removing the flexible arrays of a loop.
 *
 * @param loop  the loop to mature
 * @param obst  an obstack, where the new arrays are allocated on
 */
void mature_loops(ir_loop *loop, struct obstack *obst)
{
	loop_element *new_children = DUP_ARR_D(loop_element, obst, loop->children);
	DEL_ARR_F(loop->children);
	loop->children = new_children;

	if (loop->n_sons > 0) {
		/* we have child loops, mature them */
		int i;

		for (i = ARR_LEN(new_children) - 1; i >= 0; --i) {
			loop_element child = new_children[i];

			if (*child.kind == k_ir_loop) {
				mature_loops(child.son, obst);
			}
		}
	}
}

/* Returns outer loop, itself if outermost. */
ir_loop *(get_loop_outer_loop)(const ir_loop *loop)
{
	return _get_loop_outer_loop(loop);
}

/* Returns nesting depth of this loop */
unsigned (get_loop_depth)(const ir_loop *loop)
{
	return _get_loop_depth(loop);
}

/* Returns the number of inner loops */
size_t (get_loop_n_sons)(const ir_loop *loop)
{
	return _get_loop_n_sons(loop);
}

/* Returns the pos`th loop_node-child              *
 * TODO: This method isn`t very efficient !        *
 * Returns NULL if there isn`t a pos`th loop_node */
ir_loop *get_loop_son(ir_loop *loop, size_t pos)
{
	size_t child_nr = 0;
	size_t loop_nr = 0;

	assert(loop && loop->kind == k_ir_loop);
	for (child_nr = 0; child_nr < ARR_LEN(loop->children); ++child_nr) {
		if (*(loop->children[child_nr].kind) != k_ir_loop)
			continue;
		if (loop_nr == pos)
			return loop->children[child_nr].son;
		loop_nr++;
	}
	return NULL;
}

/* Returns the number of nodes in the loop */
size_t get_loop_n_nodes(const ir_loop *loop)
{
	assert(loop);
	assert(loop->kind == k_ir_loop);
	return loop->n_nodes;
}

/* Returns the pos'th ir_node-child                *
 * TODO: This method isn't very efficient !        *
 * Returns NULL if there isn't a pos'th ir_node   */
ir_node *get_loop_node(const ir_loop *loop, size_t pos)
{
	size_t node_nr = 0;
	size_t child_nr;

	assert(loop && loop->kind == k_ir_loop);
	assert(pos < get_loop_n_nodes(loop));

	for (child_nr = 0; child_nr < ARR_LEN(loop->children); ++child_nr) {
		if (*(loop->children[child_nr].kind) != k_ir_node)
			continue;
		if (node_nr == pos)
			return loop -> children[child_nr].node;
		node_nr++;
	}
	panic("no child at pos found");
}

/* Returns the number of elements contained in loop.  */
size_t get_loop_n_elements(const ir_loop *loop)
{
	assert(loop && loop->kind == k_ir_loop);
	return(ARR_LEN(loop->children));
}

loop_element get_loop_element(const ir_loop *loop, size_t pos)
{
	assert(loop && loop->kind == k_ir_loop && pos < ARR_LEN(loop->children));
	return(loop -> children[pos]);
}

size_t get_loop_element_pos(const ir_loop *loop, void *le)
{
	size_t n;
	size_t i;
	assert(loop && loop->kind == k_ir_loop);

	n = get_loop_n_elements(loop);
	for (i = 0; i < n; i++)
		if (get_loop_element(loop, i).node == le)
			return i;
	return (size_t)-1;
}


/**
 * Sets the loop for a node.
 */
void set_irn_loop(ir_node *n, ir_loop *loop)
{
	n->loop = loop;
}

ir_loop *(get_irn_loop)(const ir_node *n)
{
	return _get_irn_loop(n);
}

long get_loop_loop_nr(const ir_loop *loop)
{
	assert(loop && loop->kind == k_ir_loop);
#ifdef DEBUG_libfirm
	return loop->loop_nr;
#else
	return (long)loop;
#endif
}

void set_loop_link(ir_loop *loop, void *link)
{
	assert(loop && loop->kind == k_ir_loop);
	loop->link = link;
}

void *get_loop_link(const ir_loop *loop)
{
	assert(loop && loop->kind == k_ir_loop);
	return loop->link;
}

int (is_ir_loop)(const void *thing)
{
	return _is_ir_loop(thing);
}

/* The outermost loop is remarked in the surrounding graph. */
void (set_irg_loop)(ir_graph *irg, ir_loop *loop)
{
	_set_irg_loop(irg, loop);
}

/* Returns the root loop info (if exists) for an irg. */
ir_loop *(get_irg_loop)(const ir_graph *irg)
{
	return _get_irg_loop(irg);
}

/*
 * Allocates a new loop as son of father on the given obstack.
 * If father is equal NULL, a new root loop is created.
 */
ir_loop *alloc_loop(ir_loop *father, struct obstack *obst)
{
	ir_loop *son;

	son = OALLOCZ(obst, ir_loop);
	son->kind     = k_ir_loop;
	son->children = NEW_ARR_F(loop_element, 0);
	son->n_nodes  = 0;
	son->n_sons   = 0;
	son->link     = NULL;
	if (father) {
		son->outer_loop = father;
		add_loop_son(father, son);
		son->depth = father->depth + 1;
	} else {  /* The root loop */
		son->outer_loop = son;
		son->depth      = 0;
	}

#ifdef DEBUG_libfirm
	son->loop_nr = get_irp_new_node_nr();
#endif

	return son;
}
