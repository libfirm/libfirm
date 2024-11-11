/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Loop data structure and access functions -- private stuff.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 */
#include "irloop_t.h"

#include "irprog_t.h"
#include <stdlib.h>

void add_loop_son(ir_loop *loop, ir_loop *son)
{
	assert(loop->kind == k_ir_loop);
	assert(get_kind(son) == k_ir_loop);
	loop_element lson;
	lson.son = son;
	ARR_APP1(loop_element, loop->children, lson);
}

void add_loop_node(ir_loop *loop, ir_node *n)
{
	assert(loop->kind == k_ir_loop);
	loop_element ln;
	ln.node = n;
	ARR_APP1(loop_element, loop->children, ln);
}

void add_loop_irg(ir_loop *loop, ir_graph *irg)
{
	assert(loop->kind == k_ir_loop);
	loop_element ln;
	ln.irg = irg;
	ARR_APP1(loop_element, loop->children, ln);
}

void mature_loops(ir_loop *loop, struct obstack *obst)
{
	loop_element *new_children = DUP_ARR_D(loop_element, obst, loop->children);
	DEL_ARR_F(loop->children);
	loop->children = new_children;

	/* mature child loops */
	for (size_t i = ARR_LEN(new_children); i-- > 0;) {
		loop_element child = new_children[i];

		if (*child.kind == k_ir_loop) {
			mature_loops(child.son, obst);
		}
	}
}

ir_loop *(get_loop_outer_loop)(const ir_loop *loop)
{
	return _get_loop_outer_loop(loop);
}

unsigned (get_loop_depth)(const ir_loop *loop)
{
	return _get_loop_depth(loop);
}

size_t get_loop_n_elements(const ir_loop *loop)
{
	assert(loop->kind == k_ir_loop);
	return ARR_LEN(loop->children);
}

loop_element get_loop_element(const ir_loop *loop, size_t pos)
{
	assert(loop->kind == k_ir_loop && pos < ARR_LEN(loop->children));
	return loop->children[pos];
}

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
	assert(loop->kind == k_ir_loop);
#ifdef DEBUG_libfirm
	return loop->loop_nr;
#else
	return (long)loop;
#endif
}

void set_loop_link(ir_loop *loop, void *link)
{
	assert(loop->kind == k_ir_loop);
	loop->link = link;
}

void *get_loop_link(const ir_loop *loop)
{
	assert(loop->kind == k_ir_loop);
	return loop->link;
}

void (set_irg_loop)(ir_graph *irg, ir_loop *loop)
{
	_set_irg_loop(irg, loop);
}

ir_loop *(get_irg_loop)(const ir_graph *irg)
{
	return _get_irg_loop(irg);
}

ir_loop *alloc_loop(ir_loop *father, struct obstack *obst)
{
	ir_loop *son = OALLOCZ(obst, ir_loop);
	son->kind     = k_ir_loop;
	son->children = NEW_ARR_F(loop_element, 0);
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

static bool is_loop_variant(ir_loop *l, ir_loop *b)
{
	if (l == b)
		return true;

	for (size_t i = 0, n_elems = get_loop_n_elements(l); i < n_elems; ++i) {
		loop_element e = get_loop_element(l, i);
		if (is_ir_loop(e.kind) && is_loop_variant(e.son, b))
			return true;
	}

	return false;
}

int is_loop_invariant(const ir_node *n, const ir_node *block)
{
	ir_loop       *const l = get_irn_loop(block);
	ir_node const *const b = get_block_const(n);
	return !is_loop_variant(l, get_irn_loop(b));
}
