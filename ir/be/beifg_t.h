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

/**
 * Common use interference graph.
 * Originally written by Sebastian Hack. Refactored into a seperate
 * source file and header by Kimon Hoffmann.
 * @author Sebastian Hack
 * @date 27.06.2005
 */
#ifndef _BEIFG_T_H_
#define _BEIFG_T_H_

#include "beifg.h"

struct _be_ifg_impl_t {
	size_t nodes_iter_size;
	size_t neighbours_iter_size;
	size_t cliques_iter_size;

	void (*free)(void *self);
	int (*connected)(const void *self, const ir_node *a, const ir_node *b);

	ir_node *(*neighbours_begin)(const void *self, void *iter, const ir_node *irn);
	ir_node *(*neighbours_next)(const void *self, void *iter);
	void (*neighbours_break)(const void *self, void *iter);

	ir_node *(*nodes_begin)(const void *self, void *iter);
	ir_node *(*nodes_next)(const void *self, void *iter);
	void (*nodes_break)(const void *self, void *iter);

	int (*cliques_begin)(const void *self, void *iter, ir_node **buf);
	int (*cliques_next)(const void *self, void *iter);
	void (*cliques_break)(const void *self, void *iter);

	int (*degree)(const void *self, const ir_node *irn);
};

struct _be_ifg_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
};

#ifdef _BE_IFG_USE_MACROS

#define be_ifg_nodes_iter_size(self)              ((self)->impl->nodes_iter_size)
#define be_ifg_neighbours_iter_size(self)         ((self)->impl->neighbours_iter_size)
#define be_ifg_cliques_iter_size(self)            ((self)->impl->cliques_iter_size)

#define be_ifg_free(self)                         ((self)->impl->free(self))
#define be_ifg_connected(self,a,b)                ((self)->impl->connected(self, a, b))
#define be_ifg_neighbours_begin(self, iter, irn)  ((self)->impl->neighbours_begin(self, iter, irn))
#define be_ifg_neighbours_next(self, iter)        ((self)->impl->neighbours_next(self, iter))
#define be_ifg_neighbours_break(self, iter)       ((self)->impl->neighbours_break(self, iter))
#define be_ifg_nodes_begin(self, iter)            ((self)->impl->nodes_begin(self, iter))
#define be_ifg_nodes_next(self, iter)             ((self)->impl->nodes_next(self, iter))
#define be_ifg_nodes_break(self, iter)            ((self)->impl->nodes_break(self, iter))
#define be_ifg_cliques_begin(self, iter, buf)     ((self)->impl->cliques_begin(self, iter, buf))
#define be_ifg_cliques_next(self, iter)           ((self)->impl->cliques_next(self, iter))
#define be_ifg_cliques_break(self, iter)          ((self)->impl->cliques_break(self, iter))
#define be_ifg_degree(self,irn)                   ((self)->impl->degree(self, irn))

#endif

void be_ifg_check(const be_ifg_t *ifg);

#endif /*_BEIFG_T_H_*/
