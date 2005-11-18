/**
 * @file   beifg.h
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEIFG_H
#define _BEIFG_H

#include "obst.h"
#include "bechordal_t.h"

typedef struct _be_ifg_impl_t 	be_ifg_impl_t;
typedef struct _be_ifg_t 				be_ifg_t;

struct _be_ifg_impl_t {
	void (*free)(void *self);

	int (*connected)(void *self, const ir_node *a, const ir_node *b);
	int (*neighbours_arr)(void *self, const ir_node *irn, ir_node **arr, size_t n);
	int (*neighbours_obst)(void *self, const ir_node *irn, struct obstack *obst);
	int (*degree)(void *self, const ir_node *irn);
};

struct _be_ifg_t {
	const be_ifg_impl_t *impl;
};

#define be_ifg_free(self) 										((self)->impl->free(self))
#define be_ifg_connected(self,a,b)						((self)->impl->connected(self, a, b))
#define be_ifg_neighbours_arr(self,irn,arr,n)	((self)->impl->neighbours_arr(self, irn, arr, n))
#define be_ifg_neighbours_obst(self,irn,obst)	((self)->impl->neighbours_arr(self, irn, obst))
#define be_ifg_degree(self,irn)								((self)->impl->degree(self, irn))

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env);

#endif /* _BEIFG_H */
