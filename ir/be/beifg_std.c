/**
 * @file   beifg_std.c
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include "beifg.h"

typedef struct _ifg_std_t ifg_std_t;

struct _ifg_std_t {
	const be_ifg_impl_t *impl;
	/* Members. */
};

static void ifg_std_free(void *self)
{
	free(self);
}

static int ifg_std_connected(void *self, const ir_node *a, const ir_node *b)
{
	ifg_std_t *ifg = self;
	return -1;
}

static int ifg_std_neighbours_arr(void *self, const ir_node *irn, ir_node **arr, size_t n)
{
	ifg_std_t *ifg = self;
	return -1;
}

static int ifg_std_neighbours_obst(void *self, const ir_node *irn, struct obstack *obst)
{
	ifg_std_t *ifg = self;
	return -1;
}

static int ifg_std_degree(void *self, const ir_node *irn)
{
	ifg_std_t *ifg = self;
	return -1;
}

static const be_ifg_impl_t ifg_std_impl = {
	ifg_std_free,
	ifg_std_connected,
	ifg_std_neighbours_arr,
	ifg_std_neighbours_obst,
	ifg_std_degree
};

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env)
{
	ifg_std_t *ifg = malloc(sizeof(*ifg));

	ifg->impl = &ifg_std_impl;

	/* Initialize members. */

	return (be_ifg_t *) ifg;
}
