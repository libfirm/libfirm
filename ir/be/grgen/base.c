/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/base.c
 * Purpose:     Basic stuff for the firm graph rewriting module
 * Author:      Veit Batz
 * Created:		9. May 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

#include <stdlib.h>

#include "irnode.h"


#include "common_t.h"
#include "auxilary_t.h"
#include "base_t.h"
#include "match_t.h"
#include "grshooks_t.h"
#include "action_t.h"

/* maps opcodes to ops */
ir_op **_ext_grs_op_map;
/* maps modecodes to modes */
ir_mode **_ext_grs_mode_map;

/* a 2-dim flexible array telling wether an opcode belongs
 * to a sub op of the op given by a second opcode. usage:
 * _ext_grs_op_is_a[o1][o2] != 0   IFF   o1 inherits o2 */
//int *_ext_grs_op_is_a = NULL;
unsigned char *_ext_grs_op_is_a = NULL;
/* the width of the  */
int _ext_grs_op_is_a_width;

/* a flexible array of flexible arrays each containing all inheriting
 * firm ops of that firm op, the opcode of wich is the index entered
 * in the array of arrays
 * USAGE: _ext_grs_all_sub_ops_of_op[opcode]
 * this yields an flexible array of ptrs to firm ops inheriting
 * form the given opcodes op */
ir_op ***_ext_grs_all_sub_ops_of_op = NULL;
/* a flexible array of flexible arrays each containing all firm ops
 * a given firm op inherits from, the given opcode is the index entered
 * in the array of arrays
 * USAGE: _ext_grs_all_super_ops_of_op[opcode]
 * this yields an flexible array of ptrs to firm ops inheriting
 * form the given opcodes op */
ir_op ***_ext_grs_all_super_ops_of_op = NULL;

/* dimension of dynamic allocated 2-dim arrays in private data
 * area of firm graphs. These two arrays are the n_instances
 * and the node_list. Note that these two arrays have the same
 * dimension for all ir graphs. That means if the maximal
 * opcode/modecode exceeds the respsective dimension the
 * arrays of ALL ir graphs have to be reallcated (this is done
 * by the hooks of new ops and modes) */
int _ext_grs_irgpr_op_dim;
int _ext_grs_irgpr_mode_dim;

/* offset of custom data in ir nodes */
unsigned _ext_grs_private_ofs_n;
/* offset of custom data in ir graphs */
unsigned _ext_grs_private_ofs_g;
/* offset of custom data in ir edges */
unsigned _ext_grs_private_ofs_e;

/* maps ir op names (e.g. "Add") to ir ops (i.e. *ir_op) */
lc_pset *_ext_grs_op_name_table;
/* maps mode names (e.g. "M") to modes (i.e. *ir_mode) */
lc_pset *_ext_grs_mode_name_table;
/* remembers the maximum opcode present */
int _ext_grs_max_opcode = 0;
/* remembers the maximum modecode present */
int _ext_grs_max_modecode = 0;


/* op compare function for the hash table mapping op
 * names to the appropriate firm objects */
static int _op_cmp_func(const void *e1, const void *e2) {
	const char *s1 = get_op_name((ir_op *) e1);
	const char *s2 = get_op_name((ir_op *) e2);
	int ret = strcmp(s1, s2);
	assert ((ret != 0 || get_op_code((ir_op *) e1) == get_op_code((ir_op *) e1)) &&
		"found different ir ops with an equal op name");
	return ret;
}

/* mode compare function for the hash table mapping mode
 * names to the appropriate firm objects */
static int _mode_cmp_func(const void *e1, const void *e2) {
	const char *s1 = get_mode_name((ir_mode *) e1);
	const char *s2 = get_mode_name((ir_mode *) e2);
	int ret = strcmp(s1, s2);
	assert ((ret != 0 ||
		get_mode_modecode((ir_mode *) e1) == get_mode_modecode((ir_mode *) e1)) &&
		"found different ir modes with an equal mode name");
	return ret;
}


#define INITAL_OP_SPACE 500

/* activate the libfirm grs module */
void ext_grs_activate(void)
{
	printf("activate libFIRM grs plugin... ");
	/* private data of ir graphs, ir nodes and ir edges */
	_ext_grs_private_ofs_n =
		firm_register_additional_node_data(sizeof(ext_grs_irn_private_t));
	_ext_grs_private_ofs_g =
		register_additional_graph_data(sizeof(ext_grs_irg_private_t));
	_ext_grs_private_ofs_e =
		edges_register_private_data(sizeof(ext_grs_iredges_private_t));

	/* init fast computation of log_2 */
	_ext_grs_log_table_init();

	/* init the hash tables mapping op and mode names to the respective firm objects */
	_ext_grs_op_name_table = lc_pset_new(_op_cmp_func, 256);
	_ext_grs_mode_name_table = lc_pset_new(_mode_cmp_func, 64);

	/* init the flexible array representing the inheritance
	 * relation on firm ops */
	_ext_grs_op_is_a = malloc(INITAL_OP_SPACE * INITAL_OP_SPACE*sizeof(*_ext_grs_op_is_a));
	_ext_grs_op_is_a_width = INITAL_OP_SPACE;
	memset(_ext_grs_op_is_a, 0, INITAL_OP_SPACE*INITAL_OP_SPACE*sizeof(*_ext_grs_op_is_a));

	/* init the flexible array mapping op- and modecodes to ptrs */
	_ext_grs_op_map = NEW_ARR_F(ir_op*, INITAL_OP_SPACE);
	memset(_ext_grs_op_map, 0, INITAL_OP_SPACE * sizeof(*_ext_grs_op_map));
	_ext_grs_mode_map = NEW_ARR_F(ir_mode*, 100);
	memset(_ext_grs_mode_map, 0, 100 * sizeof(*_ext_grs_mode_map));

	/* init the flexible array of flexible arrays keeping arrays of
	 * all sub ops of the op with the respective index as opcode */
	_ext_grs_all_sub_ops_of_op = NEW_ARR_F(ir_op**, INITAL_OP_SPACE);
	memset (_ext_grs_all_sub_ops_of_op, 0, INITAL_OP_SPACE * sizeof(ir_op**));
	/* init the flexible array of flexible arrays keeping arrays of
	 * all sub ops of the op with the respective index as opcode */
	_ext_grs_all_super_ops_of_op = NEW_ARR_F(ir_op**, INITAL_OP_SPACE);
	memset (_ext_grs_all_super_ops_of_op, 0, INITAL_OP_SPACE* sizeof(ir_op**));

	/* dimension of the dynamically 2-dim arrays n_instances and
	 * node_list in the private date area of ir graphs. */
	_ext_grs_irgpr_op_dim = INITAL_OP_SPACE;
	_ext_grs_irgpr_mode_dim = 25;

	/* register firm some firm hooks */
	_ext_grs_register_hooks();

	/* init the matcher */
	_ext_grs_match_init();
	/* init the action part */
	_ext_grs_act_init();

	printf("done\n");
}

void ext_grs_finalize() {
	_ext_grs_act_finalize();
}

/**
 *  announce that firm op o1 inherits from firm op o2
 */
void ext_grs_appoint_heir(ir_op *o1, ir_op *o2) {

	/* check params */
	if (get_op_code(o2) > _ext_grs_max_opcode || get_op_code(o1) > _ext_grs_max_opcode) {
		printf("module ext/grs: ERROR in function ext_grs_appoint_heir()\n");
		printf("  Bad paramter: Unknown firm op.\n");
		return;
	}
	if (o1 == o2) {
		printf("module ext/grs: ERROR in function ext_grs_appoint_heir()\n");
		printf("  The parameters given are equal.\n");
		return;
	}

	/* the new entry into the inheritance table */
	_ext_grs_OP_IS_A(get_op_code(o1), get_op_code(o2)) = 1;
}

void ext_grs_inheritance_mature(void) {

	int i, j, k;

	for (i = 0; i <= _ext_grs_max_opcode; i++) {

		if(_ext_grs_all_sub_ops_of_op[i])
			DEL_ARR_F(_ext_grs_all_sub_ops_of_op[i]);
		_ext_grs_all_sub_ops_of_op[i] = NEW_ARR_F(ir_op *, 1);
		/* every op is subop of itself */
		_ext_grs_all_sub_ops_of_op[i][0] = _ext_grs_op_map[i];

		if(_ext_grs_all_super_ops_of_op[i])
			DEL_ARR_F(_ext_grs_all_super_ops_of_op[i]);
		_ext_grs_all_super_ops_of_op[i] = NEW_ARR_F(ir_op *, 1);
		/* every op is superop of itself */
		_ext_grs_all_super_ops_of_op[i][0] = _ext_grs_op_map[i];
	}

	/* compute the transitive closure of the modified table
	 * using the Floyd-Warshall-Algorithm */
	for (k = 0; k <= _ext_grs_max_opcode; k++)
		for (i = 0; i <= _ext_grs_max_opcode; i++)
			for (j = 0; j <= _ext_grs_max_opcode; j++)
				_ext_grs_OP_IS_A(i,j) = _ext_grs_OP_IS_A(i,j) ||
						(_ext_grs_OP_IS_A(i, k) && _ext_grs_OP_IS_A(k, j));

	for (i = 0; i <= _ext_grs_max_opcode; i++)
		for (j = 0; j <= _ext_grs_max_opcode; j++)
			if (_ext_grs_OP_IS_A(i,j)) {
				ARR_APP1(ir_op *, _ext_grs_all_sub_ops_of_op[j], _ext_grs_op_map[i]);
				ARR_APP1(ir_op *, _ext_grs_all_super_ops_of_op[i], _ext_grs_op_map[j]);
			}

}

ir_op *(ext_grs_lookup_op)(char *name) {
	return _ext_grs_lookup_op(name);
}

ir_mode *(ext_grs_lookup_mode)(char *name) {
	return _ext_grs_lookup_mode(name);
}






void ext_grs_enable_irg(ir_graph *irg)
{
	int i, j;
	ext_grs_irg_private_t *pr_g;

	if (!irg) {
		printf("module ext/grs: ERROR in function ext_grs_enable_matching()\n");
		printf("  Given ir graph was NULL.\n\n");
		return;
	}
	pr_g = _ext_grs_get_irg_private(irg);

	/* if matching is already enabled for this ir graph, do nothing */
	if (pr_g->matching_enabled) return;

	/* init the dyn array of the heads of all oplists */
	pr_g->node_list =
		malloc(_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim *
			sizeof(*(pr_g->node_list)));

	if (! pr_g->node_list) {
		printf("module ext/grs: Internal ERROR in function ext_grs_enable_matching()\n");
		printf("  Failed to allocate memory for internal data strucures.\n\n");
		printf("  Aborting!\n\n");
		abort();
	}

	for (i = 0; i < _ext_grs_irgpr_op_dim; i++)
		for (j = 0; j < _ext_grs_irgpr_mode_dim; j++)
			LC_INIT_LIST_HEAD( &_ext_grs_NODE_LIST(pr_g, i, j) );

	/* init the dyn array of number of instances of each pair (op,mode) */
	pr_g->n_instances =
		malloc(_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim *
			sizeof(*(pr_g->n_instances)));
	memset(pr_g->n_instances, 0,
		_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim *
			sizeof(*(pr_g->n_instances)));

	/* matching now enabled */
	pr_g->matching_enabled = 1;
}

void ext_grs_disable_irg(ir_graph *irg)
{
	ext_grs_irg_private_t *pr_g;

	if (!irg) {
		printf("module ext/grs: ERROR in function ext_grs_enable_matching()\n");
		printf("  Given ir graph was NULL.\n\n");
		return;
	}
	pr_g = _ext_grs_get_irg_private(irg);

	/* if matching is already disabled for this ir graph, do nothing */
	if (! pr_g->matching_enabled) return;

	free(pr_g->node_list);
	free(pr_g->n_instances);

	/* matching now disabled */
	pr_g->matching_enabled = 0;
}
