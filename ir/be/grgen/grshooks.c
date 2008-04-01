/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/base.c
 * Purpose:     hook register stuff
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		9. May 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdlib.h>

#include "irhooks.h"
#include "common_t.h"
#include "base_t.h"
#include "irtools.h"

#include "grshooks_t.h"



// #define MAX(a, b) (((a)>=(b)) ? (a) : (b))



/* array of all hooks used by the matcher */
hook_entry_t _ext_grs_hooks[hook_last];


/* auxilary function reallocating the 2-dim dynamic arrays
 * n_instances and node_list in the private data area of
 * ir graphs */
static void realloc_all_irgpr(void)
{
	ir_graph *irg;
	int n_irgs = get_irp_n_allirgs();
	int i,j,k,opc,mc;
	ext_grs_irg_private_t *pr_g;

	int old_mode_dim = _ext_grs_irgpr_mode_dim;
	int old_op_dim = _ext_grs_irgpr_op_dim;

	_ext_grs_irgpr_op_dim = MAX(_ext_grs_max_opcode + 50, old_op_dim);
	_ext_grs_irgpr_mode_dim = MAX(_ext_grs_max_modecode + 50, old_mode_dim);


	for (i=0 ; i<n_irgs ; i++) {
		lc_list_t *old_node_list;
		int *old_n_ins;

		/* get the current irg */
		irg = get_irp_allirg(i);
		pr_g = _ext_grs_get_irg_private(irg);

		/* if matching not enabled, there are no data structres
		 * present which have to be reallocated */
		if (! pr_g->matching_enabled) continue;

		/* alloc a new node list array */
		old_node_list = pr_g->node_list;
		pr_g->node_list = malloc(
			_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim * sizeof(*(pr_g->node_list)));
		/* init the list heads */
		for (j = 0; j < _ext_grs_irgpr_op_dim; j++)
			for (k = 0; k < _ext_grs_irgpr_mode_dim; k++)
				LC_INIT_LIST_HEAD(& _ext_grs_NODE_LIST(pr_g, j, k));

		/* alloc a new array of instance counters */
		old_n_ins = pr_g->n_instances;
		pr_g->n_instances = malloc(
			_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim * sizeof(*(pr_g->n_instances)));
		memset(pr_g->n_instances, 0,
			_ext_grs_irgpr_op_dim * _ext_grs_irgpr_mode_dim * sizeof(*(pr_g->n_instances)));

		/* copy everything from the old to the new one */
		for (opc = 0; opc < old_op_dim; opc++)
			for (mc = 0; mc < old_mode_dim; mc++) {
				_ext_grs_NODE_LIST(pr_g, opc, mc) =
					old_node_list[old_mode_dim * opc + mc];
				_ext_grs_N_INSTANCES(pr_g, opc, mc) =
					old_n_ins[old_mode_dim * opc + mc];
			}
		free(old_node_list);
		free(old_n_ins);
	}
}




static void _hook_new_mode(void *context, const ir_mode *tmpl, ir_mode *mode)
{
	int old_len, new_len;
	modecode new_modecode = get_mode_modecode(mode);

	/* add pair (name, *ir_mode) to hash table */
	const char *name = get_mode_name(mode);
	int len = strlen(name);
	lc_pset_insert(_ext_grs_mode_name_table, mode, HASH_STR(name, len));

	if(new_modecode < 0)
		return;


	/* if new modecode is greater than any other modecode present */
	if (new_modecode > _ext_grs_max_modecode) {
		/* ensure that the flexible array is great enough */
		old_len = ARR_LEN(_ext_grs_mode_map);
		ARR_EXTO(*_ext_grs_mode_map, _ext_grs_mode_map, new_modecode + 1);
		new_len = ARR_LEN(_ext_grs_mode_map);
		if (new_len > old_len)
			memset(& _ext_grs_mode_map[old_len], 0,
				sizeof(*_ext_grs_mode_map) * (new_len - old_len) );
		/* remember the maximum opcode present */
		_ext_grs_max_modecode = new_modecode;
	}

	/* map modecodes to modes */
	if (_ext_grs_mode_map[new_modecode])
		printf("WARNING in module ext/grs: multiple modecode encountered: %s, %d\n",
			name, new_modecode);
/*	else printf("new modecode: %s, %d\n", name, new_modecode);*/

	_ext_grs_mode_map[new_modecode] = mode;

	/* if the new maximal opcode exceeds the dimension of the dynamic
	 * n_instance and the node_list array in they have to be allocated
	 * for ALL ir graphs present */
	if (_ext_grs_max_modecode >= _ext_grs_irgpr_mode_dim)
		realloc_all_irgpr();
}


static void _hook_new_ir_op(void *context, ir_op *op)
{
	ir_opcode new_opcode = get_op_code(op);

	/* add pair (name, *ir_op) to hash table */
	const char *name = get_op_name(op);
	int len = strlen(name);


	ir_op** op_array;

	lc_pset_insert(_ext_grs_op_name_table, op, HASH_STR(name, len));

	/* if array is to small... */
	if (new_opcode >= _ext_grs_op_is_a_width) {
		unsigned char *old_array = _ext_grs_op_is_a;
		int old_width = _ext_grs_op_is_a_width;
		int new_len;
		int i,j;

		/* resize array  >>>> BUGGY!!!! <<<<< */
		_ext_grs_op_is_a_width += 20;
		new_len = _ext_grs_op_is_a_width * _ext_grs_op_is_a_width; //(new_opcode+20) * (new_opcode+20);
		_ext_grs_op_is_a = malloc(new_len*sizeof(unsigned char));
		memset(_ext_grs_op_is_a, 0, new_len*sizeof(unsigned char));

		/* because of being 2-dim we have to copy the content ourselves */
		for (i = 0; i < old_width/*_ext_grs_op_is_a_width*/; i++)
			for (j = 0; j < old_width/*_ext_grs_op_is_a_width*/; j++)
				_ext_grs_OP_IS_A(i,j) =
					old_array[i * (old_width) + j];

		/* delete the old array */
		free(old_array);
	}

	/* assure that the arrays representing the op map and
	 * the sets of sub ops of a given op are great enough */
	/* if new opcode is greater than any other opcode present */
	if (new_opcode > _ext_grs_max_opcode) {
		/* ensure that the op map array is graet enough */
		int old_len, new_len;

		old_len = ARR_LEN(_ext_grs_op_map);
		ARR_EXTO(ir_op*, _ext_grs_op_map, new_opcode + 1);
		new_len = ARR_LEN(_ext_grs_op_map);
		if (new_len > old_len)
			memset( & _ext_grs_op_map[old_len], 0,
				sizeof(*_ext_grs_op_map) * (new_len - old_len) );

		/* ensure that the sub op array is great enough */
		old_len = ARR_LEN(_ext_grs_all_sub_ops_of_op);
		ARR_EXTO(ir_op**, _ext_grs_all_sub_ops_of_op, new_opcode + 1);
		new_len = ARR_LEN(_ext_grs_all_sub_ops_of_op);
		if (new_len > old_len)
			memset( & _ext_grs_all_sub_ops_of_op[old_len], 0,
				sizeof(*_ext_grs_all_sub_ops_of_op) * (new_len - old_len) );

		/* ensure that the super op array is great enough */
		old_len = ARR_LEN(_ext_grs_all_super_ops_of_op);
		ARR_EXTO(ir_op**, _ext_grs_all_super_ops_of_op, new_opcode + 1);
		new_len = ARR_LEN(_ext_grs_all_super_ops_of_op);
		if (new_len > old_len)
			memset( & _ext_grs_all_super_ops_of_op[old_len], 0,
				sizeof(*_ext_grs_all_super_ops_of_op) * (new_len - old_len) );

		/* remember the maximum opcode present */
		_ext_grs_max_opcode = new_opcode;
	}

	/* map opcodes to ops */
	if (_ext_grs_op_map[new_opcode] != NULL)
		printf("WARNING in module ext/grs: multiple opcode of op %s encountered: %d\n",
			name, new_opcode);
/*	else printf("new opcode of op %s encountered: %d\n", name, new_opcode);*/

	_ext_grs_op_map[new_opcode] = op;

	/* if the new maximal opcode exceeds the dimension of the dynamic
	 * n_instance and the node_list array in they have to be allocated
	 * for ALL ir graphs present */
	if (_ext_grs_max_opcode >= _ext_grs_irgpr_op_dim)
		realloc_all_irgpr();

	/* every op is a subop of itself */
	op_array = _ext_grs_all_sub_ops_of_op[new_opcode];
	if (op_array == NULL) /* this is because of multiple opcodes */ {
		op_array = NEW_ARR_F(ir_op*, 1);
		op_array[0] = op;
		/* if ptr has changed... */
		_ext_grs_all_sub_ops_of_op[new_opcode] = op_array;
	}

	/* every op is a superop of itself */
	op_array = _ext_grs_all_super_ops_of_op[new_opcode];
	if (op_array == NULL) /* this is because of multiple opcodes */ {
		op_array = NEW_ARR_F(ir_op*, 1);
		op_array[0] = op;
		/* if ptr has changed... */
		_ext_grs_all_super_ops_of_op[new_opcode] = op_array;
	}
}


void _ext_grs_register_hooks(void) {
	/* simplify hook registrtation */
	#define HOOK(h, fkt) \
  		_ext_grs_hooks[h].hook._##h = fkt; register_hook(h, &_ext_grs_hooks[h])

	/* register op creation hook */
	HOOK(hook_new_ir_op, _hook_new_ir_op);
	/* register mode creation hooks */
	HOOK(hook_new_mode, _hook_new_mode);
	/* HOOK(hook_new_vector_mode, _hook_new_vector_mode);*/

	#undef HOOK
}
