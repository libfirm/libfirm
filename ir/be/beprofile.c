/** vim: set sw=4 ts=4:
 * @file   beprofile.c
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 *
 * Code instrumentation and execution count profiling
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "list.h"
#include "pmap.h"

#include "irprintf.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "phiclass_t.h"
#include "iredges.h"
#include "execfreq.h"
#include "irvrfy.h"

#include <lpp/lpp.h>
#include <lpp/mps.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>
//#include <lc_pset.h>
//#include <libcore/lc_bitset.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "beabi.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"
#include "bepressurestat.h"

#include "bechordal_t.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#endif /* WITH_LIBCORE */

typedef struct _block_id_walker_data {
	tarval        **array;
	unsigned int    id;
	const ir_node  *symconst;
} block_id_walker_data;

static void
block_counter(ir_node * bb, void * data)
{
	unsigned int  *count = data;
	*count = *count + 1;
}

static unsigned int
count_blocks(const ir_graph * irg)
{
	unsigned int count = 0;

	irg_block_walk_graph(irg, block_counter, NULL, &count);
	return count;
}

/**
 * Instrument a block with code needed for profiling
 */
static void
instrument_block(const ir_node * bb, const ir_node * address, unsigned int id)
{
	ir_node  *load, *store, *offset, *add, *projm, *proji;

	offset = new_r_Add(get_irn_irg(bb), bb, address, new_Const_long(mode_Is, get_mode_size_bytes(mode_Iu)*id), mode_P);
	load = new_r_Load(get_irn_irg(bb), bb, new_NoMem(), offset, mode_Iu);
	projm = new_r_Proj(get_irn_irg(bb), bb, load, mode_M, 0);
	proji = new_r_Proj(get_irn_irg(bb), bb, load, mode_Iu, 2);
	add = new_r_Add(get_irn_irg(bb), bb, proji, new_Const_long(mode_Iu, 1), mode_Iu);
	store = new_r_Store(get_irn_irg(bb), bb, projm, offset, add);
	keep_alive(new_r_Proj(get_irn_irg(bb), bb, load, mode_M, 0));
}

/**
 * Generates a new irg which calls the initializer
 */
static ir_graph *
gen_initializer_irg(entity * bblock_id, entitiy * bblock_counts, entity * bblock_count)
{
	ir_graph  *irg;
	ir_node   *ins[3] = {bblock_id, bblock_counts, bblock_count};
	ident     *name = new_id_from_str("__firmprof_initializer");
	entity    *ent = new_entity(get_glob_type(), name, new_type_method(name, 0, 0));
	ir_node   *ret, *call;

	ident     *init_name = new_id_from_str("__init_firmprof");
	type      *init_type = new_type_method(init_name, 3, 0);
	/* TODO */
	set_method_param_type(init_type, 0, arrayptr);
	set_method_param_type(init_type, 1, arrayptr);
	set_method_param_type(init_type, 2, uint);
	entify    *init_ent = new_entity(get_glob_type(), init_name, init_type);

	irg = new_ir_graph(ent, 0);
	bb = get_cur_block();

	call = new_r_Call( irg,
			bb,							//ir_node *  	block,
			get_irn_initial_mem(irg),	//ir_node *  	store,
			ir_node *  	callee,
			3,							//int  	arity,
			ins,						//ir_node **  	in,
			ir_type *  	tp
			);

	ret = new_r_Return ( irg,
			bb,										//ir_node *  	block,
			new_r_Proj(irg, bb, call, mode_M, 0),	//ir_node *  	store,
			0,										//int  	arity,
			NULL									//ir_node **  	in
			);

	mature_immBlock(bb);

	add_immBlock_pred(get_irg_end_block(irg), ret);
	mature_immBlock(get_irg_end_block(irg));

	return irg;
}

static void
block_id_walker(ir_node * bb, void * data)
{
	block_id_walker_data *wd = data;

	*wd->array[wd->id] = new_tarval_from_long(get_irn_node_nr(bb), mode_Iu);
	instrument_block(bb, wd->symconst, wd->id);
	++wd->id;
}

void
be_profile_instrument(void)
{
	int            n, i;
	unsigned int   n_blocks = 0;
	ir_entity     *bblock_id, *bblock_counts, *bblock_count;
	ir_type       *array_type, *integer_type;
	tarval       **tarval_array;

	block_id_walker_data  wd;
	symconst_symbol       sym;

	integer_type = new_type_primitive(new_id_from_str("__uint"), mode_Iu);
	array_type = new_type_array(new_id_from_str("__block_info_array"), 1, integer_type);
	set_array_bounds_int(array_type, 0, 0, n_blocks);
	bblock_id = new_entity(get_glob_type(), new_id_from_str("__BLOCK_IDS"), array_type);
	bblock_counts = new_entity(get_glob_type(), new_id_from_str("__BLOCK_COUNTS"), array_type);
	bblock_count = new_entity(get_glob_type(), new_id_from_str("__N_BLOCKS"), integer_type);

	for (n = get_irp_n_irgs()-1; n>=0; --n) {
		ir_graph      *irg = get_irp_irg(n);

		n_blocks += count_blocks(irg);
	}

	/* initialize count array */
	tarval_array = alloca(sizeof(*null_array)*n_blocks);
	for(i=0; i<n_blocks; ++i) {
		null_array[i] = get_tarval_null(mode_Iu);
	}
	set_array_entitiy_values(bblock_counts, tarval_array, n_blocks);

	/* initialize the block count entity */
	set_atomic_ent_value(bblock_count, new_Const_long(mode_Iu, n_blocks));

	/* generate a symbolic constant pointing to the count array */
	sym.entity_p = bblock_count;
	wd.symconst = new_SymConst(sym, symconst_addr_ent);

	/* initialize block id array and instrument blocks */
	wd.array = tarval_array;
	wd.id = 0;
	for (n = get_irp_n_irgs()-1; n>=0; --n) {
		ir_graph      *irg = get_irp_irg(n);

		irg_block_walk_graph(irg, block_id_walker, null, &wd);
	}
	set_array_entitiy_values(bblock_id, tarval_array, n_blocks);

	gen_initializer_irg(bblock_id, bblock_counts, bblock_count);
}


void
be_profile_read(void)
{

}
