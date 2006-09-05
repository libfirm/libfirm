/** vim: set sw=4 ts=4:
 * @file   beprofile.c
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 * @cvs-id $Id$
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

#include "entity.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "iredges.h"
#include "execfreq.h"
#include "irvrfy.h"
#include "type.h"
#include "entity.h"

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "beabi.h"
#include "benode_t.h"
#include "beutil.h"
#include "ircons.h"
#include "irhooks.h"

#include "bechordal_t.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#endif /* WITH_LIBCORE */

#include "beprofile.h"

typedef struct _block_id_walker_data_t {
	tarval        **array;
	unsigned int    id;
	ir_node *symconst;
} block_id_walker_data_t;

typedef struct _execcount_t {
	unsigned int block;
	unsigned int count;
} execcount_t;

static int
cmp_execcount(const void * a, const void * b, size_t size)
{
	return ((execcount_t*)a)->block != ((execcount_t*)b)->block;
}

static void
block_counter(ir_node * bb, void * data)
{
	unsigned int  *count = data;
	*count = *count + 1;

}

static unsigned int
count_blocks(ir_graph * irg)
{
	unsigned int count = 0;

	irg_block_walk_graph(irg, block_counter, NULL, &count);
	return count;
}

/* keep the execcounts here because they are only read once per compiler run */
static set * profile = NULL;
static hook_entry_t hook;

/**
 * Instrument a block with code needed for profiling
 */
static void
instrument_block(ir_node * bb, ir_node * address, unsigned int id)
{
	ir_graph *irg = get_irn_irg(bb);
	ir_node *start_block = get_irg_start_block(irg);
	ir_node  *load, *store, *offset, *add, *projm, *proji;
	ir_node *cnst;

	if(bb == start_block || bb == get_irg_end_block(irg))
		return;

	cnst   = new_r_Const_long(irg, start_block, mode_Iu, get_mode_size_bytes(mode_Iu) * id);
	offset = new_r_Add(irg, bb, address, cnst, mode_P);
	load   = new_r_Load(irg, bb, new_NoMem(), offset, mode_Iu);
	projm  = new_r_Proj(irg, bb, load, mode_M, pn_Load_M);
	proji  = new_r_Proj(irg, bb, load, mode_Iu, pn_Load_res);
	cnst   = new_r_Const_long(irg, start_block, mode_Iu, 1);
	add    = new_r_Add(irg, bb, proji, cnst, mode_Iu);
	store  = new_r_Store(irg, bb, projm, offset, add);
	projm  = new_r_Proj(irg, bb, store, mode_M, pn_Store_M);
	keep_alive(projm);
}

/**
 * Generates a new irg which calls the initializer
 */
static ir_graph *
gen_initializer_irg(entity * ent_filename, entity * bblock_id, entity * bblock_counts, int n_blocks)
{
	ir_node *start_block;

	ir_node   *ins[4];
	ident     *name = new_id_from_str("__firmprof_initializer");
	entity    *ent  = new_entity(get_glob_type(), name, new_type_method(name, 0, 0));
	ir_node   *ret, *call, *symconst;
	symconst_symbol sym;

	ident     *init_name = new_id_from_str("__init_firmprof");
	ir_type   *init_type = new_type_method(init_name, 4, 0);
	ir_type   *uint, *uintptr, *string;
	entity    *init_ent;
	ir_graph  *irg;
	ir_node   *bb;
	ir_type   *empty_frame_type;

	set_entity_ld_ident(ent, name);

	uint    = new_type_primitive(new_id_from_str("__uint"), mode_Iu);
	uintptr = new_type_pointer(new_id_from_str("__uintptr"), uint, mode_P);
	string  = new_type_pointer(new_id_from_str("__charptr"), new_type_primitive(new_id_from_str("__char"), mode_Bs), mode_P);

	set_method_param_type(init_type, 0, string);
	set_method_param_type(init_type, 1, uintptr);
	set_method_param_type(init_type, 2, uintptr);
	set_method_param_type(init_type, 3, uint);
	init_ent = new_entity(get_glob_type(), init_name, init_type);
	set_entity_ld_ident(init_ent, init_name);

	irg = new_ir_graph(ent, 0);
	set_current_ir_graph(irg);
	empty_frame_type = get_irg_frame_type(irg);
	set_type_size_bytes(empty_frame_type, 0);

	bb = get_cur_block();

	start_block = get_irg_start_block(irg);

	sym.entity_p = init_ent;
	symconst     = new_r_SymConst(irg, start_block, sym, symconst_addr_ent);

	sym.entity_p = ent_filename;
	ins[0] = new_r_SymConst(irg, start_block, sym, symconst_addr_ent);
	sym.entity_p = bblock_id;
	ins[1] = new_r_SymConst(irg, start_block, sym, symconst_addr_ent);
	sym.entity_p = bblock_counts;
	ins[2] = new_r_SymConst(irg, start_block, sym, symconst_addr_ent);
	ins[3] = new_r_Const_long(irg, start_block, mode_Iu, n_blocks);

	call = new_r_Call(irg, bb, get_irg_initial_mem(irg), symconst, 4, ins, init_type);
	ret = new_r_Return(irg, bb, new_r_Proj(irg, bb, call, mode_M, pn_Call_M_regular), 0, NULL);
	mature_immBlock(bb);

	add_immBlock_pred(get_irg_end_block(irg), ret);
	mature_immBlock(get_irg_end_block(irg));

	irg_finalize_cons(irg);

	return irg;
}

static void
block_id_walker(ir_node * bb, void * data)
{
	block_id_walker_data_t *wd = data;

	wd->array[wd->id] = new_tarval_from_long(get_irn_node_nr(bb), mode_Iu);
	instrument_block(bb, wd->symconst, wd->id);
	++wd->id;
}

ir_graph *
be_profile_instrument(char * filename)
{
	int            n, i;
	unsigned int   n_blocks = 0;
	entity        *bblock_id, *bblock_counts, *ent_filename;
	ir_type       *array_type, *integer_type, *string_type, *character_type;
	tarval       **tarval_array, **tarval_string;
	int            filename_len = strlen(filename)+1;
	ident         *cur_ident;

	block_id_walker_data_t  wd;
	symconst_symbol sym;

	integer_type   = new_type_primitive(new_id_from_str("__uint"), mode_Iu);
	array_type     = new_type_array(new_id_from_str("__block_info_array"), 1, integer_type);
	set_array_bounds_int(array_type, 0, 0, n_blocks);

	character_type = new_type_primitive(new_id_from_str("__char"), mode_Bs);
	string_type    = new_type_array(new_id_from_str("__filename"), 1, character_type);
	set_array_bounds_int(string_type, 0, 0, filename_len);

	cur_ident      = new_id_from_str("__FIRMPROF__BLOCK_IDS");
	bblock_id      = new_entity(get_glob_type(), cur_ident, array_type);
	set_entity_ld_ident(bblock_id, cur_ident);
	set_entity_variability(bblock_id, variability_initialized);

	cur_ident      = new_id_from_str("__FIRMPROF__BLOCK_COUNTS");
	bblock_counts  = new_entity(get_glob_type(), cur_ident, array_type);
	set_entity_ld_ident(bblock_counts, cur_ident);
	set_entity_variability(bblock_counts, variability_initialized);

	cur_ident      = new_id_from_str("__FIRMPROF__FILE_NAME");
	ent_filename   = new_entity(get_glob_type(), cur_ident, string_type);
	set_entity_ld_ident(ent_filename, cur_ident);
	set_entity_variability(ent_filename, variability_initialized);

	for (n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);

		n_blocks += count_blocks(irg);
	}

	/* initialize count array */
	tarval_array = alloca(sizeof(*tarval_array) * n_blocks);
	for (i = 0; i < n_blocks; ++i) {
		tarval_array[i] = get_tarval_null(mode_Iu);
	}
	set_array_entity_values(bblock_counts, tarval_array, n_blocks);

	/* initialize function name string constant */
	tarval_string = alloca(sizeof(*tarval_string) * (filename_len));
	for (i = 0; i < filename_len; ++i) {
		tarval_string[i] = new_tarval_from_long(filename[i], mode_Bs);
	}
	set_array_entity_values(ent_filename, tarval_string, filename_len);


	/* initialize block id array and instrument blocks */
	wd.array = tarval_array;
	wd.id    = 0;
	for (n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);

		/* generate a symbolic constant pointing to the count array */
		sym.entity_p = bblock_counts;
		wd.symconst  = new_r_SymConst(irg, get_irg_start_block(irg), sym, symconst_addr_ent);

		irg_block_walk_graph(irg, block_id_walker, NULL, &wd);
	}
	set_array_entity_values(bblock_id, tarval_array, n_blocks);

	return gen_initializer_irg(ent_filename, bblock_id, bblock_counts, n_blocks);
}

static void
profile_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	if(is_Block(irn)) {
		fprintf(f, "profiled execution count: %u\n", be_profile_get_block_execcount(irn));
	}
}

static void
register_vcg_hook(void)
{
	memset(&hook, 0, sizeof(hook));
	hook.hook._hook_node_info = profile_node_info;
	register_hook(hook_node_info, &hook);
}

static void
unregister_vcg_hook(void)
{
	unregister_hook(hook_node_info, &hook);
}

/**
 * Reads the corresponding profile info file if it exists and returns a
 * profile info struct
 */
void
be_profile_read(char * filename)
{
	FILE   *f;
	char    buf[8];
	size_t  ret;

	f = fopen(filename, "r");
	if(f == NULL) {
		perror("opening of profile data failed");
		return;
	}

	/* check magic */
	ret = fread(buf, 8, 1, f);
	if(ret == 0 || strncmp(buf, "firmprof", 8) != 0) {
		return;
	}

	if(profile) be_profile_free();
	profile = new_set(cmp_execcount, 16);

	do {
		execcount_t  query;
		ret = fread(&query, sizeof(unsigned int), 2, f);

		if(ret != 2) break;

		set_insert(profile, &query, sizeof(query), query.block);
	} while(1);

	fclose(f);
	register_vcg_hook();
}

/**
 * Frees the profile info
 */
void
be_profile_free(void)
{
	if(profile) {
		unregister_vcg_hook();
		del_set(profile);
	}
}

/**
 * Tells whether profile module has aquired data
 */
int
be_profile_has_data(void)
{
	return (profile != NULL);
}

/**
 * Get block execution count as determined be profiling
 */
unsigned int
be_profile_get_block_execcount(const ir_node * block)
{
	execcount_t *ec, query;

	if(!profile)
		return 1;

	query.block = get_irn_node_nr(block);
	ec = set_find(profile, &query, sizeof(query), get_irn_node_nr(block));

	if(ec) {
		return ec->count;
	} else {
		return 1;
	}
}
