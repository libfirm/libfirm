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
 * @file
 * @brief       Code instrumentation and execution count profiling.
 * @author      Adam M. Szalkowski
 * @date        06.04.2006
 * @version     $Id$
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
#include "execfreq.h"
#include "typerep.h"

#include "dbginfo.h"
#include "irhooks.h"
#include "iredges.h"

#include "irprofile.h"

/** An entry in the id-to-location map */
typedef struct loc_entry {
	ir_entity    *fname;   /**< the entity holding the file name */
	unsigned int lineno;   /**< line number */
} loc_entry;

typedef struct _block_id_walker_data_t {
	tarval         **array;    /**< the entity the holds the block counts */
	unsigned int   id;         /**< current block id number */
	ir_node        *symconst;  /**< the SymConst representing array */
	pmap           *fname_map; /**< set containing all found filenames */
	loc_entry      *locs;      /**< locations */
	ir_type        *tp_char;   /**< the character type */
	unsigned       flags;      /**< profile flags */
} block_id_walker_data_t;

typedef struct _execcount_t {
	unsigned long block;
	unsigned int count;
} execcount_t;

/**
 * Compare two execcount_t entries.
 */
static int cmp_execcount(const void *a, const void *b, size_t size) {
	const execcount_t *ea = a;
	const execcount_t *eb = b;
	(void) size;
	return ea->block != eb->block;
}

/**
 * Block walker, count number of blocks.
 */
static void block_counter(ir_node * bb, void * data) {
	unsigned int *count = data;
	(void) bb;
	*count = *count + 1;
}

/**
 * Return the number of blocks the given graph.
 */
static unsigned int count_blocks(ir_graph *irg) {
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
instrument_block(ir_node *bb, ir_node *address, unsigned int id)
{
	ir_graph *irg = get_irn_irg(bb);
	ir_node  *start_block = get_irg_start_block(irg);
	ir_node  *load, *store, *offset, *add, *projm, *proji, *unknown;
	ir_node  *cnst;

	/**
	 * We can't instrument the end block as there are no real instructions there
	 */
	if(bb == get_irg_end_block(irg))
		return;

	unknown = new_r_Unknown(irg, mode_M);
	cnst    = new_r_Const_long(irg, start_block, mode_Iu, get_mode_size_bytes(mode_Iu) * id);
	offset  = new_r_Add(irg, bb, address, cnst, mode_P);
	load    = new_r_Load(irg, bb, unknown, offset, mode_Iu);
	projm   = new_r_Proj(irg, bb, load, mode_M, pn_Load_M);
	proji   = new_r_Proj(irg, bb, load, mode_Iu, pn_Load_res);
	cnst    = new_r_Const_long(irg, start_block, mode_Iu, 1);
	add     = new_r_Add(irg, bb, proji, cnst, mode_Iu);
	store   = new_r_Store(irg, bb, projm, offset, add);
	projm   = new_r_Proj(irg, bb, store, mode_M, pn_Store_M);
	set_irn_link(bb, projm);
	set_irn_link(projm, load);
}

typedef struct fix_env {
	ir_node *end_block;
} fix_env;

/**
 * SSA Construction for instrumentation code memory
 */
static void
fix_ssa(ir_node * bb, void * data)
{
	fix_env *env = data;
	ir_node *mem;
	int     arity = get_Block_n_cfgpreds(bb);

	/* start and end block are not instrumented, skip! */
	if (bb == env->end_block)
		return;

	if (bb == get_irg_start_block(get_irn_irg(bb))) {
		mem = new_NoMem();
	} else if (arity == 1) {
		mem = get_irn_link(get_Block_cfgpred_block(bb, 0));
	} else {
		int n;
		ir_node **ins;
		ir_graph *irg = current_ir_graph;

		NEW_ARR_A(ir_node*, ins, arity);
		for (n = arity - 1; n >= 0; --n) {
			ins[n] = get_irn_link(get_Block_cfgpred_block(bb, n));
		}
		mem = new_r_Phi(irg, bb, arity, ins, mode_M);
	}
	set_Load_mem(get_irn_link(get_irn_link(bb)), mem);
}


/**
 * Generates a new irg which calls the initializer
 *
 * Pseudocode:
 *	 void __firmprof_initializer(void) { __init_firmprof(ent_filename, bblock_id, bblock_counts, n_blocks); }
 */
static ir_graph *
gen_initializer_irg(ir_entity * ent_filename, ir_entity * bblock_id, ir_entity * bblock_counts, int n_blocks)
{
	ir_node *start_block;

	ir_node   *ins[4];
	ident     *name = new_id_from_str("__firmprof_initializer");
	ir_entity *ent  = new_entity(get_glob_type(), name, new_type_method(name, 0, 0));
	ir_node   *ret, *call, *symconst;
	symconst_symbol sym;

	ident     *init_name = new_id_from_str("__init_firmprof");
	ir_type   *init_type = new_type_method(init_name, 4, 0);
	ir_type   *uint, *uintptr, *string;
	ir_entity *init_ent;
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

/**
 * Create the location data for the given debug info.
 */
static void create_location_data(dbg_info *dbg, block_id_walker_data_t *wd)
{
	unsigned lineno;
	const char *fname = ir_retrieve_dbg_info(dbg, &lineno);

	if (fname) {
		pmap_entry *entry = pmap_find(wd->fname_map, (void *)fname);
		ir_entity  *ent;

		if (! entry) {
			static unsigned nr = 0;
			ident   *id;
			char    buf[128];
			ir_type *arr;
			int     i, len = strlen(fname) + 1;
			tarval  **tarval_string;

			snprintf(buf, sizeof(buf), "firm_name_arr.%d", nr);
			arr = new_type_array(new_id_from_str(buf), 1, wd->tp_char);
			set_array_bounds_int(arr, 0, 0, len);

			snprintf(buf, sizeof(buf), "__firm_name.%d", nr++);
			id = new_id_from_str(buf);
			ent = new_entity(get_glob_type(), id, arr);
			set_entity_ld_ident(ent, id);

			pmap_insert(wd->fname_map, (void *)fname, ent);

			/* initialize file name string constant */
			tarval_string = alloca(sizeof(*tarval_string) * (len));
			for (i = 0; i < len; ++i) {
				tarval_string[i] = new_tarval_from_long(fname[i], mode_Bs);
			}
			set_entity_variability(ent, variability_constant);
			set_array_entity_values(ent, tarval_string, len);
		} else {
			ent = entry->value;
		}
		wd->locs[wd->id].fname  = ent;
		wd->locs[wd->id].lineno = lineno;
	} else {
		wd->locs[wd->id].fname  = NULL;
		wd->locs[wd->id].lineno = 0;
	}
}

/**
 * Walker: assigns an ID to every block.
 * Builds the string table
 */
static void
block_id_walker(ir_node * bb, void * data)
{
	block_id_walker_data_t *wd = data;

	wd->array[wd->id] = new_tarval_from_long(get_irn_node_nr(bb), mode_Iu);
	instrument_block(bb, wd->symconst, wd->id);

	if (wd->flags & profile_with_locations) {
		dbg_info *dbg = get_irn_dbg_info(bb);
		create_location_data(dbg, wd);
	}
	++wd->id;
}

#define IDENT(x)	new_id_from_chars(x, sizeof(x) - 1)

ir_graph *
ir_profile_instrument(const char *filename, unsigned flags)
{
	int n, i;
	int n_blocks = 0;
	ir_entity *bblock_id;
	ir_entity *bblock_counts;
	ir_entity *ent_filename;
	ir_entity *ent_locations = NULL;
	ir_entity *loc_lineno = NULL;
	ir_entity *loc_name = NULL;
	ir_entity *ent;
	ir_type *array_type;
	ir_type *uint_type;
	ir_type *string_type;
	ir_type *character_type;
	ir_type *loc_type = NULL;
	ir_type *charptr_type;
	ir_type *gtp;
	ir_node *start_block;
	tarval **tarval_array;
	tarval **tarval_string;
	tarval *tv;
	int filename_len = strlen(filename)+1;
	ident *cur_ident;
	int align_l, align_n, size;
	ir_graph *rem;
	block_id_walker_data_t  wd;
	symconst_symbol sym;

	/* count the number of block first */
	for (n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);

		n_blocks += count_blocks(irg);
	}

	/* create all the necessary types and entities. Note that the
	   types must have a fixed layout, because we already running in the
	   backend */
	uint_type      = new_type_primitive(IDENT("__uint"), mode_Iu);
	set_type_alignment_bytes(uint_type, get_type_size_bytes(uint_type));

	array_type     = new_type_array(IDENT("__block_info_array"), 1, uint_type);
	set_array_bounds_int(array_type, 0, 0, n_blocks);
	set_type_size_bytes(array_type, n_blocks * get_mode_size_bytes(mode_Iu));
	set_type_alignment_bytes(array_type, get_mode_size_bytes(mode_Iu));
	set_type_state(array_type, layout_fixed);

	character_type = new_type_primitive(IDENT("__char"), mode_Bs);
	string_type    = new_type_array(IDENT("__filename"), 1, character_type);
	set_array_bounds_int(string_type, 0, 0, filename_len);
	set_type_size_bytes(string_type, filename_len);
	set_type_alignment_bytes(string_type, 1);
	set_type_state(string_type, layout_fixed);

	gtp            = get_glob_type();

	cur_ident      = IDENT("__FIRMPROF__BLOCK_IDS");
	bblock_id      = new_entity(gtp, cur_ident, array_type);
	set_entity_ld_ident(bblock_id, cur_ident);
	set_entity_variability(bblock_id, variability_initialized);

	cur_ident      = IDENT("__FIRMPROF__BLOCK_COUNTS");
	bblock_counts  = new_entity(gtp, cur_ident, array_type);
	set_entity_ld_ident(bblock_counts, cur_ident);
	set_entity_variability(bblock_counts, variability_initialized);

	cur_ident      = IDENT("__FIRMPROF__FILE_NAME");
	ent_filename   = new_entity(gtp, cur_ident, string_type);
	set_entity_ld_ident(ent_filename, cur_ident);

	if (flags & profile_with_locations) {
		loc_type       = new_type_struct(IDENT("__location"));
		loc_lineno     = new_entity(loc_type, IDENT("lineno"), uint_type);
		align_l        = get_type_alignment_bytes(uint_type);
		size           = get_type_size_bytes(uint_type);
		set_entity_offset(loc_lineno, 0);

		charptr_type   = new_type_pointer(IDENT("__charptr"), character_type, mode_P_data);
		align_n        = get_type_size_bytes(charptr_type);
		set_type_alignment_bytes(charptr_type, align_n);
		loc_name       = new_entity(loc_type, IDENT("name"), charptr_type);
		size           = (size + align_n - 1) & -align_n;
		set_entity_offset(loc_name, size);
		size          += align_n;

		if (align_n > align_l)
			align_l = align_n;
		size = (size + align_l - 1) & -align_l;
		set_type_size_bytes(loc_type, size);
		set_type_state(loc_type, layout_fixed);

		loc_type = new_type_array(IDENT("__locarray"), 1, loc_type);
		set_array_bounds_int(string_type, 0, 0, n_blocks);

		cur_ident      = IDENT("__FIRMPROF__LOCATIONS");
		ent_locations   = new_entity(gtp, cur_ident, loc_type);
		set_entity_ld_ident(ent_locations, cur_ident);
	}

	/* initialize count array */
	NEW_ARR_A(tarval *, tarval_array, n_blocks);
	tv = get_tarval_null(mode_Iu);
	for (i = 0; i < n_blocks; ++i) {
		tarval_array[i] = tv;
	}
	set_array_entity_values(bblock_counts, tarval_array, n_blocks);

	/* initialize function name string constant */
	tarval_string = alloca(sizeof(*tarval_string) * (filename_len));
	for (i = 0; i < filename_len; ++i) {
		tarval_string[i] = new_tarval_from_long(filename[i], mode_Bs);
	}
	set_entity_variability(ent_filename, variability_constant);
	set_array_entity_values(ent_filename, tarval_string, filename_len);

	/* initialize block id array and instrument blocks */
	wd.array     = tarval_array;
	wd.id        = 0;
	wd.tp_char   = character_type;
	wd.flags     = flags;
	if (flags & profile_with_locations) {
		wd.fname_map = pmap_create();
		NEW_ARR_A(loc_entry, wd.locs, n_blocks);
	}

	for (n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph      *irg = get_irp_irg(n);
		int            i;
		ir_node       *endbb = get_irg_end_block(irg);
		fix_env       env;

		set_current_ir_graph(irg);

		/* generate a symbolic constant pointing to the count array */
		sym.entity_p = bblock_counts;
		wd.symconst  = new_r_SymConst(irg, get_irg_start_block(irg), sym, symconst_addr_ent);

		irg_block_walk_graph(irg, block_id_walker, NULL, &wd);
		start_block = get_irg_start_block(irg);
		env.end_block   = get_irg_end_block(irg);
		irg_block_walk_graph(irg, fix_ssa, NULL, &env);
		for (i = get_Block_n_cfgpreds(endbb) - 1; i >= 0; --i) {
			ir_node *node = skip_Proj(get_Block_cfgpred(endbb, i));
			ir_node *bb   = get_Block_cfgpred_block(endbb, i);
			ir_node *sync;
			ir_node *ins[2];

			switch (get_irn_opcode(node)) {
			case iro_Return:
				ins[0] = get_irn_link(bb);
				ins[1] = get_Return_mem(node);
				sync   = new_r_Sync(irg, bb, 2, ins);
				set_Return_mem(node, sync);
				break;
			case iro_Raise:
				ins[0] = get_irn_link(bb);
				ins[1] = get_Raise_mem(node);
				sync   = new_r_Sync(irg, bb, 2, ins);
				set_Raise_mem(node, sync);
				break;
			default:
				/* a fragile's op exception. There should be another path to End,
				   so ignore it */
				assert(is_fragile_op(node) && "unexpected End control flow predecessor");
			}
		}
	}
	set_array_entity_values(bblock_id, tarval_array, n_blocks);

	if (flags & profile_with_locations) {
		/* build the initializer for the locations */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		ent = get_array_element_entity(loc_type);
		set_entity_variability(ent_locations, variability_constant);
		for (i = 0; i < n_blocks; ++i) {
			compound_graph_path *path;
			tarval *tv;
			ir_node *n;

			/* lineno */
			path = new_compound_graph_path(loc_type, 2);
			set_compound_graph_path_array_index(path, 0, i);
			set_compound_graph_path_node(path, 0, ent);
			set_compound_graph_path_node(path, 1, loc_lineno);
			tv = new_tarval_from_long(wd.locs[i].lineno, mode_Iu);
			add_compound_ent_value_w_path(ent_locations, new_Const(mode_Iu, tv), path);

			/* name */
			path = new_compound_graph_path(loc_type, 2);
			set_compound_graph_path_array_index(path, 0, i);
			set_compound_graph_path_node(path, 0, ent);
			set_compound_graph_path_node(path, 1, loc_name);
			if (wd.locs[i].fname) {
				sym.entity_p = wd.locs[i].fname;
				n = new_SymConst(sym, symconst_addr_ent);
			} else {
				n = new_Const(mode_P_data, get_mode_null(mode_P_data));
			}
			add_compound_ent_value_w_path(ent_locations, n, path);
		}
		pmap_destroy(wd.fname_map);
	}
	return gen_initializer_irg(ent_filename, bblock_id, bblock_counts, n_blocks);
}

static void
profile_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	(void) ctx;
	if(is_Block(irn)) {
		fprintf(f, "profiled execution count: %u\n", ir_profile_get_block_execcount(irn));
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
ir_profile_read(const char *filename)
{
	FILE   *f;
	char    buf[8];
	size_t  ret;

	f = fopen(filename, "r");
	if(f == NULL) {
		return;
	}
	printf("found profile data '%s'.\n", filename);

	/* check magic */
	ret = fread(buf, 8, 1, f);
	if(ret == 0 || strncmp(buf, "firmprof", 8) != 0) {
		return;
	}

	if(profile) ir_profile_free();
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
ir_profile_free(void)
{
	if(profile) {
		unregister_vcg_hook();
		del_set(profile);
	}
}

/**
 * Tells whether profile module has acquired data
 */
int
ir_profile_has_data(void)
{
	return (profile != NULL);
}

/**
 * Get block execution count as determined be profiling
 */
unsigned int
ir_profile_get_block_execcount(const ir_node *block)
{
	execcount_t *ec, query;

	if(!profile)
		return 1;

	query.block = get_irn_node_nr(block);
	ec = set_find(profile, &query, sizeof(query), get_irn_node_nr(block));

	if(ec != NULL) {
		return ec->count;
	} else {
		ir_fprintf(stderr, "Warning: Profile contains no data for %+F\n",
		           block);
		return 1;
	}
}

typedef struct _intialize_execfreq_env_t {
	ir_graph *irg;
	ir_exec_freq *execfreqs;
	double freq_factor;
} initialize_execfreq_env_t;

// minimal execution frequency (an execfreq of 0 confuses algos)
static const double MIN_EXECFREQ = 0.00001;

static void initialize_execfreq(ir_node *block, void *data) {
	initialize_execfreq_env_t *env = data;
	double freq;

	if(block == get_irg_start_block(env->irg)
	   || block == get_irg_end_block(env->irg)) {
		freq = 1.0;
	} else {
		freq = ir_profile_get_block_execcount(block);
		freq *= env->freq_factor;
		if(freq < MIN_EXECFREQ)
			freq = MIN_EXECFREQ;
	}

	set_execfreq(env->execfreqs, block, freq);
}

ir_exec_freq *ir_create_execfreqs_from_profile(ir_graph *irg)
{
	ir_node *start_block;
	initialize_execfreq_env_t env;
	unsigned count;

	env.irg = irg;
	env.execfreqs = create_execfreq(irg);
	start_block = get_irg_start_block(irg);

	count = ir_profile_get_block_execcount(start_block);
	if(count == 0) {
		// the function was never executed, so fallback to estimated freqs
		free_execfreq(env.execfreqs);

		return compute_execfreq(irg, 10);
	}

	env.freq_factor = 1.0 / count;
	irg_block_walk_graph(irg, initialize_execfreq, NULL, &env);

	return env.execfreqs;
}
