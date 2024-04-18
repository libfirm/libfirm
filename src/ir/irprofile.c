/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Code instrumentation and execution count profiling.
 * @author      Adam M. Szalkowski, Steven Schaefer
 * @date        06.04.2006, 11.11.2010
 */
#include "irprofile.h"

#include "debug.h"
#include "execfreq_t.h"
#include "hashptr.h"
#include "ident_t.h"
#include "ircons_t.h"
#include "irdump_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "obst.h"
#include "set.h"
#include "typerep.h"
#include "util.h"
#include "xmalloc.h"

/* Instrument blocks walker. */
typedef struct block_id_walker_data_t {
	unsigned int  id;       /**< current block id number */
	ir_node      *counters; /**< the node representing the counter array */
} block_id_walker_data_t;

/* Associate counters with blocks. */
typedef struct block_assoc_t {
	unsigned int i;          /**< current block id number */
	unsigned int *counters;  /**< block execution counts */
} block_assoc_t;

/* minimal execution frequency (an execfreq of 0 confuses algos) */
#define MIN_EXECFREQ 0.00001

/* keep the execcounts here because they are only read once per compiler run */
static set *profile = NULL;

/* Hook for vcg output. */
static hook_entry_t *hook;

/* The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Since the backend creates a new firm graph we cannot associate counts with
 * blocks directly. Instead we associate them with the block ids, which are
 * maintained.
 */
typedef struct execcount_t {
	unsigned long block; /**< block id */
	uint32_t      count; /**< execution count */
} execcount_t;

/**
 * Compare two execcount_t entries.
 */
static int cmp_execcount(const void *a, const void *b, size_t size)
{
	const execcount_t *ea = (const execcount_t*)a;
	const execcount_t *eb = (const execcount_t*)b;
	(void)size;
	return ea->block != eb->block;
}

uint32_t ir_profile_get_block_execcount(const ir_node *block)
{
	execcount_t  const query = { .block = get_irn_node_nr(block), .count = 0 };
	execcount_t *const ec    = set_find(execcount_t, profile, &query, sizeof(query), query.block);

	if (ec != NULL) {
		return ec->count;
	} else {
		DBG((dbg, LEVEL_3, "Warning: Profile contains no data for %+F\n", block));
		return 0;
	}
}

/**
 * Block walker, count number of blocks.
 */
static void block_counter(ir_node *bb, void *data)
{
	unsigned *const count = (unsigned*)data;
	(void)bb;
	++*count;
}

/**
 * Returns the number of blocks the given graph.
 */
static unsigned int get_irg_n_blocks(ir_graph *irg)
{
	unsigned int count = 0;
	irg_block_walk_graph(irg, block_counter, NULL, &count);
	return count;
}

/**
 * Returns the number of basic blocks in the current ir program.
 */
static unsigned int get_irp_n_blocks(void)
{
	unsigned int count = 0;
	foreach_irp_irg(i, irg) {
		count += get_irg_n_blocks(irg);
	}
	return count;
}

/* vcg helper */
static void dump_profile_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	(void)ctx;
	if (is_Block(irn)) {
		unsigned int execcount = ir_profile_get_block_execcount(irn);
		fprintf(f, "profiled execution count: %u\n", execcount);
	}
}

/**
 * Add the given method entity as a constructor.
 */
static void add_constructor(ir_entity *method)
{
	ir_type   *const method_type  = get_entity_type(method);
	ir_type   *const ptr_type     = new_type_pointer(method_type);
	ir_type   *const constructors = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
	ident     *const ide          = id_unique("constructor_ptr");
	ir_entity *const ptr          = new_global_entity(constructors, ide, ptr_type, ir_visibility_private, IR_LINKAGE_CONSTANT | IR_LINKAGE_HIDDEN_USER);
	set_entity_ld_ident(ptr, NEW_IDENT(""));

	ir_graph         *const irg  = get_const_code_irg();
	ir_node          *const val  = new_r_Address(irg, method);
	ir_initializer_t *const init = create_initializer_const(val);
	set_entity_initializer(ptr, init);
}

/**
 * Returns an entity representing the __init_firmprof function from libfirmprof
 * This is the equivalent of:
 * extern void __init_firmprof(char *filename, uint *counters, uint size)
 */
static ir_entity *get_init_firmprof_ref(void)
{
	ident   *const init_name = new_id_from_str("__init_firmprof");
	ir_type *const init_type = new_type_method(3, 0, false, cc_cdecl_set, mtp_no_property);
	ir_type *const uint      = get_type_for_mode(mode_Iu);
	ir_type *const uintptr   = new_type_pointer(uint);
	ir_type *const string    = new_type_pointer(get_type_for_mode(mode_Bs));

	set_method_param_type(init_type, 0, string);
	set_method_param_type(init_type, 1, uintptr);
	set_method_param_type(init_type, 2, uint);

	return new_entity(get_glob_type(), init_name, init_type);
}

/**
 * Generates a new irg which calls the initializer
 *
 * Pseudocode:
 *    static void __firmprof_initializer(void) __attribute__ ((constructor))
 *    {
 *        __init_firmprof(ent_filename, bblock_counts, n_blocks);
 *    }
 */
static ir_graph *gen_initializer_irg(ir_entity *ent_filename, ir_entity *bblock_counts, int n_blocks)
{
	ident     *const name  = new_id_from_str("__firmprof_initializer");
	ir_type   *const owner = get_glob_type();
	ir_type   *const type  = new_type_method(0, 0, false, cc_cdecl_set, mtp_no_property);
	ir_entity *const ent   = new_global_entity(owner, name, type, ir_visibility_local, IR_LINKAGE_DEFAULT);

	ir_graph  *const irg       = new_ir_graph(ent, 0);
	ir_node   *const bb        = get_r_cur_block(irg);
	ir_node   *const init_mem  = get_irg_initial_mem(irg);
	ir_entity *const init_ent  = get_init_firmprof_ref();
	ir_node   *const callee    = new_r_Address(irg, init_ent);
	ir_node   *const filename  = new_r_Address(irg, ent_filename);
	ir_node   *const counters  = new_r_Address(irg, bblock_counts);
	ir_node   *const size      = new_r_Const_long(irg, mode_Iu, n_blocks);
	ir_node   *const ins[]     = { filename, counters, size };
	ir_type   *const call_type = get_entity_type(init_ent);
	ir_node   *const call      = new_r_Call(bb, init_mem, callee, ARRAY_SIZE(ins), ins, call_type);
	ir_node   *const call_mem  = new_r_Proj(call, mode_M, pn_Call_M);
	ir_node   *const ret       = new_r_Return(bb, call_mem, 0, NULL);

	add_immBlock_pred(get_irg_end_block(irg), ret);
	irg_finalize_cons(irg);
	add_constructor(ent);

	return irg;
}

/**
 * Instrument a block with code needed for profiling.
 * This just inserts the instruction nodes, it doesn't connect the memory
 * nodes in a meaningful way.
 */
static void instrument_block(ir_node *const bb, ir_node *const address, unsigned int const id)
{
	ir_graph *const irg = get_irn_irg(bb);

	/* We can't instrument the end block */
	if (bb == get_irg_end_block(irg))
		return;

	ir_type *const type_arr = get_entity_type(get_irn_entity_attr(address));
	ir_type *const type_ctr = get_array_element_type(type_arr);
	ir_mode *const mode_ctr = get_type_mode(type_ctr);
	ir_node *const unknown  = new_r_Unknown(irg, mode_M);
	ir_mode *const mode_off = get_reference_offset_mode(get_irn_mode(address));
	ir_node *const cnst     = new_r_Const_long(irg, mode_off, get_mode_size_bytes(mode_ctr) * id);
	ir_node *const offset   = new_r_Add(bb, address, cnst);
	ir_node *const load     = new_r_Load(bb, unknown, offset, mode_ctr, type_arr, cons_none);
	ir_node *const lmem     = new_r_Proj(load, mode_M, pn_Load_M);
	ir_node *const proji    = new_r_Proj(load, mode_ctr, pn_Load_res);
	ir_node *const one      = new_r_Const_one(irg, mode_ctr);
	ir_node *const add      = new_r_Add(bb, proji, one);
	ir_node *const store    = new_r_Store(bb, lmem, offset, add, type_arr, cons_none);
	ir_node *const smem     = new_r_Proj(store, mode_M, pn_Store_M);

	set_irn_link(bb, smem);
	set_irn_link(smem, load);
}

/**
 * SSA Construction for instrumentation code memory.
 *
 * This introduces a new memory node and connects it to the instrumentation
 * codes, inserting phiM nodes as necessary. Note that afterwards, the new
 * memory is not connected to any return nodes and thus still dead.
 */
static void fix_ssa(ir_node *const bb, void *const data)
{
	(void)data;

	ir_graph *const irg = get_irn_irg(bb);

	/* end blocks are not instrumented, skip! */
	if (bb == get_irg_end_block(irg))
		return;

	ir_node  *mem;
	int const arity = get_Block_n_cfgpreds(bb);
	if (bb == get_irg_start_block(irg)) {
		mem = get_irg_initial_mem(irg);
	} else if (arity == 1) {
		ir_node *const pred = get_Block_cfgpred_block(bb, 0);
		mem = pred ? (ir_node*)get_irn_link(pred) : new_r_NoMem(irg);
	} else {
		ir_node **ins = ALLOCAN(ir_node*, arity);
		for (int n = arity; n-- != 0;) {
			ir_node *const pred = get_Block_cfgpred_block(bb, n);
			ins[n] = pred ? (ir_node*)get_irn_link(pred) : new_r_NoMem(irg);
		}
		mem = new_r_Phi(bb, arity, ins, mode_M);
	}

	/* The block link fields point to the projm from the instrumentation code,
	 * the projm in turn links to the initial load which lacks a memory
	 * argument at this point. */
	ir_node *const proj = (ir_node*)get_irn_link(bb);
	ir_node *const load = (ir_node*)get_irn_link(proj);
	set_Load_mem(load, mem);
}

/**
 * Instrument a single block.
 */
static void block_instrument_walker(ir_node *bb, void *data)
{
	block_id_walker_data_t *wd = (block_id_walker_data_t*)data;
	instrument_block(bb, wd->counters, wd->id);
	++wd->id;
}

/**
 * Synchronize the original memory input of node with the additional operand
 * from the profiling code.
 */
static ir_node *sync_mem(ir_node *bb, ir_node *mem)
{
	ir_node *const ins[] = { (ir_node*)get_irn_link(bb), mem };
	return new_r_Sync(bb, ARRAY_SIZE(ins), ins);
}

/**
 * Instrument a single ir_graph, counters should point to the bblock
 * counters array.
 */
static void instrument_irg(ir_graph *irg, ir_entity *counters, block_id_walker_data_t *wd)
{
	/* generate a node pointing to the count array */
	wd->counters = new_r_Address(irg, counters);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* instrument each block in the current irg */
	irg_block_walk_graph(irg, block_instrument_walker, NULL, wd);
	irg_block_walk_graph(irg, fix_ssa, NULL, NULL);

	/* connect the new memory nodes to the return nodes */
	ir_node *const endbb = get_irg_end_block(irg);
	for (unsigned i = get_Block_n_cfgpreds(endbb); i-- > 0;) {
		ir_node *const node = skip_Proj(get_Block_cfgpred(endbb, i));
		ir_node *const bb   = get_Block_cfgpred_block(endbb, i);
		if (bb == NULL)
			continue;

		ir_node *mem;
		switch (get_irn_opcode(node)) {
		case iro_Return:
			mem = get_Return_mem(node);
			set_Return_mem(node, sync_mem(bb, mem));
			break;
		case iro_Raise:
			mem = get_Raise_mem(node);
			set_Raise_mem(node, sync_mem(bb, mem));
			break;
		case iro_Bad:
			break;
		default:
			/* A fragile's op exception. There should be another path to End,
			 * so ignore it. */
			assert(is_fragile_op(node) && "unexpected End control flow predecessor");
		}
	}

	/* as well as calls with attribute noreturn */
	ir_node *const end = get_irg_end(irg);
	for (unsigned i = get_End_n_keepalives(end); i-- > 0;) {
		ir_node *node = get_End_keepalive(end, i);
		if (is_Call(node)) {
			ir_node *const bb  = get_nodes_block(node);
			ir_node *const mem = get_Call_mem(node);
			set_Call_mem(node, sync_mem(bb, mem));
		}
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

/**
 * Creates a new entity representing the equivalent of
 * static <element_mode> <name>[<length>];
 */
static ir_entity *new_array_entity(ident *const name, ir_mode *const element_mode, unsigned const length, ir_linkage const linkage)
{
	ir_type *const element_type = get_type_for_mode(element_mode);
	ir_type *const array_type   = new_type_array(element_type, length);
	ident   *const id           = new_id_from_str(name);
	ir_type *const owner        = get_glob_type();
	return new_global_entity(owner, id, array_type, ir_visibility_private, linkage);
}

/**
 * Creates a new entity representing the equivalent of
 * static const char name[strlen(string)+1] = string
 */
static ir_entity *new_static_string_entity(char const *const name, char const *const string)
{
	/* Create the type for a fixed-length string */
	ir_mode   *const mode   = mode_Bs;
	size_t     const length = strlen(string) + 1;
	ir_entity *const result = new_array_entity(name, mode, length, IR_LINKAGE_CONSTANT);

	/* There seems to be no simpler way to do this. Or at least, cparser
	 * does exactly the same thing... */
	ir_initializer_t *const contents = create_initializer_compound(length);
	for (size_t i = 0; i < length; i++) {
		ir_tarval        *const c    = new_tarval_from_long(string[i], mode);
		ir_initializer_t *const init = create_initializer_tarval(c);
		set_initializer_compound_value(contents, i, init);
	}
	set_entity_initializer(result, contents);

	return result;
}

ir_graph *ir_profile_instrument(const char *filename)
{
	FIRM_DBG_REGISTER(dbg, "firm.ir.profile");

	/* Don't do anything for modules without code. Else the linker will
	 * complain. */
	if (get_irp_n_irgs() == 0)
		return NULL;

	/* count the number of block first */
	unsigned const n_blocks = get_irp_n_blocks();

	/* create all the necessary types and entities. Note that the
	 * types must have a fixed layout, because we are already running in the
	 * backend */
	ir_entity *const bblock_counts = new_array_entity("__FIRMPROF__BLOCK_COUNTS", mode_Iu, n_blocks, IR_LINKAGE_DEFAULT);

	ir_entity *const ent_filename = new_static_string_entity("__FIRMPROF__FILE_NAME", filename);

	/* initialize block id array and instrument blocks */
	block_id_walker_data_t wd = { .id = 0 };
	foreach_irp_irg_r(i, irg) {
		instrument_irg(irg, bblock_counts, &wd);
	}

	return gen_initializer_irg(ent_filename, bblock_counts, n_blocks);
}

static unsigned int *parse_profile(const char *filename, unsigned int num_blocks)
{
	FILE *const f = fopen(filename, "rb");
	if (!f) {
		DBG((dbg, LEVEL_2, "Failed to open profile file (%s)\n", filename));
		return NULL;
	}

	/* check header */
	uint32_t *result = NULL;
	char      buf[8];
	size_t    ret = fread(buf, 8, 1, f);
	if (ret == 0 || strncmp(buf, "firmprof", 8) != 0) {
		DBG((dbg, LEVEL_2, "Broken fileheader in profile\n"));
		goto end;
	}

	result = XMALLOCN(unsigned int, num_blocks);

	/* The profiling output format is defined to be a sequence of integer
	 * values stored little endian format. */
	for (unsigned i = 0; i < num_blocks; ++i) {
		unsigned char bytes[4];
		if ((ret = fread(bytes, 1, 4, f)) < 1)
			break;

		result[i] = (bytes[0] <<  0) | (bytes[1] <<  8)
		          | (bytes[2] << 16) | (bytes[3] << 24);
	}

	if (ret < 1) {
		DBG((dbg, LEVEL_4, "Failed to read counters... (size: %u)\n",
			sizeof(unsigned int) * num_blocks));
		free(result);
		result = NULL;
	}

end:
	fclose(f);
	return result;
}

/**
 * Reads the corresponding profile info file if it exists.
 */
static void block_associate_walker(ir_node *bb, void *env)
{
	block_assoc_t *b = (block_assoc_t*)env;
	execcount_t query;

	query.block = get_irn_node_nr(bb);
	query.count = b->counters[b->i++];
	DBG((dbg, LEVEL_4, "execcount(%+F, %u): %u\n", bb, query.block, query.count));
	(void)set_insert(execcount_t, profile, &query, sizeof(query), query.block);
}

static void irp_associate_blocks(block_assoc_t *env)
{
	foreach_irp_irg_r(i, irg) {
		irg_block_walk_graph(irg, block_associate_walker, NULL, env);
	}
}

void ir_profile_free(void)
{
	if (profile) {
		del_set(profile);
		profile = NULL;
	}

	if (hook != NULL) {
		dump_remove_node_info_callback(hook);
		hook = NULL;
	}
}

bool ir_profile_read(const char *filename)
{
	FIRM_DBG_REGISTER(dbg, "firm.ir.profile");

	unsigned n_blocks = get_irp_n_blocks();
	block_assoc_t env = {
		.i        = 0,
		.counters = parse_profile(filename, n_blocks)
	};
	if (!env.counters)
		return false;

	ir_profile_free();
	profile = new_set(cmp_execcount, 16);

	irp_associate_blocks(&env);
	free(env.counters);

	/* register the vcg hook */
	hook = dump_add_node_info_callback(dump_profile_node_info, NULL);
	return 1;
}

typedef struct initialize_execfreq_env_t {
	double freq_factor;
} initialize_execfreq_env_t;

static void initialize_execfreq(ir_node *block, void *data)
{
	const initialize_execfreq_env_t *env = (const initialize_execfreq_env_t*)data;

	double          freq;
	ir_graph *const irg = get_irn_irg(block);
	if (block == get_irg_start_block(irg) || block == get_irg_end_block(irg)) {
		freq = 1.0;
	} else {
		freq = ir_profile_get_block_execcount(block);
		freq *= env->freq_factor;
		if (freq < MIN_EXECFREQ)
			freq = MIN_EXECFREQ;
	}

	set_block_execfreq(block, freq);
}

static void ir_set_execfreqs_from_profile(ir_graph *irg)
{
	/* Find the first block containing instructions */
	ir_node *const start_block = get_irg_start_block(irg);
	unsigned const count       = ir_profile_get_block_execcount(start_block);
	if (count == 0) {
		/* the function was never executed, so fallback to estimated freqs */
		ir_estimate_execfreq(irg);
		return;
	}

	initialize_execfreq_env_t env = { .freq_factor = 1.0 / count };
	irg_block_walk_graph(irg, initialize_execfreq, NULL, &env);
}

void ir_create_execfreqs_from_profile(void)
{
	foreach_irp_irg_r(i, irg) {
		ir_set_execfreqs_from_profile(irg);
	}
}
