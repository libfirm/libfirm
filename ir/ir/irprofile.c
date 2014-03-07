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
#include <math.h>
#include <stdio.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "xmalloc.h"
#include "set.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "execfreq_t.h"
#include "irprofile.h"
#include "typerep.h"

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

/* Since the backend creates a new firm graph we cannot associate counts with
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
	(void) size;
	return ea->block != eb->block;
}

uint32_t ir_profile_get_block_execcount(const ir_node *block)
{
	execcount_t *ec, query;

	query.block = get_irn_node_nr(block);
	ec = set_find(execcount_t, profile, &query, sizeof(query), query.block);

	if (ec != NULL) {
		return ec->count;
	} else {
		DBG((dbg, LEVEL_3,
			"Warning: Profile contains no data for %+F\n", block));
		return 0;
	}
}

/**
 * Block walker, count number of blocks.
 */
static void block_counter(ir_node *bb, void *data)
{
	unsigned *count = (unsigned*) data;
	(void) bb;
	++(*count);
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
	int i, n = get_irp_n_irgs();
	unsigned int count = 0;

	for (i = 0; i < n; i++) {
		ir_graph *irg = get_irp_irg(i);
		count += get_irg_n_blocks(irg);
	}

	return count;
}

/* vcg helper */
static void dump_profile_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	(void) ctx;
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
    ir_type   *method_type  = get_entity_type(method);
    ir_type   *ptr_type     = new_type_pointer(method_type);

    ir_type   *constructors = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
	ident     *ide = id_unique("constructor_ptr.%u");
    ir_entity *ptr = new_entity(constructors, ide, ptr_type);
    ir_graph  *irg = get_const_code_irg();
    ir_node   *val = new_r_Address(irg, method);

	set_entity_ld_ident(ptr, new_id_from_chars("", 0));
    set_entity_compiler_generated(ptr, 1);
    set_entity_linkage(ptr, IR_LINKAGE_CONSTANT | IR_LINKAGE_HIDDEN_USER);
    set_entity_visibility(ptr, ir_visibility_private);
    set_atomic_ent_value(ptr, val);
}

/**
 * Returns an entity representing the __init_firmprof function from libfirmprof
 * This is the equivalent of:
 * extern void __init_firmprof(char *filename, uint *counters, uint size)
 */
static ir_entity *get_init_firmprof_ref(void)
{
	ident   *init_name = new_id_from_str("__init_firmprof");
	ir_type *init_type = new_type_method(3, 0);
	ir_type *uint      = new_type_primitive(mode_Iu);
	ir_type *uintptr   = new_type_pointer(uint);
	ir_type *string    = new_type_pointer(new_type_primitive(mode_Bs));
	ir_entity *result;

	set_method_param_type(init_type, 0, string);
	set_method_param_type(init_type, 1, uintptr);
	set_method_param_type(init_type, 2, uint);

	result = new_entity(get_glob_type(), init_name, init_type);
	set_entity_visibility(result, ir_visibility_external);

	return result;
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
static ir_graph *gen_initializer_irg(ir_entity *ent_filename,
                                     ir_entity *bblock_counts, int n_blocks)
{
	ir_graph *irg;
	ir_node  *ins[3];
	ir_node  *bb, *ret, *call;
	ir_type  *empty_frame_type;

	ir_entity *init_ent = get_init_firmprof_ref();

	ident     *name = new_id_from_str("__firmprof_initializer");
	ir_entity *ent  = new_entity(get_glob_type(), name, new_type_method(0, 0));
	set_entity_visibility(ent, ir_visibility_local);
	set_entity_ld_ident(ent, name);

	/* create the new ir_graph */
	irg = new_ir_graph(ent, 0);
	set_current_ir_graph(irg);
	empty_frame_type = get_irg_frame_type(irg);
	set_type_size_bytes(empty_frame_type, 0);
	set_type_state(empty_frame_type, layout_fixed);

	bb = get_r_cur_block(irg);

	ir_node *const callee = new_r_Address(irg, init_ent);

	ins[0] = new_r_Address(irg, ent_filename);
	ins[1] = new_r_Address(irg, bblock_counts);
	ins[2] = new_r_Const_long(irg, mode_Iu, n_blocks);

	call = new_r_Call(bb, get_irg_initial_mem(irg), callee, 3, ins,
	        get_entity_type(init_ent));
	ret  = new_r_Return(bb, new_r_Proj(call, mode_M, pn_Call_M), 0, NULL);
	mature_immBlock(bb);

	add_immBlock_pred(get_irg_end_block(irg), ret);
	mature_immBlock(get_irg_end_block(irg));

	irg_finalize_cons(irg);

	/* add a pointer to the new function in the constructor section */
	add_constructor(ent);

	return irg;
}

/**
 * Instrument a block with code needed for profiling.
 * This just inserts the instruction nodes, it doesn't connect the memory
 * nodes in a meaningful way.
 */
static void instrument_block(ir_node *bb, ir_node *address, unsigned int id)
{
	ir_graph *irg = get_irn_irg(bb);
	ir_node  *load, *store, *offset, *add, *projm, *proji, *unknown, *cnst;

	/* We can't instrument the end block */
	if (bb == get_irg_end_block(irg))
		return;

	unknown = new_r_Unknown(irg, mode_M);
	cnst    = new_r_Const_long(irg, mode_Iu, get_mode_size_bytes(mode_Iu) * id);
	offset  = new_r_Add(bb, address, cnst, get_modeP_data());
	load    = new_r_Load(bb, unknown, offset, mode_Iu, cons_none);
	projm   = new_r_Proj(load, mode_M, pn_Load_M);
	proji   = new_r_Proj(load, mode_Iu, pn_Load_res);
	cnst    = new_r_Const(irg, get_mode_one(mode_Iu));
	add     = new_r_Add(bb, proji, cnst, mode_Iu);
	store   = new_r_Store(bb, projm, offset, add, cons_none);
	projm   = new_r_Proj(store, mode_M, pn_Store_M);

	set_irn_link(bb, projm);
	set_irn_link(projm, load);
}

/**
 * SSA Construction for instrumentation code memory.
 *
 * This introduces a new memory node and connects it to the instrumentation
 * codes, inserting phiM nodes as necessary. Note that afterwards, the new
 * memory is not connected to any return nodes and thus still dead.
 */
static void fix_ssa(ir_node *bb, void *data)
{
	ir_graph *irg = get_irn_irg(bb);
	ir_node *mem, *proj, *load;
	int n, arity = get_Block_n_cfgpreds(bb);

	(void) data;

	/* end blocks are not instrumented, skip! */
	if (bb == get_irg_end_block(irg))
		return;

	if (bb == get_irg_start_block(irg)) {
		mem = get_irg_initial_mem(irg);
	} else if (arity == 1) {
		ir_node *pred = get_Block_cfgpred_block(bb, 0);
		if (!is_Bad(pred))
			mem = (ir_node*) get_irn_link(pred);
		else
			mem = new_r_NoMem(irg);
	} else {
		ir_node **ins = ALLOCAN(ir_node*, arity);
		for (n = arity - 1; n >= 0; --n) {
			ir_node *pred = get_Block_cfgpred_block(bb, n);
			if (!is_Bad(pred))
				ins[n] = (ir_node*) get_irn_link(pred);
			else
				ins[n] = new_r_NoMem(irg);
		}
		mem = new_r_Phi(bb, arity, ins, mode_M);
	}

	/* The block link fields point to the projm from the instrumentation code,
	 * the projm in turn links to the initial load which lacks a memory
	 * argument at this point. */
	proj = (ir_node*) get_irn_link(bb);
	load = (ir_node*) get_irn_link(proj);
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
	ir_node *ins[2];
	ins[0] = (ir_node*) get_irn_link(bb);
	ins[1] = mem;
	return new_r_Sync(bb, 2, ins);
}

/**
 * Instrument a single ir_graph, counters should point to the bblock
 * counters array.
 */
static void instrument_irg(ir_graph *irg, ir_entity *counters,
                           block_id_walker_data_t *wd)
{
	ir_node *end   = get_irg_end(irg);
	ir_node *endbb = get_irg_end_block(irg);
	int i;

	/* generate a node pointing to the count array */
	wd->counters = new_r_Address(irg, counters);

	/* instrument each block in the current irg */
	irg_block_walk_graph(irg, block_instrument_walker, NULL, wd);
	irg_block_walk_graph(irg, fix_ssa, NULL, NULL);

	/* connect the new memory nodes to the return nodes */
	for (i = get_Block_n_cfgpreds(endbb) - 1; i >= 0; --i) {
		ir_node *node = skip_Proj(get_Block_cfgpred(endbb, i));
		ir_node *bb   = get_Block_cfgpred_block(endbb, i);
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
			 * so ignore it.
			 */
			assert(is_fragile_op(node) && \
				"unexpected End control flow predecessor");
		}
	}

	/* as well as calls with attribute noreturn */
	for (i = get_End_n_keepalives(end) - 1; i >= 0; --i) {
		ir_node *node = get_End_keepalive(end, i);
		if (is_Call(node)) {
			ir_node *bb  = get_nodes_block(node);
			ir_node *mem = get_Call_mem(node);
			set_Call_mem(node, sync_mem(bb, mem));
		}
	}
}

/**
 * Creates a new entity representing the equivalent of
 * static unsigned int name[size]
 */
static ir_entity *new_array_entity(ident *name, int size)
{
	ir_entity *result;
	ir_type *uint_type, *array_type;

	uint_type = new_type_primitive(mode_Iu);
	set_type_alignment_bytes(uint_type, get_type_size_bytes(uint_type));

	array_type = new_type_array(uint_type);
	set_array_size_int(array_type, size);
	set_type_size_bytes(array_type, size * get_mode_size_bytes(mode_Iu));
	set_type_alignment_bytes(array_type, get_mode_size_bytes(mode_Iu));
	set_type_state(array_type, layout_fixed);

	result = new_entity(get_glob_type(), name, array_type);
	set_entity_visibility(result, ir_visibility_local);
	set_entity_compiler_generated(result, 1);

	return result;
}

/**
 * Creates a new entity representing the equivalent of
 * static const char name[strlen(string)+1] = string
 */
static ir_entity *new_static_string_entity(ident *name, const char *string)
{
	ir_entity *result;

	ir_type *char_type   = new_type_primitive(mode_Bs);
	ir_type *string_type = new_type_array(char_type);

	ir_initializer_t *contents;

	size_t i, length = strlen(string)+1;

	/* Create the type for a fixed-length string */
	set_array_size_int(string_type, length);
	set_type_size_bytes(string_type, length);
	set_type_alignment_bytes(string_type, 1);
	set_type_state(string_type, layout_fixed);

	result = new_entity(get_glob_type(), name, string_type);
	set_entity_visibility(result, ir_visibility_local);
	set_entity_linkage(result, IR_LINKAGE_CONSTANT);
	set_entity_compiler_generated(result, 1);

	/* There seems to be no simpler way to do this. Or at least, cparser
	 * does exactly the same thing... */
	contents = create_initializer_compound(length);
	for (i = 0; i < length; i++) {
		ir_tarval *c = new_tarval_from_long(string[i], mode_Bs);
		ir_initializer_t *init = create_initializer_tarval(c);
		set_initializer_compound_value(contents, i, init);
	}
	set_entity_initializer(result, contents);

	return result;
}

ir_graph *ir_profile_instrument(const char *filename)
{
	int n, n_blocks = 0;
	ident *counter_id, *filename_id;
	ir_entity *bblock_counts, *ent_filename;
	block_id_walker_data_t wd;
	FIRM_DBG_REGISTER(dbg, "firm.ir.profile");

	/* Don't do anything for modules without code. Else the linker will
	 * complain. */
	if (get_irp_n_irgs() == 0)
		return NULL;

	/* count the number of block first */
	n_blocks = get_irp_n_blocks();

	/* create all the necessary types and entities. Note that the
	 * types must have a fixed layout, because we are already running in the
	 * backend */
	counter_id    = new_id_from_str("__FIRMPROF__BLOCK_COUNTS");
	bblock_counts = new_array_entity(counter_id, n_blocks);

	filename_id  = new_id_from_str("__FIRMPROF__FILE_NAME");
	ent_filename = new_static_string_entity(filename_id, filename);

	/* initialize block id array and instrument blocks */
	wd.id  = 0;
	for (n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);
		instrument_irg(irg, bblock_counts, &wd);
	}

	return gen_initializer_irg(ent_filename, bblock_counts, n_blocks);
}

static unsigned int *parse_profile(const char *filename, unsigned int num_blocks)
{
	FILE *f = fopen(filename, "rb");
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
	block_assoc_t *b = (block_assoc_t*) env;
	execcount_t query;

	query.block = get_irn_node_nr(bb);
	query.count = b->counters[(b->i)++];
	DBG((dbg, LEVEL_4, "execcount(%+F, %u): %u\n", bb, query.block,
	    query.count));
	(void)set_insert(execcount_t, profile, &query, sizeof(query), query.block);
}

static void irp_associate_blocks(block_assoc_t *env)
{
	for (int n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);
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
	block_assoc_t env;
	FIRM_DBG_REGISTER(dbg, "firm.ir.profile");

	unsigned n_blocks = get_irp_n_blocks();
	env.i        = 0;
	env.counters = parse_profile(filename, n_blocks);
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
	const initialize_execfreq_env_t *env
		= (const initialize_execfreq_env_t*) data;
	ir_graph *irg = get_irn_irg(block);
	double freq;

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
	ir_node *start_block = get_irg_start_block(irg);
	unsigned count       = ir_profile_get_block_execcount(start_block);
	if (count == 0) {
		/* the function was never executed, so fallback to estimated freqs */
		ir_estimate_execfreq(irg);
		return;
	}

	initialize_execfreq_env_t env;
	env.freq_factor = 1.0 / count;
	irg_block_walk_graph(irg, initialize_execfreq, NULL, &env);
}

void ir_create_execfreqs_from_profile(void)
{
	for (int n = get_irp_n_irgs() - 1; n >= 0; --n) {
		ir_graph *irg = get_irp_irg(n);
		ir_set_execfreqs_from_profile(irg);
	}
}
