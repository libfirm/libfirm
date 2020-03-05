#include "lfasan.h"
#include "array.h"
#include "firm_types.h"
#include "ident.h"
#include "irgmod.h"
#include "ircons.h"
#include "irdom.h"
#include "irdom_t.h"
#include "irdump.h"
#include "iredges.h"
#include "irflag.h"
#include "irgraph.h"
#include "irgraph_t.h"
#include "irmode.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irop.h"
#include "irprog.h"
#include "irprog_t.h"
#include "tv.h"
#include "type_t.h"
#include "typerep.h"
#include "util.h"
#include "debug.h"
#include "pmap.h"
#include "dbginfo.h"

#include "gen_lfasan_sizes.h"

#include <assert.h>
#include <stdint.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#define REPLACE(ent, n, x) { \
	if (streq(n, x)) { \
		DB((dbg, LEVEL_3, "Replacing '" x "' with 'lf_" x "' in node %+F.\n", \
			func_ptr)); \
		set_entity_ld_ident(ent, "lf_" x); \
		assert(streq(get_entity_ld_name(ent), "lf_" x)); \
	} \
}

// Rename allocation functions to refer to lfmallocs allocation functions specifically.
static void func_ptr_rename(ir_node *irn, void *env) {
	(void) env;
	if (!is_Call(irn)) {
		return;
	}
	ir_node *func_ptr = get_Call_ptr(irn);
	if (!is_Address(func_ptr)) {
		return;
	}

	ir_entity *entity = get_irn_entity_attr(func_ptr);
	const char* ld_name = get_entity_ld_name(entity);
	DB((dbg, LEVEL_5, "Node %+F with %+F.\n",
		irn,
		func_ptr));

	REPLACE(entity, ld_name, "malloc");
	REPLACE(entity, ld_name, "free");
	REPLACE(entity, ld_name, "calloc");
	REPLACE(entity, ld_name, "realloc");

	REPLACE(entity, ld_name, "posix_memalign");
	REPLACE(entity, ld_name, "aligned_alloc");
	REPLACE(entity, ld_name, "valloc");
	REPLACE(entity, ld_name, "memalign");
	REPLACE(entity, ld_name, "pvalloc");
	REPLACE(entity, ld_name, "malloc_usable_size");
}

#define SIZES_LD_NAME "LF_ASAN_SIZES"

static ir_entity *create_global_lookup_table(void) {
	ir_graph *const_irg = get_const_code_irg();

	ir_type *glob_type = get_glob_type();
	int glob_members = get_compound_n_members(glob_type);

	ir_entity* table_entity = NULL;
	for (int i = 0; i < glob_members; i++) {
		ir_entity *cur_entity = get_compound_member(glob_type, i);
		if (streq(get_entity_ld_name(cur_entity), SIZES_LD_NAME)) {
			table_entity = cur_entity;
		}
	}
	if (table_entity == NULL) {
		ir_initializer_t* init = create_initializer_compound(ARRAY_SIZE(SIZES));
		for (uint32_t i = 0; i < ARRAY_SIZE(SIZES); i++) {
			ir_node* const_node = new_r_Const_long(const_irg, get_modeLu(), SIZES[i]);
			ir_initializer_t* init_const = create_initializer_const(const_node);
			set_initializer_compound_value(init, i, init_const);
		}
		table_entity = new_entity(get_glob_type(), SIZES_LD_NAME,
			new_type_array(get_type_for_mode(get_modeLu()), ARRAY_SIZE(SIZES)));
		ir_linkage linkage = IR_LINKAGE_CONSTANT;
		linkage |= IR_LINKAGE_WEAK;
		set_entity_linkage(table_entity, linkage);
		set_entity_initializer(table_entity, init);
	}

	DB((dbg, LEVEL_4, "Global Lookup entity: %+F\n", table_entity));
	return table_entity;
}

// Represents a bound check which might need to be inserted.
typedef struct {
	ir_node *ptr;      // The node which needs to be checked.
	ir_node *node;     // The node which uses the pointer.
	unsigned int type;
} bounds_check_pair;

// creates a new bound check pair and adds it to the list.
static void add_pair(bounds_check_pair **pairs, ir_node *irn, ir_node *ptr,
                     unsigned int type) {
	bounds_check_pair new_pair = {ptr, irn, type};
	ARR_APP1(bounds_check_pair, *pairs, new_pair);
}

// Analyzes a node to check if a bound check might be required.
static void collect_bounds_check_node_pairs(ir_node *irn, void *env) {
	bounds_check_pair **collected_pairs = (bounds_check_pair**) env;

	if (is_Block(irn)) {
		return;
	}

	if (is_Store(irn)) {
		ir_node* ptr = get_Store_ptr(irn);
		DB((dbg, LEVEL_5, "Collected ptr %+F for %+F (MEMORY_WRITE)\n", ptr, irn));
		add_pair(collected_pairs, irn, ptr, MEMORY_WRITE);
		ir_node* val = get_Store_value(irn);
		if (get_irn_mode(val) == get_modeP() && !is_Const(irn) && !is_Address(irn)) {
			DB((dbg, LEVEL_5, "Collected ptr %+F for %+F (MEMORY_ESCAPE)\n", val, irn));
			add_pair(collected_pairs, irn, val, MEMORY_ESCAPE);
		}
	} else if (is_Load(irn)) {
		ir_node* ptr = get_Load_ptr(irn);
		DB((dbg, LEVEL_5, "Collected ptr %+F for %+F (MEMORY_READ)\n", ptr, irn));
		add_pair(collected_pairs, irn, ptr, MEMORY_READ);
	} else if (is_Call(irn) && is_Address(get_Call_ptr(irn))) {
		int call_params = get_Call_n_params(irn);
		for (int i = 0; i < call_params; i++) {
			ir_node *param = get_Call_param(irn, i);
			if (get_irn_mode(param) == get_modeP()) {
				DB((dbg, LEVEL_5, "Collected ptr %+F for %+F (FUNCTION_ESCAPE)\n", param, irn));
				add_pair(collected_pairs, irn, param, FUNCTION_ESCAPE);
			}
		}
	} else if (is_Return(irn)) {
		int return_res = get_Return_n_ress(irn);
		for (int i = 0; i < return_res; i++) {
			ir_node *res = get_Return_res(irn, i);
			if (get_irn_mode(res) == get_modeP()) {
				DB((dbg, LEVEL_5, "Collected ptr %+F for %+F (RETURN_ESCAPE)\n", res, irn));
				add_pair(collected_pairs, irn, res, RETURN_ESCAPE);
			}
		}
	}
}

// Represents the metadata of a pointer node
typedef struct _lfptr_meta {
	struct _lfptr_meta *next;
	ir_node *base;
	ir_node *size;
	bool arithmetic; // If in the current function context,
	                 // this meta was changed by pointer arithmetic.
} lfptr_meta;

// List which owns all metadata objects.
lfptr_meta *lfptr_meta_first = NULL;

// constructor for lfptr_meta uninitialized lfptr_meta.
static lfptr_meta *new_lfptr_meta(ir_node* base, ir_node *size, bool arithmetic) {
	lfptr_meta *new = (lfptr_meta*)malloc(sizeof(lfptr_meta));
	new->next = lfptr_meta_first;
	new->base = base;
	new->size = size;
	new->arithmetic = arithmetic;
	lfptr_meta_first = new;
	return new;
}

// Analyzses irn to check if it is an allocation call.
// In this case it infers the metadata and returns it.
static lfptr_meta *is_alloc_res(ir_node *irn) {
	dbg_info *dbgi = get_irn_dbg_info(irn);

	ir_node *block = get_nodes_block(irn);

	assert(get_irn_mode(irn) == get_modeP());
	if (is_Proj(irn)) {
		ir_node *call_proj = get_Proj_pred(irn);
		if (is_Proj(call_proj)) {
			ir_node* call = get_Proj_pred(call_proj);
			if (is_Call(call)) {
				ir_node* ptr = get_Call_ptr(call);
					if (is_Address(ptr)) {
					const char* ld_name = get_entity_ld_name(get_irn_entity_attr(ptr));
					ir_node *size = NULL;
					// Check every allocation (except lf_posix_memalign) to infer the
					// correct metadata by their parameters.
					// lf_posix_memalign metadata is calculated when it is used.
					if (streq(ld_name, "lf_malloc")
							|| streq(ld_name, "lf_pvalloc")
							|| streq(ld_name, "lf_valloc")) {
						size = get_Call_param(call, 0);
					} else if (streq(ld_name, "lf_realloc")
							|| streq(ld_name, "lf_aligned_alloc")
							|| streq(ld_name, "lf_memalign")) {
						size = get_Call_param(call, 1);
					} else if (streq(ld_name, "lf_calloc")) {
						size = new_rd_Mul(dbgi, block, get_Call_param(call, 0),
										  get_Call_param(call, 1));
					}
					if (size != NULL) {
						return new_lfptr_meta(irn, size, false);
					} else {
						return NULL;
					}
				}
			}
		}
	}
	return NULL;
}

// Inserts metadata reconstruction logic of a pointer node into the firm graph.
static lfptr_meta *calc_metadata(ir_node *irn, ir_node *sizes_lookup) {
	dbg_info *dbgi = get_irn_dbg_info_(irn);

	assert(get_irn_mode(irn) == get_modeP());
	ir_node  *block = get_nodes_block(irn);
	DB((dbg, LEVEL_5, "Calc metadata in block %+F\n", block));
	ir_graph *irg   = get_irn_irg(irn);
	ir_node  *nomem = get_irg_no_mem(irg);
	mark_irn_visited(sizes_lookup);

	ir_node *const_region_bits = new_r_Const_long(irg, get_modeLu(), REGION_BITS);
	ir_node *conv_p_lu         = new_rd_Conv(dbgi, block, irn, get_modeLu());
	ir_node *index             = new_rd_Shr(dbgi, block, conv_p_lu, const_region_bits); //shift by bits of size of region
	ir_node *byte_offset       = new_rd_Mul(dbgi, block, index,
	                                    new_rd_Const_long(dbgi, irg, get_modeLu(), 8)); //size of element in lookup table
	ir_node *conv_lu_ls   = new_rd_Conv(dbgi, block, byte_offset, get_modeLs());
	ir_node *size_address = new_rd_Add(dbgi, block, sizes_lookup, conv_lu_ls);
	mark_irn_visited(size_address);

	ir_node *load = new_rd_Load(dbgi, block, nomem, size_address,
	                            get_modeLu(),
	                            get_type_for_mode(get_modeLu()),
	                            cons_none);
	ir_node *size = new_rd_Proj(dbgi, load, get_modeLu(), pn_Load_res);
	mark_irn_visited(size);

	ir_node *mod          = new_rd_Mod(dbgi, block, nomem, conv_p_lu, size, false);
	ir_node *mod_res      = new_rd_Proj(dbgi, mod, get_modeLu(), pn_Mod_res);
	ir_node *conv_mod_res = new_rd_Conv(dbgi, block, mod_res, get_modeLs());
	ir_node *sub          = new_rd_Sub(dbgi, block, irn, conv_mod_res);
	ir_node *base         = sub; //new_rd_Conv(dbgi, block, sub, get_modeP());
	mark_irn_visited(base);

	lfptr_meta *res = new_lfptr_meta(base, size, false);
	return res;
}

// Debug function for pretty printing the metadata map
static void pp_metadata_map(pmap* map) {
	if (pmap_count(map) == 0) {
		DB((dbg, LEVEL_5, "metadata map is empty\n"));
		return;
	}

	foreach_pmap(map, cur) {
		lfptr_meta* ptr  = (lfptr_meta*) cur->key;
		lfptr_meta* meta = (lfptr_meta*) cur->value;
		DB((dbg, LEVEL_5, "%+F\t->\tbase: %+F, size: %+F\n", ptr, meta->base, meta->size));
	}
}

// The following functions:
// lf_move, lf_move_node, move_node, update_startblock, lf_part_block
// were taken from irgmod.c and tweaked to consider the metadata nodes.
static void lf_move(pmap *lf_map, ir_node *node, ir_node *from_bl, ir_node *to_bl);

static void lf_move_node(pmap *lf_map, ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	set_nodes_block(node, to_bl);
	/* if no mode_T node, do not move Projs. Note that BadT shouldn't have
	 * any Projs as well and is part of the start_block list and therefore
	 * doesn't have a valid Proj list */
	if (get_irn_mode(node) != mode_T || is_Bad(node))
		return;

	for (ir_node *proj = (ir_node*)get_irn_link(node);
	     proj != NULL; proj = (ir_node*)get_irn_link(proj)) {

         set_nodes_block(proj, to_bl);
		if (pmap_contains(lf_map, proj)) {
			DB((dbg, LEVEL_5, "Moving (move_node) Metadata nodes of %+F\n", proj));
			lfptr_meta* meta = pmap_find(lf_map, proj)->value;
			if (get_nodes_block(meta->base) == from_bl)
				lf_move(lf_map, meta->base, from_bl, to_bl);
			if (get_nodes_block(meta->size) == from_bl)
				lf_move(lf_map, meta->size, from_bl, to_bl);
		}
	}
}

static void move_node(ir_node *node, ir_node *to_bl)
{
	set_nodes_block(node, to_bl);
	/* if no mode_T node, do not move Projs. Note that BadT shouldn't have
	 * any Projs as well and is part of the start_block list and therefore
	 * doesn't have a valid Proj list */
	if (get_irn_mode(node) != mode_T || is_Bad(node))
		return;

	for (ir_node *proj = (ir_node*)get_irn_link(node);
	     proj != NULL; proj = (ir_node*)get_irn_link(proj)) {

         set_nodes_block(proj, to_bl);
	}
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void lf_move(pmap *lf_map, ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	if (get_nodes_block(node) != from_bl) {
		return;
	}

	lf_move_node(lf_map, node, from_bl, to_bl);

	if (is_Phi(node))
		return;

	/* recursion ... */
	foreach_irn_in(node, i, pred) {
		if (get_nodes_block(pred) == from_bl)
			lf_move(lf_map, pred, from_bl, to_bl);
	}

	//Don't leave metadata nodes behind
	if (pmap_contains(lf_map, node)) {
		DB((dbg, LEVEL_5, "Moving Metadata nodes of %+F\n", node));
		lfptr_meta* meta = pmap_find(lf_map, node)->value;
		if (get_nodes_block(meta->base) == from_bl)
			lf_move(lf_map, meta->base, from_bl, to_bl);
		if (get_nodes_block(meta->size) == from_bl)
			lf_move(lf_map, meta->size, from_bl, to_bl);
	}
}

static void update_startblock(ir_node *old_block, ir_node *new_block)
{
	ir_graph *irg = get_irn_irg(old_block);
	set_irg_start_block(irg, new_block);
	/* move constants around */
	ir_node *end = get_irg_end(irg);
	for (ir_node *cnst = get_irn_link(end); cnst != end;
	     cnst = (ir_node*)get_irn_link(cnst)) {
		move_node(cnst, new_block);
	}
	ir_node *start = get_irg_start(irg);
	move_node(start, new_block);
}

static void lf_part_block(pmap *lf_map, ir_node *node)
{
	/* Turn off optimizations so that blocks are not merged again. */
	int rem_opt = get_optimize();
	set_optimize(0);

	/* Transform the control flow */
	ir_node  *old_block = get_nodes_block(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = new_r_Block(irg, get_Block_n_cfgpreds(old_block),
	                                  get_Block_cfgpred_arr(old_block));

	/* create a jump from new_block to old_block, which is now the lower one */
	ir_node *jmp = new_r_Jmp(new_block);
	set_irn_in(old_block, 1, &jmp);

	/* move node and its predecessors to new_block */
	lf_move(lf_map, node, old_block, new_block);

	/* move Phi nodes to new_block */
	ir_node *phi = get_Block_phis(old_block);
	set_Block_phis(new_block, phi);
	set_Block_phis(old_block, NULL);
	while (phi) {
		set_nodes_block(phi, new_block);
		phi = get_Phi_next(phi);
	}

	if (old_block == get_irg_start_block(irg))
		update_startblock(old_block, new_block);

	set_optimize(rem_opt);
}

// Creates an entity for a string_literal.
static ir_entity* string_literal(char const* string) {
	ir_graph *const_irg = get_const_code_irg();

	long len = strlen(string) + 1; //including 0-terminator

	ir_initializer_t* init = create_initializer_compound(len);
	for (uint32_t i = 0; i < len; i++) {
		ir_node* const_node = new_r_Const_long(const_irg, get_modeBu(), string[i]);
		ir_initializer_t *init_const = create_initializer_const(const_node);
		set_initializer_compound_value(init, i, init_const);
	}
	ir_type   *type   = new_type_array(get_type_for_mode(get_modeBu()), len);
	ir_entity *entity = new_global_entity(get_glob_type(), id_unique("filename"), type,
	                                      ir_visibility_private,
	                                      IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);
	set_entity_initializer(entity, init);
	return entity;
}

// Manages the singleton entity for the current filename that is compiled.
static ir_entity *current_filename_entity(const char* filename) {
	/* For filename in error output, an entity with the current filename bytes is remembered here. */
	static ir_entity *current_file_entity = NULL;
	static const char *current_file = NULL;

	if (current_file == NULL || !streq(current_file, filename)) {
		current_file = filename;
		DB((dbg, LEVEL_5, "Creating new filename entity\n"));
		current_file_entity = string_literal(current_file);
	}
	assert(current_file != NULL);
	assert(current_file_entity != NULL);
	return current_file_entity;
}

/* Returns the Call node to lf_error for the node irn (which is the OOB pointer) */
static ir_node* error_call(ir_node *irn, ir_node *block, pmap* lf_map,
	                   unsigned int reason, dbg_info *dbgi) {
	ir_graph *irg = get_irn_irg(irn);

	src_loc_t src_loc = ir_retrieve_dbg_info(dbgi);
	ir_node *line_const = new_rd_Const_long(dbgi, irg, get_modeIu(), src_loc.line); /*if dbg info doesn't exist src_loc.line is 0, which is considered by lf_error*/

	ir_node *file_name;
	if (src_loc.file != NULL) {
		ir_entity *file_entity = current_filename_entity(src_loc.file);
		file_name = new_rd_Address(dbgi, irg, file_entity);
	} else {
		file_name = new_r_Const_long(irg, get_modeP(), 0);
	}

	lfptr_meta *meta = pmap_find(lf_map, irn)->value;
	assert(meta != NULL);
	ir_node *base = meta->base;
	ir_node *size = meta->size;

	ir_node *reason_const = new_rd_Const_long(dbgi, irg, get_modeIu(), reason);

	ir_node *error_args[6] = { irn, base, size, reason_const, file_name, line_const };

	static ir_entity* abort_entity = NULL;
	ir_type *glob_type = get_glob_type();
	if (abort_entity == NULL) {
		ir_type *abort_type = new_type_method(ARRAY_SIZE(error_args), 0, false,
				cc_cdecl_set, mtp_property_noreturn | mtp_property_nothrow);
		set_method_param_type(abort_type, 0, get_type_for_mode(get_modeP()));
		set_method_param_type(abort_type, 1, get_type_for_mode(get_modeP()));
		set_method_param_type(abort_type, 2, get_type_for_mode(get_modeLu()));
		set_method_param_type(abort_type, 3, get_type_for_mode(get_modeIu()));
		set_method_param_type(abort_type, 4, get_type_for_mode(get_modeP()));
		set_method_param_type(abort_type, 5, get_type_for_mode(get_modeIu()));
		abort_entity = new_entity(glob_type, "lf_error", abort_type);
	}
	assert(abort_entity != NULL);

	ir_node *abort_address = new_rd_Address(dbgi, irg, abort_entity);
	mark_irn_visited(abort_address);
	ir_node *abort_call = new_rd_Call(dbgi, block, get_irg_no_mem(irg), abort_address,
	                                  ARRAY_SIZE(error_args), error_args,
	                                  get_entity_type(abort_entity));
	return abort_call;
}

// Environment for during instrumentation insertion phase.
typedef struct {
	pmap *lf_map;
	ir_node *sizes_address;
	lfptr_meta *nonlf_meta;
} insert_instrumentation_env;

// Lazily determines the metadata of a node recursively.
static lfptr_meta *get_node_meta(insert_instrumentation_env *env, ir_node *irn) {
	DB((dbg, LEVEL_3, "Need meta for %+F", irn));
	assert(get_irn_mode(irn) == get_modeP());

	pmap       *lf_map        = env->lf_map;
	ir_node    *sizes_address = env->sizes_address;
	lfptr_meta *nonlf_meta    = env->nonlf_meta;

	if (pmap_contains(lf_map, irn)) {
		DB((dbg, LEVEL_3, " which already exists\n"));
		return pmap_find(lf_map, irn)->value;
	}
	DB((dbg, LEVEL_3, " which needs to be calculated\n"));

	dbg_info *dbgi = get_irn_dbg_info(irn);

	ir_node  *block = get_nodes_block(irn);
	ir_graph *irg   = get_irn_irg(irn);

	lfptr_meta* meta = is_alloc_res(irn);
	if (meta != NULL) {
		DB((dbg, LEVEL_5, "%+F is newly allocated pointer, with base: %+F, size: %+F\n", irn, meta->base, meta->size));
		assert(get_irn_mode(meta->base) == get_modeP());
		assert(get_irn_mode(meta->size) == get_modeLu());
		assert(meta->base == irn);
		pmap_insert(lf_map, irn, meta);

	} else if (is_Add(irn) || is_Sub(irn)) {
		ir_node* left;
		ir_node* right;
		if (is_Add(irn)) {
			left  = get_Add_left(irn);
			right = get_Add_right(irn);
		} else {
			left  = get_Sub_left(irn);
			right = get_Sub_right(irn);
		}
		ir_node* p_pred = NULL;
		/*TODO: is this necessary? Maybe Add always has ptr on one side.*/
		assert((get_irn_mode(left) == get_modeP())
				^ (get_irn_mode(right) == get_modeP()));
		if (get_irn_mode(left) == get_modeP()) {
			p_pred = left;
		} else if (get_irn_mode(right) == get_modeP()) {
			//panic("Pointer on the right!"); //Pointer apparently isn't always on the left
			p_pred = right;
		}
		assert(p_pred != NULL);
		lfptr_meta *add_meta = get_node_meta(env, p_pred);
		meta = add_meta;
		pmap_insert(lf_map, irn, meta);
		DB((dbg, LEVEL_5, "%+F has transitive base: %+F, size: %+F inherited from %+F\n", irn, add_meta->base, add_meta->size, p_pred));

	} else if (is_Const(irn) || is_Address(irn)) {
		meta = nonlf_meta;
		pmap_insert(lf_map, irn, meta);
		DB((dbg, LEVEL_5, "%+F has default meta\n", irn));

	} else if (is_Phi(irn)) {
		int phi_arity = get_Phi_n_preds(irn);
		ir_node **base_phi_preds = (ir_node**)malloc(sizeof(ir_node*) * phi_arity);
		ir_node **size_phi_preds = (ir_node**)malloc(sizeof(ir_node*) * phi_arity);

		for (int i = 0; i < phi_arity; i++) {
			base_phi_preds[i] = new_r_Bad(irg, get_modeP());
			size_phi_preds[i] = new_r_Bad(irg, get_modeLu());
		}

		ir_node *base_phi = new_rd_Phi(dbgi, block, phi_arity, base_phi_preds, get_modeP());
		ir_node *size_phi = new_rd_Phi(dbgi, block, phi_arity, size_phi_preds, get_modeLu());

		free(base_phi_preds);
		free(size_phi_preds);

		meta = new_lfptr_meta(base_phi, size_phi, false); //set tmp values for new meta
		pmap_insert(lf_map, irn, meta);

		bool arithmetic = false;
		for (int i = 0; i < phi_arity; i++) {
			lfptr_meta *pred_meta = get_node_meta(env, get_Phi_pred(irn, i));
			arithmetic |= pred_meta->arithmetic;
			set_Phi_pred(base_phi, i, pred_meta->base);
			set_Phi_pred(size_phi, i, pred_meta->size);
		}
		meta->arithmetic = arithmetic;

		add_Block_phi(block, base_phi);
		add_Block_phi(block, size_phi);
		mark_irn_visited(base_phi);
		mark_irn_visited(size_phi);
		assert(get_irn_mode(base_phi) == get_modeP());
		assert(get_irn_mode(size_phi) == get_modeLu());
		DB((dbg, LEVEL_5, "%+F with base: %+F, size: %+F\n", irn, base_phi, size_phi));
	} else if (is_Member(irn)) {
		ir_node *pred = get_Member_ptr(irn);
		assert(get_irn_mode(pred) == get_modeP());
		meta = get_node_meta(env, pred);
		pmap_insert(lf_map, irn, meta);
		DB((dbg, LEVEL_5,
			"%+F is Member and inherits meta from predecessor %+F: base: %+F, size: %+F\n",
			irn, pred, meta->base, meta->size));
	} else {
		meta = calc_metadata(irn, sizes_address);
		pmap_insert(lf_map, irn, meta);
		assert(get_irn_mode(meta->base) == get_modeP());
		assert(get_irn_mode(meta->size) == get_modeLu());
		DB((dbg, LEVEL_5,
			"%+F is of unknown type with calculated metadata: base: %+F, size: %+F\n",
			irn, meta->base, meta->size));
	}
	return meta;
}

// Insert bound check between irn and ptr. Doesn't if the metadata of ptr isn't arithmetic.
static void insert_bound_check_between(ir_node *irn, ir_node *ptr,
	                                   insert_instrumentation_env *env,
                                       unsigned int reason) {
	DB((dbg, LEVEL_5, "try inserting check between %+F and %+F (reason: %i)\n", irn, ptr, reason));

	dbg_info *dbgi = get_irn_dbg_info(irn);

	assert(get_irn_mode(ptr) == get_modeP());

	lfptr_meta *meta   = get_node_meta(env, ptr);
	if (!meta->arithmetic) {
		DB((dbg, LEVEL_5, "Skipping check, pointer hasn't been changed by pointer arithmetic\n"));
		return;
	}
	DB((dbg, LEVEL_5, "Check needed\n"));
	ir_node    *base   = meta->base;
	ir_node    *size   = meta->size;

	pmap *lf_map = env->lf_map;

	// No bounds checking when ptr is constant.
	if (is_Address(ptr) || is_Const(ptr)) {
		return;
	}

	ir_graph *irg = get_irn_irg(irn);

	ir_node *old_block = get_nodes_block(ptr);
	DB((dbg, LEVEL_5, "old block: %+F\n", old_block));
	lf_part_block(lf_map, ptr);
	ir_node *new_block = get_nodes_block(ptr);
	DB((dbg, LEVEL_5, "new block: %+F\n", new_block));

	ir_node *start = base;
	assert(get_irn_mode(base) == get_modeP());
	ir_node *end = new_rd_Add(dbgi, new_block, base,
	                          new_rd_Conv(dbgi, new_block, size, get_modeLs()));
	mark_irn_visited(end);

	ir_node *cmp_lower = new_rd_Cmp(dbgi, new_block, start, ptr, ir_relation_less_equal);
	ir_node *cmp_upper = new_rd_Cmp(dbgi, new_block, end,   ptr, ir_relation_greater);

	ir_node *in_bounds = new_rd_And(dbgi, new_block, cmp_lower, cmp_upper);

	ir_node *cond = new_rd_Cond(dbgi, new_block, in_bounds);
	mark_irn_visited(cond);

	ir_node *proj_true  = new_rd_Proj(dbgi, cond, get_modeX(), pn_Cond_true);
	ir_node *proj_false = new_rd_Proj(dbgi, cond, get_modeX(), pn_Cond_false);

	ir_node *bb_false_in[1] = { proj_false };
	ir_node *bb_err = new_rd_Block(dbgi, irg, ARRAY_SIZE(bb_false_in), bb_false_in);

	ir_node* abort_call = error_call(ptr, bb_err, lf_map, reason, dbgi);
	mark_irn_visited(abort_call);

	keep_alive(abort_call);
	keep_alive(bb_err);

	ir_node* bb_true_in[1] = { proj_true };
	set_irn_in(old_block, ARRAY_SIZE(bb_true_in), bb_true_in);

	//dump_ir_graph(irg, "lf-asan-boundcheck");
}

/*
 * During the second phase const nodes move into other blocks, which causes dominace issues.
 * This pass moves them back to the start block.
 * Hacky but it works.
 */
static void fix_const_nodes(ir_node *irn, void *env) {
	(void) env;
	if (is_Const(irn) || is_Address(irn)) {
		ir_graph *irg = get_irn_irg(irn);
		ir_node *start_node = get_irg_start(irg);
		ir_node *start_block = get_nodes_block(start_node);
		set_nodes_block(irn, start_block);
	}
}

FIRM_API void lowfat_asan(ir_graph *irg) {
	//printf("lf asan running\n");
	//assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);

	int opt_level = get_optimize(); //Don't optimize anything, while manipulating the graph
	set_optimize(0);

	FIRM_DBG_REGISTER(dbg, "firm.opt.lfasan");
	DB((dbg, LEVEL_1, "\n===> instrumenting %+F for lowfat adresssanitizer.\n", irg));

	dump_ir_graph(irg, "lf-asan-before");

	DB((dbg, LEVEL_2, "=> Replacing memory functions with lowfat alternatives.\n"));
	irg_walk_graph(irg, NULL, func_ptr_rename, NULL);

	DB((dbg, LEVEL_2, "=> Creating global lookup table.\n"));
	ir_entity *sizes_entity  = create_global_lookup_table();
	ir_node   *sizes_address = new_r_Address(irg, sizes_entity);
	DB((dbg, LEVEL_4, "Global Lookup node: %+F\n", sizes_address));

	DB((dbg, LEVEL_2, "=> Collect bound check node pairs.\n"));
	bounds_check_pair* f_array = NEW_ARR_F(bounds_check_pair, 0);
	bounds_check_pair** collected_pairs = &f_array;
	irg_walk_graph(irg, NULL, collect_bounds_check_node_pairs, collected_pairs);

	DB((dbg, LEVEL_2, "=> Inserting instrumentation nodes.\n"));
	pmap* lf_map = pmap_create(); /*ir_node -> lfptr_meta*/
	lfptr_meta *nonlf_meta = new_lfptr_meta(new_r_Const_long(irg, get_modeP(), 0),
	                                        new_r_Const_long(irg, get_modeLu(), -1),
	                                        false);
	insert_instrumentation_env env; //TODO: change name of insert_instrumentation_env
	env.lf_map        = lf_map;
	env.sizes_address = sizes_address;
	env.nonlf_meta    = nonlf_meta;
	for (unsigned int i = 0; i < ARR_LEN(*collected_pairs); i++) {
		bounds_check_pair pair = (*collected_pairs)[i];
		insert_bound_check_between(pair.node, pair.ptr, &env, pair.type);
	}
	DEL_ARR_F(*collected_pairs);

	DB((dbg, LEVEL_5, "-metadata-map-----------------------------------------\n"));
	pp_metadata_map(lf_map);

	DB((dbg, LEVEL_2, "=> Fixing blocks of const/address nodes.\n"));
	irg_walk_graph(irg, NULL, fix_const_nodes, NULL);

	/*Freeing allocated memory*/
	lfptr_meta* cur = lfptr_meta_first;
	while (cur != NULL) {
		lfptr_meta* f = cur;
		cur = cur->next;
		free(f);
	}
	lfptr_meta_first = NULL;
	pmap_destroy(lf_map);

	set_optimize(opt_level);
	dump_ir_graph(irg, "lf-asan-after");

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}
