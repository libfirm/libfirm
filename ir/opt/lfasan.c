#include "lfasan.h"

#include "firm_types.h"
#include "ident.h"
#include "ircons.h"
#include "irdom.h"
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
#include "irgmod.h"
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

static void func_ptr_rename(ir_node *irn, void *env) {
	(void) env;
	if (is_Call(irn)) {
		ir_node *func_ptr = get_Call_ptr(irn);
		assert(streq("Address", get_op_name(get_irn_op(func_ptr))));

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
		set_entity_initializer(table_entity, init);
	}

	DB((dbg, LEVEL_4, "Global Lookup entity: %+F\n", table_entity));
	return table_entity;
}

typedef struct _lfptr_meta {
	struct _lfptr_meta *next;
	ir_node *base;
	ir_node *size;
} lfptr_meta;

lfptr_meta *lfptr_meta_first = NULL;

/*returns uninitialized lfptr_meta*/
static lfptr_meta *new_lfptr_meta(ir_node* base, ir_node *size) {
	lfptr_meta *new = (lfptr_meta*)malloc(sizeof(lfptr_meta));
	new->next = lfptr_meta_first;
	new->base = base;
	new->size = size;
	lfptr_meta_first = new;
	return new;
}

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
				const char* ld_name = get_entity_ld_name(get_irn_entity_attr(ptr));
				ir_node *size = NULL;
				// Check every allocation (except lf_posix_memalign) to infer the correc
				// metadata by their parameters.
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
					return new_lfptr_meta(irn, size);
				} else {
					return NULL;
				}
			}
		}
	}
	return NULL;
}

static lfptr_meta *calc_metadata(ir_node *irn, ir_node *sizes_lookup) {
	dbg_info *dbgi = get_irn_dbg_info_(irn);

	assert(get_irn_mode(irn) == get_modeP());
	ir_node *block = get_nodes_block(irn);
	ir_graph *irg = get_irn_irg(irn);
	ir_node *nomem = get_irg_no_mem(irg);
	mark_irn_visited(sizes_lookup);

	ir_node *const_region_bits = new_r_Const_long(irg, get_modeLu(), REGION_BITS);
	ir_node *conv_p_lu         = new_rd_Conv(dbgi, block, irn, get_modeLu());
	ir_node *index             = new_rd_Shr(dbgi, block, conv_p_lu, const_region_bits);
	ir_node *byte_offset       = new_rd_Mul(dbgi, block, index,
	                                    new_rd_Const_long(dbgi, irg, get_modeLu(), 8));
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
	ir_node *base         = new_rd_Conv(dbgi, block, sub, get_modeP());
	mark_irn_visited(base);

	lfptr_meta *res = new_lfptr_meta(base, size);
	return res;
}

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
static void move(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	if (get_nodes_block(node) != from_bl) {
		return;
	}

	move_node(node, to_bl);

	if (is_Phi(node))
		return;

	/* recursion ... */
	foreach_irn_in(node, i, pred) {
		if (get_nodes_block(pred) == from_bl)
			move(pred, from_bl, to_bl);
	}
}

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
	ir_entity *entity = new_entity(get_glob_type(), id_unique("filename"), type);
	set_entity_initializer(entity, init);
	return entity;
}

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

static void insert_bound_check_between(ir_node *irn, ir_node *ptr, pmap *lf_map,
                                       unsigned int reason) {
	dbg_info *dbgi = get_irn_dbg_info(irn);

	assert(get_irn_mode(ptr) == get_modeP());
	assert(pmap_contains(lf_map, ptr));

	// No bounds checking when ptr is constant.
	if (is_Address(ptr) || is_Const(ptr)) {
		return;
	}

	DB((dbg, LEVEL_5, "inserting check between %+F and %+F (reason: %i)\n", irn, ptr, reason));

	ir_graph *irg = get_irn_irg(irn);

	ir_node *old_block = get_nodes_block(ptr);
	DB((dbg, LEVEL_5, "old block: %+F\n", old_block));
	part_block(ptr);
	ir_node *new_block = get_nodes_block(ptr);
	DB((dbg, LEVEL_5, "new block: %+F\n", new_block));

	assert(pmap_contains(lf_map, ptr));
	lfptr_meta *meta = (lfptr_meta*)pmap_find(lf_map, ptr)->value;
	ir_node *base = meta->base;
	ir_node *size = meta->size;

	move(base, old_block, new_block);
	move(size, old_block, new_block);

	ir_node *start = base;
	assert(get_irn_mode(base) == get_modeP());
	ir_node *end = new_rd_Add(dbgi, new_block, base,
	                          new_rd_Conv(dbgi, new_block, size, get_modeLs()));
	mark_irn_visited(end);

	ir_node *cmp_lower = new_rd_Cmp(dbgi, new_block, start, ptr, ir_relation_less_equal);
	ir_node *cmp_upper = new_rd_Cmp(dbgi, new_block, end, ptr, ir_relation_greater);

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
}

typedef struct {
	pmap *lf_map;
	ir_node *sizes_address;
	lfptr_meta *nonlf_meta;
} insert_instrumentation_env;

static void insert_instrumentation(ir_node *irn, void * env) {
	pmap       *lf_map        = ((insert_instrumentation_env*) env)->lf_map;
	ir_node    *sizes_address = ((insert_instrumentation_env*) env)->sizes_address;
	lfptr_meta *nonlf_meta    = ((insert_instrumentation_env*) env)->nonlf_meta;
	mark_irn_visited(nonlf_meta->base);
	mark_irn_visited(nonlf_meta->size);

	dbg_info *dbgi = get_irn_dbg_info(irn);

	ir_mode *mode = get_irn_mode(irn);
	if (is_Block(irn)) {
		return;
	}
	ir_node *block = get_nodes_block(irn);

	if (mode == get_modeP()) {
		lfptr_meta* meta = is_alloc_res(irn);
		if (meta != NULL) {
			DB((dbg, LEVEL_5, "Newly allocated pointer %+F with base: %+F, size: %+F\n", irn, meta->base, meta->size));
			assert(get_irn_mode(meta->base) == get_modeP());
			assert(get_irn_mode(meta->size) == get_modeLu());
			pmap_insert(lf_map, meta->base, meta);
		} else if (is_Add(irn) || is_Sub(irn)) {
			DB((dbg, LEVEL_5, "%+F", irn));
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
			if (get_irn_mode(left) == get_modeP()) {
				assert(pmap_contains(lf_map, left));
				p_pred = left;
			} else if (get_irn_mode(right) == get_modeP()) {
				panic("Pointer on the right!");
				//assert(pmap_contains(lf_map, right));
				//p_pred = right;
			}
			assert(p_pred != NULL);
			lfptr_meta *add_meta = pmap_find(lf_map, left)->value;
			pmap_insert(lf_map, irn, add_meta);
			DB((dbg, LEVEL_5, " with transitive base: %+F, size: %+F inherited from %+F\n", add_meta->base, add_meta->size, p_pred));
		} else if (is_Const(irn) || is_Address(irn)) {
			pmap_insert(lf_map, irn, nonlf_meta);
			DB((dbg, LEVEL_5, "%+F with default meta\n", irn));
		} else if (is_Phi(irn)) {
			DB((dbg, LEVEL_5, "Phi %+F", irn));
			int phi_arity = get_Phi_n_preds(irn);
			ir_node **base_phi_preds = (ir_node**)malloc(sizeof(ir_node*) * phi_arity);
			ir_node **size_phi_preds = (ir_node**)malloc(sizeof(ir_node*) * phi_arity);
			for (int i = 0; i < phi_arity; i++) {
				lfptr_meta* meta  = pmap_find(lf_map, get_Phi_pred(irn, i))->value;
				base_phi_preds[i] = meta->base;
				size_phi_preds[i] = meta->size;
			}
			ir_node *base_phi = new_rd_Phi(dbgi, block, phi_arity, base_phi_preds, get_modeP());
			ir_node *size_phi = new_rd_Phi(dbgi, block, phi_arity, size_phi_preds, get_modeLu());
			add_Block_phi(block, base_phi);
			add_Block_phi(block, size_phi);
			mark_irn_visited(base_phi);
			mark_irn_visited(size_phi);
			assert(get_irn_mode(base_phi) == get_modeP());
			assert(get_irn_mode(size_phi) == get_modeLu());
			pmap_insert(lf_map, irn, new_lfptr_meta(base_phi, size_phi));
			DB((dbg, LEVEL_5, " with base: %+F, size: %+F\n", base_phi, size_phi));
		} else {
			DB((dbg, LEVEL_5, "Unknown Pointer %+F", irn));
			lfptr_meta* meta = calc_metadata(irn, sizes_address);
			assert(get_irn_mode(meta->base) == get_modeP());
			assert(get_irn_mode(meta->size) == get_modeLu());
			pmap_insert(lf_map, irn, meta);
			DB((dbg, LEVEL_5, " with calculated metadata: base: %+F, size: %+F\n",
			    meta->base, meta->size));
		}
	} else if (is_Store(irn)) {
		ir_node* ptr = get_Store_ptr(irn);
		insert_bound_check_between(irn, ptr, lf_map, MEMORY_WRITE);
		ir_node* val = get_Store_value(irn);
		if (get_irn_mode(val) == get_modeP() && !is_Const(irn) && !is_Address(irn)) {
			insert_bound_check_between(irn, val, lf_map, MEMORY_ESCAPE);
		}
	} else if (is_Load(irn)) {
		ir_node* ptr = get_Load_ptr(irn);
		insert_bound_check_between(irn, ptr, lf_map, MEMORY_READ);
	} else if (is_Call(irn)) {
		int call_params = get_Call_n_params(irn);
		for (int i = 0; i < call_params; i++) {
			ir_node *param = get_Call_param(irn, i);
			DB((dbg, LEVEL_5, "%+F and %+F\n", irn, param));
			if (get_irn_mode(param) == get_modeP()) {
				insert_bound_check_between(irn, param, lf_map, FUNCTION_ESCAPE);
			}
		}
	}
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
	//assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);

	int opt_level = get_optimize();
	set_optimize(0);

	FIRM_DBG_REGISTER(dbg, "firm.opt.lfasan");
	DB((dbg, LEVEL_1, "\n===> instrumenting %+F for lowfat adresssanitizer.\n", irg));

	DB((dbg, LEVEL_2, "=> Replacing memory functions with lowfat alternatives.\n"));
	irg_walk_graph(irg, NULL, func_ptr_rename, NULL);

	DB((dbg, LEVEL_2, "=> Creating global lookup table.\n"));
	ir_entity *sizes_entity  = create_global_lookup_table();
	ir_node   *sizes_address = new_r_Address(irg, sizes_entity);
	DB((dbg, LEVEL_4, "Global Lookup node: %+F\n", sizes_address));

	DB((dbg, LEVEL_2, "=> Inserting instrumentation nodes.\n"));
	pmap* lf_map = pmap_create(); /*ir_node -> lfptr_meta*/
	lfptr_meta *nonlf_meta = new_lfptr_meta(new_r_Const_long(irg, get_modeP(), 0),
	                                        new_r_Const_long(irg, get_modeLu(), -1));
	insert_instrumentation_env env;
	env.lf_map        = lf_map;
	env.sizes_address = sizes_address;
	env.nonlf_meta    = nonlf_meta;
	irg_walk_graph(irg, NULL, insert_instrumentation, &env);

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
	DB((dbg, LEVEL_2, "Done instrumenting %+F\n", irg));

	set_optimize(opt_level);
	dump_ir_graph(irg, "lf-asan");

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	printf("Compiled a file with lfasan\n");
}
