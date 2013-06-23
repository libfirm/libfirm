/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower small CopyB nodes into a series of Load/Store nodes
 * @author  Michael Beck, Matthias Braun, Manuel Mohr
 */
#include "adt/list.h"
#include "ircons.h"
#include "lowering.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irgmod.h"
#include "error.h"
#include "be.h"
#include "util.h"

typedef struct entry entry_t;
struct entry {
	struct list_head list;
	ir_node *copyb;
};

/**
 * Every CopyB is assigned a size category as follows:
 *  - 'small'  iff                  size <= max_small_size,
 *  - 'medium' iff max_small_size < size <  min_large_size,
 *  - 'large'  iff                  size >= min_large_size.
 *
 * The idea is that each backend can apply different optimizations in each
 * of the three categories.
 *
 * For small CopyBs, the x86 backend could, e.g., emit a single SSE
 * instruction to copy 16 bytes.  Other backends might just go with a series
 * of Load/Stores.  Therefore, x86 would like to keep the small CopyB nodes
 * around whereas other backends would not.
 * For medium-sized CopyBs, the x86 backend might generate a rep-prefixed mov
 * instruction.  Hence, it also wants to keep the CopyBs in these cases.  Other
 * backends might handle this differently.
 * For large CopyBs, a call to memcpy is worth the call overhead, so large
 * CopyBs should always be lowered to memcpy calls.
 *
 * The lowerer performs the following actions if the CopyB is
 * - 'small':  Replace it with a series of Loads/Stores
 * - 'medium': Nothing.
 * - 'large':  Replace it with a call to memcpy.
 *
 * max_small_size and min_large_size allow for a flexible configuration.
 * For example, one backend could specify max_small_size == 0 and
 * min_large_size == 8192 to keep all CopyB nodes smaller than 8192 and get
 * memcpy Calls for all others.  Here, the set of small CopyBs is empty.
 * Another backend could specify max_small_size == 63 and min_large_size == 64
 * to lower all small CopyBs to Loads/Stores and all big CopyBs to memcpy.
 * Hence, the set of medium-sized CopyBs is empty and this backend never
 * sees a CopyB node at all.
 * If memcpy is not available, min_large_size can be set to UINT_MAX to prevent
 * the creation of calls to memcpy.  Note that CopyBs whose size is UINT_MAX
 * will still be lowered to memcpy calls because we check if the size is greater
 * *or equal* to min_large_size.  However, this should never occur in practice.
 */

static unsigned max_small_size; /**< The maximum size of a CopyB node
                                     so that it is regarded as 'small'. */
static unsigned min_large_size; /**< The minimum size of a CopyB node
                                     so that it is regarded as 'large'. */
static unsigned native_mode_bytes; /**< The size of the native mode in bytes. */
static int allow_misalignments; /**< Whether backend can handle misaligned
                                     loads and stores. */

typedef struct walk_env {
	struct obstack   obst;           /**< the obstack where data is allocated
	                                      on. */
	struct list_head list;           /**< the list of copyb nodes. */
} walk_env_t;

static ir_mode *get_ir_mode(unsigned mode_bytes)
{
	switch (mode_bytes) {
	case 1:  return mode_Bu;
	case 2:  return mode_Hu;
	case 4:  return mode_Iu;
	case 8:  return mode_Lu;
	case 16: return mode_LLu;
	default:
		panic("unexpected mode size requested in copyb lowering");
	}
}

/**
 * Turn a small CopyB node into a series of Load/Store nodes.
 */
static void lower_small_copyb_node(ir_node *irn)
{
	ir_graph *irg        = get_irn_irg(irn);
	ir_node  *block      = get_nodes_block(irn);
	ir_type  *tp         = get_CopyB_type(irn);
	ir_node  *addr_src   = get_CopyB_src(irn);
	ir_node  *addr_dst   = get_CopyB_dst(irn);
	ir_node  *mem        = get_CopyB_mem(irn);
	ir_mode  *addr_mode  = get_irn_mode(addr_src);
	unsigned  mode_bytes = allow_misalignments ? native_mode_bytes : tp->align;
	unsigned  size       = get_type_size_bytes(tp);
	unsigned  offset     = 0;
	ir_mode  *mode;

	while (offset < size) {
		mode = get_ir_mode(mode_bytes);
		for (; offset + mode_bytes <= size; offset += mode_bytes) {
			/* construct offset */
			ir_node *addr_const;
			ir_node *add;
			ir_node *load;
			ir_node *load_res;
			ir_node *load_mem;
			ir_node *store;
			ir_node *store_mem;

			addr_const = new_r_Const_long(irg, mode_Iu, offset);
			add        = new_r_Add(block, addr_src, addr_const, addr_mode);

			load     = new_r_Load(block, mem, add, mode, cons_none);
			load_res = new_r_Proj(load, mode, pn_Load_res);
			load_mem = new_r_Proj(load, mode_M, pn_Load_M);

			addr_const = new_r_Const_long(irg, mode_Iu, offset);
			add        = new_r_Add(block, addr_dst, addr_const, addr_mode);

			store     = new_r_Store(block, load_mem, add, load_res, cons_none);
			store_mem = new_r_Proj(store, mode_M, pn_Store_M);

			mem = store_mem;
		}

		mode_bytes /= 2;
	}

	exchange(irn, mem);
}

static ir_type *get_memcpy_methodtype(void)
{
	ir_type *tp          = new_type_method(3, 1);
	ir_mode *size_t_mode = get_ir_mode(native_mode_bytes);

	set_method_param_type(tp, 0, get_type_for_mode(mode_P));
	set_method_param_type(tp, 1, get_type_for_mode(mode_P));
	set_method_param_type(tp, 2, get_type_for_mode(size_t_mode));
	set_method_res_type  (tp, 0, get_type_for_mode(mode_P));

	return tp;
}

static ir_node *get_memcpy_symconst(ir_graph *irg)
{
	ident     *id  = new_id_from_str("memcpy");
	ir_type   *mt  = get_memcpy_methodtype();
	ir_entity *ent = create_compilerlib_entity(id, mt);
	symconst_symbol sym;

	sym.entity_p = ent;
	return new_r_SymConst(irg, mode_P_code, sym, symconst_addr_ent);
}

/**
 * Turn a large CopyB node into a memcpy call.
 */
static void lower_large_copyb_node(ir_node *irn)
{
	ir_graph *irg      = get_irn_irg(irn);
	ir_node  *block    = get_nodes_block(irn);
	dbg_info *dbgi     = get_irn_dbg_info(irn);
	ir_node  *mem      = get_CopyB_mem(irn);
	ir_node  *addr_src = get_CopyB_src(irn);
	ir_node  *addr_dst = get_CopyB_dst(irn);
	ir_type  *copyb_tp = get_CopyB_type(irn);
	unsigned  size     = get_type_size_bytes(copyb_tp);

	ir_node  *symconst    = get_memcpy_symconst(irg);
	ir_type  *call_tp     = get_memcpy_methodtype();
	ir_mode  *mode_size_t = get_ir_mode(native_mode_bytes);
	ir_node  *in[3];
	ir_node  *call;
	ir_node  *call_mem;

	in[0]    = addr_dst;
	in[1]    = addr_src;
	in[2]    = new_r_Const_long(irg, mode_size_t, size);
	call     = new_rd_Call(dbgi, block, mem, symconst, 3, in, call_tp);
	call_mem = new_r_Proj(call, mode_M, pn_Call_M);

	exchange(irn, call_mem);
}

static void lower_copyb_node(ir_node *irn)
{
	ir_type *tp   = get_CopyB_type(irn);
	unsigned size = get_type_size_bytes(tp);

	if (size <= max_small_size)
		lower_small_copyb_node(irn);
	else if (size >= min_large_size)
		lower_large_copyb_node(irn);
	else
		panic("CopyB of invalid size");
}

/**
 * Post-Walker: find CopyB nodes.
 */
static void find_copyb_nodes(ir_node *irn, void *ctx)
{
	walk_env_t *env = (walk_env_t*)ctx;
	ir_type    *tp;
	unsigned   size;
	entry_t    *entry;
	bool        medium_sized;

	if (! is_CopyB(irn))
		return;

	tp = get_CopyB_type(irn);
	if (get_type_state(tp) != layout_fixed)
		return;

	size         = get_type_size_bytes(tp);
	medium_sized = max_small_size < size && size < min_large_size;
	if (medium_sized)
		return; /* Nothing to do for medium-sized CopyBs. */

	/* Okay, either small or large CopyB, so link it in and lower it later. */
	entry = OALLOC(&env->obst, entry_t);
	entry->copyb = irn;
	INIT_LIST_HEAD(&entry->list);
	set_irn_link(irn, entry);
	list_add_tail(&entry->list, &env->list);
}

void lower_CopyB(ir_graph *irg, unsigned max_small_sz, unsigned min_large_sz,
                 int allow_misaligns)
{
	const backend_params *bparams = be_get_backend_param();
	walk_env_t            env;

	assert(max_small_sz < min_large_sz && "CopyB size ranges must not overlap");

	max_small_size      = max_small_sz;
	min_large_size      = min_large_sz;
	native_mode_bytes   = bparams->machine_size / 8;
	allow_misalignments = allow_misaligns;

	obstack_init(&env.obst);
	INIT_LIST_HEAD(&env.list);
	irg_walk_graph(irg, NULL, find_copyb_nodes, &env);

	list_for_each_entry(entry_t, entry, &env.list, list) {
		lower_copyb_node(entry->copyb);
	}

	obstack_free(&env.obst, NULL);
}
