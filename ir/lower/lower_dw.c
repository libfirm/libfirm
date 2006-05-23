/**
 * @file irlwrdw.c
 * @date 8.10.2004
 * @author Michael Beck
 * @brief Lower Double word operations, ie Mode L -> I.
 *
 * $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "tv_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "firmstat.h"
#include "irgwalk.h"
#include "ircons.h"
#include "lower_dw.h"
#include "irflag.h"
#include "irtools.h"
#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "pdeq.h"
#include "irdump.h"

/** A map from mode to a primitive type. */
static pmap *prim_types;

/** A map from (op, imode, omode) to Intrinsic functions entities. */
static set *intrinsic_fkt;

/** A map from (imode, omode) to conv function types. */
static set *conv_types;

/** A map from a method type to its lowered type. */
static pmap *lowered_type;

/** The types for the binop and unop intrinsics. */
static ir_type *binop_tp_u, *binop_tp_s, *unop_tp_u, *unop_tp_s, *shiftop_tp_u, *shiftop_tp_s, *tp_s, *tp_u;

/** the debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * An entry in the (op, imode, omode) -> entity map.
 */
typedef struct _op_mode_entry {
	const ir_op   *op;    /**< the op */
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	entity        *ent;   /**< the associated entity of this (op, imode, omode) triple */
} op_mode_entry_t;

/**
 * An entry in the (imode, omode) -> tp map.
 */
typedef struct _conv_tp_entry {
	const ir_mode *imode; /**< the input mode */
	const ir_mode *omode; /**< the output mode */
	ir_type       *mtd;   /**< the associated method type of this (imode, omode) pair */
} conv_tp_entry_t;

/**
 * Every double word node will be replaced,
 * we need some store to hold the replacement:
 */
typedef struct _node_entry_t {
	ir_node *low_word;        /**< the low word */
	ir_node *high_word;       /**< the high word */
} node_entry_t;

enum lower_flags {
	MUST_BE_LOWERED = 1,   /**< graph must be lowered */
	CF_CHANGED      = 2,   /**< control flow was changed */
};

/**
 * The lower environment.
 */
typedef struct _lower_env_t {
	node_entry_t **entries;       /**< entries per node */
	struct obstack obst;          /**< an obstack holding the temporary data */
	tarval   *tv_mode_bytes;      /**< a tarval containing the number of bytes in the lowered modes */
	tarval   *tv_mode_bits;       /**< a tarval containing the number of bits in the lowered modes */
	pdeq     *waitq;              /**< a wait queue of all nodes that must be handled later */
	pmap     *proj_2_block;       /**< a map from ProjX to its destination blocks */
	const lwrdw_param_t *params;  /**< transformation parameter */
	unsigned flags;               /**< some flags */
	int      n_entries;           /**< number of entries */
} lower_env_t;

/**
 * Get a primitive mode for a mode.
 */
static ir_type *get_primitive_type(ir_mode *mode) {
	pmap_entry *entry = pmap_find(prim_types, mode);
	ir_type *tp;
	char buf[64];

	if (entry)
		return entry->value;

	snprintf(buf, sizeof(buf), "_prim_%s", get_mode_name(mode));
	tp = new_type_primitive(new_id_from_str(buf), mode);

	pmap_insert(prim_types, mode, tp);
	return tp;
}

/**
 * Create a method type for a Conv emulation from imode to omode.
 */
static ir_type *get_conv_type(ir_mode *imode, ir_mode *omode, lower_env_t *env) {
	conv_tp_entry_t key, *entry;
	ir_type *mtd;

	key.imode = imode;
	key.omode = omode;
	key.mtd   = NULL;

	entry = set_insert(conv_types, &key, sizeof(key), HASH_PTR(imode) ^ HASH_PTR(omode));
	if (! entry->mtd) {
		int n_param = 1, n_res = 1;
		char buf[64];

		if (imode == env->params->high_signed || imode == env->params->high_unsigned)
			n_param = 2;
		if (omode == env->params->high_signed || omode == env->params->high_unsigned)
			n_res = 2;

		/* create a new one */
		snprintf(buf, sizeof(buf), "LConv%s%s", get_mode_name(imode), get_mode_name(omode));
		mtd = new_type_method(new_id_from_str(buf), n_param, n_res);

		/* set param types and result types */
		n_param = 0;
		if (imode == env->params->high_signed) {
			set_method_param_type(mtd, n_param++, tp_s);
			set_method_param_type(mtd, n_param++, tp_s);
		}
		else if (imode == env->params->high_unsigned) {
			set_method_param_type(mtd, n_param++, tp_u);
			set_method_param_type(mtd, n_param++, tp_u);
		}
		else {
			ir_type *tp = get_primitive_type(imode);
			set_method_param_type(mtd, n_param++, tp);
		}

		n_res = 0;
		if (omode == env->params->high_signed) {
			set_method_res_type(mtd, n_res++, tp_s);
			set_method_res_type(mtd, n_res++, tp_s);
		}
		else if (omode == env->params->high_unsigned) {
			set_method_res_type(mtd, n_res++, tp_u);
			set_method_res_type(mtd, n_res++, tp_u);
		}
		else {
			ir_type *tp = get_primitive_type(omode);
			set_method_res_type(mtd, n_res++, tp);
		}

		entry->mtd = mtd;
	}
	else
		mtd = entry->mtd;
	return mtd;
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input number nr.
 */
static void add_block_cf_input_nr(ir_node *block, int nr, ir_node *cf)
{
	int i, arity = get_irn_arity(block);
	ir_node **in, *phi;

	assert(nr < arity);

	NEW_ARR_A(ir_node *, in, arity + 1);
	for (i = 0; i < arity; ++i)
		in[i] = get_irn_n(block, i);
	in[i] = cf;

	set_irn_in(block, i + 1, in);

	for (phi = get_irn_link(block); phi; phi = get_irn_link(phi)) {
		for (i = 0; i < arity; ++i)
			in[i] = get_irn_n(phi, i);
		in[i] = in[nr];
		set_irn_in(phi, i + 1, in);
	}
}

/**
 * Add an additional control flow input to a block.
 * Patch all Phi nodes. The new Phi inputs are copied from
 * old input from cf tmpl.
 */
static void add_block_cf_input(ir_node *block, ir_node *tmpl, ir_node *cf)
{
	int i, arity = get_irn_arity(block);
	int nr = 0;

	for (i = 0; i < arity; ++i) {
		if (get_irn_n(block, i) == tmpl) {
			nr = i;
			break;
		}
	}
	assert(i < arity);
	add_block_cf_input_nr(block, nr, cf);
}

/**
 * Return the "operational" mode of a Firm node.
 */
static ir_mode *get_irn_op_mode(ir_node *node)
{
	switch (get_irn_opcode(node)) {
	case iro_Load:
		return get_Load_mode(node);

	case iro_Store:
		return get_irn_mode(get_Store_value(node));

	case iro_DivMod:
		return get_irn_mode(get_DivMod_left(node));

	case iro_Div:
		return get_irn_mode(get_Div_left(node));

	case iro_Mod:
		return get_irn_mode(get_Mod_left(node));

	case iro_Cmp:
		return get_irn_mode(get_Cmp_left(node));

	default:
		return get_irn_mode(node);
	}
}

/**
 * walker, prepare the node links
 */
static void prepare_links(ir_node *node, void *env)
{
	lower_env_t  *lenv = env;
	ir_mode      *mode = get_irn_op_mode(node);
	node_entry_t *link;
	int          i;

	if (mode == lenv->params->high_signed ||
		mode == lenv->params->high_unsigned) {
		/* ok, found a node that will be lowered */
		link = obstack_alloc(&lenv->obst, sizeof(*link));

		memset(link, 0, sizeof(*link));

		lenv->entries[get_irn_idx(node)] = link;
		lenv->flags |= MUST_BE_LOWERED;
	}
	else if (get_irn_op(node) == op_Conv) {
		/* Conv nodes have two modes */
		ir_node *pred = get_Conv_op(node);
		mode = get_irn_mode(pred);

		if (mode == lenv->params->high_signed ||
			mode == lenv->params->high_unsigned) {
			/* must lower this node either */
			link = obstack_alloc(&lenv->obst, sizeof(*link));

			memset(link, 0, sizeof(*link));

			lenv->entries[get_irn_idx(node)] = link;
			lenv->flags |= MUST_BE_LOWERED;
		}
		return;
	}

	if (is_Proj(node)) {
		/* link all Proj nodes to its predecessor:
		   Note that Tuple Proj's and its Projs are linked either. */
		ir_node *pred = get_Proj_pred(node);

		set_irn_link(node, get_irn_link(pred));
		set_irn_link(pred, node);
	}
	else if (is_Phi(node)) {
		/* link all Phi nodes to its block */
		ir_node *block = get_nodes_block(node);

		set_irn_link(node, get_irn_link(block));
		set_irn_link(block, node);
	}
	else if (is_Block(node)) {
		/* fill the Proj -> Block map */
		for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred(node, i);

			if (is_Proj(pred))
				pmap_insert(lenv->proj_2_block, pred, node);
		}
	}
}

/**
 * Translate a Constant: create two.
 */
static void lower_Const(ir_node *node, ir_mode *mode, lower_env_t *env) {
	tarval   *tv, *tv_l, *tv_h;
	ir_node  *low, *high;
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	int      idx;

	tv   = get_Const_tarval(node);

	tv_l = tarval_convert_to(tv, mode);
	low  = new_rd_Const(dbg, current_ir_graph, block, mode, tv_l);

	tv_h = tarval_convert_to(tarval_shrs(tv, env->tv_mode_bits), mode);
	high = new_rd_Const(dbg, current_ir_graph, block, mode, tv_h);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;
}

/**
 * Translate a Load: create two.
 */
static void lower_Load(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	ir_node  *adr = get_Load_ptr(node);
	ir_node  *mem = get_Load_mem(node);
	ir_node  *low, *high, *proj;
	dbg_info *dbg;
	ir_node  *block = get_nodes_block(node);
	int      idx;

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
	}
	else {
		low  = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
		high = adr;
	}

	/* create two loads */
	dbg  = get_irn_dbg_info(node);
	low  = new_rd_Load(dbg, irg, block, mem, low,  mode);
	proj = new_r_Proj(irg, block, low, mode_M, pn_Load_M);
	high = new_rd_Load(dbg, irg, block, proj, high, mode);

	set_Load_volatility(low,  get_Load_volatility(node));
	set_Load_volatility(high, get_Load_volatility(node));

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		idx = get_irn_idx(proj);

		switch (get_Proj_proj(proj)) {
		case pn_Load_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Load_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		case pn_Load_res:       /* Result of load operation. */
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(irg, block, low, mode, pn_Load_res);
			env->entries[idx]->high_word = new_r_Proj(irg, block, high, mode, pn_Load_res);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Store: create two.
 */
static void lower_Store(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg;
	ir_node  *block, *adr, *mem;
	ir_node  *low, *high, *irn, *proj;
	dbg_info *dbg;
	int      idx;
	node_entry_t *entry;

	irn = get_Store_value(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	irg = current_ir_graph;
	adr = get_Store_ptr(node);
	mem = get_Store_mem(node);
	block = get_nodes_block(node);

	if (env->params->little_endian) {
		low  = adr;
		high = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
	}
	else {
		low  = new_r_Add(irg, block, adr,
			new_r_Const(irg, block, get_tarval_mode(env->tv_mode_bytes), env->tv_mode_bytes),
			get_irn_mode(adr));
		high = adr;
	}

	/* create two Stores */
	dbg = get_irn_dbg_info(node);
	low  = new_rd_Store(dbg, irg, block, mem, low,  entry->low_word);
	proj = new_r_Proj(irg, block, low, mode_M, pn_Store_M);
	high = new_rd_Store(dbg, irg, block, proj, high, entry->high_word);

	set_Store_volatility(low,  get_Store_volatility(node));
	set_Store_volatility(high, get_Store_volatility(node));

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = low;
	env->entries[idx]->high_word = high;

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		idx = get_irn_idx(proj);

		switch (get_Proj_proj(proj)) {
		case pn_Store_M:         /* Memory result. */
			/* put it to the second one */
			set_Proj_pred(proj, high);
			break;
		case pn_Store_X_except:  /* Execution result if exception occurred. */
			/* put it to the first one */
			set_Proj_pred(proj, low);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Return a node containing the address of the intrinsic emulation function.
 *
 * @param method  the method type of the emulation function
 * @param op      the emulated ir_op
 * @param imode   the input mode of the emulated opcode
 * @param omode   the output mode of the emulated opcode
 * @param block   where the new mode is created
 * @param env     the lower environment
 */
static ir_node *get_intrinsic_address(ir_type *method, ir_op *op,
                                      ir_mode *imode, ir_mode *omode,
                                      ir_node *block, lower_env_t *env) {
	symconst_symbol sym;
	entity *ent;
	op_mode_entry_t key, *entry;

	key.op    = op;
	key.imode = imode;
	key.omode = omode;
	key.ent   = NULL;

	entry = set_insert(intrinsic_fkt, &key, sizeof(key),
				HASH_PTR(op) ^ HASH_PTR(imode) ^ (HASH_PTR(omode) << 8));
	if (! entry->ent) {
		/* create a new one */
		ent = env->params->create_intrinsic(method, op, imode, omode, env->params->ctx);

		assert(ent && "Intrinsic creator must return an entity");
		entry->ent = ent;
	}
	else
		ent = entry->ent;

	sym.entity_p = ent;
	return new_r_SymConst(current_ir_graph, block, sym, symconst_addr_ent);
}

/**
 * Translate a Div.
 *
 * Create an intrinsic Call.
 */
static void lower_Div(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn, *call, *proj;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;

	irn   = get_Div_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_Div_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	opmode = get_irn_op_mode(node);
	irn = get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, block, env);
	call = new_rd_Call(dbg, current_ir_graph, block, get_Div_mem(node),
		irn, 4, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));
	irn = new_r_Proj(current_ir_graph, block, call, mode_T, pn_Call_T_result);

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Div_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_Div_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Div_res:       /* Result of computation. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, mode, 0);
			env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode, 1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Mod.
 *
 * Create an intrinsic Call.
 */
static void lower_Mod(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *proj, *irn, *call;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;

	irn   = get_Mod_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_Mod_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	opmode = get_irn_op_mode(node);
	irn = get_intrinsic_address(mtp, get_irn_op(node), opmode, opmode, block, env);
	call = new_rd_Call(dbg, current_ir_graph, block, get_Mod_mem(node),
		irn, 4, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));
	irn = new_r_Proj(current_ir_graph, block, call, mode_T, pn_Call_T_result);

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_Mod_M:         /* Memory result. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_Mod_X_except:  /* Execution result if exception occurred. */
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Mod_res:       /* Result of computation. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, mode, 0);
			env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode, 1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a DivMod.
 *
 * Create two intrinsic Calls.
 */
static void lower_DivMod(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *proj, *irn, *mem, *callDiv, *callMod, *resDiv, *resMod;
	ir_node  *in[4];
	ir_mode  *opmode;
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;
	unsigned flags = 0;

	/* check if both results are needed */
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_DivMod_res_div: flags |= 1; break;
		case pn_DivMod_res_mod: flags |= 2; break;
		default: break;
		}
	}

	irn   = get_DivMod_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_DivMod_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mem = get_DivMod_mem(node);

	callDiv = callMod = NULL;
	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	if (flags & 1) {
		opmode = get_irn_op_mode(node);
		irn = get_intrinsic_address(mtp, op_Div, opmode, opmode, block, env);
		callDiv = new_rd_Call(dbg, current_ir_graph, block, mem,
			irn, 4, in, mtp);
		set_irn_pinned(callDiv, get_irn_pinned(node));
		resDiv = new_r_Proj(current_ir_graph, block, callDiv, mode_T, pn_Call_T_result);
	}
	if (flags & 2) {
		if (flags & 1)
			mem = new_r_Proj(current_ir_graph, block, callDiv, mode_M, pn_Call_M);
		opmode = get_irn_op_mode(node);
		irn = get_intrinsic_address(mtp, op_Mod, opmode, opmode, block, env);
		callMod = new_rd_Call(dbg, current_ir_graph, block, mem,
			irn, 4, in, mtp);
		set_irn_pinned(callMod, get_irn_pinned(node));
		resMod = new_r_Proj(current_ir_graph, block, callMod, mode_T, pn_Call_T_result);
	}

	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		switch (get_Proj_proj(proj)) {
		case pn_DivMod_M:         /* Memory result. */
			/* reroute to the first call */
			set_Proj_pred(proj, callDiv ? callDiv : (callMod ? callMod : mem));
			set_Proj_proj(proj, pn_Call_M_except);
			break;
		case pn_DivMod_X_except:  /* Execution result if exception occurred. */
			/* reroute to the first call */
			set_Proj_pred(proj, callDiv ? callDiv : (callMod ? callMod : mem));
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_DivMod_res_div:   /* Result of Div. */
			idx = get_irn_idx(proj);
			assert(idx < env->n_entries);
			env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, resDiv, mode, 0);
			env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, resDiv, mode, 1);
			break;
		case pn_DivMod_res_mod:   /* Result of Mod. */
			idx = get_irn_idx(proj);
			env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, resMod, mode, 0);
			env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, resMod, mode, 1);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
		/* mark this proj: we have handled it already, otherwise we might fall into
		 * out new nodes. */
		mark_irn_visited(proj);
	}
}

/**
 * Translate a Binop.
 *
 * Create an intrinsic Call.
 */
static void lower_Binop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[4];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	irn   = get_binop_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[2] = entry->low_word;
	in[3] = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mtp = mode_is_signed(mode) ? binop_tp_s : binop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, current_ir_graph, block, get_irg_no_mem(current_ir_graph),
		irn, 4, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(current_ir_graph, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, mode, 0);
	env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode, 1);
}

/**
 * Translate a Shiftop.
 *
 * Create an intrinsic Call.
 */
static void lower_Shiftop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[3];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	/* The shift count is always mode_Iu in firm, so there is no need for lowering */
	in[2] = get_binop_right(node);

	dbg   = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mtp = mode_is_signed(mode) ? shiftop_tp_s : shiftop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, current_ir_graph, block, get_irg_no_mem(current_ir_graph),
		irn, 3, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(current_ir_graph, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, mode, 0);
	env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode, 1);
}

/**
 * Translate a Shr and handle special cases.
 */
static void lower_Shr(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *right = get_Shr_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) == get_mode_size_bits(env->params->low_signed)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shr_left(node);
			int idx = get_irn_idx(left);

			left = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			env->entries[idx]->low_word  = left;
			env->entries[idx]->high_word = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));

			return;
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Translate a Shl and handle special cases.
 */
static void lower_Shl(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *right = get_Shl_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) == get_mode_size_bits(env->params->low_signed)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shl_left(node);
			int idx = get_irn_idx(left);

			left = env->entries[idx]->low_word;
			idx = get_irn_idx(node);

			env->entries[idx]->low_word  = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
			env->entries[idx]->high_word = left;

			return;
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Translate a Shrs and handle special cases.
 */
static void lower_Shrs(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *right = get_Shrs_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) == get_mode_size_bits(env->params->low_signed)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Shrs_left(node);
			ir_node *c;
			int idx = get_irn_idx(left);

			left = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			c = new_r_Const_long(current_ir_graph, block, mode_Iu, get_mode_size_bits(mode) - 1);
			env->entries[idx]->low_word  = left;
			env->entries[idx]->high_word = new_r_Shrs(current_ir_graph, block, left, c, mode);

			return;
		}
	}
	lower_Shiftop(node, mode, env);
}

/**
 * Translate a Rot and handle special cases.
 */
static void lower_Rot(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *right = get_Rot_right(node);

	if (get_mode_arithmetic(mode) == irma_twos_complement && is_Const(right)) {
		tarval *tv = get_Const_tarval(right);

		if (tarval_is_long(tv) &&
			get_tarval_long(tv) == get_mode_size_bits(env->params->low_signed)) {
			ir_node *block = get_nodes_block(node);
			ir_node *left = get_Rot_left(node);
			ir_node *h, *l;
			int idx = get_irn_idx(left);

			l = env->entries[idx]->low_word;
			h = env->entries[idx]->high_word;
			idx = get_irn_idx(node);

			env->entries[idx]->low_word  = h;
			env->entries[idx]->high_word = l;

			return;
		}
	}
	lower_Shiftop(node, mode, env);
}
/**
 * Translate an Unop.
 *
 * Create an intrinsic Call.
 */
static void lower_Unop(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *in[2];
	dbg_info *dbg;
	ir_type  *mtp;
	int      idx;
	node_entry_t *entry;

	irn   = get_unop_op(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	in[0] = entry->low_word;
	in[1] = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	mtp = mode_is_signed(mode) ? unop_tp_s : unop_tp_u;
	irn = get_intrinsic_address(mtp, get_irn_op(node), mode, mode, block, env);
	irn = new_rd_Call(dbg, current_ir_graph, block, get_irg_no_mem(current_ir_graph),
		irn, 2, in, mtp);
	set_irn_pinned(irn, get_irn_pinned(node));
	irn = new_r_Proj(current_ir_graph, block, irn, mode_T, pn_Call_T_result);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_r_Proj(current_ir_graph, block, irn, mode, 0);
	env->entries[idx]->high_word = new_r_Proj(current_ir_graph, block, irn, mode, 1);
}

/**
 * Translate a logical Binop.
 *
 * Create two logical Binops.
 */
static void lower_Binop_logical(ir_node *node, ir_mode *mode, lower_env_t *env,
								ir_node *(*constr_rd)(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) ) {
	ir_node  *block, *irn;
	ir_node  *lop_l, *lop_h, *rop_l, *rop_h;
	dbg_info *dbg;
	int      idx;
	node_entry_t *entry;

	irn   = get_binop_left(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	lop_l = entry->low_word;
	lop_h = entry->high_word;

	irn   = get_binop_right(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	rop_l = entry->low_word;
	rop_h = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = constr_rd(dbg, current_ir_graph, block, lop_l, rop_l, mode);
	env->entries[idx]->high_word = constr_rd(dbg, current_ir_graph, block, lop_h, rop_h, mode);
}

/** create a logical operation tranformation */
#define lower_logical(op)                                                \
static void lower_##op(ir_node *node, ir_mode *mode, lower_env_t *env) { \
	lower_Binop_logical(node, mode, env, new_rd_##op);                   \
}

lower_logical(And)
lower_logical(Or)
lower_logical(Eor)

/**
 * Translate a Not.
 *
 * Create two logical Nots.
 */
static void lower_Not(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *irn;
	ir_node  *op_l, *op_h;
	dbg_info *dbg;
	int      idx;
	node_entry_t *entry;

	irn   = get_Not_op(node);
	entry = env->entries[get_irn_idx(irn)];
	assert(entry);

	if (! entry->low_word) {
		/* not ready yet, wait */
		pdeq_putr(env->waitq, node);
		return;
	}

	op_l = entry->low_word;
	op_h = entry->high_word;

	dbg = get_irn_dbg_info(node);
	block = get_nodes_block(node);

	idx = get_irn_idx(node);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_rd_Not(dbg, current_ir_graph, block, op_l, mode);
	env->entries[idx]->high_word = new_rd_Not(dbg, current_ir_graph, block, op_h, mode);
}

/**
 * Translate a Cond.
 */
static void lower_Cond(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_node *cmp, *left, *right, *block;
	ir_node *sel = get_Cond_selector(node);
	ir_mode *m = get_irn_mode(sel);
	int     idx;

	if (m == mode_b) {
		node_entry_t *lentry, *rentry;
		ir_node  *proj, *projT = NULL, *projF = NULL;
		ir_node  *new_bl, *cmpH, *cmpL, *irn;
		ir_node  *projHF, *projHT;
		ir_node  *dst_blk;
		ir_graph *irg;
		pn_Cmp   pnc;
		dbg_info *dbg;

		cmp   = get_Proj_pred(sel);
		left  = get_Cmp_left(cmp);
		idx   = get_irn_idx(left);
		lentry = env->entries[idx];

		if (! lentry)
			/* a normal Cmp */
			return;

		right = get_Cmp_right(cmp);
		idx   = get_irn_idx(right);
		rentry = env->entries[idx];
		assert(rentry);

		if (! lentry->low_word || !rentry->low_word) {
			/* not yet ready */
			pdeq_putr(env->waitq, node);
			return;
		}

		/* all right, build the code */
		for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
			long proj_nr = get_Proj_proj(proj);

			if (proj_nr == pn_Cond_true) {
				assert(projT == NULL && "more than one Proj(true)");
				projT = proj;
			}
			else {
				assert(proj_nr == pn_Cond_false);
				assert(projF == NULL && "more than one Proj(false)");
				projF = proj;
			}
			mark_irn_visited(proj);
		}
		assert(projT && projF);

		/* create a new high compare */
		block = get_nodes_block(cmp);
		dbg   = get_irn_dbg_info(cmp);
		irg   = current_ir_graph;

		cmpH = new_rd_Cmp(dbg, irg, block, lentry->high_word, rentry->high_word);

		pnc = get_Proj_proj(sel);
		if (pnc == pn_Cmp_Eq) {
			/* simple case:a == b <==> a_h == b_h && a_l == b_l */
			pmap_entry *entry = pmap_find(env->proj_2_block, projF);

			assert(entry);
			dst_blk = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pn_Cmp_Eq);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);
			exchange(projF, projHF);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);

			new_bl = new_r_Block(irg, 1, &projHT);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, new_bl, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, new_bl, cmpL, mode_b, pn_Cmp_Eq);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, new_bl, irn);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			add_block_cf_input(dst_blk, projHF, proj);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			exchange(projT, proj);
		}
		else if (pnc == pn_Cmp_Lg) {
			/* simple case:a != b <==> a_h != b_h || a_l != b_l */
			pmap_entry *entry = pmap_find(env->proj_2_block, projT);

			assert(entry);
			dst_blk = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pn_Cmp_Lg);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);
			exchange(projT, projHT);

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);

			new_bl = new_r_Block(irg, 1, &projHF);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, new_bl, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, new_bl, cmpL, mode_b, pn_Cmp_Lg);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, new_bl, irn);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			add_block_cf_input(dst_blk, projHT, proj);

			proj = new_r_Proj(irg, new_bl, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			exchange(projF, proj);
		}
		else {
			/* a rel b <==> a_h rel b_h || (a_h == b_h && a_l rel b_l) */
			ir_node *dstT, *dstF, *newbl_eq, *newbl_l;
			pmap_entry *entry;

			entry = pmap_find(env->proj_2_block, projT);
			assert(entry);
			dstT = entry->value;

			entry = pmap_find(env->proj_2_block, projF);
			assert(entry);
			dstF = entry->value;

			irn = new_r_Proj(irg, block, cmpH, mode_b, pnc);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, block, irn);

			projHT = new_r_Proj(irg, block, irn, mode_X, pn_Cond_true);
			mark_irn_visited(projHT);
			exchange(projT, projHT);
			projT = projHT;

			projHF = new_r_Proj(irg, block, irn, mode_X, pn_Cond_false);
			mark_irn_visited(projHF);

			newbl_eq = new_r_Block(irg, 1, &projHF);

			irn = new_r_Proj(irg, newbl_eq, cmpH, mode_b, pn_Cmp_Eq);
			irn = new_rd_Cond(dbg, irg, newbl_eq, irn);

			proj = new_r_Proj(irg, newbl_eq, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			exchange(projF, proj);
			projF = proj;

			proj = new_r_Proj(irg, newbl_eq, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);

			newbl_l = new_r_Block(irg, 1, &proj);

			dbg   = get_irn_dbg_info(cmp);
			cmpL = new_rd_Cmp(dbg, irg, newbl_l, lentry->low_word, rentry->low_word);
			irn = new_r_Proj(irg, newbl_l, cmpL, mode_b, pnc);
			dbg = get_irn_dbg_info(node);
			irn = new_rd_Cond(dbg, irg, newbl_l, irn);

			proj = new_r_Proj(irg, newbl_l, irn, mode_X, pn_Cond_true);
			mark_irn_visited(proj);
			add_block_cf_input(dstT, projT, proj);

			proj = new_r_Proj(irg, newbl_l, irn, mode_X, pn_Cond_false);
			mark_irn_visited(proj);
			add_block_cf_input(dstF, projF, proj);
		}

		/* we have changed the control flow */
		env->flags |= CF_CHANGED;
	}
	else {
		idx = get_irn_idx(sel);

		if (env->entries[idx]) {
			/*
			   Bad, a jump-table with double-word index.
			   This should not happen, but if it does we handle
			   it like a Conv were between (in other words, ignore
			   the high part.
			 */

			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}
			set_Cond_selector(node, env->entries[idx]->low_word);
		}
	}
}

/**
 * Translate a Conv to higher_signed
 */
static void lower_Conv_to_Ls(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *imode = get_irn_mode(op);
	ir_mode  *dst_mode = env->params->low_signed;
	int      idx = get_irn_idx(node);
	ir_graph *irg = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	assert(idx < env->n_entries);

	if (mode_is_int(imode) || mode_is_reference(imode)) {
		/* simple case: create a high word */
		if (imode != dst_mode)
			op = new_rd_Conv(dbg, irg, block, op, dst_mode);

		env->entries[idx]->low_word  = op;
		env->entries[idx]->high_word = new_rd_Shrs(dbg, irg, block, op,
			new_Const_long(mode_Iu, get_mode_size_bits(dst_mode) - 1), dst_mode);
	}
	else {
		ir_node *irn, *call;
		ir_mode *omode = env->params->high_signed;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, dst_mode, 0);
		env->entries[idx]->high_word = new_r_Proj(irg, block, irn, dst_mode, 1);
	}
}

/**
 * Translate a Conv to higher_unsigned
 */
static void lower_Conv_to_Lu(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *imode  = get_irn_mode(op);
	ir_mode  *dst_mode = env->params->low_unsigned;
	int      idx = get_irn_idx(node);
	ir_graph *irg = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	assert(idx < env->n_entries);

	if (mode_is_int(imode) || mode_is_reference(imode)) {
		/* simple case: create a high word */
		if (imode != dst_mode)
			op = new_rd_Conv(dbg, irg, block, op, dst_mode);

		env->entries[idx]->low_word  = op;
		env->entries[idx]->high_word = new_Const(dst_mode, get_mode_null(dst_mode));
	}
	else {
		ir_node *irn, *call;
		ir_mode *omode = env->params->high_unsigned;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 1, &op, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		env->entries[idx]->low_word  = new_r_Proj(irg, block, irn, dst_mode, 0);
		env->entries[idx]->high_word = new_r_Proj(irg, block, irn, dst_mode, 1);
	}
}

/**
 * Translate a Conv from higher_signed
 */
static void lower_Conv_from_Ls(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *omode = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	int      idx = get_irn_idx(op);
	ir_graph *irg = current_ir_graph;

	assert(idx < env->n_entries);

	if (mode_is_int(omode) || mode_is_reference(omode)) {
		op = env->entries[idx]->low_word;

		/* simple case: create a high word */
		if (omode != env->params->low_signed)
			op = new_rd_Conv(dbg, irg, block, op, omode);

		set_Conv_op(node, op);
	}
	else {
		ir_node *irn, *call, *in[2];
		ir_mode *imode = env->params->high_signed;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		in[0] = env->entries[idx]->low_word;
		in[1] = env->entries[idx]->high_word;

		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		exchange(node, new_r_Proj(irg, block, irn, omode, 0));
	}
}

/**
 * Translate a Conv from higher_unsigned
 */
static void lower_Conv_from_Lu(ir_node *node, lower_env_t *env) {
	ir_node  *op    = get_Conv_op(node);
	ir_mode  *omode = get_irn_mode(node);
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	int      idx = get_irn_idx(op);
	ir_graph *irg = current_ir_graph;

	assert(idx < env->n_entries);

	if (mode_is_int(omode) || mode_is_reference(omode)) {
		op = env->entries[idx]->low_word;

		/* simple case: create a high word */
		if (omode != env->params->low_unsigned)
			op = new_rd_Conv(dbg, irg, block, op, omode);

		set_Conv_op(node, op);
	}
	else {
		ir_node *irn, *call, *in[2];
		ir_mode *imode = env->params->high_unsigned;
		ir_type *mtp = get_conv_type(imode, omode, env);

		irn = get_intrinsic_address(mtp, get_irn_op(node), imode, omode, block, env);
		in[0] = env->entries[idx]->low_word;
		in[1] = env->entries[idx]->high_word;

		call = new_rd_Call(dbg, irg, block, get_irg_no_mem(irg), irn, 2, in, mtp);
		set_irn_pinned(call, get_irn_pinned(node));
		irn = new_r_Proj(irg, block, call, mode_T, pn_Call_T_result);

		exchange(node, new_r_Proj(irg, block, irn, omode, 0));
	}
}

/**
 * Translate a Conv.
 */
static void lower_Conv(ir_node *node, ir_mode *mode, lower_env_t *env) {
	mode = get_irn_mode(node);

	if (mode == env->params->high_signed)
		lower_Conv_to_Ls(node, env);
	else if (mode == env->params->high_unsigned)
		lower_Conv_to_Lu(node, env);
	else {
		ir_mode *mode = get_irn_mode(get_Conv_op(node));

		if (mode == env->params->high_signed)
			lower_Conv_from_Ls(node, env);
		else {
			assert(mode == env->params->high_unsigned);
			lower_Conv_from_Lu(node, env);
		}
	}
}

/**
 * Lower the method type.
 */
static ir_type *lower_mtp(ir_type *mtp, lower_env_t *env) {
	pmap_entry *entry;
	ident      *id;
	ir_type    *res;

	if (is_lowered_type(mtp))
		return mtp;

	entry = pmap_find(lowered_type, mtp);
	if (! entry) {
		int i, n, r, n_param, n_res;

		/* count new number of params */
		n_param = n = get_method_n_params(mtp);
		for (i = n_param - 1; i >= 0; --i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned)
					++n_param;
			}
		}

		/* count new number of results */
		n_res = r = get_method_n_ress(mtp);
		for (i = n_res - 1; i >= 0; --i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned)
					++n_res;
			}
		}

		id = mangle_u(new_id_from_chars("L", 1), get_type_ident(mtp));
		res = new_type_method(id, n_param, n_res);

		/* set param types and result types */
		for (i = n_param = 0; i < n; ++i) {
			ir_type *tp = get_method_param_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed) {
					set_method_param_type(res, n_param++, tp_s);
					set_method_param_type(res, n_param++, tp_s);
				}
				else if (mode == env->params->high_unsigned) {
					set_method_param_type(res, n_param++, tp_u);
					set_method_param_type(res, n_param++, tp_u);
				}
				else
					set_method_param_type(res, n_param++, tp);
			}
			else
				set_method_param_type(res, n_param++, tp);
		}
		for (i = n_res = 0; i < r; ++i) {
			ir_type *tp = get_method_res_type(mtp, i);

			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed) {
					set_method_res_type(res, n_res++, tp_s);
					set_method_res_type(res, n_res++, tp_s);
				}
				else if (mode == env->params->high_unsigned) {
					set_method_res_type(res, n_res++, tp_u);
					set_method_res_type(res, n_res++, tp_u);
				}
				else
					set_method_res_type(res, n_res++, tp);
			}
			else
				set_method_res_type(res, n_res++, tp);
		}
		set_lowered_type(mtp, res);
		pmap_insert(lowered_type, mtp, res);
	}
	else
		res = entry->value;
	return res;
}

/**
 * Translate a Return.
 */
static void lower_Return(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	entity   *ent = get_irg_entity(irg);
	ir_type  *mtp = get_entity_type(ent);
	ir_node  **in;
	int      i, j, n, idx;
	int      need_conv = 0;

	/* check if this return must be lowered */
	for (i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred = get_Return_res(node, i);
		ir_mode *mode = get_irn_op_mode(pred);

		if (mode == env->params->high_signed ||
			mode == env->params->high_unsigned) {
			idx = get_irn_idx(pred);
			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}
			need_conv = 1;
		}
	}
	if (! need_conv)
		return;

	ent = get_irg_entity(irg);
	mtp = get_entity_type(ent);

	mtp = lower_mtp(mtp, env);
	set_entity_type(ent, mtp);

	/* create a new in array */
	NEW_ARR_A(ir_node *, in, get_method_n_ress(mtp) + 1);
	in[0] = get_Return_mem(node);

	for (j = i = 0, n = get_Return_n_ress(node); i < n; ++i) {
		ir_node *pred = get_Return_res(node, i);

		idx = get_irn_idx(pred);
		assert(idx < env->n_entries);

		if (env->entries[idx]) {
			in[++j] = env->entries[idx]->low_word;
			in[++j] = env->entries[idx]->high_word;
		}
		else
			in[++j] = pred;
	}

	set_irn_in(node, j+1, in);
}

/**
 * Translate the parameters.
 */
static void lower_Start(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	entity   *ent = get_irg_entity(irg);
	ir_type  *tp  = get_entity_type(ent);
	ir_type  *mtp;
	long     *new_projs;
	int      i, j, n_params, rem;
	ir_node  *proj, *args;

	if (is_lowered_type(tp))
		mtp = get_associated_type(tp);
	else
		mtp = tp;

	assert(! is_lowered_type(mtp));

	n_params = get_method_n_params(mtp);
	if (n_params <= 0)
		return;

	NEW_ARR_A(long, new_projs, n_params);

	/* first check if we have parameters that must be fixed */
	for (i = j = 0; i < n_params; ++i, ++j) {
		ir_type *tp = get_method_param_type(mtp, i);

		new_projs[i] = j;
		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->params->high_signed ||
				mode == env->params->high_unsigned)
				++j;
		}
	}
	if (i == j)
		return;

	mtp = lower_mtp(mtp, env);
	set_entity_type(ent, mtp);

	/* switch off optimization for new Proj nodes or they might be CSE'ed
	   with not patched one's */
	rem = get_optimize();
	set_optimize(0);

	/* ok, fix all Proj's and create new ones */
	args = get_irg_args(irg);
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		ir_node *pred = get_Proj_pred(proj);
		long proj_nr;
		int idx;
		ir_mode *mode;
		dbg_info *dbg;

		/* do not visit this node again */
		mark_irn_visited(proj);

		if (pred != args)
			continue;

		proj_nr = get_Proj_proj(proj);
		set_Proj_proj(proj, new_projs[proj_nr]);

		idx = get_irn_idx(proj);
		if (env->entries[idx]) {
			mode = get_irn_mode(proj);

			if (mode == env->params->high_signed)
				mode = env->params->low_signed;
			else
				mode = env->params->low_unsigned;

			dbg = get_irn_dbg_info(proj);
			env->entries[idx]->low_word  =
				new_rd_Proj(dbg, irg, get_nodes_block(proj), args, mode, new_projs[proj_nr]);
			env->entries[idx]->high_word =
				new_rd_Proj(dbg, irg, get_nodes_block(proj), args, mode, new_projs[proj_nr] + 1);
		}
	}
	set_optimize(rem);
}

/**
 * Translate a Call.
 */
static void lower_Call(ir_node *node, ir_mode *mode, lower_env_t *env) {
	ir_graph *irg = current_ir_graph;
	ir_type  *tp = get_Call_type(node);
	ir_type  *call_tp;
	ir_node  **in, *proj, *results;
	int      n_params, n_res, need_lower = 0;
	int      i, j;
	long     *res_numbers = NULL;

	if (is_lowered_type(tp))
		call_tp = get_associated_type(tp);
	else
		call_tp = tp;

	assert(! is_lowered_type(call_tp));

	n_params = get_method_n_params(call_tp);
	for (i = 0; i < n_params; ++i) {
		ir_type *tp = get_method_param_type(call_tp, i);

		if (is_Primitive_type(tp)) {
			ir_mode *mode = get_type_mode(tp);

			if (mode == env->params->high_signed ||
				mode == env->params->high_unsigned) {
				need_lower = 1;
				break;
			}
		}
	}
	n_res = get_method_n_ress(call_tp);
	if (n_res > 0) {
		NEW_ARR_A(long, res_numbers, n_res);

		for (i = j = 0; i < n_res; ++i, ++j) {
			ir_type *tp = get_method_res_type(call_tp, i);

			res_numbers[i] = j;
			if (is_Primitive_type(tp)) {
				ir_mode *mode = get_type_mode(tp);

				if (mode == env->params->high_signed ||
					mode == env->params->high_unsigned) {
					need_lower = 1;
					++j;
				}
			}
		}
	}

	if (! need_lower)
		return;

	/* let's lower it */
	call_tp = lower_mtp(call_tp, env);
	set_Call_type(node, call_tp);

	NEW_ARR_A(ir_node *, in, get_method_n_params(call_tp) + 2);

	in[0] = get_Call_mem(node);
	in[1] = get_Call_ptr(node);

	for (j = 2, i = 0; i < n_params; ++i) {
		ir_node *pred = get_Call_param(node, i);
		int     idx = get_irn_idx(pred);

		if (env->entries[idx]) {
			if (! env->entries[idx]->low_word) {
				/* not ready yet, wait */
				pdeq_putr(env->waitq, node);
				return;
			}
			in[j++] = env->entries[idx]->low_word;
			in[j++] = env->entries[idx]->high_word;
		}
		else
			in[j++] = pred;
	}

	set_irn_in(node, j, in);

	/* fix the results */
	results = NULL;
	for (proj = get_irn_link(node); proj; proj = get_irn_link(proj)) {
		long proj_nr = get_Proj_proj(proj);

		if (proj_nr == pn_Call_T_result && get_Proj_pred(proj) == node) {
			/* found the result proj */
			results = proj;
			break;
		}
	}

	if (results) {		/* there are results */
		int rem = get_optimize();

		/* switch off optimization for new Proj nodes or they might be CSE'ed
		   with not patched one's */
		set_optimize(0);
		for (i = j = 0, proj = get_irn_link(results); proj; proj = get_irn_link(proj), ++i, ++j) {
			if (get_Proj_pred(proj) == results) {
				long proj_nr = get_Proj_proj(proj);
				int idx;

				/* found a result */
				set_Proj_proj(proj, res_numbers[proj_nr]);
				idx = get_irn_idx(proj);
				if (env->entries[idx]) {
					ir_mode *mode = get_irn_mode(proj);
					dbg_info *dbg;

					if (mode == env->params->high_signed)
						mode = env->params->low_signed;
					else
						mode = env->params->low_unsigned;

					dbg = get_irn_dbg_info(proj);
					env->entries[idx]->low_word  =
						new_rd_Proj(dbg, irg, get_nodes_block(proj), results, mode, res_numbers[proj_nr]);
					env->entries[idx]->high_word =
						new_rd_Proj(dbg, irg, get_nodes_block(proj), results, mode, res_numbers[proj_nr] + 1);
				}
				mark_irn_visited(proj);
			}
		}
		set_optimize(rem);
	}
}

/**
 * Translate an Unknown into two.
 */
static void lower_Unknown(ir_node *node, ir_mode *mode, lower_env_t *env) {
	int idx = get_irn_idx(node);

	env->entries[idx]->low_word  =
	env->entries[idx]->high_word = new_r_Unknown(current_ir_graph, mode);
}

/**
 * Translate a Phi.
 *
 * First step: just create two templates
 */
static void lower_Phi(ir_node *phi, ir_mode *mode, lower_env_t *env) {
	ir_node  *block, *unk;
	ir_node  **inl, **inh;
	dbg_info *dbg;
	int      idx, i, arity = get_Phi_n_preds(phi);
	int      enq = 0;

	idx = get_irn_idx(phi);
	if (env->entries[idx]->low_word) {
		/* Phi nodes already build, check for inputs */
		ir_node *phil = env->entries[idx]->low_word;
		ir_node *phih = env->entries[idx]->high_word;

		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_Phi_pred(phi, i);
			int     idx = get_irn_idx(pred);

			if (env->entries[idx]->low_word) {
				set_Phi_pred(phil, i, env->entries[idx]->low_word);
				set_Phi_pred(phil, i, env->entries[idx]->high_word);
			}
			else {
				/* still not ready */
				pdeq_putr(env->waitq, phi);
				return;
			}
		}
	}

	/* first create a new in array */
	NEW_ARR_A(ir_node *, inl, arity);
	NEW_ARR_A(ir_node *, inh, arity);
	unk = new_r_Unknown(current_ir_graph, mode);

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);
		int     idx = get_irn_idx(pred);

		if (env->entries[idx]->low_word) {
			inl[i] = env->entries[idx]->low_word;
			inh[i] = env->entries[idx]->high_word;
		}
		else {
			inl[i] = unk;
			inh[i] = unk;
			enq = 1;
		}
	}

	dbg = get_irn_dbg_info(phi);
	block = get_nodes_block(phi);

	idx = get_irn_idx(phi);
	assert(idx < env->n_entries);
	env->entries[idx]->low_word  = new_rd_Phi(dbg, current_ir_graph, block, arity, inl, mode);
	env->entries[idx]->high_word = new_rd_Phi(dbg, current_ir_graph, block, arity, inh, mode);

	if (enq) {
		/* not yet finished */
		pdeq_putr(env->waitq, phi);
	}
}

/**
 * check for opcodes that must always be lowered.
 */
static int always_lower(opcode code) {
	switch (code) {
	case iro_Start:
	case iro_Call:
	case iro_Return:
	case iro_Cond:
		return 1;
	default:
		return 0;
	}
}

/** The type of a lower function. */
typedef void (*lower_func)(ir_node *node, ir_mode *mode, lower_env_t *env);

/**
 * lower a node.
 */
static void lower_ops(ir_node *node, void *env)
{
	lower_env_t  *lenv = env;
	node_entry_t *entry;
	int          idx = get_irn_idx(node);

	entry = lenv->entries[idx];
	if (entry || always_lower(get_irn_opcode(node))) {
		ir_op      *op = get_irn_op(node);
		lower_func func = (lower_func)op->ops.generic;

		if (func) {
			ir_mode *mode = get_irn_op_mode(node);

			if (mode == lenv->params->high_signed)
				mode = lenv->params->low_signed;
			else
				mode = lenv->params->low_unsigned;

			DB((dbg, LEVEL_1, "  %+F\n", node));
			func(node, mode, lenv);
		}
	}
}

#define IDENT(s)  new_id_from_chars(s, sizeof(s)-1)

/**
 * Compare two op_mode_entry_t's.
 */
static int cmp_op_mode(const void *elt, const void *key, size_t size) {
	const op_mode_entry_t *e1 = elt;
	const op_mode_entry_t *e2 = key;

	return (e1->op - e2->op) | (e1->imode - e2->imode) | (e1->omode - e2->omode);
}

/**
 * Compare two conv_tp_entry_t's.
 */
static int cmp_conv_tp(const void *elt, const void *key, size_t size) {
	const conv_tp_entry_t *e1 = elt;
	const conv_tp_entry_t *e2 = key;

	return (e1->imode - e2->imode) | (e1->omode - e2->omode);
}

/*
 * Do the lowering.
 */
void lower_dw_ops(const lwrdw_param_t *param)
{
	lower_env_t lenv;
	int i;

	if (! param)
		return;

	if (! param->enable)
		return;

	FIRM_DBG_REGISTER(dbg, "firm.lower.dw");

	assert(2 * get_mode_size_bits(param->low_signed)   == get_mode_size_bits(param->high_signed));
	assert(2 * get_mode_size_bits(param->low_unsigned) == get_mode_size_bits(param->high_unsigned));
	assert(get_mode_size_bits(param->low_signed) == get_mode_size_bits(param->low_unsigned));

	if (! prim_types)
		prim_types = pmap_create();
	if (! intrinsic_fkt)
		intrinsic_fkt = new_set(cmp_op_mode, iro_MaxOpcode);
	if (! conv_types)
		conv_types = new_set(cmp_conv_tp, 16);
	if (! lowered_type)
		lowered_type = pmap_create();

	if (! tp_u)
		tp_u = get_primitive_type(param->low_unsigned);
	if (! tp_s)
		tp_s = get_primitive_type(param->low_signed);


	if (! binop_tp_u) {
		binop_tp_u = new_type_method(IDENT("binop_u_intrinsic"), 4, 2);
		set_method_param_type(binop_tp_u, 0, tp_u);
		set_method_param_type(binop_tp_u, 1, tp_u);
		set_method_param_type(binop_tp_u, 2, tp_u);
		set_method_param_type(binop_tp_u, 3, tp_u);
		set_method_res_type(binop_tp_u, 0, tp_u);
		set_method_res_type(binop_tp_u, 1, tp_u);
	}
	if (! binop_tp_s) {
		binop_tp_s = new_type_method(IDENT("binop_s_intrinsic"), 4, 2);
		set_method_param_type(binop_tp_s, 0, tp_s);
		set_method_param_type(binop_tp_s, 1, tp_s);
		set_method_param_type(binop_tp_s, 2, tp_s);
		set_method_param_type(binop_tp_s, 3, tp_s);
		set_method_res_type(binop_tp_s, 0, tp_s);
		set_method_res_type(binop_tp_s, 1, tp_s);
	}
	if (! shiftop_tp_u) {
		shiftop_tp_u = new_type_method(IDENT("shiftop_u_intrinsic"), 3, 2);
		set_method_param_type(shiftop_tp_u, 0, tp_u);
		set_method_param_type(shiftop_tp_u, 1, tp_u);
		set_method_param_type(shiftop_tp_u, 2, tp_u);
		set_method_res_type(shiftop_tp_u, 0, tp_u);
		set_method_res_type(shiftop_tp_u, 1, tp_u);
	}
	if (! shiftop_tp_s) {
		shiftop_tp_s = new_type_method(IDENT("shiftop_s_intrinsic"), 3, 2);
		set_method_param_type(shiftop_tp_s, 0, tp_s);
		set_method_param_type(shiftop_tp_s, 1, tp_s);
		/* beware: shift count is always mode_Iu */
		set_method_param_type(shiftop_tp_s, 2, tp_u);
		set_method_res_type(shiftop_tp_s, 0, tp_s);
		set_method_res_type(shiftop_tp_s, 1, tp_s);
	}
	if (! unop_tp_u) {
		unop_tp_u = new_type_method(IDENT("unop_u_intrinsic"), 2, 2);
		set_method_param_type(unop_tp_u, 0, tp_u);
		set_method_param_type(unop_tp_u, 1, tp_u);
		set_method_res_type(unop_tp_u, 0, tp_u);
		set_method_res_type(unop_tp_u, 1, tp_u);
	}
	if (! unop_tp_s) {
		unop_tp_s = new_type_method(IDENT("unop_s_intrinsic"), 2, 2);
		set_method_param_type(unop_tp_s, 0, tp_s);
		set_method_param_type(unop_tp_s, 1, tp_s);
		set_method_res_type(unop_tp_s, 0, tp_s);
		set_method_res_type(unop_tp_s, 1, tp_s);
	}

	lenv.tv_mode_bytes = new_tarval_from_long(get_mode_size_bytes(param->low_unsigned), mode_Iu);
	lenv.tv_mode_bits  = new_tarval_from_long(get_mode_size_bits(param->low_unsigned), mode_Iu);
	lenv.waitq         = new_pdeq();
	lenv.params        = param;

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

#define LOWER2(op, fkt)   op_##op->ops.generic = (op_func)fkt
#define LOWER(op)         LOWER2(op, lower_##op)
#define LOWER_BIN(op)     LOWER2(op, lower_Binop)
#define LOWER_UN(op)      LOWER2(op, lower_Unop)

	LOWER(Load);
	LOWER(Store);
	LOWER(Const);
	LOWER(And);
	LOWER(Or);
	LOWER(Eor);
	LOWER(Not);
	LOWER(Cond);
	LOWER(Return);
	LOWER(Call);
	LOWER(Unknown);
	LOWER(Phi);
	LOWER(Start);

	LOWER_BIN(Add);
	LOWER_BIN(Sub);
	LOWER_BIN(Mul);
	LOWER(Shl);
	LOWER(Shr);
	LOWER(Shrs);
	LOWER(Rot);
	LOWER_UN(Minus);
	LOWER(DivMod);
	LOWER(Div);
	LOWER(Mod);
	LOWER_UN(Abs);

	LOWER(Conv);

#undef LOWER_UN
#undef LOWER_BIN
#undef LOWER
#undef LOWER2

	/* transform all graphs */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		int n_idx;

		obstack_init(&lenv.obst);

		n_idx = get_irg_last_idx(irg);
		lenv.n_entries = n_idx;
		lenv.entries   = xmalloc(n_idx * sizeof(lenv.entries[0]));
		memset(lenv.entries, 0, n_idx * sizeof(lenv.entries[0]));

		/* first step: link all nodes and allocate data */
		lenv.flags = 0;
		lenv.proj_2_block = pmap_create();
		irg_walk_graph(irg, firm_clear_link, prepare_links, &lenv);

		if (lenv.flags & MUST_BE_LOWERED) {
			DB((dbg, LEVEL_1, "Lowering graph %+F\n", irg));

			/* must do some work */
			irg_walk_graph(irg, NULL, lower_ops, &lenv);

			/* last step: all waiting nodes */
			DB((dbg, LEVEL_1, "finishing waiting nodes:\n"));
			while (! pdeq_empty(lenv.waitq)) {
				ir_node *node = pdeq_getl(lenv.waitq);

				lower_ops(node, &lenv);
			}

			/* outs are invalid, we changed the graph */
			set_irg_outs_inconsistent(irg);

			if (lenv.flags & CF_CHANGED) {
				/* control flow changed, dominance info is invalid */
				set_irg_doms_inconsistent(irg);
				set_irg_extblk_inconsistent(irg);
				set_irg_loopinfo_inconsistent(irg);
			}

			dump_ir_block_graph(irg, "-dw");
		}
		pmap_destroy(lenv.proj_2_block);
		free(lenv.entries);
		obstack_free(&lenv.obst, NULL);
	}
	del_pdeq(lenv.waitq);
}

/* Default implementation. */
entity *def_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                 const ir_mode *imode, const ir_mode *omode,
                                 void *context)
{
	char buf[64];
	ident *id;
	entity *ent;

	if (imode == omode)
		snprintf(buf, sizeof(buf), "__l%s%s", get_op_name(op), get_mode_name(imode));
	else
		snprintf(buf, sizeof(buf), "__l%s%s%s", get_op_name(op),
			get_mode_name(imode), get_mode_name(omode));
	id = new_id_from_str(buf);

	ent = new_entity(get_glob_type(), id, method);
	set_entity_ld_ident(ent, get_entity_ident(ent));
	set_entity_visibility(ent, visibility_external_allocated);
	return ent;
}
