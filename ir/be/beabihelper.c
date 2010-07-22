/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Helper functions for handling ABI constraints in the code
 *              selection phase.
 * @author      Matthias Braun
 * @version     $Id$
 */
#include "config.h"

#include "beabihelper.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "ircons.h"
#include "iredges.h"
#include "irgwalk.h"
#include "irphase_t.h"
#include "height.h"

typedef struct reg_flag_t {
	const arch_register_t *reg;   /**< register at an input position.
	                                   may be NULL in case of memory input */
	arch_irn_flags_t       flags;
} reg_flag_t;

/**
 * A register state mapping keeps track of the symbol values (=firm nodes)
 * to registers. This is usefull when constructing straight line code
 * which like the function prolog or epilog in some architectures.
 */
typedef struct register_state_mapping_t {
	ir_node   **value_map;     /**< mapping of state indices to values */
	int       **reg_index_map; /**< mapping of regclass,regnum to an index
	                                into the value_map */
	reg_flag_t *regs;          /**< registers (and memory values) that form a
	                                state */
	ir_node    *last_barrier;
} register_state_mapping_t;

struct beabi_helper_env_t {
	ir_graph                 *irg;
	register_state_mapping_t  prolog;
	register_state_mapping_t  epilog;
	ir_phase                 *stack_order;
};

static void prepare_rsm(register_state_mapping_t *rsm,
                        const arch_env_t *arch_env)
{
	unsigned   n_reg_classes = arch_env_get_n_reg_class(arch_env);
	unsigned   c;
	reg_flag_t memory = { NULL, 0 };

	rsm->regs = NEW_ARR_F(reg_flag_t, 0);
	/* memory input at 0 */
	ARR_APP1(reg_flag_t, rsm->regs, memory);

	rsm->value_map     = NULL;
	rsm->reg_index_map = XMALLOCN(int*, n_reg_classes);
	for (c = 0; c < n_reg_classes; ++c) {
		const arch_register_class_t *cls = arch_env_get_reg_class(arch_env, c);
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		unsigned                     r;

		rsm->reg_index_map[c] = XMALLOCN(int, n_regs);
		for (r = 0; r < n_regs; ++r) {
			rsm->reg_index_map[c][r] = -1;
		}
	}
}

static void free_rsm(register_state_mapping_t *rsm, const arch_env_t *arch_env)
{
	unsigned n_reg_classes = arch_env_get_n_reg_class(arch_env);
	unsigned c;

	for (c = 0; c < n_reg_classes; ++c) {
		free(rsm->reg_index_map[c]);
	}

	free(rsm->reg_index_map);
	if (rsm->value_map != NULL)
		DEL_ARR_F(rsm->value_map);
	DEL_ARR_F(rsm->regs);

	rsm->regs          = NULL;
	rsm->reg_index_map = NULL;
	rsm->value_map     = NULL;
}

static void rsm_clear_regs(register_state_mapping_t *rsm,
                           const arch_env_t *arch_env)
{
	unsigned   n_reg_classes = arch_env_get_n_reg_class(arch_env);
	unsigned   c;
	reg_flag_t memory = { NULL, 0 };

	for (c = 0; c < n_reg_classes; ++c) {
		const arch_register_class_t *cls = arch_env_get_reg_class(arch_env, c);
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		unsigned                     r;

		for (r = 0; r < n_regs; ++r) {
			rsm->reg_index_map[c][r] = -1;
		}
	}
	ARR_RESIZE(reg_flag_t, rsm->regs, 0);
	ARR_APP1(reg_flag_t, rsm->regs, memory);

	if (rsm->value_map != NULL) {
		DEL_ARR_F(rsm->value_map);
		rsm->value_map = NULL;
	}
}

static int rsm_add_reg(register_state_mapping_t *rsm,
                       const arch_register_t *reg, arch_irn_flags_t flags)
{
	int        input_idx = ARR_LEN(rsm->regs);
	int        cls_idx   = reg->reg_class->index;
	int        reg_idx   = reg->index;
	reg_flag_t regflag   = { reg, flags };

	/* we must not have used get_value yet */
	assert(rsm->reg_index_map[cls_idx][reg_idx] == -1);
	rsm->reg_index_map[cls_idx][reg_idx] = input_idx;
	ARR_APP1(reg_flag_t, rsm->regs, regflag);

	if (rsm->value_map != NULL) {
		ARR_APP1(ir_node*, rsm->value_map, NULL);
		assert(ARR_LEN(rsm->value_map) == ARR_LEN(rsm->regs));
	}
	return input_idx;
}


static ir_node *rsm_get_value(register_state_mapping_t *rsm, int index)
{
	assert(0 <= index && index < ARR_LEN(rsm->value_map));
	return rsm->value_map[index];
}

static ir_node *rsm_get_reg_value(register_state_mapping_t *rsm,
                                  const arch_register_t *reg)
{
	int cls_idx   = reg->reg_class->index;
	int reg_idx   = reg->index;
	int input_idx = rsm->reg_index_map[cls_idx][reg_idx];

	return rsm_get_value(rsm, input_idx);
}

static void rsm_set_value(register_state_mapping_t *rsm, int index,
                          ir_node *value)
{
	assert(0 <= index && index < ARR_LEN(rsm->value_map));
	rsm->value_map[index] = value;
}

static void rsm_set_reg_value(register_state_mapping_t *rsm,
                              const arch_register_t *reg, ir_node *value)
{
	int cls_idx   = reg->reg_class->index;
	int reg_idx   = reg->index;
	int input_idx = rsm->reg_index_map[cls_idx][reg_idx];
	rsm_set_value(rsm, input_idx, value);
}

static ir_node *rsm_create_barrier(register_state_mapping_t *rsm,
                                   ir_node *block)
{
	int       n_barrier_outs = ARR_LEN(rsm->regs);
	ir_node **in             = rsm->value_map;
	ir_node  *barrier;
	int       o;

	assert(ARR_LEN(rsm->value_map) == n_barrier_outs);

	barrier = be_new_Barrier(block, n_barrier_outs, in);

	for (o = 0; o < n_barrier_outs; ++o) {
		const reg_flag_t      *regflag = &rsm->regs[o];
		const arch_register_t *reg     = regflag->reg;
		ir_node               *proj;
		if (reg == NULL) {
			arch_set_out_register_req(barrier, o, arch_no_register_req);
			proj = new_r_Proj(barrier, mode_M, o);
		} else {
			be_set_constr_single_reg_in(barrier, o, reg, 0);
			be_set_constr_single_reg_out(barrier, o, reg, regflag->flags);
			proj = new_r_Proj(barrier, reg->reg_class->mode, o);
		}
		rsm->value_map[o] = proj;
	}

	rsm->last_barrier = barrier;

	return barrier;
}





beabi_helper_env_t *be_abihelper_prepare(ir_graph *irg)
{
	const arch_env_t   *arch_env = be_get_irg_arch_env(irg);
	beabi_helper_env_t *env      = XMALLOCZ(beabi_helper_env_t);

	env->irg = irg;
	prepare_rsm(&env->prolog, arch_env);
	prepare_rsm(&env->epilog, arch_env);

	return env;
}

void be_abihelper_finish(beabi_helper_env_t *env)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(env->irg);

	free_rsm(&env->prolog, arch_env);
	if (env->epilog.reg_index_map != NULL) {
		free_rsm(&env->epilog, arch_env);
	}
	free(env);
}

void be_prolog_add_reg(beabi_helper_env_t *env, const arch_register_t *reg,
                       arch_irn_flags_t flags)
{
	rsm_add_reg(&env->prolog, reg, flags);
}

ir_node *be_prolog_create_start(beabi_helper_env_t *env, dbg_info *dbgi,
                                ir_node *block)
{
	int      n_start_outs = ARR_LEN(env->prolog.regs);
	ir_node *start        = be_new_Start(dbgi, block, n_start_outs);
	int      o;

	assert(env->prolog.value_map == NULL);
	env->prolog.value_map = NEW_ARR_F(ir_node*, n_start_outs);

	for (o = 0; o < n_start_outs; ++o) {
		const reg_flag_t      *regflag = &env->prolog.regs[o];
		const arch_register_t *reg     = regflag->reg;
		ir_node               *proj;
		if (reg == NULL) {
			arch_set_out_register_req(start, o, arch_no_register_req);
			proj = new_r_Proj(start, mode_M, o);
		} else {
			be_set_constr_single_reg_out(start, o, regflag->reg,
			                             regflag->flags);
			arch_irn_set_register(start, o, regflag->reg);
			proj = new_r_Proj(start, reg->reg_class->mode, o);
		}
		env->prolog.value_map[o] = proj;
	}

	/* start node should really be the first thing constructed */
	assert(env->prolog.last_barrier == NULL);
	env->prolog.last_barrier = start;

	return start;
}

ir_node *be_prolog_create_barrier(beabi_helper_env_t *env, ir_node *block)
{
	return rsm_create_barrier(&env->prolog, block);
}

ir_node *be_prolog_get_reg_value(beabi_helper_env_t *env,
                                 const arch_register_t *reg)
{
	return rsm_get_reg_value(&env->prolog, reg);
}

ir_node *be_prolog_get_memory(beabi_helper_env_t *env)
{
	return rsm_get_value(&env->prolog, 0);
}

void be_prolog_set_reg_value(beabi_helper_env_t *env,
                             const arch_register_t *reg, ir_node *value)
{
	rsm_set_reg_value(&env->prolog, reg, value);
}

void be_prolog_set_memory(beabi_helper_env_t *env, ir_node *value)
{
	rsm_set_value(&env->prolog, 0, value);
}



void be_epilog_begin(beabi_helper_env_t *env)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(env->irg);
	rsm_clear_regs(&env->epilog, arch_env);
	env->epilog.value_map    = NEW_ARR_F(ir_node*, 1);
	env->epilog.value_map[0] = NULL;
}

void be_epilog_add_reg(beabi_helper_env_t *env, const arch_register_t *reg,
                       arch_irn_flags_t flags, ir_node *value)
{
	int index = rsm_add_reg(&env->epilog, reg, flags);
	rsm_set_value(&env->epilog, index, value);
}

void be_epilog_set_reg_value(beabi_helper_env_t *env,
                             const arch_register_t *reg, ir_node *value)
{
	rsm_set_reg_value(&env->epilog, reg, value);
}

void be_epilog_set_memory(beabi_helper_env_t *env, ir_node *value)
{
	rsm_set_value(&env->epilog, 0, value);
}

ir_node *be_epilog_get_reg_value(beabi_helper_env_t *env,
                                 const arch_register_t *reg)
{
	return rsm_get_reg_value(&env->epilog, reg);
}

ir_node *be_epilog_get_memory(beabi_helper_env_t *env)
{
	return rsm_get_value(&env->epilog, 0);
}

ir_node *be_epilog_create_barrier(beabi_helper_env_t *env, ir_node *block)
{
	return rsm_create_barrier(&env->epilog, block);
}

ir_node *be_epilog_create_return(beabi_helper_env_t *env, dbg_info *dbgi,
                                 ir_node *block)
{
	int       n_return_in = ARR_LEN(env->epilog.regs);
	ir_node **in          = env->epilog.value_map;
	int       n_res       = 1; /* TODO */
	unsigned  pop         = 0; /* TODO */
	int       i;
	ir_node  *ret;

	assert(ARR_LEN(env->epilog.value_map) == n_return_in);

	ret = be_new_Return(dbgi, get_irn_irg(block), block, n_res, pop,
	                    n_return_in, in);
	for (i = 0; i < n_return_in; ++i) {
		const reg_flag_t      *regflag = &env->epilog.regs[i];
		const arch_register_t *reg     = regflag->reg;
		if (reg != NULL) {
			be_set_constr_single_reg_in(ret, i, reg, 0);
		}
	}

	rsm_clear_regs(&env->epilog, be_get_irg_arch_env(env->irg));
	env->epilog.last_barrier = NULL;

	return ret;
}

static void add_missing_keep_walker(ir_node *node, void *data)
{
	int              n_outs, i;
	unsigned        *found_projs;
	const ir_edge_t *edge;
	ir_mode         *mode = get_irn_mode(node);
	ir_node         *last_keep;
	(void) data;
	if (mode != mode_T)
		return;

	n_outs = arch_irn_get_n_outs(node);
	if (n_outs <= 0)
		return;

	rbitset_alloca(found_projs, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		int      pn;

		/* The node could be kept */
		if (is_End(succ) || is_Anchor(succ))
			continue;

		if (get_irn_mode(succ) == mode_M)
			continue;

		pn = get_Proj_proj(succ);
		assert(pn < n_outs);
		rbitset_set(found_projs, pn);
	}


	/* are keeps missing? */
	last_keep = NULL;
	for (i = 0; i < n_outs; ++i) {
		ir_node                     *block;
		ir_node                     *in[1];
		const arch_register_req_t   *req;
		const arch_register_class_t *cls;

		if (rbitset_is_set(found_projs, i)) {
			continue;
		}

		req = arch_get_out_register_req(node, i);
		cls = req->cls;
		if (cls == NULL || (cls->flags & arch_register_class_flag_manual_ra)) {
			continue;
		}

		block = get_nodes_block(node);
		in[0] = new_r_Proj(node, arch_register_class_mode(cls), i);
		if (last_keep != NULL) {
			be_Keep_add_node(last_keep, cls, in[0]);
		} else {
			last_keep = be_new_Keep(block, 1, in);
			if (sched_is_scheduled(node)) {
				sched_add_after(node, last_keep);
			}
		}
	}
}

void be_add_missing_keeps(ir_graph *irg)
{
	irg_walk_graph(irg, add_missing_keep_walker, NULL, NULL);
}



static void collect_node(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	ir_node *old   = get_irn_link(block);

	set_irn_link(node, old);
	set_irn_link(block, node);
}

static void link_ops_in_block_walker(ir_node *node, void *data)
{
	(void) data;

	switch (get_irn_opcode(node)) {
	case iro_Return:
	case iro_Call:
		collect_node(node);
		break;
	case iro_Alloc:
		/** all non-stack alloc nodes should be lowered before the backend */
		assert(get_Alloc_where(node) == stack_alloc);
		collect_node(node);
		break;
	case iro_Free:
		assert(get_Free_where(node) == stack_alloc);
		collect_node(node);
		break;
	case iro_Builtin:
		if (get_Builtin_kind(node) == ir_bk_return_address) {
			ir_node *param = get_Builtin_param(node, 0);
			tarval  *tv    = get_Const_tarval(param); /* must be Const */
			long     value = get_tarval_long(tv);
			if (value > 0) {
				/* we need esp for the climbframe algo */
				collect_node(node);
			}
		}
		break;
	default:
		break;
	}
}

static heights_t *heights;

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static int dependent_on(const ir_node *n1, const ir_node *n2)
{
	assert(get_nodes_block(n1) == get_nodes_block(n2));

	return heights_reachable_in_block(heights, n1, n2);
}

static int cmp_call_dependency(const void *c1, const void *c2)
{
	const ir_node *n1 = *(const ir_node **) c1;
	const ir_node *n2 = *(const ir_node **) c2;

	/*
		Classical qsort() comparison function behavior:
		0  if both elements are equal
		1  if second is "smaller" that first
		-1 if first is "smaller" that second
	*/
	if (dependent_on(n1, n2))
		return 1;

	if (dependent_on(n2, n1))
		return -1;

	/* The nodes have no depth order, but we need a total order because qsort()
	 * is not stable. */
	return get_irn_idx(n2) - get_irn_idx(n1);
}

static void process_ops_in_block(ir_node *block, void *data)
{
	ir_phase *phase = data;
	unsigned  n;
	unsigned  n_nodes;
	ir_node  *node;
	ir_node **nodes;

	n_nodes = 0;
	for (node = get_irn_link(block); node != NULL; node = get_irn_link(node)) {
		++n_nodes;
	}

	if (n_nodes == 0)
		return;

	nodes = XMALLOCN(ir_node*, n_nodes);
	n = 0;
	for (node = get_irn_link(block); node != NULL; node = get_irn_link(node)) {
		nodes[n++] = node;;
	}
	assert(n == n_nodes);

	/* order nodes according to their data dependencies */
	qsort(nodes, n_nodes, sizeof(nodes[0]), cmp_call_dependency);

	for (n = n_nodes-1; n > 0; --n) {
		ir_node *node = nodes[n];
		ir_node *pred = nodes[n-1];

		phase_set_irn_data(phase, node, pred);
	}
}

void be_collect_stacknodes(beabi_helper_env_t *env)
{
	ir_graph *irg = env->irg;
	irg_walk_graph(irg, firm_clear_link, link_ops_in_block_walker, NULL);

	assert(env->stack_order == NULL);
	env->stack_order = new_phase(irg, phase_irn_init_default);

	heights = heights_new(irg);
	irg_block_walk_graph(irg, NULL, process_ops_in_block, env->stack_order);
	heights_free(heights);
}

ir_node *be_get_stack_pred(const beabi_helper_env_t *env, const ir_node *node)
{
	return phase_get_irn_data(env->stack_order, node);
}
