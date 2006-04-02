/* TEMPLATE emitter */
/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog.h"

#include "../besched.h"

#include "TEMPLATE_emitter.h"
#include "gen_TEMPLATE_emitter.h"
#include "TEMPLATE_nodes_attr.h"
#include "TEMPLATE_new_nodes.h"
#include "TEMPLATE_map_regs.h"

#define SNPRINTF_BUF_LEN 128

static const arch_env_t *arch_env = NULL;


/*************************************************************
 *             _       _    __   _          _
 *            (_)     | |  / _| | |        | |
 *  _ __  _ __ _ _ __ | |_| |_  | |__   ___| |_ __   ___ _ __
 * | '_ \| '__| | '_ \| __|  _| | '_ \ / _ \ | '_ \ / _ \ '__|
 * | |_) | |  | | | | | |_| |   | | | |  __/ | |_) |  __/ |
 * | .__/|_|  |_|_| |_|\__|_|   |_| |_|\___|_| .__/ \___|_|
 * | |                                       | |
 * |_|                                       |_|
 *************************************************************/

/**
 * Return a const or symconst as string.
 */
static const char *node_const_to_str(ir_node *n) {
	/* TODO */
}

/**
 * Returns node's offset as string.
 */
static char *node_offset_to_str(ir_node *n) {
	/* TODO */
}

/* We always pass the ir_node which is a pointer. */
static int TEMPLATE_get_arg_type(const lc_arg_occ_t *occ) {
	return lc_arg_type_ptr;
}


/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(ir_node *irn, int pos) {
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(ir_node *irn, int pos) {
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	}
	else if (is_TEMPLATE_irn(irn)) {
		reg = get_TEMPLATE_out_reg(irn, pos);
	}
	else {
		const ir_edge_t *edge;

		foreach_out_edge(irn, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "non-Proj from mode_T node");
			if (get_Proj_proj(proj) == pos) {
				reg = arch_get_irn_register(arch_env, proj);
				break;
			}
		}
	}

	assert(reg && "no out register found");
	return reg;
}

/**
 * Returns the number of the in register at position pos.
 */
int get_TEMPLATE_reg_nr(ir_node *irn, int pos, int in_out) {
	const arch_register_t *reg;

	if (in_out == 1) {
		reg = get_in_reg(irn, pos);
	}
	else {
		reg = get_out_reg(irn, pos);
	}

	return arch_register_get_index(reg);
}

/**
 * Returns the name of the in register at position pos.
 */
const char *get_TEMPLATE_reg_name(ir_node *irn, int pos, int in_out) {
	const arch_register_t *reg;

	if (in_out == 1) {
		reg = get_in_reg(irn, pos);
	}
	else {
		reg = get_out_reg(irn, pos);
	}

	return arch_register_get_name(reg);
}

/**
 * Get the register name for a node.
 */
static int TEMPLATE_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X  = arg->v_ptr;
	int         nr = occ->width - 1;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'S') {
		buf = get_TEMPLATE_reg_name(X, nr, 1);
	}
	else { /* 'D' */
		buf = get_TEMPLATE_reg_name(X, nr, 0);
	}

	return buf ? lc_arg_append(app, occ, buf, strlen(buf)) : 0;
}

/**
 * Returns the tarval or offset of an TEMPLATE node as a string.
 */
static int TEMPLATE_const_to_str(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X = arg->v_ptr;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'C') {
		buf = node_const_to_str(X);
	}
	else { /* 'O' */
		buf = node_offset_to_str(X);
	}

	return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Determines the SSE suffix depending on the mode.
 */
static int TEMPLATE_get_mode_suffix(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ir_node *X = arg->v_ptr;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (get_mode_size_bits(get_irn_mode(X)) == 32)
		return lc_appendable_chadd(app, 's');
	else
		return lc_appendable_chadd(app, 'd');
}

/**
 * Return the TEMPLATE printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *TEMPLATE_get_arg_env(void) {
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t TEMPLATE_reg_handler   = { TEMPLATE_get_arg_type, TEMPLATE_get_reg_name };
	static const lc_arg_handler_t TEMPLATE_const_handler = { TEMPLATE_get_arg_type, TEMPLATE_const_to_str };
	static const lc_arg_handler_t TEMPLATE_mode_handler  = { TEMPLATE_get_arg_type, TEMPLATE_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();
			//lc_arg_new_env();

		lc_arg_register(env, "TEMPLATE:sreg", 'S', &TEMPLATE_reg_handler);
		lc_arg_register(env, "TEMPLATE:dreg", 'D', &TEMPLATE_reg_handler);
		lc_arg_register(env, "TEMPLATE:cnst", 'C', &TEMPLATE_const_handler);
		lc_arg_register(env, "TEMPLATE:offs", 'O', &TEMPLATE_const_handler);
		lc_arg_register(env, "TEMPLATE:mode", 'M', &TEMPLATE_mode_handler);
	}

	return env;
}

/*
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix) {
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
}


/**
 * Returns the target label for a control flow node.
 */
static char *get_cfop_target(const ir_node *irn, char *buf) {
	ir_node *bl = get_irn_link(irn);

	snprintf(buf, SNPRINTF_BUF_LEN, "BLOCK_%ld", get_irn_node_nr(bl));
	return buf;
}



/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void TEMPLATE_register_emitters(void) {

/* some convienience macros to register additional emitter functions
   (other than the generated ones) */
#define TEMPLATE_EMIT(a) op_TEMPLATE_##a->ops.generic = (op_func)emit_TEMPLATE_##a
#define EMIT(a)          op_##a->ops.generic = (op_func)emit_##a
#define BE_EMIT(a)       op_be_##a->ops.generic = (op_func)emit_be_##a

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	TEMPLATE_register_spec_emitters();

	/* register addtional emitter functions if needed */

#undef TEMPLATE_EMIT
#undef BE_EMIT
#undef EMIT
}


/**
 * Emits code for a node.
 */
void TEMPLATE_emit_node(ir_node *irn, void *env) {
	TEMPLATE_emit_env_t *emit_env = env;
	FILE                *F        = emit_env->out;
	ir_op               *op       = get_irn_op(irn);
	DEBUG_ONLY(firm_dbg_module_t *mod = emit_env->mod;)

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

	if (op->ops.generic) {
		void (*emit)(const ir_node *, void *) = (void (*)(const ir_node *, void *))op->ops.generic;
		(*emit)(irn, env);
	}
	else {
		ir_fprintf(F, "\t\t\t\t\t/* %+F */\n", irn);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void TEMPLATE_gen_block(ir_node *block, void *env) {
	TEMPLATE_emit_env_t *emit_env = env;
	ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(emit_env->out, "BLOCK_%ld:\n", get_irn_node_nr(block));
	sched_foreach(block, irn) {
		TEMPLATE_emit_node(irn, env);
	}
}


/**
 * Emits code for function start.
 */
void TEMPLATE_emit_func_prolog(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	/* TODO: emit function header */
}

/**
 * Emits code for function end
 */
void TEMPLATE_emit_func_epilog(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	/* TODO: emit function end */
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void TEMPLATE_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Main driver
 */
void TEMPLATE_gen_routine(FILE *F, ir_graph *irg, const TEMPLATE_code_gen_t *cg) {
	TEMPLATE_emit_env_t emit_env;

	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.TEMPLATE.emit");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	/* register all emitter functions */
	TEMPLATE_register_emitters();

	TEMPLATE_emit_func_prolog(F, irg);
	irg_block_walk_graph(irg, TEMPLATE_gen_labels, NULL, &emit_env);
	irg_walk_blkwise_graph(irg, NULL, TEMPLATE_gen_block, &emit_env);
	TEMPLATE_emit_func_epilog(F, irg);
}
