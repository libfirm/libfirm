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

	assert(get_irn_n_edges(irn) > pos && "Invalid OUT position");

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

	lc_appendable_chadd(app, '%');
	return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Returns the tarval or offset of an ia32 as a string.
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
 * Return the ia32 printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *TEMPLATE_get_arg_env(void) {
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t ia32_reg_handler   = { ia32_get_arg_type, ia32_get_reg_name };
	static const lc_arg_handler_t ia32_const_handler = { ia32_get_arg_type, ia32_const_to_str };
	static const lc_arg_handler_t ia32_mode_handler  = { ia32_get_arg_type, ia32_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();
			//lc_arg_new_env();

		lc_arg_register(env, "ia32:sreg", 'S', &ia32_reg_handler);
		lc_arg_register(env, "ia32:dreg", 'D', &ia32_reg_handler);
		lc_arg_register(env, "ia32:cnst", 'C', &ia32_const_handler);
		lc_arg_register(env, "ia32:offs", 'O', &ia32_const_handler);
		lc_arg_register(env, "ia32:mode", 'M', &ia32_mode_handler);
	}

	return env;
}

/**
 * For 2-address code we need to make sure the first src reg is equal to dest reg.
 */
void equalize_dest_src(FILE *F, ir_node *n) {
	if (get_ia32_reg_nr(n, 0, 1) != get_ia32_reg_nr(n, 0, 0)) {
		if (get_irn_arity(n) > 1 && get_ia32_reg_nr(n, 1, 1) == get_ia32_reg_nr(n, 0, 0)) {
			if (! is_op_commutative(get_irn_op(n))) {
				/* we only need to exchange for non-commutative ops */
				lc_efprintf(ia32_get_arg_env(), F, "\txchg %1S, %2S\t\t\t/* xchg src1 <-> src2 for 2 address code */\n", n, n);
			}
		}
		else {
			lc_efprintf(ia32_get_arg_env(), F, "\tmovl %1S, %1D\t\t\t/* src -> dest for 2 address code */\n", n, n);
		}
	}
}

/*
 * Add a number to a prefix. This number will not be used a second time.
 */
char *get_unique_label(char *buf, size_t buflen, const char *prefix) {
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

/*********************************************************
 *                 _ _       _
 *                (_) |     (_)
 *   ___ _ __ ___  _| |_     _ _   _ _ __ ___  _ __  ___
 *  / _ \ '_ ` _ \| | __|   | | | | | '_ ` _ \| '_ \/ __|
 * |  __/ | | | | | | |_    | | |_| | | | | | | |_) \__ \
 *  \___|_| |_| |_|_|\__|   | |\__,_|_| |_| |_| .__/|___/
 *                         _/ |               | |
 *                        |__/                |_|
 *********************************************************/

/**
 * Emits code for a Switch (creates a jump table if
 * possible otherwise a cmp-jmp cascade).
 */
void emit_TEMPLATE_Switch(const ir_node *irn, emit_env_t *emit_env) {
	unsigned long       interval;
	jmp_tbl_t          *tbl;
	ir_node           **cases;
	int                 def_projnum;
	int                 do_jmp_tbl = 1;
	const lc_arg_env_t *env        = ia32_get_arg_env();
	FILE               *F          = emit_env->out;

	/* TODO:                                                         */
	/* - create list of projs, each corresponding to one switch case */
	/* - determine the projnumber of the default case                */

	tbl = create_jump_table(cases, def_projnum, "JMPTBL_");

	/* two-complement's magic make this work without overflow */
	interval = tbl.max_value - tbl.min_value;

	/* check value interval: do not create jump table if interval is too large */
	if (interval > 16 * 1024) {
		do_jmp_tbl = 0;
	}

	/* check ratio of value interval to number of branches */
	if (((float)(interval + 1) / (float)tbl.num_branches) > 8.0) {
		do_jmp_tbl = 0;
	}

	if (do_jmp_tbl) {
		/* TODO: emit table code */
	}
	else {
		/* TODO: emit cmp - jmp cascade */
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
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
 * Emits code for a node.
 */
void TEMPLATE_emit_node(ir_node *irn, void *env) {
	emit_env_t *emit_env   = env;
	firm_dbg_module_t *mod = emit_env->mod;
	FILE              *F   = emit_env->out;

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

#define BE_EMIT(a) if (is_TEMPLATE_##a(irn)) { emit_TEMPLATE_##a(irn, emit_env); return; }

	/* generated int emitter functions */
	BE_EMIT(Copy);
	BE_EMIT(Perm);

	BE_EMIT(Const);

	BE_EMIT(Add);
	BE_EMIT(Add_i);
	BE_EMIT(Sub);
	BE_EMIT(Sub_i);
	BE_EMIT(Minus);
	BE_EMIT(Inc);
	BE_EMIT(Dec);

	BE_EMIT(And);
	BE_EMIT(And_i);
	BE_EMIT(Or);
	BE_EMIT(Or_i);
	BE_EMIT(Eor);
	BE_EMIT(Eor_i);
	BE_EMIT(Not);

	BE_EMIT(Shl);
	BE_EMIT(Shl_i);
	BE_EMIT(Shr);
	BE_EMIT(Shr_i);
	BE_EMIT(RotL);
	BE_EMIT(RotL_i);
	BE_EMIT(RotR);

	BE_EMIT(Mul);
	BE_EMIT(Mul_i);

	BE_EMIT(Store);
	BE_EMIT(Load);

	/* generated floating point emitter */
	BE_EMIT(fConst);

	BE_EMIT(fAdd);
	BE_EMIT(fSub);
	BE_EMIT(fMinus);

	BE_EMIT(fMul);
	BE_EMIT(fDiv);

	BE_EMIT(fMin);
	BE_EMIT(fMax);

	BE_EMIT(fLoad);
	BE_EMIT(fStore);

	/* other emitter functions */
//	BE_EMIT(Switch);

	ir_fprintf(F, "\t\t\t\t\t/* %+F */\n", irn);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void TEMPLATE_gen_block(ir_node *block, void *env) {
	ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(((emit_env_t *)env)->out, "BLOCK_%ld:\n", get_irn_node_nr(block));
	sched_foreach(block, irn) {
		TEMPLATE_emit_node(irn, env);
	}
}


/**
 * Emits code for function start.
 */
void TEMPLATE_emit_start(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	/* TODO: emit function header */
}

/**
 * Emits code for function end
 */
void TEMPLATE_emit_end(FILE *F, ir_graph *irg) {
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
void TEMPLATE_gen_routine(FILE *F, ir_graph *irg, const ia32_code_gen_t *cg) {
	emit_env_t emit_env;

	emit_env.mod      = firm_dbg_register("firm.be.TEMPLATE.emit");
	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	TEMPLATE_emit_start(F, irg);
	irg_block_walk_graph(irg, TEMPLATE_gen_labels, NULL, &emit_env);
	irg_walk_blkwise_graph(irg, NULL, TEMPLATE_gen_block, &emit_env);
	TEMPLATE_emit_end(F, irg);
}
