#define SILENCER
/* arm emitter */
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
#include "irprog_t.h"
#include "irargs_t.h"

#include "../besched.h"

#include "arm_emitter.h"
#include "gen_arm_emitter.h"
#include "arm_nodes_attr.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"
#include "gen_arm_regalloc_if.h"

#include "../benode_t.h"

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

int is_immediate_node(ir_node *irn) {
	if (is_arm_Add_i(irn) || is_arm_Sub_i(irn))
		return 1;
	if (is_arm_Shr_i(irn) || is_arm_Shr_i(irn) || is_arm_Shl_i(irn))
		return 1;
	if (is_arm_And_i(irn) || is_arm_Or_i(irn) || is_arm_Eor_i(irn))
		return 1;
	if (is_arm_Or_Shl_i(irn))
		return 1;
	return 0;
}

/**
 * Return a const or symconst as string.
 */
static const char *node_const_to_str(ir_node *n) {
	char buffer[SNPRINTF_BUF_LEN];

	if ( is_immediate_node(n) ) {
		long longvalue = get_tarval_long(get_arm_value(n));
		char *str;
		assert(longvalue < 0x1000 && "constant doesn't fit in shifter_operand");
		snprintf(buffer, SNPRINTF_BUF_LEN - 1, "#%ld << %ld", longvalue & 0xff, (longvalue >> 8) << 1 );
		str = xmalloc(strlen(buffer) * sizeof(char));
		strcpy(str, buffer);
		return str;
	}
	if ( is_arm_Const(n) || is_arm_Const_Neg(n) ) {
		tarval *tv = get_arm_value(n);
		if ( mode_is_int(get_tarval_mode(tv)) ) {
			long longvalue = get_tarval_long(get_arm_value(n));
			char *str;
			assert(longvalue < 0x1000 && "constant doesn't fit in shifter_operand");
			snprintf(buffer, SNPRINTF_BUF_LEN - 1, "#%ld << %ld", longvalue & 0xff, (longvalue >> 8) << 1 );
			str = xmalloc(strlen(buffer) * sizeof(char));
			strcpy(str, buffer);
			return str;
		} else {
			return "found something else in arm_const";
		}
	} else if ( is_arm_SymConst(n) ) {
		return get_arm_symconst_label(n);
	} else {
		assert( 0 && "das ist gar keine Konstante");
		return NULL;
	}

}

/**
 * Returns node's offset as string.
 */
static char *node_offset_to_str(ir_node *n) {
	char buffer[SNPRINTF_BUF_LEN];
	char *result;
	int offset = 0;
	ir_op *irn_op = get_irn_op(n);
	if (irn_op == op_be_StackParam) {
		entity *ent = be_get_frame_entity(n);
		offset = get_entity_offset_bytes(ent);
	} else if (irn_op==op_be_Reload || irn_op==op_be_Spill) {
		entity * ent = be_get_spill_entity(n);
		offset = get_entity_offset_bytes(ent);
	} else if (irn_op==op_be_IncSP) {
		int offs = be_get_IncSP_offset(n);
		be_stack_dir_t dir  = be_get_IncSP_direction(n);
		offset = (dir == be_stack_dir_expand) ? -offs : offs;
	} else {
		return "node_offset_to_str will fuer diesen Knotentyp noch implementiert werden";
	}
	snprintf(buffer, SNPRINTF_BUF_LEN, "%d", offset);
	result = xmalloc(sizeof(char)*(strlen(buffer) + 1));
	strcpy(result, buffer);
	return result;
}

/* We always pass the ir_node which is a pointer. */
static int arm_get_arg_type(const lc_arg_occ_t *occ) {
	return lc_arg_type_ptr;
}


/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *irn, int pos) {
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	/* ONLY TEMPORARY WORK-AROUND */
// 	if (!reg) {
// 		printf("FIXME\n");
// 		reg = &arm_general_purpose_regs[REG_MURX];
// 	}

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(const ir_node *irn, int pos) {
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
	else if (is_arm_irn(irn)) {
		reg = get_arm_out_reg(irn, pos);
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
int get_arm_reg_nr(ir_node *irn, int pos, int in_out) {
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
const char *get_arm_reg_name(ir_node *irn, int pos, int in_out) {
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
static int arm_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X  = arg->v_ptr;
	int         nr = occ->width - 1;

	if (!X)
		return lc_appendable_snadd(app, "(null)", 6);

	if (occ->conversion == 'S') {
		buf = get_arm_reg_name(X, nr, 1);
	}
	else { /* 'D' */
		buf = get_arm_reg_name(X, nr, 0);
	}

	lc_appendable_chadd(app, '%');
	return lc_appendable_snadd(app, buf, strlen(buf));
}

/**
 * Returns the tarval or offset of an arm node as a string.
 */
static int arm_const_to_str(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X = arg->v_ptr;

	if (!X)
		return lc_appendable_snadd(app, "(null)", 6);

	if (occ->conversion == 'C') {
		buf = node_const_to_str(X);
	}
	else { /* 'O' */
		buf = node_offset_to_str(X);
	}

	return lc_appendable_snadd(app, buf, strlen(buf));
}

/**
 * Determines the SSE suffix depending on the mode.
 */
static int arm_get_mode_suffix(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ir_node *X = arg->v_ptr;

	if (!X)
		return lc_appendable_snadd(app, "(null)", 6);

	if (get_mode_size_bits(get_irn_mode(X)) == 32)
		return lc_appendable_chadd(app, 's');
	else
		return lc_appendable_chadd(app, 'd');
}

/**
 * Return the arm printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *arm_get_arg_env(void) {
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t arm_reg_handler   = { arm_get_arg_type, arm_get_reg_name };
	static const lc_arg_handler_t arm_const_handler = { arm_get_arg_type, arm_const_to_str };
	static const lc_arg_handler_t arm_mode_handler  = { arm_get_arg_type, arm_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();
			//lc_arg_new_env();

		lc_arg_register(env, "arm:sreg", 'S', &arm_reg_handler);
		lc_arg_register(env, "arm:dreg", 'D', &arm_reg_handler);
		lc_arg_register(env, "arm:cnst", 'C', &arm_const_handler);
		lc_arg_register(env, "arm:offs", 'O', &arm_const_handler);
		lc_arg_register(env, "arm:mode", 'M', &arm_mode_handler);
	}

	return env;
}

/**
 * Formated print of commands and comments.
 */
static void arm_fprintf_format(FILE *F, char *cmd_buf, char *cmnt_buf, ir_node *irn) {
	lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
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

/************************************************************************/
/* emit_arm                                                             */
/************************************************************************/

static void emit_arm_SymConst(ir_node *irn, void *env) {
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	char buffer1[SNPRINTF_BUF_LEN];
	char *skip_label = get_unique_label(buffer1, SNPRINTF_BUF_LEN, ".L");
	char buffer2[SNPRINTF_BUF_LEN];
	char *indi_label = get_unique_label(buffer2, SNPRINTF_BUF_LEN, ".L");
	fprintf( out, "\tB %s\t\t\t\t\t/* start of indirect SymConst */\n", skip_label );
	fprintf( out, "\t.align 2\n" );
	fprintf( out, "%s:\n", indi_label );
	lc_efprintf(arm_get_arg_env(), out, "\t.word\t%C\n", irn);
	fprintf( out, "\t.align 2\n" );
	fprintf( out, "%s:\n", skip_label );
	lc_efprintf(arm_get_arg_env(), out, "\tLDR %1D, %s\t\t\t/* end of indirect SymConst */\n", irn, indi_label);
}

static void emit_arm_CondJmp(ir_node *irn, void *env) {
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	const ir_edge_t *edge;
	ir_node *true_block = NULL;
	ir_node *false_block = NULL;
	ir_node *op1 = get_irn_n(irn, 0);
	ir_mode *opmode = get_irn_mode(op1);
	char *suffix;
	int proj_num = get_arm_proj_num(irn);
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];


 	foreach_out_edge(irn, edge) {
 		ir_node* proj = get_edge_src_irn(edge);
 		long nr = get_Proj_proj(proj);
		ir_node *block = get_irn_link(proj);
		if ( nr == pn_Cond_true) {
			true_block = block;
		} else if (nr == pn_Cond_false) {
			false_block = block;
		} else {
			assert(0 && "tertium non datur! (CondJmp)");
		}
 	}

	if (proj_num == pn_Cmp_False) {
		fprintf(out, "\tB BLOCK_%ld\t\t\t/* false case */\n", get_irn_node_nr(false_block));
	} else if (proj_num == pn_Cmp_True) {
		fprintf(out, "\tB BLOCK_%ld\t\t\t/* true case */\n", get_irn_node_nr(true_block));
	} else {
		if (mode_is_float(opmode)) {
			suffix = "ICHWILLIMPLEMENTIERTWERDEN";

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "FCMP %1S, %2S", irn, irn);
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* Compare(%1S, %2S) -> FCPSR */", irn, irn );
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "FMSTAT", irn, irn);
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* FCSPR -> CPSR */");
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "B%s BLOCK_%d", suffix, get_irn_node_nr(true_block));
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* true case */");
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "B BLOCK_%d", get_irn_node_nr(false_block));
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* false case */");
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		} else {
			switch(proj_num) {
				case pn_Cmp_Eq:  suffix = "EQ"; break;
				case pn_Cmp_Lt:  suffix = "LT"; break;
				case pn_Cmp_Le:  suffix = "LE"; break;
				case pn_Cmp_Gt:  suffix = "GT"; break;
				case pn_Cmp_Ge:  suffix = "GE"; break;
				case pn_Cmp_Lg:  suffix = "NE"; break;
				case pn_Cmp_Leg: suffix = "AL"; break;
			default: assert(0 && "komische Dinge geschehen");
			}

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "CMP %1S, %2S", irn, irn);
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* Compare(%1S, %2S) -> CPSR */", irn, irn );
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "B%s BLOCK_%d", suffix, get_irn_node_nr(true_block));
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* true case */");
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);

			lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "B BLOCK_%d", get_irn_node_nr(false_block));
			lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* false case */");
			arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		}
	}
}

static void emit_arm_CopyB(ir_node *irn, void *env) {
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
	unsigned int size = get_tarval_long(get_arm_value(irn));
	const lc_arg_env_t *arg_env = arm_get_arg_env();

	lc_esnprintf(arg_env, cmnt_buf, SNPRINTF_BUF_LEN, "/* MemCopy (%2S)->(%1S) [%d bytes], Use %3S, %4S, %5S and %%r12 */", irn, irn, size, irn, irn, irn);

	assert ( size > 0 && "CopyB needs size > 0" );
	if (size & 3)
		size += 4;
	size >>= 2;
	switch(size & 3) {
	case 0:
		break;
	case 1:
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "LDR %%r12, [%2S, #0]!", irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "STR  %%r12, [%1S, #0]!", irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		break;
	case 2:
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "LDMIA %2S!, {%%r12, %3S}", irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "STMIA %1S!, {%%r12, %3S}", irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		break;
	case 3:
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "LDMIA %2S!, {%%r12, %3S, %4S}", irn, irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "STMIA %1S!, {%%r12, %3S, %4S}", irn, irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		break;
	}
	size >>= 2;
	while (size) {
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "LDMIA %2S!, {%%r12, %3S, %4S, %5S}", irn, irn, irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		lc_esnprintf(arg_env, cmd_buf, SNPRINTF_BUF_LEN, "STMIA %1S!, {%%r12, %3S, %4S, %5S}", irn, irn, irn, irn);
		arm_fprintf_format(out, cmd_buf, cmnt_buf, irn);
		--size;
	}
}

static void emit_arm_SwitchJmp(ir_node *irn, void *env) {
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
	const ir_edge_t    *edge;
	ir_node            *proj;
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	int i;
	ir_node **projs;
	int n_projs;
	int block_nr;
	int default_block_num;

	block_nr = get_irn_node_nr(irn);
	n_projs = get_arm_n_projs(irn);

	projs = xcalloc(n_projs , sizeof(ir_node*));

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		if (get_Proj_proj(proj) == get_arm_default_proj_num(irn))
			default_block_num = get_irn_node_nr(get_irn_link(proj));

		projs[get_Proj_proj(proj)] = proj;
	}

	// CMP %1S, n_projs - 1
	// BHI default



	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "CMP %1S, #%u", irn, n_projs - 1);
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "", irn);
	lc_efprintf(arm_get_arg_env(), out, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);

	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "BHI BLOCK_%d", default_block_num);
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "", irn);
	lc_efprintf(arm_get_arg_env(), out, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);


	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "LDR %%r12, TABLE_%d_START", block_nr);
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "", irn);
	lc_efprintf(arm_get_arg_env(), out, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);

	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "ADD %%r12, %%r12, %1S, LSL #2", irn);
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "", irn);
	lc_efprintf(arm_get_arg_env(), out, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);

	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "LDR %%r15, [%%r12, #0]");
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "", irn);
	lc_efprintf(arm_get_arg_env(), out, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);

	// LDR %r12, .TABLE_X_START
	// ADD %r12, %r12, [%1S, LSL #2]
	// LDR %r15, %r12

	fprintf(out, "TABLE_%d_START:\n\t.word\tTABLE_%d\n", block_nr, block_nr);
	fprintf(out, "\t.align 2\n");
	fprintf(out, "TABLE_%d:\n", block_nr);


	for ( i=0; i<n_projs; i++) {
		ir_node *block;
		proj = projs[i];
		if ( proj ) {
			block = get_irn_link(proj);
		} else {
			block = get_irn_link(projs[get_arm_default_proj_num(irn)]);
		}
		fprintf(out, "\t.word\tBLOCK_%ld\n",get_irn_node_nr(block));
	}
	fprintf(out, "\t.align 2\n");

	xfree(projs);
}

/************************************************************************/
/* emit_be                                                              */
/************************************************************************/

static void emit_be_Call(ir_node *irn, void *env) {
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	entity *target_entity = be_Call_get_entity(irn);
	const char *target_name = get_entity_name(target_entity);
	fprintf(out, "\tBL %s\t\t\t\t/* Call */\n", target_name);
}

static void emit_be_IncSP(const ir_node *irn, arm_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	unsigned offs = be_get_IncSP_offset(irn);
	if (offs) {
		char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "ADD %1D, %1S, #%O", irn, irn, irn );
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* IncSP(%O) */", irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else {
		char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "");
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* omitted IncSP(%O) */", irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	}
}

// void emit_be_AddSP(const ir_node *irn, arm_emit_env_t *emit_env) {
// 	FILE *F = emit_env->out;
// 	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
// 	lc_esnprintf(arm_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "ADD %1D, %1S, %2S", irn, irn, irn );
// 	lc_esnprintf(arm_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* AddSP(%2S) */", irn);
// 	lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
// }

static void emit_be_Copy(const ir_node *irn, arm_emit_env_t *emit_env) {
	FILE *F    = emit_env->out;
	ir_mode *mode = get_irn_mode(irn);
	assert( (mode != mode_E) && "IEEE Extended FP not supported");

	if (get_in_reg(irn, 0) == get_out_reg(irn, 0)) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "");
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* omitted Copy: %1S -> %1D */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
		return;
	}

	if (mode == mode_F) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "FCPYS %1D, %1S", irn, irn);
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* Copy: %1S -> %1D */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else if (mode == mode_D) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "FCPYD %1D, %1S", irn, irn);
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* Copy: %1S -> %1D */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else if (mode_is_numP(mode)) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "MOV %1D, %1S", irn, irn);
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* Copy: %1S -> %1D */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else {
		assert(0 && "move not supported for this mode");
	}
//	emit_arm_Copy(irn, emit_env);
}

static void emit_be_Spill(const ir_node *irn, arm_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	ir_mode *mode = get_irn_mode(irn);
	assert( (mode != mode_E) && "IEEE Extended FP not supported");
	if (mode_is_dataM(mode)) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "STR %2S, [%1S, #%O]", irn, irn, irn );
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* Spill(%2S) -> (%1S) */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else {
		assert(0 && "spill not supported for this mode");
	}
}

static void emit_be_Reload(const ir_node* irn, arm_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	ir_mode *mode = get_irn_mode(irn);
	assert( (mode != mode_E) && "IEEE Extended FP not supported");
	if (mode_is_dataM(mode)) {
		char cmd_buf[256], cmnt_buf[256];
		lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "LDR %1D, [%1S, #%O]", irn, irn, irn );
		lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* Reload(%1S) -> (%1D) */", irn, irn);
		lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
	} else {
		assert(0 && "reload not supported for this mode");
	}
}

static void emit_be_Perm(const ir_node* irn, arm_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	ir_mode *mode = get_irn_mode(irn);
	assert( (mode != mode_E) && "IEEE Extended FP not supported");
	lc_efprintf(arm_get_arg_env(), F, "\tEOR %1S, %1S, %2S\t\t\t/* begin Perm(%1S, %2S) */\n", irn, irn, irn, irn, irn);
	lc_efprintf(arm_get_arg_env(), F, "\tEOR %2S, %1S, %2S\n", irn, irn, irn);
	lc_efprintf(arm_get_arg_env(), F, "\tEOR %1S, %1S, %2S\t\t\t/* end Perm(%1S, %2S) */\n", irn, irn, irn, irn, irn);
}

static void emit_be_StackParam(const ir_node *irn, arm_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	ir_mode *mode = get_irn_mode(irn);
	char cmd_buf[256], cmnt_buf[256];
	assert( (mode != mode_E) && "IEEE Extended FP not supported");

	lc_esnprintf(arm_get_arg_env(), cmd_buf, 256, "LDR %1D, [%1S, #%O]", irn, irn, irn );
	lc_esnprintf(arm_get_arg_env(), cmnt_buf, 256, "/* StackParam: (%1S + %O) -> %1D */",irn , irn, irn, get_irn_n(irn, 0));
	lc_efprintf(arm_get_arg_env(), F, "\t%-35s %-60s /* %+F */\n", cmd_buf, cmnt_buf, irn);
}

/************************************************************************/
/* emit                                                                 */
/************************************************************************/

static void emit_Jmp(ir_node *irn, void *env) {
	arm_emit_env_t *emit_env = env;
	FILE *out = emit_env->out;
	const ir_edge_t *edge = get_irn_out_edge_first(irn);
	ir_node *target_block = get_edge_src_irn(edge);
	fprintf(out, "\tB BLOCK_%ld\t\t\t/* unconditional Jump */\n", get_irn_node_nr(target_block));
}

static void emit_silence(ir_node *irn, void *env) {

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
static void arm_register_emitters(void) {

#define ARM_EMIT(a) op_arm_##a->ops.generic = (op_func)emit_arm_##a
#define EMIT(a)      op_##a->ops.generic = (op_func)emit_##a
#define BE_EMIT(a)   op_be_##a->ops.generic = (op_func)emit_be_##a
#define SILENCE(a)   op_##a->ops.generic = (op_func)emit_silence

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	arm_register_spec_emitters();

	/* other emitter functions */
 	ARM_EMIT(CondJmp);
// 	ARM_EMIT(SwitchJmp);
 	ARM_EMIT(CopyB);
// 	ARM_EMIT(CopyB_i);
//	ARM_EMIT(Const);
	ARM_EMIT(SymConst);
	ARM_EMIT(SwitchJmp);

	/* benode emitter */
 	BE_EMIT(Call);
 	BE_EMIT(IncSP);
// 	BE_EMIT(AddSP);
	BE_EMIT(Copy);
	BE_EMIT(Spill);
	BE_EMIT(Reload);
	BE_EMIT(Perm);
	BE_EMIT(StackParam);

	/* firm emitter */
 	EMIT(Jmp);


	/* noisy stuff */
#ifdef SILENCER
 	SILENCE(Proj);
 	SILENCE(Phi);
#endif

#undef ARM_EMIT
#undef BE_EMIT
#undef EMIT
#undef SILENCE
}

/**
 * Emits code for a node.
 */
static void arm_emit_node(const ir_node *irn, void *env) {
	arm_emit_env_t        *emit_env = env;
	FILE              *F        = emit_env->out;
	ir_op             *op       = get_irn_op(irn);
	DEBUG_ONLY(firm_dbg_module_t *mod      = emit_env->mod;)

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
void arm_gen_block(ir_node *block, void *env) {
	ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(((arm_emit_env_t *)env)->out, "BLOCK_%ld:\n", get_irn_node_nr(block));
	sched_foreach(block, irn) {
		arm_emit_node(irn, env);
	}
}


/**
 * Emits code for function start.
 */
void arm_emit_start(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));
	fprintf(F, "\t.text\n");
	fprintf(F, "\t.align  2\n");
	fprintf(F, "\t.global %s\n", irg_name);
	fprintf(F, "%s:\n", irg_name);
}

/**
 * Emits code for function end
 */
void arm_emit_end(FILE *F, ir_graph *irg) {
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void arm_gen_labels(ir_node *block, void *env) {
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
void arm_gen_routine(FILE *F, ir_graph *irg, const arm_code_gen_t *cg) {
	arm_emit_env_t emit_env;

	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.arm.emit");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	arm_register_emitters();

	arm_emit_start(F, irg);
	irg_block_walk_graph(irg, arm_gen_labels, NULL, &emit_env);
	irg_walk_blkwise_graph(irg, NULL, arm_gen_block, &emit_env);
	arm_emit_end(F, irg);
}
