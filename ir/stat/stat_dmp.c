/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm. Dumping.
 * @author  Michael Beck
 */
#include "stat_dmp.h"
#include "irhooks.h"
#include "util.h"
#include "fourcc.h"

/**
 * names of the optimizations
 */
static const struct {
	hook_opt_kind kind;
	const char    *name;
} opt_names[] = {
	{ HOOK_OPT_DEAD_BLOCK,                  "dead block elimination" },
	{ HOOK_OPT_STG,                         "straightening optimization" },
	{ HOOK_OPT_IFSIM,                       "if simplification" },
	{ HOOK_OPT_CONST_EVAL,                  "constant evaluation" },
	{ HOOK_OPT_ALGSIM,                      "algebraic simplification" },
	{ HOOK_OPT_PHI,                         "Phi optmization" },
	{ HOOK_OPT_SYNC,                        "Sync optmization" },
	{ HOOK_OPT_WAW,                         "Write-After-Write optimization" },
	{ HOOK_OPT_WAR,                         "Write-After-Read optimization" },
	{ HOOK_OPT_RAW,                         "Read-After-Write optimization" },
	{ HOOK_OPT_RAR,                         "Read-After-Read optimization" },
	{ HOOK_OPT_RC,                          "Read-a-Const optimization" },
	{ HOOK_OPT_TUPLE,                       "Tuple optimization" },
	{ HOOK_OPT_ID,                          "ID optimization" },
	{ HOOK_OPT_CSE,                         "Common subexpression elimination" },
	{ HOOK_OPT_STRENGTH_RED,                "Strength reduction" },
	{ HOOK_OPT_ARCH_DEP,                    "Architecture dependant optimization" },
	{ HOOK_OPT_REASSOC,                     "Reassociation optimization" },
	{ HOOK_OPT_POLY_CALL,                   "Polymorphic call optimization" },
	{ HOOK_OPT_IF_CONV,                     "an if conversion was tried" },
	{ HOOK_OPT_FUNC_CALL,                   "Real function call optimization" },
	{ HOOK_OPT_CONFIRM,                     "Confirm-based optimization: replacement" },
	{ HOOK_OPT_CONFIRM_C,                   "Confirm-based optimization: replaced by const" },
	{ HOOK_OPT_CONFIRM_E,                   "Confirm-based optimization: evaluated" },
	{ HOOK_OPT_EXC_REM,                     "a exception edge was removed due to a Confirmation prove" },
	{ HOOK_OPT_NORMALIZE,                   "a commutative node was normalized" },
	{ HOOK_LOWERED,                         "Lowered" },
	{ HOOK_BACKEND,                         "Backend transformation" },
	{ (hook_opt_kind)FS_OPT_NEUTRAL_0,      "algebraic simplification: a op 0 = 0 op a = a" },
	{ (hook_opt_kind)FS_OPT_NEUTRAL_1,      "algebraic simplification: a op 1 = 1 op a = a" },
	{ (hook_opt_kind)FS_OPT_ADD_A_A,        "algebraic simplification: a + a = a * 2" },
	{ (hook_opt_kind)FS_OPT_ADD_A_MINUS_B,  "algebraic simplification: a + -b = a - b" },
	{ (hook_opt_kind)FS_OPT_ADD_SUB,        "algebraic simplification: (a + x) - x = (a - x) + x = a" },
	{ (hook_opt_kind)FS_OPT_ADD_MUL_A_X_A,  "algebraic simplification: a * x + a = a * (x + 1)" },
	{ (hook_opt_kind)FS_OPT_SUB_0_A,        "algebraic simplification: 0 - a = -a" },
	{ (hook_opt_kind)FS_OPT_MINUS_SUB,      "algebraic simplification: -(a - b) = b - a" },
	{ (hook_opt_kind)FS_OPT_SUB_MINUS,      "algebraic simplification: a - (-b) = a + b" },
	{ (hook_opt_kind)FS_OPT_SUB_MUL_A_X_A,  "algebraic simplification: a * x - a = a * (x - 1)" },
	{ (hook_opt_kind)FS_OPT_SUB_SUB_X_Y_Z,  "algebraic simplification: (x - y) - z = x - (y + z)" },
	{ (hook_opt_kind)FS_OPT_SUB_C_NOT_X,    "algebraic simplification: c - ~a = a + (c+1)" },
	{ (hook_opt_kind)FS_OPT_SUB_TO_ADD,     "algebraic simplification: (-a) - b = -(a + b), a - (b - c) = a + (c - b), a - (b * C) = a + (b * -C)" },
	{ (hook_opt_kind)FS_OPT_SUB_TO_NOT,     "algebraic simplification: -1 - x -> ~x" },
	{ (hook_opt_kind)FS_OPT_SUB_TO_CONV,    "algebraic simplification: a - NULL = (int)a" },
	{ (hook_opt_kind)FS_OPT_MUL_MINUS,      "algebraic simplification: (-a) * (b - c) = a * (c - b)" },
	{ (hook_opt_kind)FS_OPT_MUL_MINUS_1,    "algebraic simplification: a * -1 = -a" },
	{ (hook_opt_kind)FS_OPT_MINUS_MUL_C,    "algebraic simplification: (-a) * C = a * (-C)" },
	{ (hook_opt_kind)FS_OPT_MUL_MINUS_MINUS,"algebraic simplification: (-a) * (-b) = a * b" },
	{ (hook_opt_kind)FS_OPT_OR,             "algebraic simplification: a | a = a | 0 = 0 | a = a" },
	{ (hook_opt_kind)FS_OPT_AND,            "algebraic simplification: a & 0b1...1 = 0b1...1 & a = a & a = (a|X) & a = a" },
	{ (hook_opt_kind)FS_OPT_TO_EOR,         "algebraic simplification: (a|b) & ~(a&b) = a^b" },
	{ (hook_opt_kind)FS_OPT_EOR_A_A,        "algebraic simplification: a ^ a = 0" },
	{ (hook_opt_kind)FS_OPT_EOR_A_B_A,      "algebraic simplification: (a ^ b) ^ a = b" },
	{ (hook_opt_kind)FS_OPT_EOR_TO_NOT_BOOL,"boolean simplification: bool ^ 1 = !bool" },
	{ (hook_opt_kind)FS_OPT_EOR_TO_NOT,     "algebraic simplification: x ^ 0b1..1 = ~x, (a ^ b) & b = ~a & b" },
	{ (hook_opt_kind)FS_OPT_NOT_CMP,        "algebraic simplification: !(a cmp b) = a !cmp b" },
	{ (hook_opt_kind)FS_OPT_REASSOC_SHIFT,  "algebraic simplification: (x SHF c1) SHF c2 = x SHF (c1+c2)" },
	{ (hook_opt_kind)FS_OPT_SHIFT_AND,      "algebraic simplification: (a SHF c) AND (b SHF c) = (a AND b) SHF c" },
	{ (hook_opt_kind)FS_OPT_SHIFT_OR,       "algebraic simplification: (a SHF c) OR (b SHF c) = (a OR b) SHF c" },
	{ (hook_opt_kind)FS_OPT_SHIFT_EOR,      "algebraic simplification: (a SHF c) XOR (b SHF c) = (a XOR b) SHF c" },
	{ (hook_opt_kind)FS_OPT_CONV,           "algebraic simplification: Conv could be removed" },
	{ (hook_opt_kind)FS_OPT_MIN_MAX_EQ,     "algebraic simplification: Min(a,a) = Max(a,a) = a" },
	{ (hook_opt_kind)FS_OPT_MUX_COMBINE,    "boolean simplification: two Mux nodes where combined into one" },
	{ (hook_opt_kind)FS_OPT_MUX_CONV,       "boolean simplification: MuxI(sel, 1, 0) = (I)sel" },
	{ (hook_opt_kind)FS_OPT_MUX_BOOL,       "boolean simplification: Muxb(sel, true, false) = sel" },
	{ (hook_opt_kind)FS_OPT_MUX_NOT_BOOL,   "boolean simplification: Muxb(sel, false, true) = Not(sel)" },
	{ (hook_opt_kind)FS_OPT_MUX_OR_BOOL,    "boolean simplification: Muxb(sel, true, x) = Or(sel, x)" },
	{ (hook_opt_kind)FS_OPT_MUX_ORNOT_BOOL, "boolean simplification: Muxb(sel, x, true) = Or(Not(sel), x)" },
	{ (hook_opt_kind)FS_OPT_MUX_AND_BOOL,   "boolean simplification: Muxb(sel, x, false) = And(sel, x)" },
	{ (hook_opt_kind)FS_OPT_MUX_ANDNOT_BOOL,"boolean simplification: Muxb(sel, false, x) = And(Not(sel), x)" },
	{ (hook_opt_kind)FS_OPT_MUX_C,          "algebraic simplification: Mux(C, f, t) = C ? t : f" },
	{ (hook_opt_kind)FS_OPT_MUX_EQ,         "algebraic simplification: Mux(v, x, x) = x" },
	{ (hook_opt_kind)FS_OPT_MUX_TRANSFORM,  "algebraic simplification: Mux(t ==/!= f, t, f) = f/t, Mux(t ==/!= 0, -t, t) = -t/t" },
	{ (hook_opt_kind)FS_OPT_MUX_TO_MIN,     "algebraic simplification: Mux(a < b, a, b) = Min(a,b)" },
	{ (hook_opt_kind)FS_OPT_MUX_TO_MAX,     "algebraic simplification: Mux(a > b, a, b) = Max(a,b)" },
	{ (hook_opt_kind)FS_OPT_MUX_TO_BITOP,   "algebraic simplification: Mux((a & 2^x) ==/!= 0, 2^x, 0) = (a & 2^x) (xor 2^x)" },
	{ (hook_opt_kind)FS_OPT_INVOLUTION,     "algebraic simplification: OP(OP(x)) = x" },
	{ (hook_opt_kind)FS_OPT_MINUS_NOT,      "algebraic simplification: -(~x) = x + 1" },
	{ (hook_opt_kind)FS_OPT_NOT_MINUS_1,    "algebraic simplification: ~(x - 1) = -x" },
	{ (hook_opt_kind)FS_OPT_NOT_PLUS_C,     "algebraic simplification: ~x + C = (C - 1) - x" },
	{ (hook_opt_kind)FS_OPT_ADD_X_NOT_X,    "algebraic simplification: ~x + x = -1" },
	{ (hook_opt_kind)FS_OPT_FP_INV_MUL,     "algebraic simplification: x / y = x * (1.0/y)" },
	{ (hook_opt_kind)FS_OPT_CONST_PHI,      "constant evaluation on Phi node" },
	{ (hook_opt_kind)FS_OPT_PREDICATE,      "predicate optimization" },
	{ (hook_opt_kind)FS_OPT_DEMORGAN,       "optimization using DeMorgan's law" },
	{ (hook_opt_kind)FS_OPT_CMP_OP_OP,      "CMP optimization: Cmp(OP(x), OP(y)) = Cmp(x, y)" },
	{ (hook_opt_kind)FS_OPT_CMP_OP_C,       "CMP optimization: Cmp(OP(x), c1) = Cmp(x, c2)" },
	{ (hook_opt_kind)FS_OPT_CMP_CONV_CONV,  "CMP optimization: Cmp(Conv(x), Conv(y)) = Cmp(x, y)" },
	{ (hook_opt_kind)FS_OPT_CMP_CONV,       "CMP optimization: Cmp(Conv(x), Conv(y)) = Cmp(Conv(x), y)" },
	{ (hook_opt_kind)FS_OPT_CMP_TO_BOOL,    "CMP optimization: Cmp(x, y) = BoolOP(x, y)" },
	{ (hook_opt_kind)FS_OPT_CMP_CNST_MAGN,  "CMP optimization: reduced magnitude of a const" },
	{ (hook_opt_kind)FS_OPT_CMP_SHF_TO_AND, "CMP optimization: transformed shift into And" },
	{ (hook_opt_kind)FS_OPT_CMP_MOD_TO_AND, "CMP optimization: transformed Mod into And" },
	{ (hook_opt_kind)FS_OPT_NOP,            "the operation is a NOP" },
	{ (hook_opt_kind)FS_OPT_GVN_FOLLOWER,   "GVN-PRE: replaced a follower" },
	{ (hook_opt_kind)FS_OPT_GVN_FULLY,      "GVN-PRE: replaced by fully redundant value" },
	{ (hook_opt_kind)FS_OPT_GVN_PARTLY,     "GVN-PRE: replaced by partly redundant value" },
	{ (hook_opt_kind)FS_OPT_COMBO_CONST,    "Combo: evaluated into Constant" },
	{ (hook_opt_kind)FS_OPT_COMBO_CF,       "Combo: removed conditional control flow" },
	{ (hook_opt_kind)FS_OPT_COMBO_FOLLOWER, "Combo: removed a follower" },
	{ (hook_opt_kind)FS_OPT_COMBO_CONGRUENT,"Combo: replaced by congruent" },
	{ (hook_opt_kind)FS_OPT_JUMPTHREADING,  "Jump threading: removed conditional control flow" },
	{ (hook_opt_kind)FS_OPT_RTS_ABS,        "RTS optimization: call to abs() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_ALLOCA,     "RTS optimization: call to alloca() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_SQRT,       "RTS optimization: call to sqrt() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_CBRT,       "RTS optimization: call to cbrt() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_POW,        "RTS optimization: call to pow() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_EXP,        "RTS optimization: call to exp() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_LOG,        "RTS optimization: call to log() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_SIN,        "RTS optimization: call to sin() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_COS,        "RTS optimization: call to cos() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_TAN,        "RTS optimization: call to tan() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_ASIN,       "RTS optimization: call to asin() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_ACOS,       "RTS optimization: call to atan() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_ATAN,       "RTS optimization: call to acos() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_SINH,       "RTS optimization: call to sinh() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_COSH,       "RTS optimization: call to cosh() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_TANH,       "RTS optimization: call to tanh() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_SYMMETRIC,  "RTS optimization: call to symmetric function f(-x) replaced by f(x)" },
	{ (hook_opt_kind)FS_OPT_RTS_STRCMP,     "RTS optimization: call to strcmp() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_STRNCMP,    "RTS optimization: call to strncmp() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_STRCPY,     "RTS optimization: call to strcpy() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_STRLEN,     "RTS optimization: call to strlen() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_MEMCPY,     "RTS optimization: call to memcpy() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_MEMPCPY,    "RTS optimization: call to mempcpy() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_MEMMOVE,    "RTS optimization: call to memmove() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_MEMSET,     "RTS optimization: call to memset() replaced" },
	{ (hook_opt_kind)FS_OPT_RTS_MEMCMP,     "RTS optimization: call to memcmp() replaced" },
};

/**
 * dumps a opcode hash into human readable form
 */
static void simple_dump_opcode_hash(dumper_t *dmp, pset *set)
{
	counter_t f_alive;
	counter_t f_new_node;
	counter_t f_Id;
	counter_t f_normlized;

	cnt_clr(&f_alive);
	cnt_clr(&f_new_node);
	cnt_clr(&f_Id);
	cnt_clr(&f_normlized);

	fprintf(dmp->f, "%-16s %-8s %-8s %-8s %-8s\n", "Opcode", "alive", "created", "->Id", "normalized");
	foreach_pset(set, node_entry_t, entry) {
		fprintf(dmp->f, "%-16s %8u %8u %8u %8u\n",
			entry->op_id,
			cnt_to_uint(&entry->cnt_alive),
			cnt_to_uint(&entry->new_node),
			cnt_to_uint(&entry->into_Id),
			cnt_to_uint(&entry->normalized)
		);

		cnt_add(&f_alive,     &entry->cnt_alive);
		cnt_add(&f_new_node,  &entry->new_node);
		cnt_add(&f_Id,        &entry->into_Id);
		cnt_add(&f_normlized, &entry->normalized);
	}
	fprintf(dmp->f, "-------------------------------------------\n");
	fprintf(dmp->f, "%-16s %8u %8u %8u %8u\n", "Sum",
		cnt_to_uint(&f_alive),
		cnt_to_uint(&f_new_node),
		cnt_to_uint(&f_Id),
		cnt_to_uint(&f_normlized)
	);
}

/**
 * Return the name of an optimization.
 */
static const char *get_opt_name(int index)
{
	assert(index < (int) ARRAY_SIZE(opt_names) && "index out of range");
	assert((int) opt_names[index].kind == index && "opt_names broken");
	return opt_names[index].name;
}

/**
 * dumps an optimization hash into human readable form
 */
static void simple_dump_opt_hash(dumper_t *dmp, pset *set, int index)
{
	if (pset_count(set) > 0) {
		const char *name = get_opt_name(index);

		fprintf(dmp->f, "\n%s:\n", name);
		fprintf(dmp->f, "%-16s %-8s\n", "Opcode", "deref");

		foreach_pset(set, opt_entry_t, entry) {
			fprintf(dmp->f, "%-16s %8u\n", entry->op_id, cnt_to_uint(&entry->count));
		}
	}
}

/**
 * dumps the register pressure for each block and for each register class
 */
static void simple_dump_be_block_reg_pressure(dumper_t *dmp, graph_entry_t *entry)
{
	/* return if no be statistic information available */
	be_block_entry_t *const b_first = pset_first(be_block_entry_t, entry->be_block_hash);
	if (!b_first)
		return;

	fprintf(dmp->f, "\nREG PRESSURE:\n");
	fprintf(dmp->f, "%12s", "Block Nr");

	/* print table head (register class names) */
	foreach_pset(b_first->reg_pressure, reg_pressure_entry_t, rp_entry)
		fprintf(dmp->f, "%15s", rp_entry->class_name);
	fprintf(dmp->f, "\n");

	/* print the reg pressure for all blocks and register classes */
	foreach_pset(entry->block_hash, be_block_entry_t, b_entry) {
		fprintf(dmp->f, "BLK   %6ld", b_entry->block_nr);

		foreach_pset(b_entry->reg_pressure, reg_pressure_entry_t, rp_entry)
			fprintf(dmp->f, "%15d", rp_entry->pressure);
		fprintf(dmp->f, "\n");
	}
}

/**
 * dumps the number of real_function_call optimization
 */
static void simple_dump_real_func_calls(dumper_t *dmp, counter_t *cnt)
{
	if (! dmp->f)
		return;

	if (! cnt_eq(cnt, 0)) {
		fprintf(dmp->f, "\nReal Function Calls optimized:\n");
		fprintf(dmp->f, "%-16s %8u\n", "Call", cnt_to_uint(cnt));
	}
}

/**
 * dumps the number of tail_recursion optimization
 */
static void simple_dump_tail_recursion(dumper_t *dmp, unsigned num_tail_recursion)
{
	if (! dmp->f)
		return;

	if (num_tail_recursion > 0) {
		fprintf(dmp->f, "\nTail recursion optimized:\n");
		fprintf(dmp->f, "%-16s %8u\n", "Call", num_tail_recursion);
	}
}

/**
 * dumps the edges count
 */
static void simple_dump_edges(dumper_t *dmp, counter_t *cnt)
{
	if (! dmp->f)
		return;

	fprintf(dmp->f, "%-16s %8u\n", "Edges", cnt_to_uint(cnt));
}

/**
 * dumps the IRG
 */
static void simple_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
	int dump_opts = 1;

	if (! dmp->f)
		return;

	if (entry->irg) {
		ir_graph *const_irg = get_const_code_irg();
		if (entry->irg == const_irg) {
			fprintf(dmp->f, "\nConst code Irg %p", (void *)entry->irg);
		} else {
			if (entry->ent)
				fprintf(dmp->f, "\nEntity %s, Irg %p", get_entity_ld_name(entry->ent), (void *)entry->irg);
			else
				fprintf(dmp->f, "\nIrg %p", (void *)entry->irg);
		}

		fprintf(dmp->f, " %swalked %u over blocks %u:\n"
			" was inlined               : %u\n"
			" got inlined               : %u\n"
			" strength red              : %u\n"
			" leaf function             : %s\n"
			" calls only leaf functions : %s\n"
			" recursive                 : %s\n"
			" chain call                : %s\n"
			" strict                    : %s\n"
			" calls                     : %u\n"
			" indirect calls            : %u\n"
			" external calls            : %u\n",
			entry->is_deleted ? "DELETED " : "",
			cnt_to_uint(&entry->cnt[gcnt_acc_walked]), cnt_to_uint(&entry->cnt[gcnt_acc_walked_blocks]),
			cnt_to_uint(&entry->cnt[gcnt_acc_was_inlined]),
			cnt_to_uint(&entry->cnt[gcnt_acc_got_inlined]),
			cnt_to_uint(&entry->cnt[gcnt_acc_strength_red]),
			entry->is_leaf ? "YES" : "NO",
			entry->is_leaf_call == LCS_NON_LEAF_CALL ? "NO" : (entry->is_leaf_call == LCS_LEAF_CALL ? "Yes" : "Maybe"),
			entry->is_recursive ? "YES" : "NO",
			entry->is_chain_call ? "YES" : "NO",
			entry->is_strict ? "YES" : "NO",
			cnt_to_uint(&entry->cnt[gcnt_all_calls]),
			cnt_to_uint(&entry->cnt[gcnt_indirect_calls]),
			cnt_to_uint(&entry->cnt[gcnt_external_calls])
		);
	} else {
		fprintf(dmp->f, "\nGlobals counts:\n");
		fprintf(dmp->f, "--------------\n");
		dump_opts = 0;
	}

	/* address ops */
	fprintf(dmp->f,
		" pure address calc ops     : %u\n"
		" all address calc ops      : %u\n",
		cnt_to_uint(&entry->cnt[gcnt_pure_adr_ops]),
		cnt_to_uint(&entry->cnt[gcnt_all_adr_ops])
	);

	/* Load/Store address classification */
	fprintf(dmp->f,
		" global Ld/St address      : %u\n"
		" local Ld/St address       : %u\n"
		" this Ld/St address        : %u\n"
		" param Ld/St address       : %u\n"
		" other Ld/St address       : %u\n",
		cnt_to_uint(&entry->cnt[gcnt_global_adr]),
		cnt_to_uint(&entry->cnt[gcnt_local_adr]),
		cnt_to_uint(&entry->cnt[gcnt_this_adr]),
		cnt_to_uint(&entry->cnt[gcnt_param_adr]),
		cnt_to_uint(&entry->cnt[gcnt_other_adr])
	);

	simple_dump_opcode_hash(dmp, entry->opcode_hash);
	simple_dump_edges(dmp, &entry->cnt[gcnt_edges]);

	/* effects of optimizations */
	if (dump_opts) {
		size_t i;

		simple_dump_real_func_calls(dmp, &entry->cnt[gcnt_acc_real_func_call]);
		simple_dump_tail_recursion(dmp, entry->num_tail_recursion);

		for (i = 0; i != ARRAY_SIZE(entry->opt_hash); ++i) {
			simple_dump_opt_hash(dmp, entry->opt_hash[i], i);
		}

		/* dump block info */
		fprintf(dmp->f, "\n%12s %12s %12s %12s %12s %12s %12s\n", "Block Nr", "Nodes", "intern E", "incoming E", "outgoing E", "Phi", "quot");
		foreach_pset(entry->block_hash, block_entry_t, b_entry) {
			fprintf(dmp->f, "BLK   %6ld %12u %12u %12u %12u %12u %4.8f %s\n",
				b_entry->block_nr,
				cnt_to_uint(&b_entry->cnt[bcnt_nodes]),
				cnt_to_uint(&b_entry->cnt[bcnt_edges]),
				cnt_to_uint(&b_entry->cnt[bcnt_in_edges]),
				cnt_to_uint(&b_entry->cnt[bcnt_out_edges]),
				cnt_to_uint(&b_entry->cnt[bcnt_phi_data]),
				cnt_to_dbl(&b_entry->cnt[bcnt_edges]) / cnt_to_dbl(&b_entry->cnt[bcnt_nodes]),
				b_entry->is_start ? "START" : (b_entry->is_end ? "END" : "")
			);
		}

		/* dump block reg pressure */
		simple_dump_be_block_reg_pressure(dmp, entry);
	}
}

/**
 * dumps the constant table
 */
static void simple_dump_const_tbl(dumper_t *dmp, const constant_info_t *tbl)
{
	size_t i;
	counter_t sum;

	if (! dmp->f)
		return;

	cnt_clr(&sum);

	fprintf(dmp->f, "\nConstant Information:\n");
	fprintf(dmp->f, "---------------------\n");

	fprintf(dmp->f, "\nBit usage for integer constants\n");
	fprintf(dmp->f, "-------------------------------\n");

	for (i = 0; i < ARRAY_SIZE(tbl->int_bits_count); ++i) {
		fprintf(dmp->f, "%5u %12u\n", (unsigned) (i + 1), cnt_to_uint(&tbl->int_bits_count[i]));
		cnt_add(&sum, &tbl->int_bits_count[i]);
	}
	fprintf(dmp->f, "-------------------------------\n");

	fprintf(dmp->f, "\nFloating point constants classification\n");
	fprintf(dmp->f, "--------------------------------------\n");
	for (i = 0; i < ARRAY_SIZE(tbl->floats); ++i) {
		fprintf(dmp->f, "%-10s %12u\n", stat_fc_name((float_classify_t)i), cnt_to_uint(&tbl->floats[i]));
		cnt_add(&sum, &tbl->floats[i]);
	}
	fprintf(dmp->f, "--------------------------------------\n");

	fprintf(dmp->f, "other %12u\n", cnt_to_uint(&tbl->others));
	cnt_add(&sum, &tbl->others);
	fprintf(dmp->f, "-------------------------------\n");

	fprintf(dmp->f, "sum   %12u\n", cnt_to_uint(&sum));
}

/**
 * Dumps a line of the parameter table
 */
static void dump_tbl_line(const distrib_entry_t *entry, void *env)
{
	dumper_t *dmp = (dumper_t*)env;

	fprintf(dmp->f, "%ld : %u\n", (long int)PTR_TO_INT(entry->object),
	        cnt_to_uint(&entry->cnt));
}

/**
 * dumps the parameter distribution table
 */
static void simple_dump_param_tbl(dumper_t *dmp, const distrib_tbl_t *tbl, graph_entry_t *global)
{
	fprintf(dmp->f, "\nCall parameter Information:\n");
	fprintf(dmp->f, "---------------------\n");

	stat_iterate_distrib_tbl(tbl, dump_tbl_line, dmp);
	fprintf(dmp->f, "-------------------------------\n");

	fprintf(dmp->f, "Number of Calls           %12u\n", cnt_to_uint(&global->cnt[gcnt_all_calls]));
	fprintf(dmp->f, "indirect calls            %12u\n", cnt_to_uint(&global->cnt[gcnt_indirect_calls]));
	fprintf(dmp->f, "external calls            %12u\n", cnt_to_uint(&global->cnt[gcnt_external_calls]));
	fprintf(dmp->f, "with const params         %12u\n", cnt_to_uint(&global->cnt[gcnt_call_with_cnst_arg]));
	fprintf(dmp->f, "with all const params     %12u\n", cnt_to_uint(&global->cnt[gcnt_call_with_all_cnst_arg]));
	fprintf(dmp->f, "with local var adr params %12u\n", cnt_to_uint(&global->cnt[gcnt_call_with_local_adr]));
}

/**
 * dumps the optimization counter table
 */
static void simple_dump_opt_cnt(dumper_t *dmp, const counter_t *tbl, unsigned len)
{
	unsigned i;

	fprintf(dmp->f, "\nOptimization counts:\n");
	fprintf(dmp->f, "---------------------\n");

	for (i = 0; i < len; ++i) {
		unsigned cnt = cnt_to_uint(&tbl[i]);

		if (cnt > 0) {
			fprintf(dmp->f, "%8u %s\n", cnt, get_opt_name(i));
		}
	}
}

/**
 * initialize the simple dumper
 */
static void simple_init(dumper_t *dmp, const char *name)
{
	char fname[2048];

	snprintf(fname, sizeof(fname), "%s.txt", name);
	dmp->f = fopen(fname, "w");
	if (! dmp->f) {
		perror(fname);
	}
}

/**
 * finishes the simple dumper
 */
static void simple_finish(dumper_t *dmp)
{
	if (dmp->f)
		fclose(dmp->f);
	dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
const dumper_t simple_dumper = {
	simple_dump_graph,
	simple_dump_const_tbl,
	simple_dump_param_tbl,
	simple_dump_opt_cnt,
	simple_init,
	simple_finish,
	NULL,
	NULL,
	NULL,
	FOURCC('S', 'M', 'P', 'L'),
};

/* ---------------------------------------------------------------------- */

/**
 * count the nodes as needed:
 *
 * 1 normal (data) Phi's
 * 2 memory Phi's
 * 3 Proj
 * 0 all other nodes
 */
static void csv_count_nodes(dumper_t *dmp, graph_entry_t *graph, counter_t cnt[])
{
	int i;

	for (i = 0; i < 4; ++i)
		cnt_clr(&cnt[i]);

	foreach_pset(graph->opcode_hash, node_entry_t, entry) {
		op_id_t const op_id = entry->op_id;
		if (op_id == get_op_name(op_Phi)) {
			/* normal Phi */
			cnt_add(&cnt[1], &entry->cnt_alive);
		} else if (op_id == dmp->status->op_PhiM) {
			/* memory Phi */
			cnt_add(&cnt[2], &entry->cnt_alive);
		} else if (op_id == get_op_name(op_Proj)) {
			/* Proj */
			cnt_add(&cnt[3], &entry->cnt_alive);
		} else {
			/* all other nodes */
			cnt_add(&cnt[0], &entry->cnt_alive);
		}
	}
}

/**
 * dumps the IRG
 */
static void csv_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
	const char *name;
	counter_t cnt[4];

	if (! dmp->f)
		return;

	if (entry->irg && !entry->is_deleted) {
		ir_graph *const_irg = get_const_code_irg();

		if (entry->irg == const_irg) {
			return;
		} else {
			if (entry->ent)
				name = get_entity_name(entry->ent);
			else
				name = "<UNKNOWN IRG>";
		}

		csv_count_nodes(dmp, entry, cnt);

		fprintf(dmp->f, "%-40s, %p, %u, %u, %u, %u\n",
			name,
			(void *)entry->irg,
			cnt_to_uint(&cnt[0]),
			cnt_to_uint(&cnt[1]),
			cnt_to_uint(&cnt[2]),
			cnt_to_uint(&cnt[3])
		);
	}
}

/**
 * dumps the IRG
 */
static void csv_dump_const_tbl(dumper_t *dmp, const constant_info_t *tbl)
{
	(void) dmp;
	(void) tbl;
	/* FIXME: NYI */
}

/**
 * dumps the parameter distribution table
 */
static void csv_dump_param_tbl(dumper_t *dmp, const distrib_tbl_t *tbl, graph_entry_t *global)
{
	(void) dmp;
	(void) tbl;
	(void) global;
	/* FIXME: NYI */
}

/**
 * dumps the optimization counter
 */
static void csv_dump_opt_cnt(dumper_t *dmp, const counter_t *tbl, unsigned len)
{
	(void) dmp;
	(void) tbl;
	(void) len;
	/* FIXME: NYI */
}

/**
 * initialize the simple dumper
 */
static void csv_init(dumper_t *dmp, const char *name)
{
	char fname[2048];

	snprintf(fname, sizeof(fname), "%s.csv", name);
	dmp->f = fopen(fname, "a");
	if (! dmp->f)
		perror(fname);
}

/**
 * finishes the simple dumper
 */
static void csv_finish(dumper_t *dmp)
{
	if (dmp->f)
		fclose(dmp->f);
	dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
const dumper_t csv_dumper = {
	csv_dump_graph,
	csv_dump_const_tbl,
	csv_dump_param_tbl,
	csv_dump_opt_cnt,
	csv_init,
	csv_finish,
	NULL,
	NULL,
	NULL,
	FOURCC('C', 'S', 'V', '\0')
};
