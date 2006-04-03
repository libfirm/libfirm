/*
 * Project:     libFIRM
 * File name:   ir/ir/stat_dmp.c
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "stat_dmp.h"
#include "irhooks.h"

/**
 * names of the optimizations
 */
static const struct {
  hook_opt_kind kind;
  const char    *name;
} opt_names[] = {
  { HOOK_OPT_DEAD_BLOCK,   "dead block elimination" },
  { HOOK_OPT_STG,          "straightening optimization" },
  { HOOK_OPT_IFSIM,        "if simplification" },
  { HOOK_OPT_CONST_EVAL,   "constant evaluation" },
  { HOOK_OPT_ALGSIM,       "algebraic simplification" },
  { HOOK_OPT_PHI,          "Phi optmization" },
  { HOOK_OPT_SYNC,         "Sync optmization" },
  { HOOK_OPT_WAW,          "Write-After-Write optimization" },
  { HOOK_OPT_WAR,          "Write-After-Read optimization" },
  { HOOK_OPT_RAW,          "Read-After-Write optimization" },
  { HOOK_OPT_RAR,          "Read-After-Read optimization" },
  { HOOK_OPT_RC,           "Read-a-Const optimization" },
  { HOOK_OPT_TUPLE,        "Tuple optimization" },
  { HOOK_OPT_ID,           "ID optimization" },
  { HOOK_OPT_CSE,          "Common subexpression elimination" },
  { HOOK_OPT_STRENGTH_RED, "Strength reduction" },
  { HOOK_OPT_ARCH_DEP,     "Architecture dependant optimization" },
  { HOOK_OPT_REASSOC,      "Reassociation optimization" },
  { HOOK_OPT_POLY_CALL,    "Polymorphic call optimization" },
  { HOOK_OPT_IF_CONV,      "an if conversion was tried" },
  { HOOK_OPT_FUNC_CALL,    "Real function call optimization" },
  { HOOK_OPT_CONFIRM,      "Confirm-based optimization: replacement" },
  { HOOK_OPT_CONFIRM_C,    "Confirm-based optimization: replaced by const" },
  { HOOK_OPT_CONFIRM_E,    "Confirm-based optimization: evaluated" },
  { HOOK_OPT_EXC_REM,      "a exception edge was removed due to a Confirmation prove" },
  { HOOK_LOWERED,          "Lowered" },
  { HOOK_BACKEND,          "Backend transformation" },
  { FS_OPT_NEUTRAL_0,      "algebraic simplification: a op 0 = 0 op a = a" },
  { FS_OPT_NEUTRAL_1,      "algebraic simplification: a op 1 = 1 op a = a" },
  { FS_OPT_ADD_A_A,        "algebraic simplification: a + a = a * 2" },
  { FS_OPT_ADD_A_MINUS_B,  "algebraic simplification: a + -b = a - b" },
  { FS_OPT_ADD_SUB,        "algebraic simplification: (a + x) - x = (a - x) + x = a" },
  { FS_OPT_ADD_MUL_A_X_A,  "algebraic simplification: a * x + a = a * (x + 1)" },
  { FS_OPT_SUB_0_A,        "algebraic simplification: 0 - a = -a" },
  { FS_OPT_SUB_MUL_A_X_A,  "algebraic simplification: a * x - a = a * (x - 1)" },
  { FS_OPT_MUL_MINUS_1,    "algebraic simplification: a * -1 = -a" },
  { FS_OPT_OR,             "algebraic simplification: a | a = a | 0 = 0 | a = a" },
  { FS_OPT_AND,            "algebraic simplification: a & 0b1...1 = 0b1...1 & a =  a & a = a" },
  { FS_OPT_EOR_A_A,        "algebraic simplification: a ^ a = 0" },
  { FS_OPT_EOR_TO_NOT_BOOL,"algebraic simplification: bool ^ 1 = !bool" },
  { FS_OPT_EOR_TO_NOT,     "algebraic simplification: x ^ 0b1..1 = ~x" },
  { FS_OPT_NOT_CMP,        "algebraic simplification: !(a cmp b) = a !cmp b" },
  { FS_OPT_OR_SHFT_TO_ROT, "algebraic simplification: (x << c) | (x >> (bits - c)) == Rot(x, c)" },
  { FS_OPT_REASSOC_SHIFT,  "algebraic simplification: (x SHF c1) SHF c2 = x SHF (c1+c2)" },
  { FS_OPT_CONV,           "algebraic simplification: Conv could be removed" },
  { FS_OPT_CAST,           "algebraic simplification: a Cast could be removed" },
  { FS_OPT_MIN_MAX_EQ,     "algebraic simplification: Min(a,a) = Max(a,a) = a" },
  { FS_OPT_MUX_C,          "algebraic simplification: Mux(C, f, t) = C ? t : f" },
  { FS_OPT_MUX_EQ,         "algebraic simplification: Mux(v, x, x) = x" },
  { FS_OPT_MUX_TRANSFORM,  "algebraic simplification: Mux(a, b, c) = b OR Mux(a,b, c) = c" },
  { FS_OPT_MUX_TO_MIN,     "algebraic simplification: Mux(a < b, a, b) = Min(a,b)" },
  { FS_OPT_MUX_TO_MAX,     "algebraic simplification: Mux(a > b, a, b) = Max(a,b)" },
  { FS_OPT_MUX_TO_ABS,     "algebraic simplification: Mux(a > b, a, b) = Abs(a,b)" },
  { FS_OPT_MUX_TO_SHR,     "algebraic simplification: Mux(a > b, a, b) = a >> b" },
  { FS_BE_IA32_LEA,        "Backend transformation: Lea was created" },
};

static const char *if_conv_names[IF_RESULT_LAST] = {
  "if conv done             ",
  "if conv side effect      ",
  "if conv Phi node found   ",
  "if conv to deep DAG's    ",
  "if conv bad control flow ",
  "if conv denied by arch   ",
};

/**
 * dumps a opcode hash into human readable form
 */
static void simple_dump_opcode_hash(dumper_t *dmp, pset *set)
{
  node_entry_t *entry;
  counter_t f_alive;
  counter_t f_new_node;
  counter_t f_Id;

  cnt_clr(&f_alive);
  cnt_clr(&f_new_node);
  cnt_clr(&f_Id);

  fprintf(dmp->f, "%-16s %-8s %-8s %-8s\n", "Opcode", "alive", "created", "->Id");
  for (entry = pset_first(set); entry; entry = pset_next(set)) {
    fprintf(dmp->f, "%-16s %8u %8u %8u\n",
      get_id_str(entry->op->name), entry->cnt_alive.cnt[0], entry->new_node.cnt[0], entry->into_Id.cnt[0]);

    cnt_add(&f_alive,    &entry->cnt_alive);
    cnt_add(&f_new_node, &entry->new_node);
    cnt_add(&f_Id,       &entry->into_Id);
  }
  fprintf(dmp->f, "-------------------------------------------\n");
  fprintf(dmp->f, "%-16s %8u %8u %8u\n", "Sum",
     f_alive.cnt[0],
     f_new_node.cnt[0],
     f_Id.cnt[0]);
}

/**
 * dumps an optimization hash into human readable form
 */
static void simple_dump_opt_hash(dumper_t *dmp, pset *set, int index)
{
  opt_entry_t *entry = pset_first(set);

  assert(index < ARR_SIZE(opt_names) && "index out of range");
  assert(opt_names[index].kind == index && "opt_names broken");
  if (entry) {
    fprintf(dmp->f, "\n%s:\n", opt_names[index].name);
    fprintf(dmp->f, "%-16s %-8s\n", "Opcode", "deref");

    for (; entry; entry = pset_next(set)) {
      fprintf(dmp->f, "%-16s %8u\n",
        get_id_str(entry->op->name), entry->count.cnt[0]);
    }
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
    fprintf(dmp->f, "%-16s %8u\n",
      "Call", cnt->cnt[0]);
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

  fprintf(dmp->f, "%-16s %8d\n", "Edges", cnt->cnt[0]);
}

/**
 * dumps the IRG
 */
static void simple_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
  int i, dump_opts = 1;
  block_entry_t *b_entry;
  extbb_entry_t *eb_entry;

  if (! dmp->f)
    return;

  if (entry->irg) {
    ir_graph *const_irg = get_const_code_irg();

    if (entry->irg == const_irg) {
      fprintf(dmp->f, "\nConst code Irg %p", (void *)entry->irg);
    }
    else {
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
        " calls                     : %u\n"
        " indirect calls            : %u\n",
        entry->is_deleted ? "DELETED " : "",
        entry->cnt_walked.cnt[0], entry->cnt_walked_blocks.cnt[0],
        entry->cnt_was_inlined.cnt[0],
        entry->cnt_got_inlined.cnt[0],
	      entry->cnt_strength_red.cnt[0],
	      entry->is_leaf ? "YES" : "NO",
	      entry->is_leaf_call == LCS_NON_LEAF_CALL ? "NO" : (entry->is_leaf_call == LCS_LEAF_CALL ? "Yes" : "Maybe"),
	      entry->is_recursive ? "YES" : "NO",
	      entry->is_chain_call ? "YES" : "NO",
        entry->cnt_all_calls.cnt[0],
        entry->cnt_indirect_calls.cnt[0]
    );

    for (i = 0; i < sizeof(entry->cnt_if_conv)/sizeof(entry->cnt_if_conv[0]); ++i) {
      fprintf(dmp->f, " %s : %u\n", if_conv_names[i], entry->cnt_if_conv[i].cnt[0]);
    }

  }
  else {
    fprintf(dmp->f, "\nGlobals counts:\n");
    fprintf(dmp->f, "--------------\n");
    dump_opts = 0;
  }

  simple_dump_opcode_hash(dmp, entry->opcode_hash);
  simple_dump_edges(dmp, &entry->cnt_edges);

  /* effects of optimizations */
  if (dump_opts) {
    int i;

    simple_dump_real_func_calls(dmp, &entry->cnt_real_func_call);
    simple_dump_tail_recursion(dmp, entry->num_tail_recursion);

    for (i = 0; i < sizeof(entry->opt_hash)/sizeof(entry->opt_hash[0]); ++i) {
      simple_dump_opt_hash(dmp, entry->opt_hash[i], i);
    }

    /* dump block info */
    fprintf(dmp->f, "\n%12s %12s %12s %12s %12s %12s %12s\n", "Block Nr", "Nodes", "intern E", "incoming E", "outgoing E", "Phi", "quot");
    for (b_entry = pset_first(entry->block_hash);
	       b_entry;
	       b_entry = pset_next(entry->block_hash)) {
      fprintf(dmp->f, "BLK   %6ld %12u %12u %12u %12u %12u %4.8f\n",
	      b_entry->block_nr,
	      b_entry->cnt_nodes.cnt[0],
	      b_entry->cnt_edges.cnt[0],
	      b_entry->cnt_in_edges.cnt[0],
	      b_entry->cnt_out_edges.cnt[0],
        b_entry->cnt_phi_data.cnt[0],
	      (double)b_entry->cnt_edges.cnt[0] / (double)b_entry->cnt_nodes.cnt[0]
      );
    }

    if (dmp->status->stat_options & FIRMSTAT_COUNT_EXTBB) {
      /* dump extended block info */
      fprintf(dmp->f, "\n%12s %12s %12s %12s %12s %12s %12s\n", "Extbb Nr", "Nodes", "intern E", "incoming E", "outgoing E", "Phi", "quot");
      for (eb_entry = pset_first(entry->extbb_hash);
           eb_entry;
           eb_entry = pset_next(entry->extbb_hash)) {
        fprintf(dmp->f, "ExtBB %6ld %12u %12u %12u %12u %12u %4.8f\n",
          eb_entry->block_nr,
          eb_entry->cnt_nodes.cnt[0],
          eb_entry->cnt_edges.cnt[0],
          eb_entry->cnt_in_edges.cnt[0],
          eb_entry->cnt_out_edges.cnt[0],
          eb_entry->cnt_phi_data.cnt[0],
          (double)eb_entry->cnt_edges.cnt[0] / (double)eb_entry->cnt_nodes.cnt[0]
        );
      }
    }
  }
}

/**
 * dumps the IRG
 */
static void simple_dump_const_tbl(dumper_t *dmp, const constant_info_t *tbl)
{
  int i;
  counter_t sum;

  if (! dmp->f)
    return;

  cnt_clr(&sum);

  fprintf(dmp->f, "\nConstant Information:\n");
  fprintf(dmp->f, "---------------------\n");

  fprintf(dmp->f, "\nBit usage for integer constants\n");
  fprintf(dmp->f, "-------------------------------\n");

  for (i = 0; i < ARR_SIZE(tbl->int_bits_count); ++i) {
    fprintf(dmp->f, "%5d %12u\n", i + 1, tbl->int_bits_count[i].cnt[0]);
    cnt_add(&sum, &tbl->int_bits_count[i]);
  }
  fprintf(dmp->f, "-------------------------------\n");

  fprintf(dmp->f, "\nFloating point constants classification\n");
  fprintf(dmp->f, "--------------------------------------\n");
  for (i = 0; i < ARR_SIZE(tbl->floats); ++i) {
    fprintf(dmp->f, "%-10s %12u\n", stat_fc_name(i), tbl->floats[i].cnt[0]);
    cnt_add(&sum, &tbl->floats[i]);
  }
  fprintf(dmp->f, "--------------------------------------\n");

  fprintf(dmp->f, "other %12u\n", tbl->others.cnt[0]);
  cnt_add(&sum, &tbl->others);
  fprintf(dmp->f, "-------------------------------\n");

  fprintf(dmp->f, "sum   %12u\n", sum.cnt[0]);
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
  simple_init,
  simple_finish,
  NULL,
  NULL,
  NULL,
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
  node_entry_t *entry;
  int i;

  for (i = 0; i < 4; ++i)
    cnt_clr(&cnt[i]);

  for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
    if (entry->op == op_Phi) {
      /* normal Phi */
      cnt_add(&cnt[1], &entry->cnt_alive);
    }
    else if (entry->op == dmp->status->op_PhiM) {
      /* memory Phi */
      cnt_add(&cnt[2], &entry->cnt_alive);
    }
    else if (entry->op == op_Proj) {
      /* Proj */
      cnt_add(&cnt[3], &entry->cnt_alive);
    }
    else {
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
      name = "<Const code Irg>";
      return;
    }
    else {
      if (entry->ent)
        name = get_entity_name(entry->ent);
      else
        name = "<UNKNOWN IRG>";
    }

    csv_count_nodes(dmp, entry, cnt);

    fprintf(dmp->f, "%-40s, %p, %d, %d, %d, %d\n",
        name,
        (void *)entry->irg,
        cnt[0].cnt[0],
        cnt[1].cnt[0],
        cnt[2].cnt[0],
        cnt[3].cnt[0]
    );
  }
}

/**
 * dumps the IRG
 */
static void csv_dump_const_tbl(dumper_t *dmp, const constant_info_t *tbl)
{
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
  if (! dmp->f) {
    perror(fname);
  }
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
  csv_init,
  csv_finish,
  NULL,
  NULL,
  NULL,
};
