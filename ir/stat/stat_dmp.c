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

/**
 * names of the optimizations
 */
static const char *opt_names[] = {
  "straightening optimization",
  "if simplification",
  "constant evaluation",
  "algebraic simplification",
  "Phi optmization",
  "Write-After-Write optimization",
  "Write-After-Read optimization",
  "Read-After-Write optimization",
  "Read-After-Read optimization",
  "Read-a-Const optimization",
  "Tuple optimization",
  "ID optimization",
  "Common subexpression elimination",
  "Strength reduction",
  "Architecture dependant optimization",
  "Reassociation optimization",
  "Polymorphic call optimization",
  "an if conversion was tried",
  "Lowered",
};

static const char *if_conv_names[] = {
  "if conv done              :",
  "if conv side effect       :",
  "if conv Phi node found    :",
  "if conv to deep DAG's     :",
  "if conv bad control flow  :",
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
    fprintf(dmp->f, "%-16s %8d %8d %8d\n",
      get_id_str(entry->op->name), entry->cnt_alive.cnt[0], entry->new_node.cnt[0], entry->into_Id.cnt[0]);

    cnt_add(&f_alive,    &entry->cnt_alive);
    cnt_add(&f_new_node, &entry->new_node);
    cnt_add(&f_Id,       &entry->into_Id);
  }
  fprintf(dmp->f, "-------------------------------------------\n");
  fprintf(dmp->f, "%-16s %8d %8d %8d\n", "Sum",
     f_alive.cnt[0],
     f_new_node.cnt[0],
     f_Id.cnt[0]);
}

/**
 * dumps a optimization hash into human readable form
 */
static void simple_dump_opt_hash(dumper_t *dmp, pset *set, int index)
{
  opt_entry_t *entry = pset_first(set);

  if (entry) {
    fprintf(dmp->f, "\n%s:\n", opt_names[index]);
    fprintf(dmp->f, "%-16s %-8s\n", "Opcode", "deref");

    for (; entry; entry = pset_next(set)) {
      fprintf(dmp->f, "%-16s %8d\n",
        get_id_str(entry->op->name), entry->count.cnt[0]);
    }
  }
}

/**
 * dumps the edges count
 */
static void simple_dump_edges(dumper_t *dmp, counter_t *cnt)
{
  fprintf(dmp->f, "%-16s %8d\n", "Edges", cnt->cnt[0]);
}

/**
 * dumps the IRG
 */
static void simple_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
  int i, dump_opts = 1;
  block_entry_t *b_entry;

  if (entry->irg) {
    ir_graph *const_irg = get_const_code_irg();

    if (entry->irg == const_irg) {
      fprintf(dmp->f, "\nConst code Irg %p", (void *)entry->irg);
    }
    else {
      if (entry->ent)
        fprintf(dmp->f, "\nEntity %s, Irg %p", get_entity_name(entry->ent), (void *)entry->irg);
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

    for (i = 0; i < sizeof(entry->opt_hash)/sizeof(entry->opt_hash[0]); ++i) {
      simple_dump_opt_hash(dmp, entry->opt_hash[i], i);
    }

    /* dump block info */
    fprintf(dmp->f, "\n%12s %12s %12s %12s %12s %12s\n", "Block Nr", "Nodes", "intern E", "incoming E", "outgoing E", "quot");
    for (b_entry = pset_first(entry->block_hash);
	 b_entry;
	 b_entry = pset_next(entry->block_hash)) {
      fprintf(dmp->f, "BLK %12ld %12u %12u %12u %12u %4.8f\n",
	  b_entry->block_nr,
	  b_entry->cnt_nodes.cnt[0],
	  b_entry->cnt_edges.cnt[0],
	  b_entry->cnt_in_edges.cnt[0],
	  b_entry->cnt_out_edges.cnt[0],
	  (double)b_entry->cnt_edges.cnt[0] / (double)b_entry->cnt_nodes.cnt[0]
      );
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
}

/**
 * finishes the simple dumper
 */
static void simple_finish(dumper_t *dmp)
{
  fclose(dmp->f);
  dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
const dumper_t simple_dumper = {
  simple_dump_graph,
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
 * initialize the simple dumper
 */
static void csv_init(dumper_t *dmp, const char *name)
{
  char fname[2048];

  snprintf(fname, sizeof(fname), "%s.csv", name);
  dmp->f = fopen(fname, "a");
}

/**
 * finishes the simple dumper
 */
static void csv_finish(dumper_t *dmp)
{
  fclose(dmp->f);
  dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
const dumper_t csv_dumper = {
  csv_dump_graph,
  csv_init,
  csv_finish,
  NULL,
  NULL,
  NULL,
};
