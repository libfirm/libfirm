/*
 * Project:     libFIRM
 * File name:   ir/ir/pattern_dmp.c
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

#include <stdio.h>

#include "ident.h"
#include "irop_t.h"
#include "irmode.h"
#include "pattern_dmp.h"

FILE *f;

/**
 * starts a new VCG graph
 */
static void vcg_dump_start(pattern_dumper_t *self)
{
  f = fopen("firmpattern.vcg", "w");

  fprintf(f,
    "graph: { title: \"Most found pattern\"\n"
    "  display_edge_labels: no\n"
    "  layoutalgorithm: mindepth\n"
    "  manhattan_edges: yes\n"
    "  port_sharing: no\n"
    "  orientation: bottom_to_top\n"
  );
}

/**
 * ends a new VCG graph
 */
static void vcg_dump_end(pattern_dumper_t *self)
{
  fprintf(f, "}\n");
  fclose(f);
}

/*
 * starts a new pattern
 */
static void vcg_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
  fprintf(f,
    "  graph: { title: \"pattern cnt %u\"\n", cnt->cnt[0]
  );

}

/**
 * Finishes current pattern
 */
static void vcg_dump_finish_pattern(pattern_dumper_t *self)
{
  fprintf(f, "  }\n");
}


static void vcg_dump_node(pattern_dumper_t *self, unsigned id,
    unsigned op_code, unsigned mode_code)
{
  ir_op *op     = (ir_op *)op_code;
  ir_mode *mode = (ir_mode *)mode_code;

  fprintf(f, "    node: {title: \"n%u\" label: \"%s%s n%u\" }\n",
    id, get_id_str(op->name), mode ? get_mode_name(mode) : "", id);
}

static void vcg_dump_edge(pattern_dumper_t *self, unsigned id, unsigned parent, unsigned position)
{
  fprintf(f, "    edge: { sourcename: \"n%u\" targetname: \"n%u\"}\n", parent, id);
}

/**
 * The VCG dumper.
 */
pattern_dumper_t vcg_dump = {
  vcg_dump_new_pattern,
  vcg_dump_finish_pattern,
  vcg_dump_node,
  NULL,
  vcg_dump_edge,
  NULL,
  NULL,
};

/**
 * starts a new pattern
 */
static void stdout_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
  printf("%8u ", cnt->cnt[0]);
}


/*
 * Finishes current pattern
 */
static void stdout_dump_finish_pattern(pattern_dumper_t *self)
{
  printf("\n");
}

/**
 * Dumps a node
 */
static void stdout_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code)
{
  ir_op *op     = (ir_op *)op_code;
  ir_mode *mode = (ir_mode *)mode_code;

  /* if (env->options & OPT_ENC_GRAPH) */
    printf("%u:", id);

  printf("%s", get_id_str(op->name));

  if (mode)
    printf("%s", get_mode_name(mode));
}

/**
 * Dump a ref
 */
static void stdout_dump_ref(pattern_dumper_t *self, unsigned id)
{
  printf("REF:%u", id);
}

/**
 * Dump an edge
 */
static void stdout_dump_edge(pattern_dumper_t *self, unsigned id, unsigned parent, unsigned position)
{
  if (position > 0)
    printf(", ");
}

/**
 * Start children dumper
 */
static void stdout_start_children(pattern_dumper_t *self, unsigned id)
{
  printf("(");
}

/**
 * finishes childred  dumper
 */
static void stdout_finish_children(pattern_dumper_t *self, unsigned id)
{
  printf(")");
}

/**
 * The stdout dumper.
 */
pattern_dumper_t stdout_dump = {
  stdout_dump_new_pattern,
  stdout_dump_finish_pattern,
  stdout_dump_node,
  stdout_dump_ref,
  stdout_dump_edge,
  stdout_start_children,
  stdout_finish_children,
};

/* ------------------------------------ API ------------------------------------- */

/*
 * starts a new pattern
 */
void pattern_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
  if (self->dump_new_pattern)
    self->dump_new_pattern(self, cnt);
}


/*
 * Finishes current pattern
 */
void pattern_dump_finish_pattern(pattern_dumper_t *self)
{
  if (self->dump_finish_pattern)
    self->dump_finish_pattern(self);
}


/*
 * Dumps a node
 */
void pattern_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code)
{
  if (self->dump_node)
    self->dump_node(self, id, op_code, mode_code);
}

/*
 * Dump a ref
 */
void pattern_dump_ref(pattern_dumper_t *self, unsigned id)
{
  if (self->dump_ref)
    self->dump_ref(self, id);
}

/*
 * Dump an edge
 */
void pattern_dump_edge(pattern_dumper_t *self, unsigned id, unsigned parent, unsigned position)
{
  if (self->dump_edge)
    self->dump_edge(self, id, parent, position);
}

/*
 * Start children dumper
 */
void pattern_start_children(pattern_dumper_t *self, unsigned id)
{
  if (self->dump_start_children)
    self->dump_start_children(self, id);
}

/*
 * finishes childred dumper
 */
void pattern_finish_children(pattern_dumper_t *self, unsigned id)
{
  if (self->dump_finish_children)
    self->dump_finish_children(self, id);
}
