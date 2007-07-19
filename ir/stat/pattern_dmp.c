/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Statistics for Firm. Dumping patterns.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef FIRM_STATISTICS

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "ident.h"
#include "irop_t.h"
#include "irmode.h"
#include "firmstat_t.h"
#include "pattern_dmp.h"

/* dumper operations */
typedef void (*DUMP_NEW_PATTERN_FUNC)(pattern_dumper_t *self, counter_t *cnt);
typedef void (*DUMP_FINISH_PATTERN_FUNC)(pattern_dumper_t *self);
typedef void (*DUMP_NODE_FUNC)(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr);
typedef void (*DUMP_REF_FUNC)(pattern_dumper_t *self, unsigned id);
typedef void (*DUMP_EDGE_FUNC)(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code);
typedef void (*DUMP_START_CHILDREN_FUNC)(pattern_dumper_t *self, unsigned id);
typedef void (*DUMP_FINISH_CHILDREN_FUNC)(pattern_dumper_t *self, unsigned id);
typedef void (*DUMP_START_FUNC)(pattern_dumper_t *self);
typedef void (*DUMP_END_FUNC)(pattern_dumper_t *self);

/**
 * the pattern dumper
 */
struct _pattern_dumper_t {
	DUMP_NEW_PATTERN_FUNC      dump_new_pattern;
	DUMP_FINISH_PATTERN_FUNC   dump_finish_pattern;
	DUMP_NODE_FUNC             dump_node;
	DUMP_REF_FUNC              dump_ref;
	DUMP_EDGE_FUNC             dump_edge;
	DUMP_START_CHILDREN_FUNC   dump_start_children;
	DUMP_FINISH_CHILDREN_FUNC  dump_finish_children;
	DUMP_START_FUNC            dump_start;
	DUMP_END_FUNC              dump_end;
	void                       *data;
};

/**
 * VCG private data
 */
typedef struct _vcg_private_t {
	FILE     *f;          /**< file to dump to */
	unsigned pattern_id;  /**< ID of the pattern */
	unsigned max_pattern; /**< maximum number of pattern to be dumped */
} vcg_private_t;

/**
 * Starts a new VCG graph.
 */
static void vcg_dump_start(pattern_dumper_t *self)
{
	vcg_private_t *priv = self->data;

	fprintf(priv->f,
		"graph: { title: \"Most found pattern\"\n"
		"  display_edge_labels: no\n"
		"  layoutalgorithm: mindepth\n"
		"  manhattan_edges: yes\n"
		"  port_sharing: no\n"
		"  orientation: bottom_to_top\n"
		);
}  /* vcg_dump_start */

/**
 * Ends a new VCG graph.
 */
static void vcg_dump_end(pattern_dumper_t *self)
{
	vcg_private_t *priv = self->data;

	fprintf(priv->f, "}\n");
	fclose(priv->f);
}  /* vcg_dump_end */

/**
 * Starts a new pattern.
 */
static void vcg_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
	vcg_private_t *priv = self->data;
	static unsigned nr = 0;

	if (priv->pattern_id > priv->max_pattern)
		return;

	fprintf(priv->f,
		"  graph: { title: \"g%u\" label: \"pattern %u\" status:clustered color:yellow\n",
		priv->pattern_id, priv->pattern_id );

	/* add a pseudo node for the count of this pattern */
	fprintf(priv->f,
		"    node: {title: \"c%u\" label: \"cnt: %u\" color:red }\n",
		++nr, cnt_to_uint(cnt)
	);
}  /* vcg_dump_new_pattern */

/**
 * Finish the current pattern.
 */
static void vcg_dump_finish_pattern(pattern_dumper_t *self)
{
	vcg_private_t *priv = self->data;

	if (priv->pattern_id > priv->max_pattern)
		return;

	fprintf(priv->f, "  }\n");

	if (priv->pattern_id > 0)
		fprintf(priv->f, "  edge: { sourcename: \"g%u\" targetname: \"g%u\" linestyle:invisible}\n",
			priv->pattern_id,
			priv->pattern_id - 1);

	++priv->pattern_id;
}  /* vcg_dump_finish_pattern */

/**
 * Dumps a node.
 */
static void vcg_dump_node(pattern_dumper_t *self, unsigned id,
    unsigned op_code, unsigned mode_code, void *attr)
{
	vcg_private_t *priv = self->data;
	ir_op *op           = stat_get_op_from_opcode(op_code);
	ir_mode *mode       = (ir_mode *)mode_code;
	long l              = attr ? *(long *)attr : 0;

	if (priv->pattern_id > priv->max_pattern)
		return;

	if (attr) {
		fprintf(priv->f, "    node: {title: \"n%u_%u\" label: \"%s%s %ld n%u\" }\n",
			priv->pattern_id, id, get_id_str(op->name), mode ? get_mode_name(mode) : "", l, id);
	} else {
		fprintf(priv->f, "    node: {title: \"n%u_%u\" label: \"%s%s n%u\" }\n",
			priv->pattern_id, id, get_id_str(op->name), mode ? get_mode_name(mode) : "", id);
	}  /* if */
}  /* vcg_dump_node */

/**
 * Dumps an edge.
 */
static void vcg_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	vcg_private_t *priv = self->data;
	(void) mode_code;

	if (priv->pattern_id > priv->max_pattern)
		return;

	fprintf(priv->f, "    edge: { sourcename: \"n%u_%u\" targetname: \"n%u_%u\" label: \"%u\" }\n",
		priv->pattern_id, src,
		priv->pattern_id, tgt,
		pos
	);
}  /* vcg_dump_edge */

/**
 * The VCG dumper.
 */
static pattern_dumper_t vcg_dump = {
	vcg_dump_new_pattern,
	vcg_dump_finish_pattern,
	vcg_dump_node,
	NULL,
	vcg_dump_edge,
	NULL,
	NULL,
	vcg_dump_start,
	vcg_dump_end,
	NULL
};

/**
 * Starts a new pattern.
 */
static void stdout_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
	FILE *f = self->data;

	fprintf(f, "%8u ", cnt_to_uint(cnt));
}  /* stdout_dump_new_pattern */


/**
 * Finish the current pattern.
 */
static void stdout_dump_finish_pattern(pattern_dumper_t *self)
{
	FILE *f = self->data;

	fprintf(f, "\n");
}  /* stdout_dump_finish_pattern */

/**
 * Dumps a node.
 */
static void stdout_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr)
{
	FILE *f       = self->data;
	ir_op *op     = stat_get_op_from_opcode(op_code);
	ir_mode *mode = (ir_mode *)mode_code;
	(void) attr;

	/* if (env->options & OPT_ENC_GRAPH) */
	fprintf(f, "%u:", id);

	fprintf(f, "%s", get_id_str(op->name));

	if (mode)
		fprintf(f, "%s", get_mode_name(mode));
}  /* stdout_dump_node */

/**
 * Dump a ref
 */
static void stdout_dump_ref(pattern_dumper_t *self, unsigned id)
{
	FILE *f = self->data;

	fprintf(f, "REF:%u", id);
}  /* stdout_dump_ref */

/**
 * Dump an edge.
 */
static void stdout_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	FILE *f = self->data;
	(void) tgt;
	(void) src;
	(void) pos;
	(void) mode_code;

	if (pos > 0)
		fprintf(f, ", ");
}  /* stdout_dump_edge */

/**
 * Start the children dumper.
 */
static void stdout_start_children(pattern_dumper_t *self, unsigned id)
{
	FILE *f = self->data;
	(void) id;

	fprintf(f, "(");
}  /* stdout_start_children */

/**
 * Finish the children dumper.
 */
static void stdout_finish_children(pattern_dumper_t *self, unsigned id)
{
	FILE *f = self->data;
	(void) id;

	fprintf(f, ")");
}  /* stdout_finish_children */

/**
 * The stdout dumper.
 */
static const pattern_dumper_t stdout_dump = {
	stdout_dump_new_pattern,
	stdout_dump_finish_pattern,
	stdout_dump_node,
	stdout_dump_ref,
	stdout_dump_edge,
	stdout_start_children,
	stdout_finish_children,
	NULL,
	NULL,
	NULL
};

/* ------------------------------------ API ------------------------------------- */

/*
 * Starts a new pattern.
 */
void pattern_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
	if (self->dump_new_pattern)
		self->dump_new_pattern(self, cnt);
}  /* pattern_dump_new_pattern */


/*
 * Finish the current pattern.
 */
void pattern_dump_finish_pattern(pattern_dumper_t *self)
{
	if (self->dump_finish_pattern)
		self->dump_finish_pattern(self);
}  /* pattern_dump_finish_pattern */


/*
 * Dumps a node.
 */
void pattern_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr)
{
	if (self->dump_node)
		self->dump_node(self, id, op_code, mode_code, attr);
}  /* pattern_dump_node */

/*
 * Dump a ref.
 */
void pattern_dump_ref(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_ref)
		self->dump_ref(self, id);
}  /* pattern_dump_ref */

/*
 * Dump an edge.
 */
void pattern_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	if (self->dump_edge)
		self->dump_edge(self, tgt, src, pos, mode_code);
}  /* pattern_dump_edge */

/*
 * Start the children dumper.
 */
void pattern_start_children(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_start_children)
		self->dump_start_children(self, id);
}  /* pattern_start_children */

/*
 * Finish the the children dumper.
 */
void pattern_finish_children(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_finish_children)
		self->dump_finish_children(self, id);
}  /* pattern_finish_children */

/*
 * Finish the the dumper.
 */
void pattern_end(pattern_dumper_t *self)
{
	if (self->dump_end)
		self->dump_end(self);

	free(self);
}  /* pattern_end */

/**
 * pattern dumper factory for text dumper
 */
pattern_dumper_t *new_text_dumper(void)
{
	pattern_dumper_t *res = malloc(sizeof(*res));

	if (res) {
		memcpy(res, &stdout_dump, sizeof(*res));
		res->data = stdout;

		if (res->dump_start)
			res->dump_start(res);
	}  /* if */
	return res;
}  /* new_text_dumper */

/**
 * pattern dumper factory for vcg dumper
 */
pattern_dumper_t *new_vcg_dumper(const char *vcg_name, unsigned max_pattern)
{
	pattern_dumper_t *res = malloc(sizeof(*res) + sizeof(vcg_private_t));
	vcg_private_t *priv;

	if (res) {
		FILE *f;

		memcpy(res, &vcg_dump, sizeof(*res));

		priv = (vcg_private_t *)(res + 1);
		memset(priv, 0, sizeof(*priv));

		f = fopen(vcg_name, "w");
		priv->f           = f;
		priv->pattern_id  = 0;
		priv->max_pattern = max_pattern ? max_pattern : (unsigned)-1;
		res->data         = priv;

		if (res->dump_start)
			res->dump_start(res);
	}  /* if */

	return res;
}  /* new_vcg_dumper */

#endif /* FIRM_STATISTICS */
