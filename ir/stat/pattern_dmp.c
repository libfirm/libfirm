/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm. Dumping patterns.
 * @author  Michael Beck
 */
#include <stdio.h>
#include <stdlib.h>

#include "ident.h"
#include "irop_t.h"
#include "irmode_t.h"
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
struct pattern_dumper_t {
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
typedef struct vcg_private_t {
	FILE     *f;          /**< file to dump to */
	unsigned pattern_id;  /**< ID of the pattern */
	unsigned max_pattern; /**< maximum number of pattern to be dumped */
} vcg_private_t;

/**
 * Starts a new VCG graph.
 */
static void vcg_dump_start(pattern_dumper_t *self)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;

	fprintf(priv->f,
		"graph: { title: \"Most found pattern\"\n"
		"  display_edge_labels: no\n"
		"  layoutalgorithm: mindepth\n"
		"  manhattan_edges: yes\n"
		"  port_sharing: no\n"
		"  orientation: bottom_to_top\n"
		);
}

/**
 * Ends a new VCG graph.
 */
static void vcg_dump_end(pattern_dumper_t *self)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;

	fprintf(priv->f, "}\n");
	fclose(priv->f);
}

/**
 * Starts a new pattern.
 */
static void vcg_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;
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
}

/**
 * Finish the current pattern.
 */
static void vcg_dump_finish_pattern(pattern_dumper_t *self)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;

	if (priv->pattern_id > priv->max_pattern)
		return;

	fprintf(priv->f, "  }\n");

	if (priv->pattern_id > 0)
		fprintf(priv->f, "  edge: { sourcename: \"g%u\" targetname: \"g%u\" linestyle:invisible}\n",
			priv->pattern_id,
			priv->pattern_id - 1);

	++priv->pattern_id;
}

/**
 * Dumps a node.
 */
static void vcg_dump_node(pattern_dumper_t *self, unsigned id,
    unsigned op_code, unsigned mode_code, void *attr)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;
	ir_op *op           = stat_get_op_from_opcode(op_code);
	ir_mode *mode       = ir_get_mode(mode_code);
	long l              = attr ? *(long *)attr : 0;

	if (priv->pattern_id > priv->max_pattern)
		return;

	if (attr) {
		fprintf(priv->f, "    node: {title: \"n%u_%u\" label: \"%s%s %ld n%u\" }\n",
			priv->pattern_id, id, get_id_str(op->name), mode ? get_mode_name(mode) : "", l, id);
	} else {
		fprintf(priv->f, "    node: {title: \"n%u_%u\" label: \"%s%s n%u\" }\n",
			priv->pattern_id, id, get_id_str(op->name), mode ? get_mode_name(mode) : "", id);
	}
}

/**
 * Dumps an edge.
 */
static void vcg_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	vcg_private_t *priv = (vcg_private_t*)self->data;
	(void) mode_code;

	if (priv->pattern_id > priv->max_pattern)
		return;

	fprintf(priv->f, "    edge: { sourcename: \"n%u_%u\" targetname: \"n%u_%u\" label: \"%u\" }\n",
		priv->pattern_id, src,
		priv->pattern_id, tgt,
		pos
	);
}

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
	FILE *f = (FILE*)self->data;

	fprintf(f, "%8u ", cnt_to_uint(cnt));
}


/**
 * Finish the current pattern.
 */
static void stdout_dump_finish_pattern(pattern_dumper_t *self)
{
	FILE *f = (FILE*)self->data;

	fprintf(f, "\n");
}

/**
 * Dumps a node.
 */
static void stdout_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr)
{
	FILE *f       = (FILE*)self->data;
	ir_op *op     = stat_get_op_from_opcode(op_code);
	ir_mode *mode = ir_get_mode(mode_code);
	(void) attr;

	/* if (env->options & OPT_ENC_GRAPH) */
	fprintf(f, "%u:", id);

	fprintf(f, "%s", get_id_str(op->name));

	if (mode)
		fprintf(f, "%s", get_mode_name(mode));
}

/**
 * Dump a ref
 */
static void stdout_dump_ref(pattern_dumper_t *self, unsigned id)
{
	FILE *f = (FILE*)self->data;

	fprintf(f, "REF:%u", id);
}

/**
 * Dump an edge.
 */
static void stdout_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	FILE *f = (FILE*)self->data;
	(void) tgt;
	(void) src;
	(void) pos;
	(void) mode_code;

	if (pos > 0)
		fprintf(f, ", ");
}

/**
 * Start the children dumper.
 */
static void stdout_start_children(pattern_dumper_t *self, unsigned id)
{
	FILE *f = (FILE*)self->data;
	(void) id;

	fprintf(f, "(");
}

/**
 * Finish the children dumper.
 */
static void stdout_finish_children(pattern_dumper_t *self, unsigned id)
{
	FILE *f = (FILE*)self->data;
	(void) id;

	fprintf(f, ")");
}

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
}


/*
 * Finish the current pattern.
 */
void pattern_dump_finish_pattern(pattern_dumper_t *self)
{
	if (self->dump_finish_pattern)
		self->dump_finish_pattern(self);
}


/*
 * Dumps a node.
 */
void pattern_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr)
{
	if (self->dump_node)
		self->dump_node(self, id, op_code, mode_code, attr);
}

/*
 * Dump a ref.
 */
void pattern_dump_ref(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_ref)
		self->dump_ref(self, id);
}

/*
 * Dump an edge.
 */
void pattern_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code)
{
	if (self->dump_edge)
		self->dump_edge(self, tgt, src, pos, mode_code);
}

/*
 * Start the children dumper.
 */
void pattern_start_children(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_start_children)
		self->dump_start_children(self, id);
}

/*
 * Finish the the children dumper.
 */
void pattern_finish_children(pattern_dumper_t *self, unsigned id)
{
	if (self->dump_finish_children)
		self->dump_finish_children(self, id);
}

/*
 * Finish the the dumper.
 */
void pattern_end(pattern_dumper_t *self)
{
	if (self->dump_end)
		self->dump_end(self);

	free(self);
}

/**
 * pattern dumper factory for text dumper
 */
pattern_dumper_t *new_text_dumper(void)
{
	pattern_dumper_t *res = XMALLOC(pattern_dumper_t);

	*res = stdout_dump;
	res->data = stdout;

	if (res->dump_start)
		res->dump_start(res);
	return res;
}

/**
 * pattern dumper factory for vcg dumper
 */
pattern_dumper_t *new_vcg_dumper(const char *vcg_name, unsigned max_pattern)
{
	pattern_dumper_t *res = (pattern_dumper_t*)malloc(sizeof(*res) + sizeof(vcg_private_t));
	vcg_private_t *priv;

	if (res) {
		FILE *f;

		*res = vcg_dump;

		priv = (vcg_private_t *)(res + 1);
		memset(priv, 0, sizeof(*priv));

		f = fopen(vcg_name, "w");
		priv->f           = f;
		priv->pattern_id  = 0;
		priv->max_pattern = max_pattern ? max_pattern : (unsigned)-1;
		res->data         = priv;

		if (res->dump_start)
			res->dump_start(res);
	}

	return res;
}
