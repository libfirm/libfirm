/*
 * Project:     libFIRM
 * File name:   ir/debug/firm_ycomp.c
 * Purpose:     Connect firm to ycomp
 * Author:      Christian Wuerdig
 * Modified by:
 * Created:     16.11.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "assert.h"
#include "irhooks.h"
#include "firmnet.h"
#include "irnode.h"
#include "irgraph.h"
#include "irprintf.h"
#include "irprog.h"
#include "iredges.h"
#include "pset.h"
#include "obst.h"

#define SEND_BUF_SIZE 256
#define HASH_EDGE(edge) \
	((get_irn_node_nr((edge)->src) << 17)          | \
	((get_irn_node_nr((edge)->tgt) & 0xEFFF) << 2) | \
	((edge)->pos & 0x2))

typedef struct _exchange_node_outs_assoc_t {
	int     n_out_edges;
	ir_node *irn;
} exchange_node_outs_assoc_t;

typedef struct _ycomp_edge_t {
	ir_node *src;
	ir_node *tgt;
	int     pos;
} ycomp_edge_t;

enum _firm_ycomp_node_realizer_values {
	NODE_REALIZER_NORMAL,
	NODE_REALIZER_PROJ,
	NODE_REALIZER_BLOCK,
	NODE_REALIZER_MEM,
	NODE_REALIZER_PHI,
	NODE_REALIZER_STARTEND,
	NODE_REALIZER_LAST
};

typedef struct _firm_ycomp_node_realizer_t {
	unsigned   id;
	const char *linecolor;
	const char *fillcolor;
	const char *shape;
} firm_ycomp_node_realizer_t;

static firm_ycomp_node_realizer_t node_realizer[NODE_REALIZER_LAST] = {
	{ NODE_REALIZER_NORMAL,   "black", "white",  "box" },
	{ NODE_REALIZER_PROJ,     "black", "yellow", "box" },
	{ NODE_REALIZER_BLOCK,    "black", "yellow", "box" },
	{ NODE_REALIZER_MEM,      "black", "blue",   "box" },
	{ NODE_REALIZER_PHI,      "black", "green",  "box" },
	{ NODE_REALIZER_STARTEND, "black", "blue",   "box" },
};

enum _firm_ycomp_edge_realizer_values {
	EDGE_REALIZER_DATA,
	EDGE_REALIZER_MEM,
	EDGE_REALIZER_DEP,
	EDGE_REALIZER_CFG,
	EDGE_REALIZER_LAST
};

typedef struct _firm_ycomp_edge_realizer_t {
	unsigned   id;
	const char *linecolor;
	unsigned   thickness;
	const char *style;
} firm_ycomp_edge_realizer_t;

static firm_ycomp_edge_realizer_t edge_realizer[EDGE_REALIZER_LAST] = {
	{ EDGE_REALIZER_DATA, "black", 1, "continuous" },
	{ EDGE_REALIZER_MEM,  "blue",  1, "continuous" },
	{ EDGE_REALIZER_DEP,  "green", 1, "continuous" },
	{ EDGE_REALIZER_CFG,  "red",   1, "continuous" },
};

typedef struct _firm_ycomp_dbg_t {
	int            fd;
	int            has_data;
	pset           *exchanged_nodes;
	pset           *edges;
	struct obstack obst;
	hook_entry_t   hook_new_irg;
	hook_entry_t   hook_new_irn;
	hook_entry_t   hook_set_edge;
	hook_entry_t   hook_exchange;
} firm_ycomp_dbg_t;

static firm_ycomp_dbg_t yy_dbg;

static int cmp_edges(const void *a, const void *b) {
	ycomp_edge_t *e1 = (ycomp_edge_t *)a;
	ycomp_edge_t *e2 = (ycomp_edge_t *)b;

	return (e1->src != e2->src) || (e1->tgt != e2->tgt) || (e1->pos != e2->pos);
}

static int cmp_nodes(const void *a, const void *b) {
	exchange_node_outs_assoc_t *n1 = (exchange_node_outs_assoc_t *)a;
	exchange_node_outs_assoc_t *n2 = (exchange_node_outs_assoc_t *)b;

	return n1->irn != n2->irn;
}

static INLINE void send_cmd(const char *buf) {
	ssize_t res, len;

	fprintf(stderr, "'%s'\n", buf);
	len = strlen(buf);
	res = firmnet_send(yy_dbg.fd, (const void *)buf, len);
	assert(res == len);
}

static void wait_for_sync(void) {
	char buf[6];

	firmnet_recv(yy_dbg.fd, buf, 6, 5);
}

static void firm_ycomp_debug_init_realizer(void) {
	int  i;
	char buf[SEND_BUF_SIZE];

	for (i = 0; i < NODE_REALIZER_LAST; ++i) {
		snprintf(buf, sizeof(buf), "addNodeRealizer \"%u\" \"%s\" \"%s\" \"%s\"\n",
			node_realizer[i].id,
			node_realizer[i].linecolor,
			node_realizer[i].fillcolor,
			node_realizer[i].shape);
		send_cmd(buf);
	}

	for (i = 0; i < EDGE_REALIZER_LAST; ++i) {
		snprintf(buf, sizeof(buf), "addEdgeRealizer \"%u\" \"%s\" \"%u\" \"%s\"\n",
			edge_realizer[i].id,
			edge_realizer[i].linecolor,
			edge_realizer[i].thickness,
			edge_realizer[i].style);
		send_cmd(buf);
	}
}

static INLINE unsigned get_node_realizer(ir_node *node) {
	unsigned realizer;
	opcode   opc = get_irn_opcode(node);

	switch (opc) {
		case iro_Block:
			realizer = NODE_REALIZER_BLOCK;
			break;
		case iro_Phi:
			realizer = NODE_REALIZER_PHI;
			break;
		case iro_Proj:
			if (get_irn_mode(node) == mode_M)
				realizer = NODE_REALIZER_MEM;
			else
				realizer = NODE_REALIZER_PROJ;
			break;
		case iro_Start:
		case iro_End:
			realizer = NODE_REALIZER_STARTEND;
			break;
		default:
			realizer = NODE_REALIZER_NORMAL;
	};

	return realizer;
}

static INLINE unsigned get_edge_realizer(ir_node *src, ir_node *tgt) {
	unsigned realizer;
	ir_mode  *mode;

	assert(! is_Block(tgt));

	mode = get_irn_mode(tgt);

	if (mode == mode_M)
		realizer = EDGE_REALIZER_MEM;
	else if (mode == mode_X)
		realizer = EDGE_REALIZER_CFG;
	else
		realizer = EDGE_REALIZER_DATA;

	return realizer;
}

static void firm_ycomp_debug_new_node(void *context, ir_graph *graph, ir_node *node) {
	firm_ycomp_dbg_t *dbg = context;
	char     buf[SEND_BUF_SIZE];
	int      i;
	unsigned src_idx = get_irn_node_nr(node);

	if (get_const_code_irg() == graph)
		return;

	dbg->has_data = 1;

	/* add node */
	ir_snprintf(buf, sizeof(buf), "addNode \"%u\" \"%u\" \"%+F\"\n",
		src_idx,                    /* node id */
		get_node_realizer(node),    /* realizerId */
		node);                      /* label */
	send_cmd(buf);

	/* add edges */
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node      *pred   = get_irn_n(node, i);
		unsigned     tgt_idx = get_irn_node_nr(pred);
		ycomp_edge_t key, *entry;

		ir_snprintf(buf, sizeof(buf), "addEdge \"n%un%up%d\" \"%u\" \"%u\" \"%u\" \"%d\"\n",
			src_idx, tgt_idx, i,            /* edge id */
			src_idx,                        /* source node id */
			tgt_idx,                        /* target node id */
			get_edge_realizer(node, pred),  /* realizer id */
			i);                             /* title */
		send_cmd(buf);

		/* insert edge */
		key.src = node;
		key.tgt = pred;
		key.pos = i;
		entry   = pset_find(dbg->edges, &key, HASH_EDGE(&key));
		if (! entry) {
			entry = obstack_alloc(&dbg->obst, sizeof(*entry));
			entry->src = node;
			entry->tgt = pred;
			entry->pos = i;
			pset_insert(dbg->edges, entry, HASH_EDGE(entry));
		}
	}

	send_cmd("show\n");
	send_cmd("sync\n");
	wait_for_sync();
}

static void firm_ycomp_debug_new_irg(void *context, ir_graph *irg, entity *ent) {
	if (yy_dbg.has_data) {
		send_cmd("deleteGraph\n");
		send_cmd("show\n");
	}
	yy_dbg.has_data = 0;
	send_cmd("sync\n");
	wait_for_sync();
}

static void firm_ycomp_debug_set_edge(void *context, ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt) {
	firm_ycomp_dbg_t           *dbg = context;
	exchange_node_outs_assoc_t *entry, key;
	ycomp_edge_t               *old_edge, *new_edge, edge_key;
	char                       buf[SEND_BUF_SIZE];
	unsigned                   src_idx, tgt_idx, old_tgt_idx;

	/* ignore block edges for now */
	if (pos < 0)
		return;

	/* check if the new edge exists */
	edge_key.src = src;
	edge_key.tgt = tgt;
	edge_key.pos = pos;
	new_edge     = pset_find(dbg->edges, &edge_key, HASH_EDGE(&edge_key));

	/* if the new edge already exists and the old target is the new target -> ignore */
	if (new_edge && tgt == old_tgt)
		return;

	/* check if the old edge exists */
	edge_key.src = src;
	edge_key.tgt = old_tgt;
	edge_key.pos = pos;
	old_edge     = pset_find(dbg->edges, &edge_key, HASH_EDGE(&edge_key));

	/* check if old target is marked for exchange */
	key.irn = old_tgt;
	entry   = pset_find(dbg->exchanged_nodes, &key, HASH_PTR(old_tgt));

	if (entry) {
		/* we are called from exchange() */
		entry->n_out_edges--;
	}

	src_idx     = get_irn_node_nr(src);
	tgt_idx     = get_irn_node_nr(tgt);
	old_tgt_idx = get_irn_node_nr(old_tgt);

	if (old_edge) {
		/* delete the old edge if it exists */
		snprintf(buf, sizeof(buf), "deleteEdge \"n%un%up%d\"\n", src_idx, old_tgt_idx, pos);
		send_cmd(buf);
		pset_remove(dbg->edges, old_edge, HASH_EDGE(old_edge));
	}

	if (! new_edge) {
		/* add the new edge if it doesn't exist */
		snprintf(buf, sizeof(buf), "addEdge \"n%un%up%d\" \"%u\" \"%u\" \"%u\" \"%d\"\n",
			src_idx, tgt_idx, pos,          /* edge id */
			src_idx,                        /* source node id */
			tgt_idx,                        /* target node id */
			get_edge_realizer(src, tgt),    /* realizer id */
			pos);                           /* title */
		send_cmd(buf);

		/* insert the new edge */
		new_edge      = obstack_alloc(&dbg->obst, sizeof(*new_edge));
		new_edge->src = src;
		new_edge->tgt = tgt;
		new_edge->pos = pos;
		pset_insert(dbg->edges, new_edge, HASH_EDGE(new_edge));
	}

	/* show and sync if all edges are rerouted or if it's a normal set_irn_n */
	if (! entry || entry->n_out_edges == 0) {
		send_cmd("show\n");
		send_cmd("sync\n");
		wait_for_sync();
	}
}

static void firm_ycomp_debug_exchange(void *context, ir_node *old_node, ir_node *new_node) {
	firm_ycomp_dbg_t           *dbg = context;
	exchange_node_outs_assoc_t key, *entry;

	key.irn = old_node;
	entry   = pset_find(dbg->exchanged_nodes, &key, HASH_PTR(old_node));
	if (entry) {
		entry->n_out_edges = get_irn_n_edges(old_node);
	}
	else {
		entry              = obstack_alloc(&dbg->obst, sizeof(*entry));
		entry->irn         = old_node;
		entry->n_out_edges = get_irn_n_edges(old_node);
		pset_insert(dbg->exchanged_nodes, entry, HASH_PTR(old_node));
	}
}

void firm_init_ycomp_debugger(const char *host, uint16_t port) {
	static int init_once = 0;

	if (init_once)
		return;

	memset(&yy_dbg, 0, sizeof(yy_dbg));
	yy_dbg.fd = -1;

	host = "localhost";
	port = 4242;

	yy_dbg.fd = firmnet_connect_tcp(host, port);

	if (yy_dbg.fd > -1) {
		/* We could establish a connection to ycomp -> register hooks */
		firm_ycomp_debug_init_realizer();
		yy_dbg.exchanged_nodes = new_pset(cmp_nodes, 20);
		yy_dbg.edges           = new_pset(cmp_edges, 20);
		obstack_init(&yy_dbg.obst);

		/* new node hook */
		yy_dbg.hook_new_irn.context             = &yy_dbg;
		yy_dbg.hook_new_irn.hook._hook_new_node = firm_ycomp_debug_new_node;
		register_hook(hook_new_node, &yy_dbg.hook_new_irn);

		/* new irg hook */
		yy_dbg.hook_new_irg.context              = &yy_dbg;
		yy_dbg.hook_new_irg.hook._hook_new_graph = firm_ycomp_debug_new_irg;
		register_hook(hook_new_graph, &yy_dbg.hook_new_irg);

		/* set irn n hook */
		yy_dbg.hook_set_edge.context              = &yy_dbg;
		yy_dbg.hook_set_edge.hook._hook_set_irn_n = firm_ycomp_debug_set_edge;
		register_hook(hook_set_irn_n, &yy_dbg.hook_set_edge);

		/* replace (exchange) hook */
		yy_dbg.hook_exchange.context            = &yy_dbg;
		yy_dbg.hook_exchange.hook._hook_replace = firm_ycomp_debug_exchange;
		register_hook(hook_replace, &yy_dbg.hook_exchange);
	}

	init_once = 1;
}

void firm_finish_ycomp_debugger(void) {
	if (yy_dbg.fd > -1) {
		firmnet_close_socket(yy_dbg.fd);
		unregister_hook(hook_new_graph, &yy_dbg.hook_new_irg);
		unregister_hook(hook_new_node, &yy_dbg.hook_new_irn);
		unregister_hook(hook_set_irn_n, &yy_dbg.hook_set_edge);
		unregister_hook(hook_replace, &yy_dbg.hook_exchange);
		del_pset(yy_dbg.exchanged_nodes);
		del_pset(yy_dbg.edges);
		yy_dbg.exchanged_nodes = NULL;
		obstack_free(&yy_dbg.obst, NULL);
	}
}
