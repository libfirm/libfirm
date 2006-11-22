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
	ir_node *nw;
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
	NODE_REALIZER_IRG,
	NODE_REALIZER_ID,
	NODE_REALIZER_LAST
};

typedef struct _firm_ycomp_node_realizer_t {
	unsigned   id;
	const char *linecolor;
	const char *fillcolor;
	const char *shape;
} firm_ycomp_node_realizer_t;

static firm_ycomp_node_realizer_t node_realizer[NODE_REALIZER_LAST] = {
	{ NODE_REALIZER_NORMAL,   "black", "white",    "box" },
	{ NODE_REALIZER_PROJ,     "black", "yellow",   "box" },
	{ NODE_REALIZER_BLOCK,    "black", "yellow",   "box" },
	{ NODE_REALIZER_MEM,      "black", "blue",     "box" },
	{ NODE_REALIZER_PHI,      "black", "green",    "box" },
	{ NODE_REALIZER_STARTEND, "black", "blue",     "box" },
	{ NODE_REALIZER_IRG,      "black", "white",    "box" },
	{ NODE_REALIZER_ID,       "black", "darkgrey", "box" },
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
	unsigned       in_dead_node_elim : 1;
	struct obstack obst;
	hook_entry_t   hook_new_irn;
	hook_entry_t   hook_new_irg;
	hook_entry_t   hook_set_edge;
	hook_entry_t   hook_exchange;
	hook_entry_t   hook_into_id;
	hook_entry_t   hook_dead_node;
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

static INLINE void send_cmd(firm_ycomp_dbg_t *dbg, const char *buf) {
	ssize_t res, len;

	len = strlen(buf);
	res = firmnet_send(dbg->fd, (const void *)buf, len);
	assert(res == len);
}

static void wait_for_sync(firm_ycomp_dbg_t *dbg) {
	char buf[6];
	firmnet_recv(dbg->fd, buf, 6, 5);
}

static void show_and_sync(firm_ycomp_dbg_t *dbg) {
	send_cmd(dbg, "show\n");
	send_cmd(dbg, "sync\n");
	wait_for_sync(dbg);
}

/**
 * Register the edge and node realizer in yComp.
 */
static void firm_ycomp_debug_init_realizer(firm_ycomp_dbg_t *dbg) {
	int  i;
	char buf[SEND_BUF_SIZE];

	for (i = 0; i < NODE_REALIZER_LAST; ++i) {
		snprintf(buf, sizeof(buf), "addNodeRealizer \"%u\" \"%s\" \"%s\" \"%s\"\n",
			node_realizer[i].id,
			node_realizer[i].linecolor,
			node_realizer[i].fillcolor,
			node_realizer[i].shape);
		send_cmd(dbg, buf);
	}

	for (i = 0; i < EDGE_REALIZER_LAST; ++i) {
		snprintf(buf, sizeof(buf), "addEdgeRealizer \"%u\" \"%s\" \"%u\" \"%s\"\n",
			edge_realizer[i].id,
			edge_realizer[i].linecolor,
			edge_realizer[i].thickness,
			edge_realizer[i].style);
		send_cmd(dbg, buf);
	}
}

/**
 * Retrieve the appropriate realizer for given node.
 */
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

/**
 * Retrieve the appropriate realizer for given edge.
 */
static INLINE unsigned get_edge_realizer(ir_node *src, ir_node *tgt) {
	unsigned realizer;
	ir_mode  *tgt_mode, *src_mode;

	assert(! is_Block(tgt));

	tgt_mode = get_irn_mode(tgt);
	src_mode = is_Block(src) ? NULL : get_irn_mode(src);

	if (tgt_mode == mode_M || src_mode)
		realizer = EDGE_REALIZER_MEM;
	else if (tgt_mode == mode_X)
		realizer = EDGE_REALIZER_CFG;
	else
		realizer = EDGE_REALIZER_DATA;

	return realizer;
}

/**
 * Add new nodes, resp. new blocks in yComp and add input edges.
 */
static void firm_ycomp_debug_new_node(void *context, ir_graph *graph, ir_node *node) {
	firm_ycomp_dbg_t *dbg = context;
	char             buf[SEND_BUF_SIZE];
	int              i;
	unsigned         src_idx;

	if (get_const_code_irg() == graph || dbg->in_dead_node_elim)
		return;

	src_idx       = get_irn_node_nr(node);
	dbg->has_data = 1;

	if (is_Block(node)) {
		/* add block (subgraph) */
		ir_snprintf(buf, sizeof(buf), "addSubgraphNode \"%d\" \"%d\" \"%u\" \"%+F\"\n",
			0,                     /* parent id */
			get_irn_node_nr(node), /* graph id */
			NODE_REALIZER_BLOCK,   /* realizer id */
			node);                 /* label */
	}
	else {
		/* add node */
		ir_snprintf(buf, sizeof(buf), "addNode \"%d\" \"%u\" \"%u\" \"%+F\"\n",
			get_irn_node_nr(get_nodes_block(node)), /* parent id */
			src_idx,                                /* node id */
			get_node_realizer(node),                /* realizer id */
			node);                                  /* label */
	}
	send_cmd(dbg, buf);

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
		send_cmd(dbg, buf);

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

	show_and_sync(dbg);
}

/**
 * Clear the old irg if it has some data and create a new one.
 */
static void firm_ycomp_debug_new_irg(void *context, ir_graph *irg, entity *ent) {
	firm_ycomp_dbg_t *dbg = context;
	char             buf[SEND_BUF_SIZE];

	if (yy_dbg.has_data) {
		send_cmd(dbg, "deleteGraph\n");
		send_cmd(dbg, "show\n");
	}
	dbg->has_data = 0;

	ir_snprintf(buf, sizeof(buf), "addSubgraphNode \"-1\" \"0\" \"%u\" \"%s\"\n",
		NODE_REALIZER_IRG, get_entity_name(ent));
	send_cmd(dbg, buf);
	send_cmd(dbg, "sync\n");
	wait_for_sync(dbg);
}

/**
 * Handle set_irn_n calls.
 * - set new Block      OR
 * - remove old edge and add new one
 */
static void firm_ycomp_debug_set_edge(void *context, ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt) {
	firm_ycomp_dbg_t           *dbg = context;
	exchange_node_outs_assoc_t *entry, key;
	ycomp_edge_t               *old_edge, *new_edge, edge_key;
	char                       buf[SEND_BUF_SIZE];
	unsigned                   src_idx, tgt_idx, old_tgt_idx;

	if (dbg->in_dead_node_elim)
		return;

	src_idx     = get_irn_node_nr(src);
	tgt_idx     = get_irn_node_nr(tgt);
	old_tgt_idx = get_irn_node_nr(old_tgt);

	/* set_irn_n with pos -1 means: node moves to new block  */
	if (pos < 0) {
		if (tgt != old_tgt) {
			snprintf(buf, sizeof(buf), "moveNode \"%d\" \"%d\"\n", src_idx, tgt_idx);
			send_cmd(dbg, buf);
			show_and_sync(dbg);
		}
		return;
	}

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

	/* delete the old edge if it exists */
	if (old_edge) {
		snprintf(buf, sizeof(buf), "deleteEdge \"n%un%up%d\"\n", src_idx, old_tgt_idx, pos);
		send_cmd(dbg, buf);
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
		send_cmd(dbg, buf);

		/* insert the new edge */
		new_edge      = obstack_alloc(&dbg->obst, sizeof(*new_edge));
		new_edge->src = src;
		new_edge->tgt = tgt;
		new_edge->pos = pos;
		pset_insert(dbg->edges, new_edge, HASH_EDGE(new_edge));
	}

	/* show and sync if all edges are rerouted or if it's a normal set_irn_n */
	if (! entry || entry->n_out_edges == 0) {
		show_and_sync(dbg);
	}
}

/**
 * Put nodes, about to be exchanged into a set.
 */
static void firm_ycomp_debug_exchange(void *context, ir_node *old_node, ir_node *new_node) {
	firm_ycomp_dbg_t           *dbg = context;
	exchange_node_outs_assoc_t key, *entry;

	/* put nodes, which are about to be exchanged into a set */

	key.irn = old_node;
	entry   = pset_find(dbg->exchanged_nodes, &key, HASH_PTR(old_node));
	if (entry) {
		entry->n_out_edges = get_irn_n_edges(old_node);
		entry->nw          = new_node;
	}
	else {
		entry              = obstack_alloc(&dbg->obst, sizeof(*entry));
		entry->irn         = old_node;
		entry->nw          = new_node;
		entry->n_out_edges = get_irn_n_edges(old_node);
		pset_insert(dbg->exchanged_nodes, entry, HASH_PTR(old_node));
	}
}

/**
 * Remove all old in edges, turn node into id node, add new input edge.
 */
static void firm_ycomp_debug_turn_into_id(void *context, ir_node *old_node) {
	firm_ycomp_dbg_t           *dbg = context;
	exchange_node_outs_assoc_t key, *entry;
	int                        i;
	char                       buf[SEND_BUF_SIZE];
	unsigned                   src_idx, tgt_idx;
	ycomp_edge_t               edge_key, *new_edge;

	key.irn = old_node;
	entry   = pset_find(dbg->exchanged_nodes, &key, HASH_PTR(old_node));

	assert(entry != NULL && "Exchange entry missing");

	src_idx = get_irn_node_nr(old_node);
	tgt_idx = get_irn_node_nr(entry->nw);

	/* remove all old edges */
	for (i = get_irn_arity(old_node) - 1; i >= 0; --i) {
		ycomp_edge_t *old_edge;
		unsigned     old_tgt_idx;

		/* check if the old edge exists */
		edge_key.src = old_node;
		edge_key.tgt = get_irn_n(old_node, i);
		edge_key.pos = i;
		old_edge     = pset_find(dbg->edges, &edge_key, HASH_EDGE(&edge_key));

		old_tgt_idx  = get_irn_node_nr(edge_key.tgt);

		/* remove the old edge */
		if (old_edge) {
			snprintf(buf, sizeof(buf), "deleteEdge \"n%un%up%d\"\n", src_idx, old_tgt_idx, i);
			send_cmd(dbg, buf);
			pset_remove(dbg->edges, old_edge, HASH_EDGE(old_edge));
		}
	}

	/* change the old node into an id node */
	snprintf(buf, sizeof(buf), "changeNode \"%ld\" \"%u\"\n", get_irn_node_nr(old_node), NODE_REALIZER_ID);
	send_cmd(dbg, buf);

	/* add new Id input */
	snprintf(buf, sizeof(buf), "addEdge \"n%un%up%d\" \"%u\" \"%u\" \"%u\" \"%d\"\n",
		src_idx, tgt_idx, 0,                       /* edge id */
		src_idx,                                   /* source node id */
		tgt_idx,                                   /* target node id */
		get_edge_realizer(old_node, entry->nw),    /* realizer id */
		0);                                        /* title */
	send_cmd(dbg, buf);

	/* go */
	show_and_sync(dbg);

	/* add the new edge to our pset */
	new_edge = obstack_alloc(&dbg->obst, sizeof(*new_edge));
	new_edge->src = old_node;
	new_edge->tgt = entry->nw;
	new_edge->pos = 0;
	pset_insert(dbg->edges, new_edge, HASH_EDGE(new_edge));
}

/**
 * Just mark start/end of dead node elimination.
 */
static void firm_ycomp_debug_dead_node_elim(void *context, ir_graph *irg, int start) {
	firm_ycomp_dbg_t *dbg  = context;
	dbg->in_dead_node_elim = start != 0;
}

/**
 * Establish connection to yComp and register all hooks.
 */
void firm_init_ycomp_debugger(const char *host, uint16_t port) {
	static int init_once = 0;

	if (init_once)
		return;

	memset(&yy_dbg, 0, sizeof(yy_dbg));
	yy_dbg.fd = -1;

	fprintf(stderr, "connecting to %s:%u\n", host, port);
	yy_dbg.fd = firmnet_connect_tcp(host, port);

	if (yy_dbg.fd > -1) {
		/* We could establish a connection to ycomp -> register hooks */
		firm_ycomp_debug_init_realizer(&yy_dbg);
		yy_dbg.exchanged_nodes = new_pset(cmp_nodes, 20);
		yy_dbg.edges           = new_pset(cmp_edges, 20);
		obstack_init(&yy_dbg.obst);

#define REGISTER_HOOK(ycomp_hook, firm_hook, func)     \
	do {                                               \
		yy_dbg.ycomp_hook.context           = &yy_dbg; \
		yy_dbg.ycomp_hook.hook._##firm_hook = func;    \
		register_hook(firm_hook, &yy_dbg.ycomp_hook);  \
	} while(0)

		REGISTER_HOOK(hook_new_irn,   hook_new_node,       firm_ycomp_debug_new_node);
		REGISTER_HOOK(hook_new_irg,   hook_new_graph,      firm_ycomp_debug_new_irg);
		REGISTER_HOOK(hook_set_edge,  hook_set_irn_n,      firm_ycomp_debug_set_edge);
		REGISTER_HOOK(hook_exchange,  hook_replace,        firm_ycomp_debug_exchange);
		REGISTER_HOOK(hook_into_id,   hook_turn_into_id,   firm_ycomp_debug_turn_into_id);
		REGISTER_HOOK(hook_dead_node, hook_dead_node_elim, firm_ycomp_debug_dead_node_elim);

#undef REGISTER_HOOK
	}

	init_once = 1;
}

/**
 * Close connection to yComp, unregister all hooks and free memory.
 */
void firm_finish_ycomp_debugger(void) {
	if (yy_dbg.fd > -1) {
		/* close connection */
		firmnet_close_socket(yy_dbg.fd);
		yy_dbg.fd = -1;

		/* unregister all hooks */
		unregister_hook(hook_new_graph,      &yy_dbg.hook_new_irg);
		unregister_hook(hook_new_node,       &yy_dbg.hook_new_irn);
		unregister_hook(hook_set_irn_n,      &yy_dbg.hook_set_edge);
		unregister_hook(hook_replace,        &yy_dbg.hook_exchange);
		unregister_hook(hook_dead_node_elim, &yy_dbg.hook_dead_node);
		unregister_hook(hook_turn_into_id,   &yy_dbg.hook_into_id);

		/* clear sets */
		del_pset(yy_dbg.exchanged_nodes);
		del_pset(yy_dbg.edges);

		/* free data obstack */
		obstack_free(&yy_dbg.obst, NULL);
	}
}
