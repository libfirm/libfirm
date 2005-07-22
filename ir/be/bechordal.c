/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 8.12.2004
 *
 * Copyright (C) Universitaet Karlsruhe
 * Released under the GPL
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <ctype.h>

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "debug.h"
#include "xmalloc.h"

#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bearch.h"

#include "bechordal_t.h"
#include "bechordal_draw.h"

#define NO_COLOR (-1)

#undef DUMP_INTERVALS
#undef DUMP_PRESSURE
#undef DUMP_IFG

#if defined(DUMP_IFG) && !defined(BUILD_GRAPH)
#error Must define BUILD_GRAPH to be able to dump it.
#endif


#ifdef DEBUG_libfirm
#include "fourcc.h"

/* Make a fourcc for border checking. */
#define BORDER_FOURCC				FOURCC('B', 'O', 'R', 'D')

#endif /* DEBUG_libfirm */

static firm_dbg_module_t *dbg;

#ifdef BUILD_GRAPH

#define IF_EDGE_HASH(e) ((e)->src)
#define IF_NODE_HASH(n) ((n)->nnr)

static int if_edge_cmp(const void *p1, const void *p2, size_t size)
{
	const if_edge_t *e1 = p1;
	const if_edge_t *e2 = p2;

	return !(e1->src == e2->src && e1->tgt == e2->tgt);
}

static int if_node_cmp(const void *p1, const void *p2, size_t size)
{
	const if_node_t *n1 = p1;
	const if_node_t *n2 = p2;

	return n1->nnr != n2->nnr;
}

static INLINE if_edge_t *edge_init(if_edge_t *edge, int src, int tgt)
{
	/* Bring the smaller entry to src. */
	if(src > tgt) {
		edge->src = tgt;
		edge->tgt = src;
	} else {
		edge->src = src;
		edge->tgt = tgt;
	}

	return edge;
}

static INLINE void add_if(const be_chordal_env_t *env, int src, int tgt)
{
	if_edge_t edge;
	if_node_t node, *src_node, *tgt_node;
	/* insert edge */
	edge_init(&edge, src, tgt);
	set_insert(env->edges, &edge, sizeof(edge), IF_EDGE_HASH(&edge));

	/* insert nodes */
	node.nnr = src;
	node.neighb = pset_new_ptr(8);
	src_node = set_insert(env->nodes, &node, sizeof(node), IF_NODE_HASH(&node));
	node.nnr = tgt;
	node.neighb = pset_new_ptr(8);
	tgt_node = set_insert(env->nodes, &node, sizeof(node), IF_NODE_HASH(&node));

	/* insert neighbors into nodes */
	pset_insert_ptr(src_node->neighb, tgt_node);
	pset_insert_ptr(tgt_node->neighb, src_node);
}

static INLINE int are_connected(const be_chordal_env_t *env, int src, int tgt)
{
	if_edge_t edge;
	edge_init(&edge, src, tgt);
	return set_find(env->edges, &edge, sizeof(edge), IF_EDGE_HASH(&edge)) != NULL;
}

int ifg_has_edge(const be_chordal_env_t *env, const if_node_t *n1, const if_node_t* n2) {
	return are_connected(env, n1->nnr, n2->nnr);
}

#ifdef DUMP_IFG

static void dump_ifg(const be_chordal_env_t *env)
{
	FILE *f;
	set *edges = env->edges;
	ir_graph *irg = env->irg;
	char filename[128];

	ir_snprintf(filename, sizeof(filename), "ifg_%s_%F.dot", env->cls->name, irg);

	if((f = fopen(filename, "wt")) != NULL) {
		bitset_pos_t pos;
		int n_edges = 0;
		if_edge_t *edge;
		bitset_t *bs = bitset_malloc(get_graph_node_count(irg));

		ir_fprintf(f, "graph \"%F\" {\n", irg);
		fprintf(f, "\tnode [shape=box,style=filled]\n");

		for(edge = set_first(edges); edge; edge = set_next(edges)) {
			bitset_set(bs, edge->src);
			bitset_set(bs, edge->tgt);
			n_edges++;
		}

		fprintf(f, "\tx [label=\"nodes: %u, edges: %d\"]\n", bitset_popcnt(bs), n_edges);

		bitset_foreach(bs, pos) {
			int nr = (int) pos;
			ir_node *irn = get_irn_for_graph_nr(irg, nr);

			ir_fprintf(f, "\tn%d [label=\"%+F\"]\n", nr, irn);
		}

		for(edge = set_first(edges); edge; edge = set_next(edges)) {
			fprintf(f, "\tn%d -- n%d [len=5]\n", edge->src, edge->tgt);
		}

		fprintf(f, "}\n");
		fclose(f);

		bitset_free(bs);
	}

}

#endif /* DUMP_IFG */

#endif /* BUILD_GRAPH */


/**
 * Add an interval border to the list of a block's list
 * of interval border.
 * @note You always have to create the use before the def.
 * @param env The environment.
 * @param head The list head to enqueue the borders.
 * @param irn The node (value) the border belongs to.
 * @param pressure The pressure at this point in time.
 * @param step A time step for the border.
 * @param is_def Is the border a use or a def.
 * @return The created border.
 */
static INLINE border_t *border_add(be_chordal_env_t *env, struct list_head *head,
			ir_node *irn, unsigned step, unsigned pressure,
			unsigned is_def, unsigned is_real)
{
	border_t *b;

	if(!is_def) {
		border_t *def;

		b = obstack_alloc(&env->obst, sizeof(*b));

		/* also allocate the def and tie it to the use. */
		def = obstack_alloc(&env->obst, sizeof(*def));
		b->other_end = def;
		def->other_end = b;

		/*
		 * Set the link field of the irn to the def.
		 * This strongly relies on the fact, that the use is always
		 * made before the def.
		 */
		set_irn_link(irn, def);

#ifdef DEBUG_libfirm
		b->magic = BORDER_FOURCC;
		def->magic = BORDER_FOURCC;
#endif
	}

	/*
	 * If the def is encountered, the use was made and so was the
	 * the def node (see the code above). It was placed into the
	 * link field of the irn, so we can get it there.
	 */
	else {
		b = get_irn_link(irn);

#ifdef DEBUG_libfirm
		assert(b && b->magic == BORDER_FOURCC && "Illegal border encountered");
#endif
	}

	b->pressure = pressure;
	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((dbg, LEVEL_5, "\t\t%s adding %+F, step: %d\n",
				is_def ? "def" : "use", irn, step));

	return b;
}

/**
 * Annotate the register pressure to the nodes and compute
 * the liveness intervals.
 * @param block The block to do it for.
 * @param env_ptr The environment.
 */
static void pressure(ir_node *block, void *env_ptr)
{
/* Convenience macro for a def */
#define border_def(irn, step, real) \
	border_add(env, head, irn, step, pressure--, 1, real)

/* Convenience macro for a use */
#define border_use(irn, step, real) \
	border_add(env, head, irn, step, ++pressure, 0, real)

	be_chordal_env_t *env = env_ptr;
	bitset_t *live = env->live;
	ir_node *irn;

	int i, n;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;
	pset *live_in = put_live_in(block, pset_new_ptr_default());
	pset *live_end = put_live_end(block, pset_new_ptr_default());
	const arch_register_class_t *cls = env->cls;

	DBG((dbg, LEVEL_1, "Computing pressure in block %+F\n", block));
	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = obstack_alloc(&env->obst, sizeof(*head));
	INIT_LIST_HEAD(head);
	pmap_insert(env->border_heads, block, head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	for(irn = pset_first(live_end); irn; irn = pset_next(live_end)) {
		DBG((dbg, LEVEL_3, "\tMaking live: %+F/%d\n", irn, get_irn_graph_nr(irn)));
		bitset_set(live, get_irn_graph_nr(irn));
		if(arch_irn_has_reg_class(env->arch_env, irn, 0, cls))
			border_use(irn, step, 0);
	}
	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "\tinsn: %+F, pressure: %d\n", irn, pressure));
		DBG((dbg, LEVEL_2, "\tlive: %b\n", live));

	    /*
	     * If the node defines some value, which can put into a
	     * register of the current class, make a border for it.
	     */
		if(arch_irn_has_reg_class(env->arch_env, irn, 0, cls)) {
			bitset_pos_t elm;
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_def(irn, step, 1);

#ifdef BUILD_GRAPH
			bitset_foreach(live, elm)
			add_if(env, nr, (int) elm);
#endif
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(arch_irn_has_reg_class(env->arch_env, op, 0, cls)) {
					int nr = get_irn_graph_nr(op);

					DBG((dbg, LEVEL_4, "\t\tpos: %d, use: %+F\n", i, op));

					if(!bitset_is_set(live, nr)) {
						border_use(op, step, 1);
						bitset_set(live, nr);
					}
				}
			}
		}
		++step;
	}

	/*
	 * Add initial defs for all values live in.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(arch_irn_has_reg_class(env->arch_env, irn, 0, cls)) {

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));

			/* Add the def */
			border_def(irn, step, 0);
		}
	}

    del_pset(live_in);
    del_pset(live_end);
}

static void assign(ir_node *block, void *env_ptr)
{
	be_chordal_env_t *env = env_ptr;
	struct obstack *obst = &env->obst;
	bitset_t *live = env->live;
	bitset_t *colors = env->colors;
	bitset_t *in_colors = env->in_colors;
	const arch_register_class_t *cls = env->cls;

	/* Mark the obstack level and allocate the temporary tmp_colors */
	void *obstack_level = obstack_base(obst);
	/*bitset_t *tmp_colors = bitset_obstack_alloc(obst, env->colors_n);*/

	const ir_node *irn;
	border_t *b;
	struct list_head *head = get_block_border_head(env, block);
	pset *live_in = put_live_in(block, pset_new_ptr_default());

	bitset_clear_all(live);
	bitset_clear_all(colors);
	bitset_clear_all(in_colors);

	DBG((dbg, LEVEL_4, "Assigning colors for block %+F\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	list_for_each_entry(border_t, b, head, list) {
		DBG((dbg, LEVEL_4, "\t%s %+F/%d\n", b->is_def ? "def" : "use",
					b->irn, get_irn_graph_nr(b->irn)));
	}

	/*
	 * Add initial defs for all values live in.
	 * Since their colors have already been assigned (The dominators were
	 * allocated before), we have to mark their colors as used also.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(arch_irn_has_reg_class(env->arch_env, irn, 0, cls)) {
      const arch_register_t *reg = arch_get_irn_register(env->arch_env, irn, 0);
      int col;

      assert(reg && "Node must have been assigned a register");
			col = arch_register_get_index(reg);

			/* Mark the color of the live in value as used. */
			bitset_set(colors, col);
			bitset_set(in_colors, col);

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));
		}
	}

	/*
	 * Mind that the sequence of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work.
	 */
	list_for_each_entry_reverse(border_t, b, head, list) {
		ir_node *irn = b->irn;
		int nr = get_irn_graph_nr(irn);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if(b->is_def && !is_live_in(block, irn)) {
      const arch_register_t *reg;
			int col = NO_COLOR;

			DBG((dbg, LEVEL_4, "\tcolors in use: %b\n", colors));

      col = bitset_next_clear(colors, 0);
      reg = arch_register_for_index(env->cls, col);

      assert(arch_get_irn_register(env->arch_env, irn, 0) == NULL
          && "This node must not have been assigned a register yet");
			assert(!bitset_is_set(live, nr) && "Value's definition must not have been encountered");

			bitset_set(colors, col);
			bitset_set(live, nr);

			arch_set_irn_register(env->arch_env, irn, 0, reg);
			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n",
            arch_register_get_name(reg), col, irn));
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
      const arch_register_t *reg = arch_get_irn_register(env->arch_env, irn, 0);
			int col;

      assert(reg && "Register must have been assigned");

      col = arch_register_get_index(reg);
			assert(bitset_is_set(live, nr) && "Cannot have a non live use");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

	/* Free the auxillary data on the obstack. */
	obstack_free(obst, obstack_level);

  del_pset(live_in);
}

void be_ra_chordal_init(void)
{
	dbg = firm_dbg_register(DBG_CHORDAL);
	firm_dbg_set_mask(dbg, 0);
}

be_chordal_env_t *be_ra_chordal(ir_graph *irg,
    const arch_env_t *arch_env,
    const arch_register_class_t *cls)
{
	int node_count = get_graph_node_count(irg);
	int colors_n = arch_register_class_n_regs(cls);
	be_chordal_env_t *env = malloc(sizeof(*env));

	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	obstack_init(&env->obst);

#ifdef BUILD_GRAPH
	env->edges = new_set(if_edge_cmp, node_count);
	env->nodes = new_set(if_node_cmp, node_count);
#endif

	env->live = bitset_obstack_alloc(&env->obst, node_count);
	env->colors = bitset_obstack_alloc(&env->obst, colors_n);
	env->in_colors = bitset_obstack_alloc(&env->obst, colors_n);
	env->colors_n = colors_n;
	env->cls = cls;
	env->arch_env = arch_env;
	env->irg = irg;
	env->border_heads = pmap_create();

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, env);

	/* Insert probable spills */
	be_ra_chordal_spill(env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, env);

#ifdef DUMP_IFG
	dump_ifg(env);
#endif

#ifdef DUMP_INTERVALS
	{
		char buf[128];
    	plotter_t *plotter;

		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", cls->name, irg);
    	plotter = new_plotter_ps(buf);

    	draw_interval_tree(&draw_chordal_def_opts, env, plotter, arch_env, cls);
    	plotter_free(plotter);
	}
#endif
	return env;
}

void be_ra_chordal_check(be_chordal_env_t *chordal_env) {
	const arch_env_t *arch_env;
	struct obstack ob;
	pmap_entry *pme;
	ir_node **nodes, *n1, *n2;
	int i, o;

	arch_env = chordal_env->arch_env;

	/* Collect all irns */
	obstack_init(&ob);
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;
		list_for_each_entry(border_t, curr, head, list)
			if (curr->is_def && curr->is_real)
				if (arch_get_irn_reg_class(arch_env, curr->irn, arch_pos_make_out(0)) == chordal_env->cls)
					obstack_ptr_grow(&ob, curr->irn);
	}
	obstack_ptr_grow(&ob, NULL);
	nodes = (ir_node **) obstack_finish(&ob);

	/* Check them */
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		const arch_register_t *n1_reg, *n2_reg;

		n1_reg = arch_get_irn_register(arch_env, n1, 0);
		assert(arch_reg_is_allocatable(arch_env, n1, arch_pos_make_out(0), n1_reg) && "Register constraint does not hold");

		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o]) {
			n2_reg = arch_get_irn_register(arch_env, n2, 0);
			assert(!(nodes_interfere(chordal_env, n1, n2) && n1_reg == n2_reg) && "Interfering values have the same color!");
		}
	}
	obstack_free(&ob, NULL);
}

/* TODO #ifdef BUILD_GRAPH --> faster version of checker with edges */

void be_ra_chordal_done(be_chordal_env_t *env)
{
#ifdef BUILD_GRAPH
	{
		if_node_t *ifn;
		for(ifn = set_first(env->nodes); ifn; ifn = set_next(env->nodes))
			free(ifn->neighb);
		free(env->nodes);
		free(env->edges);
	}
#endif

  pmap_destroy(env->border_heads);
	obstack_free(&env->obst, NULL);
	free(env);
}

int nodes_interfere(const be_chordal_env_t *env, const ir_node *a, const ir_node *b)
{
#ifdef BUILD_GRAPH
	return are_connected(env, get_irn_graph_nr(a), get_irn_graph_nr(b));
#else
	return values_interfere(a, b);
#endif /* BUILD_GRAPH */
}

#ifdef BUILD_GRAPH

set *be_ra_get_ifg_edges(const be_chordal_env_t *env) {
	return env->edges;
}

set *be_ra_get_ifg_nodes(const be_chordal_env_t *env) {
	return env->nodes;
}

#endif
