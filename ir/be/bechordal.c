/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 8.12.2004
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <ctype.h>

#include "obst.h"
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
#include "bera_t.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bechordal_t.h"



#undef DUMP_INTERVALS
#undef DUMP_PRESSURE
#define DUMP_IFG

#define BUILD_GRAPH

#ifdef USE_OLD_PHI_INTERFERENCE
#undef BUILD_GRAPH
#define BUILD_GRAPH
#endif

#ifdef DEBUG_libfirm
#include "fourcc.h"

/* Make a fourcc for border checking. */
#define BORDER_FOURCC				FOURCC('B', 'O', 'R', 'D')

#endif /* DEBUG_libfirm */

#define TEST_COLORS 2048

static firm_dbg_module_t *dbg;

/**
 * Environment for each of the chordal register allocator phases
 */
typedef struct _env_t {
	struct obstack obst;	/**< An obstack for temporary storage. */
#ifdef BUILD_GRAPH
	set *graph;						/**< The interference graph. */
#endif

	bitset_t *live;				/**< A liveness bitset. */
	bitset_t *colors;			/**< The color mask. */
	bitset_t *in_colors;	/**< Colors used by live in values. */
	int colors_n;					/**< The number of colors. */
} env_t;


typedef struct _be_chordal_dump_params_t {
	int x_dist;
	int y_dist;
	double font_scale;
} be_chordal_dump_params_t;

static const be_chordal_dump_params_t dump_params = {
	30,
	10,
	4
};

static void draw_interval_graphs(ir_node *block,
		struct list_head *border_head,
		const be_chordal_dump_params_t *params)
{
	int i;
	int x_dist = params->x_dist;
	int y_dist = params->y_dist;
	ir_graph *irg = get_irn_irg(block);

	FILE *f;
	char buf[1024];

	ir_snprintf(buf, sizeof(buf), "intv_%s_bl%N.eps",
			get_entity_name(get_irg_entity(irg)), block);

	if((f = fopen(buf, "wt")) != NULL) {
		border_t *b;
		int *seen = xcalloc(get_graph_node_count(irg), sizeof(seen[0]));
		int last_pos = list_empty(border_head) ? 0 : list_entry(border_head->prev, border_t, list)->step;
		int max_col = 0;

		list_for_each_entry_reverse(border_t, b, border_head, list) {
			const ir_node *irn = b->irn;
			int col = get_irn_color(irn);
			max_col = max_col > col ? max_col : col;
		}

		fprintf(f, "%%!PS-Adobe-2.0\n");
		fprintf(f, "%%%%BoundingBox: -10 -10 %d %d\n",
				x_dist * last_pos + x_dist, y_dist * max_col + y_dist);
		fprintf(f, "/mainfont /Courier findfont %f scalefont def\n", params->font_scale);
		fprintf(f, "mainfont setfont\n");
		fprintf(f, "0.2 setlinewidth\n");

		for(i = 0; i <= last_pos; ++i) {
			fprintf(f, "0 0 0 setrgbcolor\n");
			fprintf(f, "%d %d moveto\n", i * x_dist, -2);
			fprintf(f, "%d %d lineto\n", i * x_dist, max_col * y_dist + 2);
			fprintf(f, "stroke\n");
		}
		fprintf(f, "0.5 setlinewidth\n");

		list_for_each_entry_reverse(border_t, b, border_head, list) {
			const ir_node *irn = b->irn;
			int nr = get_irn_graph_nr(irn);

			if(b->is_def)
				seen[nr] = b->step;
			else {
				int col = get_irn_color(irn);

				int pos = last_pos - seen[nr];
				int end_pos = last_pos - b->step;
				int live_in = is_live_in(block, irn);
				int live_end = is_live_end(block, irn);
				int y_val = y_dist * col;

				int red = 0;
				int green = live_end;
				int blue = live_in;

				fprintf(f, "0 0 0 setrgbcolor\n");
				fprintf(f, "%d %d moveto\n", x_dist * pos + 2, y_val + 2);
				ir_fprintf(f, "(%n/%d%s) show\n", irn, nr, is_phi_operand(irn) ? "*" : "");
				fprintf(f, "%d %d %d setrgbcolor\n", red, green, blue);
				fprintf(f, "%d %d moveto\n", x_dist * pos, y_val);
				fprintf(f, "%d %d lineto\n", (x_dist * end_pos) - 5, y_val);
				fprintf(f, "stroke\n");
			}
		}

		free(seen);
		fclose(f);
	}
}

#ifdef BUILD_GRAPH

typedef struct _if_edge_t {
	int src, tgt;
} if_edge_t;

#define IF_EDGE_HASH(e) ((e)->src)

static int if_edge_cmp(const void *p1, const void *p2, size_t size)
{
	const if_edge_t *e1 = p1;
	const if_edge_t *e2 = p2;

	return !(e1->src == e2->src && e1->tgt == e2->tgt);
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

static INLINE void add_if(const env_t *env, int src, int tgt)
{
	if_edge_t edge;
	edge_init(&edge, src, tgt);
	set_insert(env->graph, &edge, sizeof(edge), IF_EDGE_HASH(&edge));
}

static INLINE int are_connected(const env_t *env, int src, int tgt)
{
	if_edge_t edge;
	edge_init(&edge, src, tgt);
	return set_find(env->graph, &edge, sizeof(edge), IF_EDGE_HASH(&edge)) != NULL;
}

static void dump_ifg(set *edges, const char *filename)
{
	FILE *f;

	if((f = fopen(filename, "wt")) != NULL) {
		if_edge_t *edge;

		fprintf(f, "graph G {\n");

		for(edge = set_first(edges); edge; edge = set_next(edges)) {
			fprintf(f, "i\tn%d -- n%d\n", edge->src, edge->tgt);
		}

		fprintf(f, "}\n");
		fclose(f);
	}

}

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
static INLINE border_t *border_add(env_t *env, struct list_head *head,
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


	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((dbg, LEVEL_5, "\t\t%s adding %n, step: %d\n",
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

	env_t *env = env_ptr;
	bitset_t *live = env->live;
	ir_node *irn;

	int i, n;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;
	pset *live_in = get_live_in(block);
	pset *live_end = get_live_end(block);

	DBG((dbg, LEVEL_1, "Computing pressure in block %n\n", block));
	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = &get_ra_block_info(block)->border_head;
	INIT_LIST_HEAD(head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are neccessary to build up real intervals.
	 */
	for(irn = pset_first(live_end); irn; irn = pset_next(live_end)) {
		DBG((dbg, LEVEL_3, "\tMaking live: %n/%d\n", irn, get_irn_graph_nr(irn)));
		bitset_set(live, get_irn_graph_nr(irn));
		if(!is_Phi(irn) && is_allocatable_irn(irn))
			border_use(irn, step, 0);
	}

	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "\tinsn: %n, pressure: %d\n", irn, pressure));
		DBG((dbg, LEVEL_2, "\tlive: %b\n", live));

		/* Erase the color of each node encountered. */
		set_irn_color(irn, NO_COLOR);

		/*
		 * If the node defines a datab value, i.e. something, registers must
		 * be allocated for, add a new def border to the border list.
		 */
		if(is_allocatable_irn(irn)) {
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_def(irn, step, 1);

#ifdef BUILD_GRAPH
			{
				unsigned long elm;
				bitset_foreach(live, elm) {
					int live_nr = (int) elm;
					add_if(env, nr, live_nr);
				}
			}
#endif
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(is_allocatable_irn(op)) {
					int nr = get_irn_graph_nr(op);

					DBG((dbg, LEVEL_4, "\t\tpos: %d, use: %n\n", i, op));

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
		if(is_allocatable_irn(irn)) {

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));

			/* Add the def */
			border_def(irn, step, 0);
		}
	}
}

static void assign(ir_node *block, void *env_ptr)
{
	env_t *env = env_ptr;
	struct obstack *obst = &env->obst;
	bitset_t *live = env->live;
	bitset_t *colors = env->colors;
	bitset_t *in_colors = env->in_colors;

	/* The used colors will remain on the obstack. */
	bitset_t *used_colors = bitset_obstack_alloc(obst, env->colors_n);

	/* Mark the obstack level and allocate the temporary tmp_colors */
	void *obstack_level = obstack_base(obst);
	bitset_t *tmp_colors = bitset_obstack_alloc(obst, env->colors_n);

	const ir_node *irn;
	border_t *b;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	pset *live_in = get_live_in(block);

	bitset_clear_all(live);
	bitset_clear_all(colors);
	bitset_clear_all(in_colors);

	DBG((dbg, LEVEL_4, "Assigning colors for block %n\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	list_for_each_entry(border_t, b, head, list) {
		DBG((dbg, LEVEL_4, "\t%s %n/%d\n", b->is_def ? "def" : "use",
					b->irn, get_irn_graph_nr(b->irn)));
	}

	/*
	 * Add initial defs for all values live in.
	 * Since their colors have already been assigned (The dominators were
	 * allocated before), we have to mark their colors as used also.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(is_allocatable_irn(irn)) {
			int col = get_irn_color(irn);

			/* Mark the color of the live in value as used. */
			assert(is_color(col) && "Node must have been assigned a color.");
			bitset_set(colors, col);
			bitset_set(in_colors, col);
			bitset_set(used_colors, col);

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
		const ir_node *irn = b->irn;
		int nr = get_irn_graph_nr(irn);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if(b->is_def && !is_live_in(block, irn)) {
			ra_node_info_t *ri = get_ra_node_info(irn);
			int col = NO_COLOR;

			DBG((dbg, LEVEL_4, "\tcolors in use: %b\n", colors));

			/*
			 * Try to assign live out values colors which are not used by live
			 * in values.
			 */
#if 0
			if(is_live_out(block, irn)) {
				int next_clear;

				bitset_copy(tmp_colors, colors);
				bitset_or(tmp_colors, in_colors);
				next_clear = bitset_next_clear(tmp_colors, 0);
				col = next_clear != -1 ? next_clear : NO_COLOR;

				DBG((dbg, LEVEL_5, "next clear in only outs %b: %d\n", tmp_colors, col));
			}
#endif

			/* If a color is not yet assigned, do it now. */
			if(!is_color(col))
				col = bitset_next_clear(colors, 0);

			assert(!is_color(get_irn_color(irn)) && "Color must not have assigned");
			assert(!bitset_is_set(live, nr) && "Value def must not have been encountered");

			bitset_set(colors, col);
			bitset_set(used_colors, col);
			bitset_set(live, nr);

			ri->color = col;

			DBG((dbg, LEVEL_1, "\tassigning color %d to %n\n", col, irn));
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			int col = get_irn_color(irn);

			assert(bitset_is_set(live, nr) && "Cannot have a non live use");
			assert(is_color(col) && "A color must have been assigned");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

#ifdef DUMP_INTERVALS
	draw_interval_graphs(block, &head, &dump_params);
#endif

#ifdef DUMP_PRESSURE
	{
		char buf[128];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "pres_%s_bl_%N.txt",
				get_entity_name(get_irg_entity(irg)), block);

		if((f = fopen(buf, "wt")) != NULL) {
			sched_foreach_reverse(block, irn) {
				if(is_allocatable_irn(irn))
					ir_fprintf(f, "\"%n\" %d %d\n", irn, sched_get_time_step(irn),
							get_ra_node_info(irn)->pressure);

			}
			fclose(f);
		}
	}
#endif


	/*
	 * Allocate the used colors array in the blocks ra info structure and
	 * fill it.
	 */
	get_ra_block_info(block)->used_colors = used_colors;

	/* Free the auxillary data on the obstack. */
	obstack_free(obst, obstack_level);
}


#if 0
static void block_alloc(ir_node *block, void *env_ptr)
{
	env_t *env = env_ptr;
	struct obstack *obst = &env->obst;
	void *obstack_level = obstack_base(obst);
	bitset_t *live = env->live;
	bitset_t *colors = env->colors;
	bitset_t *in_colors = env->in_colors;
	bitset_t *used_colors = bitset_malloc(env->colors_n);
	bitset_t *tmp_colors = bitset_obstack_alloc(obst, env->colors_n);
	ir_graph *irg = get_irn_irg(block);
	int also_assign = env->assign;

	int i, n;
	int block_nr = get_block_graph_nr(block);
	const ir_node *irn;
	border_t *b;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	pset *live_in = get_live_in(block);
	pset *live_end = get_live_end(block);

	/*
	 * Check, if this block has already been processed, if true, return
	 * immediately.
	 */
	if(bitset_is_set(env->processed, block_nr))
		return;

	/*
	 * Ensure, that the immediate dominator of this block is allocated
	 * before this block, since the values live in at this block are
	 * defined in the dominators of this block. Coloring the dominators
	 * thus is vital before coloring this block.
	 */
	if(idom)
		block_alloc(idom, env);

	/* Clear the live and allocate the color bitset. */
	bitset_clear_all(live);
	bitset_clear_all(colors);
	bitset_clear_all(in_colors);

	INIT_LIST_HEAD(&head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are neccessary to build up real intervals.
	 */
	for(irn = pset_first(live_end); irn; irn = pset_next(live_end)) {
		DBG((dbg, LEVEL_3, "Making live: %n/%d\n", irn, get_irn_graph_nr(irn)));
		bitset_set(live, get_irn_graph_nr(irn));
		if(!is_Phi(irn) && is_allocatable_irn(irn))
			border_add(env, &head, irn, step, 0);
	}

	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "insn: %n\n", irn));
		DBG((dbg, LEVEL_2, "live: %b\n", live));

		set_irn_color(irn, NO_COLOR);

		/*
		 * If the node defines a datab value, i.e. something, registers must
		 * be allocated for, add a new def border to the border list.
		 */
		if(is_allocatable_irn(irn)) {
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_add(env, &head, irn, step, 1);

#ifdef BUILD_GRAPH
			{
				unsigned long elm;
				bitset_foreach(live, elm) {
					int live_nr = (int) elm;
					ir_node *live_irn = get_irn_for_graph_nr(irg, live_nr);
					if(is_phi_operand(live_irn)) {
						add_if(env, nr, live_nr);
					}
				}
			}
#endif
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(is_allocatable_irn(op)) {
					int nr = get_irn_graph_nr(op);

					DBG((dbg, LEVEL_4, "\t\tpos: %d, use: %n\n", i, op));

					if(!bitset_is_set(live, nr)) {
						border_add(env, &head, op, step, 0);
						bitset_set(live, nr);
					}
				}
			}
		}

		++step;
	}

	bitset_clear_all(live);

	/*
	 * Add initial defs for all values live in.
	 * Since their colors have already been assigned (The dominators were
	 * allocated before), we have to mark their colors as used also.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(is_allocatable_irn(irn)) {
			int col = get_irn_color(irn);

			/* Mark the color of the live in value as used. */
			assert(is_color(col) && "Node must have been assigned a color.");
			bitset_set(colors, col);
			bitset_set(in_colors, col);
			bitset_set(used_colors, col);

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));

			/* Add the def */
			border_add(env, &head, irn, step, 1);
		}
	}

	DBG((dbg, LEVEL_4, "usedef chain for block %n\n", block));
	list_for_each_entry(border_t, b, &head, list) {
		DBG((dbg, LEVEL_4, "\t%s %n %d\n", b->is_def ? "def" : "use", b->irn, get_irn_graph_nr(b->irn)));
	}

	/*
	 * Mind that the sequence of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work.
	 */
	list_for_each_entry_reverse(border_t, b, &head, list) {
		const ir_node *irn = b->irn;
		int nr = get_irn_graph_nr(irn);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if(b->is_def && !is_live_in(block, irn)) {
			ra_node_info_t *ri = get_ra_node_info(irn);
			int col = NO_COLOR;

			DBG((dbg, LEVEL_4, "colors in use: %b\n", colors));

			/*
			 * Try to assign live out values colors which are not used by live
			 * in values.
			 */
			if(is_live_out(block, irn)) {
				bitset_copy(tmp_colors, colors);
				bitset_or(tmp_colors, in_colors);
				col = bitset_next_clear(tmp_colors, 0);
				DBG((dbg, LEVEL_5, "next clear in only outs %b: %d\n", tmp_colors, col));
			}

			/* If a color is not yet assigned, do it now. */
			if(!is_color(col))
				col = bitset_next_clear(colors, 0);

			assert(!is_color(get_irn_color(irn)) && "Color must not have assigned");
			assert(!bitset_is_set(live, nr) && "Value def must not have been encountered");

			bitset_set(colors, col);
			bitset_set(used_colors, col);
			bitset_set(live, nr);

			ri->color = col;
			ri->pressure = bitset_popcnt(colors);

			DBG((dbg, LEVEL_1, "\tassigning color %d to %n\n", col, irn));
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			int col = get_irn_color(irn);

			assert(bitset_is_set(live, nr) && "Cannot have a non live use");
			assert(is_color(col) && "A color must have been assigned");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

#ifdef DUMP_INTERVALS
	draw_interval_graphs(block, &head, &dump_params);
#endif

#ifdef DUMP_PRESSURE
	{
		char buf[128];
		FILE *f;

		ir_snprintf(buf, sizeof(buf), "pres_%s_bl_%N.txt",
				get_entity_name(get_irg_entity(irg)), block);

		if((f = fopen(buf, "wt")) != NULL) {
			sched_foreach_reverse(block, irn) {
				if(is_allocatable_irn(irn))
					ir_fprintf(f, "\"%n\" %d %d\n", irn, sched_get_time_step(irn),
							get_ra_node_info(irn)->pressure);

			}
			fclose(f);
		}
	}
#endif


	/*
	 * Allocate the used colors array in the blocks ra info structure and
	 * fill it.
	 */
	get_ra_block_info(block)->used_colors = used_colors;

	/* Mark this block has processed. */
	bitset_set(env->processed, block_nr);

	/* Reset the obstack to its initial level */
	obstack_free(obst, obstack_level);
}

#endif

void be_ra_chordal_init(void)
{
	dbg = firm_dbg_register(DBG_BERA);
	firm_dbg_set_mask(dbg, -1);
}

void be_ra_chordal(ir_graph *irg)
{
	int node_count = get_graph_node_count(irg);
	env_t *env = malloc(sizeof(*env));

	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	obstack_init(&env->obst);

#ifdef BUILD_GRAPH
	env->graph = new_set(if_edge_cmp, node_count);
#endif

	env->live = bitset_obstack_alloc(&env->obst, node_count);
	env->colors = bitset_obstack_alloc(&env->obst, TEST_COLORS);
	env->in_colors = bitset_obstack_alloc(&env->obst, TEST_COLORS);
	env->colors_n = TEST_COLORS;

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, env);

	/* Insert probable spills */
	be_ra_chordal_spill(irg);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, env);

#ifdef DUMP_IFG
	{
		char buf[128];

		ir_snprintf(buf, sizeof(buf), "ifg_%s.dot", get_entity_name(get_irg_entity(irg)));
		dump_ifg(env->graph, buf);
	}
#endif

	set_irg_ra_link(irg, env);
}

void be_ra_chordal_done(ir_graph *irg)
{
	env_t *env = get_irg_ra_link(irg);

#ifdef BUILD_GRAPH
	free(env->graph);
#endif

	obstack_free(&env->obst, NULL);
	free(env);
}

int phi_ops_interfere(const ir_node *a, const ir_node *b)
{
#ifdef BUILD_GRAPH
	ir_graph *irg = get_irn_irg(a);
	env_t *env = get_irg_ra_link(irg);

	assert(irg == get_irn_irg(b) && "Both nodes must be in the same graph");

	return are_connected(env, get_irn_graph_nr(a), get_irn_graph_nr(b));
#else
	return values_interfere(a, b);
#endif /* BUILD_GRAPH */
}
