/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#include <ctype.h>

#include "obst.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"

#include "irmode_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irgraph.h"
#include "irdump.h"
#include "irdom.h"

#include "beutil.h"
#include "besched.h"
#include "bera_t.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"

#define TEST_COLORS 2048

/** An interval border. */
typedef struct _border_t {
	struct list_head list;		/**< list head for queuing. */
	const ir_node *irn;				/**< The node. */
	unsigned step;						/**< The number equal to the interval border. */
	unsigned is_def : 1;			/**< Does this border denote a use or a def. */
} border_t;

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

typedef struct _env_t {
	struct obstack obst;	/**< An obstack for temporary storage. */
	set *phi_if;					/**< The phi interference map. */
	bitset_t *live;   		/**< A live bitset to use in every block. */
	bitset_t *processed;	/**< A set marking processed blocks. */
	bitset_t *colors;			/**< The color mask. */
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

	ir_snprintf(buf, sizeof(buf), "%s_bl%N.eps",
			get_entity_name(get_irg_entity(irg)), block);

	if((f = fopen(buf, "wt")) != NULL) {
		border_t *b;
		int *seen = calloc(get_graph_node_count(irg), sizeof(*seen));
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
				int live_out = is_live_out(block, irn);
				int y_val = y_dist * col;

				int red = 0;
				int green = live_out;
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
	set_insert(env->phi_if, &edge, sizeof(edge), IF_EDGE_HASH(&edge));
}

static INLINE int are_connected(const env_t *env, int src, int tgt)
{
	if_edge_t edge;
	edge_init(&edge, src, tgt);
	return set_find(env->phi_if, &edge, sizeof(edge), IF_EDGE_HASH(&edge)) != NULL;
}



static INLINE border_t *border_add(env_t *env, struct list_head *head,
			const ir_node *irn, int step, int is_def)
{
	border_t *b = obstack_alloc(&env->obst, sizeof(*b));
	b->is_def = is_def;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	return b;
}

static void block_alloc(ir_node *block, void *env_ptr)
{
	env_t *env = env_ptr;
	struct obstack *obst = &env->obst;
	void *obstack_level = obstack_base(obst);
	bitset_t *live = env->live;
	bitset_t *colors = env->colors;
	ir_graph *irg = get_irn_irg(block);

	int i, n;
	unsigned step = 0;
	int block_nr = get_block_graph_nr(block);
	const ir_node *irn;
	border_t *b;
	struct list_head head;
	pset *live_in = get_live_in(block);
	pset *live_out = get_live_out(block);
	ir_node *idom = get_Block_idom(block);

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

	INIT_LIST_HEAD(&head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are neccessary to build up real intervals.
	 */
	for(irn = pset_first(live_out); irn; irn = pset_next(live_out)) {
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
		ir_debugf("insn: %n\n", irn);
		ir_debugf("live: %b\n", live);

		get_irn_ra_info(irn)->color = NO_COLOR;

		/*
		 * If the node defines a datab value, i.e. something, registers must
		 * be allocated for, add a new def border to the border list.
		 */
		if(is_allocatable_irn(irn)) {
			unsigned long elm;
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_add(env, &head, irn, step, 1);

			if(is_phi_operand(irn)) {
				bitset_foreach(live, elm) {
					int live_nr = (int) elm;
					ir_node *live_irn = get_irn_for_graph_nr(irg, live_nr);
					if(is_phi_operand(live_irn)) {
						ir_debugf("\t\tinterfering phi operands: %n, %n\n", irn, live_irn);
						add_if(env, nr, live_nr);
					}
				}
			}
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(is_allocatable_irn(op)) {
					int nr = get_irn_graph_nr(op);

					ir_debugf("\t\tpos: %d, use: %n\n", i, op);

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

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));

			/* Add the def */
			border_add(env, &head, irn, step, 1);
		}
	}

	ir_debugf("usedef chain for block %n\n", block);
	list_for_each_entry(border_t, b, &head, list) {
		ir_debugf("\t%s %n %d\n", b->is_def ? "def" : "use", b->irn, get_irn_graph_nr(b->irn));
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
			ra_info_t *ri = get_irn_ra_info(irn);
			int col = bitset_next_clear(colors, 0);

			assert(!is_color(get_irn_color(irn)) && "Color must not have assigned");
			assert(!bitset_is_set(live, nr) && "Value def must not have been encountered");

			bitset_set(colors, col);
			bitset_set(live, nr);

			ri->color = col;
			ri->pressure = bitset_popcnt(colors);

			ir_debugf("\tassigning color %d to %n\n", col, irn);
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			int col = get_irn_ra_info(irn)->color;

			assert(bitset_is_set(live, nr) && "Cannot have a non live use");
			assert(is_color(col) && "A color must have been assigned");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

	draw_interval_graphs(block, &head, &dump_params);

	/* Mark this block has processed. */
	bitset_set(env->processed, block_nr);

	/* Reset the obstack to its initial level */
	obstack_free(obst, obstack_level);
}


void be_ra_chordal(ir_graph *irg)
{
	int node_count = get_graph_node_count(irg);
	env_t *env = malloc(sizeof(*env));

	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	obstack_init(&env->obst);
	env->phi_if = new_set(if_edge_cmp, node_count);
	env->live = bitset_obstack_alloc(&env->obst, node_count);
	env->processed = bitset_obstack_alloc(&env->obst, get_graph_block_count(irg));
	env->colors = bitset_obstack_alloc(&env->obst, TEST_COLORS);

	irg_block_walk_graph(irg, block_alloc, NULL, env);
	obstack_free(&env->obst, NULL);

	set_irg_ra_link(irg, env);
}

void be_ra_chordal_done(ir_graph *irg)
{
	env_t *env = get_irg_ra_link(irg);
	free(env->phi_if);
	free(env);
}

int phi_ops_interfere(const ir_node *a, const ir_node *b)
{
	ir_graph *irg = get_irn_irg(a);
	env_t *env = get_irg_ra_link(irg);

	assert(irg == get_irn_irg(b) && "Both nodes must be in the same graph");

	return are_connected(env, get_irn_graph_nr(a), get_irn_graph_nr(b));
}
