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

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
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
#include "benode_t.h"
#include "bearch.h"
#include "beifg.h"

#include "bechordal_t.h"
#include "bechordal_draw.h"

#define DBG_LEVEL SET_LEVEL_0
#define DBG_LEVEL_CHECK SET_LEVEL_0

#define NO_COLOR (-1)

#undef DUMP_INTERVALS

typedef struct _be_chordal_alloc_env_t {
	be_chordal_env_t *chordal_env;

	pset *pre_colored;    /**< Set of precolored nodes. */
	bitset_t *live;				/**< A liveness bitset. */
	bitset_t *colors;			/**< The color mask. */
	bitset_t *in_colors;        /**< Colors used by live in values. */
	int colors_n;               /**< The number of colors. */
} be_chordal_alloc_env_t;

#include "fourcc.h"

/* Make a fourcc for border checking. */
#define BORDER_FOURCC				FOURCC('B', 'O', 'R', 'D')

static void check_border_list(struct list_head *head)
{
  border_t *x;
  list_for_each_entry(border_t, x, head, list) {
    assert(x->magic == BORDER_FOURCC);
  }
}

static void check_heads(be_chordal_env_t *env)
{
  pmap_entry *ent;
  for(ent = pmap_first(env->border_heads); ent; ent = pmap_next(env->border_heads)) {
    /* ir_printf("checking border list of block %+F\n", ent->key); */
    check_border_list(ent->value);
  }
}


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
		memset(def, 0, sizeof(*def));
		b->other_end = def;
		def->other_end = b;

		/*
		 * Set the link field of the irn to the def.
		 * This strongly relies on the fact, that the use is always
		 * made before the def.
		 */
		set_irn_link(irn, def);

		b->magic = BORDER_FOURCC;
		def->magic = BORDER_FOURCC;
	}

	/*
	 * If the def is encountered, the use was made and so was the
	 * the def node (see the code above). It was placed into the
	 * link field of the irn, so we can get it there.
	 */
	else {
		b = get_irn_link(irn);

		assert(b && b->magic == BORDER_FOURCC && "Illegal border encountered");
	}

	b->pressure = pressure;
	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((env->dbg, LEVEL_5, "\t\t%s adding %+F, step: %d\n", is_def ? "def" : "use", irn, step));


	return b;
}

/**
 * Check, if an irn is of the register class currently under processing.
 * @param env The chordal environment.
 * @param irn The node.
 * @return 1, if the node is of that register class, 0 if not.
 */
static INLINE int has_reg_class(const be_chordal_env_t *env, const ir_node *irn)
{
  return arch_irn_has_reg_class(env->main_env->arch_env, irn, -1, env->cls);
}

#define has_limited_constr(req, irn) \
	(arch_get_register_req(arch_env, (req), irn, -1) && (req)->type == arch_register_req_type_limited)

static int try_pre_color(be_chordal_env_t *env, ir_node *irn,
						 pset *pre_colored, bitset_t *colors_used)
{
	arch_register_req_t req;

	if(arch_get_register_req(env->main_env->arch_env, &req, irn, -1) && arch_register_req_is(&req, limited)) {

		bitset_t *bs          = bitset_alloca(env->cls->n_regs);
		const arch_register_t *reg;
		int col;

		req.limited(irn, -1, bs);
		col = bitset_next_set(bs, 0);
		reg = arch_register_for_index(env->cls, col);

		pset_insert_ptr(pre_colored, irn);
		arch_set_irn_register(env->main_env->arch_env, irn, reg);

		bitset_set(colors_used, col);

		DBG((env->dbg, LEVEL_2, "pre coloring %+F with %s\n", irn, reg->name));

		return 1;
	}

	return 0;
}

/**
 * Handle register targeting constraints signaled by a Perm.
 * @param alloc_env    Private data for the allocation phase.
 * @param perm         The Perm node guarding the constrained node.
 * @return             The constrained node.

		Pro-coloring works as follows:

         +-----------------------------------+
         |            Perm                   |
         +---.-------.--------.---------.----+
             |       |        |         |
         +---+--+    |        |         |
         | Proj |    |        |         |
         +------+    |        |         |
                     |        |         |
                  +--+---+    |         |
                  | Proj |    |         |
                  +--.---+    |         |
                     |        |         |
                     |     +--+---+     |
                     |     | Proj |     |
                     |     +------+     |
                      |                 |
                      |             +---+--+
                       `-.          | Proj | Result:
                          `._       +---.--+ R1
                             `.         |
                               `-.      |
                                  `._   |
                                    +`.-+--+
                                    |Constr| Result:
                                    +------+ R2

		1) Look at all Projs of the Perm if they have output constraints.
		   If one has an output constraint, pre-color it, else record it
		   in the set leftover. Its color has to be chosen after all
		   constrained nodes are colored. Furthermore record all colors
		   used in the pre-coloring in the set colors_used.

		2) Look whether the first node not a Proj (this is the constrained
		   node due to which the Perm has been inserted) has an output
		   constraint. If yes, pre-color the node accordingly else do nothing
		   since the node's input constraints are modelled by the Proj's
		   output constraint.

		   There's one subtle point here: If thenode has an output constraint
		   and the live range of some Proj ends at that node, we must give
		   that Proj the color of the constrained node. Otherwise the
		   available colors may not suffice for the rest of the projs.

		3) At last, color the Projs which have not been colored yet with the
		   left over colors.

		   So afterwards, everything including the constrained node will
		   be colored and the assign() phase can complete this coloring.
		   Note that therefore, we put the pre-colored nodes in a set
		   called pre_colored().

 */
static ir_node *handle_constraints_at_perm(be_chordal_alloc_env_t *alloc_env, ir_node *perm)
{
	be_chordal_env_t *env      = alloc_env->chordal_env;
	firm_dbg_module_t *dbg     = env->dbg;
	const arch_env_t *arch_env = env->main_env->arch_env;

	pset *leftover        = pset_new_ptr(8);
	pset *pre_colored     = pset_new_ptr(8);
	bitset_t *colors_used = bitset_alloca(env->cls->n_regs);
	ir_node *irn, *cnstr, *last;
	int has_cnstr = 0;

	assert(be_is_Perm(perm));

	DBG((dbg, LEVEL_2, "Constraints on %+F\n", perm));

	/*
	 * Color constrained Projs first.
	 */
	for(irn = sched_next(perm); is_Proj(irn); irn = sched_next(irn))
		if(!try_pre_color(env, irn, pre_colored, colors_used))
			pset_insert_ptr(leftover, irn);

	cnstr = irn;
	last  = irn;

	if(get_irn_mode(cnstr) == mode_T) {
		for(irn = sched_next(cnstr); is_Proj(irn); irn = sched_next(irn))
			if(!try_pre_color(env, irn, pre_colored, colors_used))
				pset_insert_ptr(leftover, irn);

		last = sched_prev(irn);
	}

	else
		try_pre_color(env, cnstr, pre_colored, colors_used);

	for(irn = pset_first(leftover); irn; irn = pset_next(leftover)) {
		const arch_register_t *reg;
		ir_node *precol;
		int colored = 0;

		for(precol = pset_first(pre_colored); precol; precol = pset_next(pre_colored)) {
			if(!values_interfere(irn, precol)) {
				reg = arch_get_irn_register(arch_env, irn);
				arch_set_irn_register(arch_env, irn, reg);
				pset_break(pre_colored);
				colored = 1;
				break;
			}
		}

		if(!colored) {
			int col = bitset_next_clear(colors_used, 0);

			assert(col >=0 && "There must be a register left");
			reg = arch_register_for_index(env->cls, col);
			arch_set_irn_register(arch_env, irn, reg);
			bitset_set(colors_used, reg->index);
			pset_insert_ptr(alloc_env->pre_colored, irn);

			DBG((dbg, LEVEL_2, "coloring leftover %+F with %s\n", irn, reg->name));
		}
	}

	pset_insert_pset_ptr(alloc_env->pre_colored, pre_colored);

	del_pset(leftover);
	del_pset(pre_colored);

	return last;
}

/**
 * Handle constraint nodes in each basic block.
 * be_insert_constr_perms() inserts Perm nodes which perm
 * over all values live at the constrained node right in front
 * of the constrained node. These Perms signal a constrained node.
 * For further comments, refer to handle_constraints_at_perm().
 */
static void constraints(ir_node *bl, void *data)
{
	be_chordal_alloc_env_t *env = data;
	arch_env_t *arch_env        = env->chordal_env->main_env->arch_env;
	ir_node *irn;

	for(irn = sched_first(bl); !sched_is_end(irn); irn = sched_next(irn)) {
		if(be_is_Perm(irn) && arch_irn_has_reg_class(arch_env, irn, 0, env->chordal_env->cls))
			irn = handle_constraints_at_perm(env, irn);
	}
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

	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env             = alloc_env->chordal_env;
	bitset_t *live                    = alloc_env->live;
	firm_dbg_module_t *dbg            = env->dbg;
	ir_node *irn;

	int i, n;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;
	pset *live_in = put_live_in(block, pset_new_ptr_default());
	pset *live_end = put_live_end(block, pset_new_ptr_default());

	DBG((dbg, LEVEL_1, "Computing pressure in block %+F\n", block));
	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = obstack_alloc(&env->obst, sizeof(*head));
	INIT_LIST_HEAD(head);
	assert(pmap_get(env->border_heads, block) == NULL);
	pmap_insert(env->border_heads, block, head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	for(irn = pset_first(live_end); irn; irn = pset_next(live_end)) {
		if(has_reg_class(env, irn)) {
			DBG((dbg, LEVEL_3, "\tMaking live: %+F/%d\n", irn, get_irn_graph_nr(irn)));
			bitset_set(live, get_irn_graph_nr(irn));
			border_use(irn, step, 0);
		}
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
		if(has_reg_class(env, irn)) {
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_def(irn, step, 1);
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(has_reg_class(env, op)) {
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
		if(has_reg_class(env, irn)) {

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
	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env       = alloc_env->chordal_env;
	firm_dbg_module_t *dbg      = env->dbg;
	bitset_t *live              = alloc_env->live;
	bitset_t *colors            = alloc_env->colors;
	bitset_t *in_colors         = alloc_env->in_colors;
	const arch_env_t *arch_env  = env->main_env->arch_env;

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
		if(has_reg_class(env, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
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

			if(pset_find_ptr(alloc_env->pre_colored, irn)) {
				reg = arch_get_irn_register(arch_env, irn);
				col = reg->index;
				assert(!bitset_is_set(colors, col) && "pre-colored register must be free");
			}

			else {
				col = bitset_next_clear(colors, 0);
				reg = arch_register_for_index(env->cls, col);
				assert(arch_get_irn_register(arch_env, irn) == NULL && "This node must not have been assigned a register yet");
			}

			bitset_set(colors, col);
			arch_set_irn_register(arch_env, irn, reg);

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n",
            arch_register_get_name(reg), col, irn));

			assert(!bitset_is_set(live, nr) && "Value's definition must not have been encountered");
			bitset_set(live, nr);
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			int col;

			assert(reg && "Register must have been assigned");

			col = arch_register_get_index(reg);
			assert(bitset_is_set(live, nr) && "Cannot have a non live use");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

	del_pset(live_in);
}

void be_ra_chordal_color(be_chordal_env_t *chordal_env)
{
	int node_count        = get_graph_node_count(chordal_env->irg);
	int colors_n          = arch_register_class_n_regs(chordal_env->cls);
	ir_graph *irg         = chordal_env->irg;

	be_chordal_alloc_env_t env;

	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	env.chordal_env  = chordal_env;
	env.live         = bitset_malloc(node_count);
	env.colors       = bitset_malloc(colors_n);
	env.in_colors    = bitset_malloc(colors_n);
	env.colors_n     = colors_n;
	env.pre_colored  = pset_new_ptr_default();

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, &env);

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, &env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, &env);

#ifdef DUMP_INTERVALS
	{
		char buf[128];
    	plotter_t *plotter;

		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", cls->name, irg);
    	plotter = new_plotter_ps(buf);

    	draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter, env->arch_env, cls);
    	plotter_free(plotter);
	}
#endif

	free(env.live);
	free(env.colors);
	free(env.in_colors);

	del_pset(env.pre_colored);
}
