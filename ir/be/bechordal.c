/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 */
#include "bechordal_t.h"

#include "bechordal_common.h"
#include "beinsn_t.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "besched.h"
#include "bipartite.h"
#include "debug.h"
#include "hungarian.h"
#include "irdump.h"
#include "iredges_t.h"
#include "util.h"

#define USE_HUNGARIAN 0

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/*
 * Searches for consecutive free registers.
 * When consecutive is greater 1, the returned position is even.
 */
static int get_next_free_reg(bitset_t *const available, unsigned consecutive)
{
	int pos = 0;
	unsigned free_length = 0;
	while (free_length < consecutive) {
		pos = bitset_next_set(available, pos + free_length);
		if (consecutive > 1 && pos % 2 != 0) {
			free_length = 0;
			pos++;
			continue;
		}
		free_length = 1; // we don't need to check the pos index again
		while (bitset_is_set(available, pos + free_length) && free_length < consecutive) {
			free_length++;
		}
	}
	printf("[chordal] Return pos %d as result of %d-consecutive search\n", pos, consecutive);
	return pos;
}

static unsigned const *get_decisive_partner_regs(be_operand_t const *const o1, size_t const n_regs)
{
	be_operand_t const *const o2 = o1->partner;
	if (!o2 || rbitset_contains(o1->regs, o2->regs, n_regs)) {
		return o1->regs;
	} else if (rbitset_contains(o2->regs, o1->regs, n_regs)) {
		return o2->regs;
	} else {
		return NULL;
	}
}

typedef struct pair_entry {
	be_operand_t *operand;
	int           pos;
	bool          is_def;
} pair_entry_t;

static unsigned n_regs;

static int compare_entries(const void *a, const void *b)
{
	pair_entry_t *a_entry = (pair_entry_t *)a;
	pair_entry_t *b_entry = (pair_entry_t *)b;

	be_operand_t   *a_op    = a_entry->operand;
	be_operand_t   *b_op    = b_entry->operand;
	const unsigned *a_regs  = a_op->regs;
	const unsigned *b_regs  = b_op->regs;
	int             a_count = rbitset_popcount(a_regs, n_regs);
	int             b_count = rbitset_popcount(b_regs, n_regs);
	if (a_count != b_count) {
		return a_count - b_count;
	}

	for (size_t i = 0, n = BITSET_SIZE_ELEMS(n_regs); i < n; ++i) {
		unsigned a_regs_i = a_regs[i];
		unsigned b_regs_i = b_regs[i];
		if (a_regs_i != b_regs_i) {
			return a_regs_i < b_regs_i ? -1 : 1;
		}
	}

	bool a_is_def = a_entry->is_def;
	bool b_is_def = b_entry->is_def;
	if (a_is_def != b_is_def) {
		return a_is_def - b_is_def;
	}

	return a_entry->pos - b_entry->pos;
}

static bool list_has_irn_else_add(ir_node **const list, size_t const n, ir_node *const irn)
{
	for (ir_node *const *i = list; i != list + n; ++i) {
		if (*i == irn)
			return true;
	}
	list[n] = irn;
	return false;
}

static void pair_up_operands(be_chordal_env_t *const env, be_insn_t *const insn)
{
	int           n_ops     = insn->n_ops;
	int           use_start = insn->use_start;
	pair_entry_t *entries   = OALLOCNZ(&env->obst, pair_entry_t, n_ops);

	/* Put definitions and uses into a single list. */
	for (int i = 0; i < n_ops; ++i) {
		pair_entry_t *entry = &entries[i];
		be_operand_t *op    = &insn->ops[i];
		entry->operand = op;
		entry->is_def  = i < use_start;
		if (entry->is_def) {
			ir_node *carrier = op->carrier;
			if (is_Proj(carrier)) {
				entry->pos = get_Proj_num(carrier);
			} else {
				assert(i == 0);
				entry->pos = 0;
			}
		} else {
			entry->pos = i - use_start;
		}
	}

	/**
	 * Only use most restricted node for each carrier.
	 */
	n_regs = env->cls->n_regs;
	QSORT(entries + use_start, n_ops - use_start, compare_entries);

	size_t          n_uses = 0;
	ir_node **const uses   = ALLOCAN(ir_node*, n_regs);
	for (int i = use_start; i < n_ops; ++i) {
		be_operand_t *op = entries[i].operand;
		if (list_has_irn_else_add(uses, n_uses, op->carrier)) {
			op->carrier = NULL;
		} else {
			++n_uses;
		}
	}

	/**
	 * Sort the list by register constraints (more restricted operands first).
	 * Use a stable compare function that only depends on the graph structure.
	 */
	QSORT(entries, n_ops, compare_entries);

	/* Greedily pair definitions/uses. */
	for (int i = 0; i < n_ops; ++i) {
		pair_entry_t *op_entry  = &entries[i];
		be_operand_t *op        = op_entry->operand;
		bool          op_is_def = op_entry->is_def;
		if (op->partner || !op->carrier ||
		    (!op_is_def && be_value_live_after(op->carrier, insn->irn))) {
			continue;
		}

		for (int j = i + 1; j < n_ops; ++j) {
			pair_entry_t *partner_entry  = &entries[j];
			be_operand_t *partner        = partner_entry->operand;
			bool          partner_is_def = partner_entry->is_def;
			if (!partner->partner &&
			    partner->carrier &&
			    op_is_def != partner_is_def &&
			    rbitsets_have_common(op->regs, partner->regs, n_regs) &&
			    (partner_is_def || !be_value_live_after(partner->carrier, insn->irn))) {
				op->partner      = partner;
				partner->partner = op;
				break;
			}
		}
	}
}

static void handle_constraints(be_chordal_env_t *const env, ir_node *const irn)
{
	void *const base = obstack_base(&env->obst);
	be_insn_t  *insn = be_scan_insn(env, irn);

	/* Perms inserted before the constraint handling phase are considered to be
	 * correctly precolored. These Perms arise during the ABI handling phase. */
	if (!insn || is_Phi(irn))
		goto end;

	/* Prepare the constraint handling of this node.
	 * Perms are constructed and Copies are created for constrained values
	 * interfering with the instruction. */
	ir_node *const perm = pre_process_constraints(env, &insn);

	/* find suitable in operands to the out operands of the node. */
	pair_up_operands(env, insn);

	/* Look at the in/out operands and add each operand (and its possible partner)
	 * to a bipartite graph (left: nodes with partners, right: admissible colors). */
	int                        n_alloc     = 0;
	int                  const n_regs      = env->cls->n_regs;
	ir_node            **const alloc_nodes = ALLOCAN(ir_node*, n_regs);
	pmap                *const partners    = pmap_create();
#if USE_HUNGARIAN
	hungarian_problem_t *const bp          = hungarian_new(n_regs, n_regs, HUNGARIAN_MATCH_PERFECT);
#else
	bipartite_t         *const bp          = bipartite_new(n_regs, n_regs);
#endif
	bool double_register_capable = (strcmp(env->cls->name, "sparc_fp") == 0);

	bitset_t *const available = bitset_alloca(n_regs);
	bitset_set_all(available);

	int *const assignment = ALLOCAN(int, n_regs);

	for (int i = 0, n_ops = insn->n_ops; i < n_ops; ++i) {
		/* If the operand has no partner or the partner has not been marked
		 * for allocation, determine the admissible registers and mark it
		 * for allocation by associating the node and its partner with the
		 * set of admissible registers via a bipartite graph. */
		be_operand_t *const op = &insn->ops[i];
		if (!op->carrier)
			continue;
		if (op->partner && pmap_contains(partners, op->partner->carrier))
			continue;

		ir_node *const partner = op->partner ? op->partner->carrier : NULL;
		pmap_insert(partners, op->carrier, partner);
		if (partner != NULL)
			pmap_insert(partners, partner, op->carrier);

		alloc_nodes[n_alloc] = op->carrier;

		DBG((dbg, LEVEL_2, "\tassociating %+F (reg width required: %d) and %+F\n", op->carrier, arch_get_irn_register_req_width(op->carrier), partner));

		unsigned const *const bs = get_decisive_partner_regs(op, n_regs);
		if (bs) {
#ifdef DEBUG_libfirm
			DBG((dbg, LEVEL_2, "\tallowed registers for %+F:", op->carrier, bs[0]));
			rbitset_foreach(bs, n_regs, col) {
				arch_register_t const *const reg = arch_register_for_index(env->cls, col);
				DB((dbg, LEVEL_2, " %s", reg->name));
			}
			DB((dbg, LEVEL_2, "\n"));
#endif

			if (double_register_capable) {
				bool assignment_found = false;
				rbitset_foreach(bs, n_regs, col) {
					if ((arch_get_irn_register_req_width(alloc_nodes[n_alloc]) != 2 || col % 2 == 0) && bitset_is_set(available, col)) {
						bool fits = true;
						for (int i = 1; i < arch_get_irn_register_req_width(op->carrier); i++) {
							if (!bitset_is_set(available, col + i)) {
								fits = false;
							}
						}
						if (!fits) {
							continue;
						}
						for (int i = 0; i < arch_get_irn_register_req_width(op->carrier); i++) {
							bitset_clear(available, col + i);
						}
						DBG((dbg, LEVEL_2, "\tassign register %s to %+F\n", arch_register_for_index(env->cls, col)->name, op->carrier));
						assignment[n_alloc] = col;
						assignment_found = true;
						break;
					}
				}
				if (!assignment_found) {
					DBG((dbg, LEVEL_2, "\tfailed to find register for %+F\n", op->carrier));
				}
			} else {
				rbitset_foreach(bs, n_regs, col) {
#if USE_HUNGARIAN
					hungarian_add(bp, n_alloc, col, 1);
#else
					if (arch_get_irn_register_req_width(alloc_nodes[n_alloc]) != 2 || col % 2 == 0) {
						bipartite_add(bp, n_alloc, col);
						ir_printf("bipartite: add edge from %+F to reg %s\n", alloc_nodes[n_alloc], arch_register_for_index(env->cls, col)->name);
					}
#endif
				}
			}

		} else {
			DBG((dbg, LEVEL_2, "\tallowed registers for %+F: none\n", op->carrier));
		}

		n_alloc++;
	}

	/* Put all nodes which live through the constrained instruction also to the
	 * allocation bipartite graph. They are considered unconstrained. */
	if (perm != NULL) {
		foreach_out_edge(perm, edge) {
			ir_node *const proj = get_edge_src_irn(edge);
			assert(is_Proj(proj));

			/* Don't insert a node twice. */
			if (pmap_contains(partners, proj))
				continue;

			assert(n_alloc < n_regs);

			alloc_nodes[n_alloc] = proj;
			pmap_insert(partners, proj, NULL);

			if (double_register_capable) {
				bool assignment_found = false;
				bitset_foreach(env->allocatable_regs, col) {
					if ((arch_get_irn_register_req_width(alloc_nodes[n_alloc]) != 2 || col % 2 == 0) && bitset_is_set(available, col)) {
						bool fits = true;
						for (int i = 1; i < arch_get_irn_register_req_width(alloc_nodes[n_alloc]); i++) {
							if (!bitset_is_set(available, col + i)) {
								fits = false;
							}
						}
						if (!fits) {
							continue;
						}
						for (int i = 0; i < arch_get_irn_register_req_width(alloc_nodes[n_alloc]); i++) {
							bitset_clear(available, col + i);
						}
						DBG((dbg, LEVEL_2, "\tassign register %s to %+F\n", arch_register_for_index(env->cls, col)->name, alloc_nodes[n_alloc]));
						assignment[n_alloc] = col;
						assignment_found = true;
						break;
					}
				}
				if (!assignment_found) {
					DBG((dbg, LEVEL_2, "\tfailed to find register for %+F\n", alloc_nodes[n_alloc]));
				}
			} else {
				bitset_foreach(env->allocatable_regs, col) {
#if USE_HUNGARIAN
					hungarian_add(bp, n_alloc, col, 1);
#else
					if (arch_get_irn_register_req_width(alloc_nodes[n_alloc]) != 2 || col % 2 == 0) {
						bipartite_add(bp, n_alloc, col);
						ir_printf("bipartite [2]: add edge from %+F to reg %s\n", alloc_nodes[n_alloc], arch_register_for_index(env->cls, col)->name);
					}
#endif
				}
			}

			n_alloc++;
		}
	}
	printf("n_alloc: %d\n", n_alloc);

	if (!double_register_capable) {
		/* Compute a valid register allocation. */
#if USE_HUNGARIAN
		hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);
		int const match_res = hungarian_solve(bp, assignment, NULL, 1);
		assert(match_res == 0 && "matching failed");
#else
		bipartite_matching(bp, assignment);
#endif
	}


	printf("assignment: [");
	for (int i = 0; i < n_alloc; ++i) {
		printf("%d ", assignment[i]);
	}
	printf("]\n");
	/* Assign colors obtained from the matching. */
	for (int i = 0; i < n_alloc; ++i) {
		assert(assignment[i] >= 0 && "there must have been a register assigned (node not register pressure faithful?)");
		arch_register_t const *const reg = arch_register_for_index(env->cls, assignment[i]);

		ir_node *const irn = alloc_nodes[i];

		assert(((arch_get_irn_register_req_width(irn) != 2) || (reg->index % 2 == 0)) && "wrong register index assigned as double register");
		arch_set_irn_register(irn, reg);
		DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", irn, reg->name));

		ir_node *const partner = pmap_get(ir_node, partners, irn);
		if (partner != NULL) {
			arch_set_irn_register(partner, reg);
			DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", partner, reg->name));
		}
	}

#if USE_HUNGARIAN
	hungarian_free(bp);
#else
	bipartite_free(bp);
#endif
	pmap_destroy(partners);

end:
	obstack_free(&env->obst, base);
}

/**
 * Handle constraint nodes in each basic block.
 * handle_constraints() inserts Perm nodes which perm
 * over all values live at the constrained node right in front
 * of the constrained node. These Perms signal a constrained node.
 * For further comments, refer to handle_constraints().
 */
static void constraints(ir_node *const bl, void *const data)
{
	be_chordal_env_t *const env = (be_chordal_env_t*)data;
	sched_foreach_safe(bl, irn) {
		handle_constraints(env, irn);
	}
}

static void assign(ir_node *const block, void *const env_ptr)
{
	be_chordal_env_t *const env  = (be_chordal_env_t*)env_ptr;
	struct list_head *const head = get_block_border_head(env, block);

	DBG((dbg, LEVEL_4, "Assigning colors for block %+F\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	foreach_border_head(head, b) {
		DBG((dbg, LEVEL_4, "\t%s %+F/%d\n", b->is_def ? "def" : "use",
		     b->irn, get_irn_idx(b->irn)));
	}

	bitset_t *const available = bitset_alloca(env->allocatable_regs->size);
	bitset_copy(available, env->allocatable_regs);

	printf("bitset available: [");
	for(unsigned i = 0; i < bitset_size(available); i++) {
		printf("%d ", bitset_is_set(available, i));
	}
	printf("]\n");

	/* Mind that the sequence of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work. */
	foreach_border_head(head, b) {
		ir_node *const irn = b->irn;

		/* Assign a color, if it is a local def. Global defs already have a
		 * color. */
		if (!b->is_def) {
			/* Make the color available upon a use. */
			arch_register_t const *const reg = arch_get_irn_register(irn);
			assert(reg && "Register must have been assigned");
			for (int i = 0; i < arch_get_irn_register_req_width(irn); i++) {
				bitset_set(available, reg->index + i);
			}
			//bitset_set(available, reg->index);
		} else {
			arch_register_t const *const reg = arch_get_irn_register(irn);
			/* All live-ins must have a register assigned. (The dominators were
			 * allocated before.) */
			assert(b->is_real || reg);
			unsigned col;
			if (reg) {
				DBG((dbg, LEVEL_4, "%+F has reg %s\n", irn, reg->name));
				col = reg->index;
				for (int i = 0; i < arch_get_irn_register_req_width(irn); i++) {
					if (!bitset_is_set(available, col + i)) {
						ir_printf("%+F\n", irn);
					}
					assert(bitset_is_set(available, col + i) && "pre-colored register must be free");
				}
			} else {
				assert(!arch_irn_is_ignore(irn));
				col = get_next_free_reg(available, arch_get_irn_register_req_width(irn));
				arch_set_irn_register_idx(irn, col);
			}
			for (int i = 0; i < arch_get_irn_register_req_width(irn); i++) {
				bitset_clear(available, col + i);
			}

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n", arch_get_irn_register(irn)->name, col, irn));
		}
	}
}

static void be_ra_chordal_color(be_chordal_env_t *const chordal_env)
{
	ir_graph *const irg = chordal_env->irg;
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	be_assure_live_sets(irg);

	/* Handle register targeting constraints */
	be_timer_push(T_CONSTR);
	dom_tree_walk_irg(irg, constraints, NULL, chordal_env);
	be_timer_pop(T_CONSTR);

	dump_ir_graph(irg, "constraints");

	be_chordal_dump(BE_CH_DUMP_CONSTR, irg, chordal_env->cls, "constr");

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, create_borders, NULL, chordal_env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, chordal_env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal)
void be_init_chordal(void)
{
	static be_ra_chordal_coloring_t coloring = {
		be_ra_chordal_color
	};
	FIRM_DBG_REGISTER(dbg, "firm.be.chordal");

	be_register_chordal_coloring("default", &coloring);
}
