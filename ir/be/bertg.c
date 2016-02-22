#include "bertg.h"

#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irdump.h"
#include "irprintf.h"
#include "irtools.h"

#include "be_t.h"
#include "beutil.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "belive_t.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespillutil.h"
#include "statev_t.h"
#include "beirg.h"
#include "beintlive_t.h"

#include "gen_sparc_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg_rtg = NULL;)

static const arch_register_class_t *cls;

static const char *get_reg_name(unsigned index)
{
	return arch_register_for_index(cls, index)->name;
}

void print_parcopy(unsigned *parcopy_orig, unsigned *n_used_orig,
                   const arch_register_class_t *c, firm_dbg_module_t *dbg)
{
	cls = c;
	const unsigned n_regs = cls->n_regs;
	unsigned parcopy[n_regs];
	unsigned n_used[n_regs];
	memcpy(parcopy, parcopy_orig, sizeof(unsigned) * n_regs);
	memcpy(n_used, n_used_orig, sizeof(unsigned) * n_regs);

	for (unsigned i = 0; i < n_regs; ++i)
		if (n_used_orig[i] != 0)
			DB((dbg, LEVEL_2, "#users[%s(%u)] = %u\n", get_reg_name(i), i, n_used_orig[i]));

	unsigned comp[n_regs];
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* Perfect, end of a chain. */
		unsigned len = 0;
		comp[len++] = r;
		unsigned s = r;
		while (n_used[s] == 0 && parcopy[s] != s) {
			unsigned src = parcopy[s];
			parcopy[s] = s;
			comp[len++] = src;
			assert(n_used[src] > 0);
			--n_used[src];
			s = src;
		}

		/* Reverse. */
		for (unsigned i = 0; i < len / 2; ++i) {
			unsigned t = comp[i];
			comp[i] = comp[len - i - 1];
			comp[len - i - 1] = t;
		}

		for (unsigned i = 0; i + 1 < len; ++i)
			DB((dbg, LEVEL_2, "%s(%u) -> ", get_reg_name(comp[i]), comp[i]));
		DB((dbg, LEVEL_2, "%s(%i)\n", get_reg_name(comp[len - 1]), comp[len - 1]));
	}

	/* Only cycles left. */
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r) {
			++r;
			continue;
		}

		assert(n_used[r] == 1);

		unsigned len = 0;
		unsigned s = r;
		while (parcopy[s] != s) {
			unsigned src = parcopy[s];
			comp[len++] = s;
			parcopy[s] = s;
			s = src;
		}

		for (unsigned i = 0; i < len / 2; ++i) {
			unsigned t = comp[i];
			comp[i] = comp[len - i - 1];
			comp[len - i - 1] = t;
		}

		for (unsigned i = 0; i < len; ++i)
			DB((dbg, LEVEL_2, "%s(%u) -> ", get_reg_name(comp[i]), comp[i]));
		DB((dbg, LEVEL_2, "%s(%u)\n", get_reg_name(comp[0]), comp[0]));
	}
}

static void mark_cycle_parts(bool *part_of_cycle, unsigned *parcopy_orig,
                             unsigned *n_used_orig)
{
	const unsigned n_regs = cls->n_regs;
	unsigned parcopy[n_regs];
	unsigned n_used[n_regs];
	memcpy(parcopy, parcopy_orig, sizeof(unsigned) * n_regs);
	memcpy(n_used, n_used_orig, sizeof(unsigned) * n_regs);
	memset(part_of_cycle, 0, sizeof(bool) * n_regs);

	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* Perfect, end of a chain. */
		unsigned s = r;
		while (n_used[s] == 0 && parcopy[s] != s) {
			part_of_cycle[s] = false;

			unsigned src = parcopy[s];
			parcopy[s] = s;
			assert(n_used[src] > 0);
			--n_used[src];
			s = src;
		}
	}

	/* Only cycles left. */
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r) {
			if (n_used[r] > 0)
				part_of_cycle[r] = true;
			++r;
			continue;
		}

		assert(n_used[r] == 1);

		unsigned s = r;
		while (parcopy[s] != s) {
			part_of_cycle[s] = true;
			unsigned src = parcopy[s];
			parcopy[s] = s;
			s = src;
		}
	}
}

static unsigned find_longest_chain(unsigned *parcopy, unsigned *n_used,
                                   unsigned fork_reg)
{
	/* fork_reg must be a fork. */
	assert(n_used[fork_reg] > 1);
	const unsigned n_regs = cls->n_regs;

	DB((dbg_rtg, LEVEL_2, "  Searching for longest chain starting at %s\n", get_reg_name(fork_reg)));

	/* Search the longest chain starting from r. */
	unsigned max_len = 0;
	unsigned max_dst = (unsigned) -1;

	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg || n_used[to_reg] > 0) {
			++to_reg;
			continue;
		}

		DB((dbg_rtg, LEVEL_2, "  Found candidate ending in %s\n", get_reg_name(to_reg)));
		unsigned r   = to_reg;
		unsigned len = 0;
		while (r != parcopy[r]) {
			unsigned src = parcopy[r];
			++len;
			if (src == fork_reg && len > max_len) {
				DB((dbg_rtg, LEVEL_2, "  Chain starts in %s, continues via %s, length %u\n", get_reg_name(fork_reg), get_reg_name(r), len));
				max_len = len;
				max_dst = r;
				break;
			}
			r = src;
		}

		++to_reg;
	}

	return max_dst;
}

void decompose_rtg(unsigned *parcopy, unsigned *n_used,
                   unsigned *restore_srcs, unsigned *restore_dsts,
                   unsigned *num_restores, const arch_register_class_t *c)
{
	FIRM_DBG_REGISTER(dbg_rtg, "ir.be.rtg");
	cls = c;
	unsigned n_regs = cls->n_regs;

	DB((dbg_rtg, LEVEL_2, "Searching for out-of-cycle propagations.\n"));
	bool is_part_of_cycle[n_regs];
	mark_cycle_parts(is_part_of_cycle, parcopy, n_used);
	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg) {
			++to_reg;
			continue;
		}

		if (is_part_of_cycle[from_reg] && !is_part_of_cycle[to_reg]) {
			DB((dbg_rtg, LEVEL_2, "  Found out-of-cycle propagation %s -> %s\n", get_reg_name(from_reg), get_reg_name(to_reg)));
			unsigned new_src = (unsigned) -1;
			for (unsigned src = 0; src < n_regs; ++src) {
				if (parcopy[src] == from_reg && is_part_of_cycle[src]) {
					/* new_src must be unambiguous. */
					new_src = src;
					break;
				}
			}
			assert((new_src != ((unsigned) -1)) && "Could not find new source for out-of-cycle propagation");

			restore_srcs[*num_restores] = new_src;
			restore_dsts[*num_restores] = to_reg;
			++(*num_restores);
			DB((dbg_rtg, LEVEL_2, "  Added restore %s -> %s\n", get_reg_name(new_src), get_reg_name(to_reg)));
			--n_used[from_reg];
			parcopy[to_reg] = to_reg;
		}

		++to_reg;
	}
	DB((dbg_rtg, LEVEL_2, "Finished search for out-of-cycle propagation.\n"));

	DB((dbg_rtg, LEVEL_2, "Searching for forks.\n"));
	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg || n_used[to_reg] > 0) {
			++to_reg;
			continue;
		}

		/* Found the end of a chain, follow it. */
		unsigned r = to_reg;
		while (r != parcopy[r]) {
			r = parcopy[r];
			if (n_used[r] > 1) {
				/* Found a fork. */
				DB((dbg_rtg, LEVEL_2, "  Found a fork at %s\n", get_reg_name(r)));
				unsigned longest_next = find_longest_chain(parcopy, n_used, r);
				DB((dbg_rtg, LEVEL_2, "  Longest chain from %s via %s\n", get_reg_name(r), get_reg_name(longest_next)));

				/* Reroute all others. */
				for (unsigned dst = 0; dst < n_regs; ++dst) {
					if (dst != longest_next && dst != r && parcopy[dst] == r) {
						restore_srcs[*num_restores] = longest_next;
						restore_dsts[*num_restores] = dst;
						++(*num_restores);
						DB((dbg_rtg, LEVEL_2, "  Added restore %s -> %s\n", get_reg_name(longest_next), get_reg_name(dst)));
						--n_used[r];
						parcopy[dst] = dst;
					}
				}
			}
		}

		++to_reg;
	}
	DB((dbg_rtg, LEVEL_2, "Finished searching for forks.\n"));

	DB((dbg_rtg, LEVEL_2, "Current parallel copy:\n"));
#ifndef NDEBUG
	print_parcopy(parcopy, n_used, cls, dbg_rtg);
#endif
}
