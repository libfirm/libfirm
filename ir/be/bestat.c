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
 * @brief       Provides several statistic functions for the backend.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>

#include "irnode_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "execfreq.h"
#include "dbginfo_t.h"
#include "firmstat_t.h"
#include "irtools.h"
#include "pset.h"

#include "bearch_t.h"
#include "bestat.h"
#include "belive_t.h"
#include "besched.h"
#include "benode_t.h"

#ifdef FIRM_STATISTICS

typedef struct _be_stat_irg_t {
	ir_graph         *irg;       /**< the irg, the statistic is about */
	pset             *phases;    /**< node statistics for each phase  */
	struct obstack   obst;       /**< the obstack containing the information */
	const arch_env_t *arch_env;  /**< the current arch env */
} be_stat_irg_t;

typedef struct _be_stat_phase_t {
	const arch_env_t *arch_env;  /**< the current arch env */
	const char       *phase;     /**< the name of the phase the statistic is about */
	unsigned long    num_nodes;  /**< overall number of reachable nodes in the irg */
	unsigned long    num_data;   /**< number of data nodes ((mode_datab && ! Proj && ! Phi)  || mode_T) */
	unsigned long    num_proj;   /**< number of Projs */
	unsigned long    num_phi;    /**< number of Phis */
	unsigned long    num_load;   /**< number of Loads */
	unsigned long    num_store;  /**< number of Stores */
	unsigned long    num_spill;  /**< number of Spills */
	unsigned long    num_reload; /**< number of Reloads */
} be_stat_phase_t;

static set *be_stat_data = NULL;

static int cmp_stat_phase(const void *a, const void *b) {
	const be_stat_phase_t *p1 = a;
	const be_stat_phase_t *p2 = b;

	return p1->phase != p2->phase;
}

static int cmp_stat_data(const void *a, const void *b, size_t len) {
	const be_stat_irg_t *p1 = a;
	const be_stat_irg_t *p2 = b;
	(void) len;

	return p1->irg != p2->irg;
}

static be_stat_irg_t *find_stat_irg_entry(ir_graph *irg) {
	be_stat_irg_t *entry, key;

	if (! be_stat_data)
		return NULL;

	key.irg = irg;
	entry   = set_find(be_stat_data, &key, sizeof(key), HASH_PTR(irg));

	return entry;
}

static be_stat_irg_t *get_stat_irg_entry(ir_graph *irg) {
	be_stat_irg_t *entry, key;

	if (! be_stat_data)
		return NULL;

	entry = find_stat_irg_entry(irg);

	if (! entry) {
		key.irg = irg;
		entry   = set_insert(be_stat_data, &key, sizeof(key), HASH_PTR(irg));
	}

	return entry;
}

struct a_pressure_walker {
	be_irg_t *birg;
	be_lv_t *lv;
};

/**
 * Collect reg pressure statistics per block and per class.
 */
static void stat_reg_pressure_block(ir_node *block, void *data) {
	struct a_pressure_walker *env = data;
	be_irg_t         *birg = env->birg;
	ir_graph         *irg  = be_get_birg_irg(birg);
	const arch_env_t *aenv = be_get_birg_arch_env(birg);
	int i, n = arch_isa_get_n_reg_class(aenv->isa);

	for (i = 0; i < n; i++) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(aenv->isa, i);
		ir_node  *irn;
		pset     *live_nodes = pset_new_ptr(64);
		int       max_live;

		live_nodes = be_liveness_end_of_block(env->lv, aenv, cls, block, live_nodes);
		max_live   = pset_count(live_nodes);

		sched_foreach_reverse(block, irn) {
			int cnt;

			if(is_Phi(irn))
				break;

			live_nodes = be_liveness_transfer(aenv, cls, irn, live_nodes);
			cnt        = pset_count(live_nodes);
			max_live   = cnt < max_live ? max_live : cnt;
		}

		stat_be_block_regpressure(irg, block, max_live, cls->name);
	}
}

void be_do_stat_reg_pressure(be_irg_t *birg) {
	ir_graph *irg = be_get_birg_irg(birg);

	if (stat_is_active()) {
		struct a_pressure_walker w;

		w.birg = birg;
		w.lv   = be_liveness(irg);
		/* Collect register pressure information for each block */
		irg_block_walk_graph(irg, stat_reg_pressure_block, NULL, &w);
		be_liveness_free(w.lv);
	}
}

/**
 * Notify statistic module about amount of ready nodes.
 */
void be_do_stat_sched_ready(ir_node *block, const ir_nodeset_t *ready_set) {
	if (stat_is_active()) {
		stat_be_block_sched_ready(get_irn_irg(block), block, MIN(ir_nodeset_size(ready_set), 5));
	}
}

/**
 * Pass information about a perm to the statistic module.
 */
void be_do_stat_perm(const char *class_name, int n_regs, ir_node *perm, ir_node *block, int n, int real_size) {
	if (stat_is_active()) {
		stat_be_block_stat_perm(class_name, n_regs, perm, block, n, real_size);
	}
}

/**
 * Pass information about a cycle or chain in a perm to the statistic module.
 */
void be_do_stat_permcycle(const char *class_name, ir_node *perm, ir_node *block, int is_chain, int n_elems, int n_ops) {
	if (stat_is_active()) {
		stat_be_block_stat_permcycle(class_name, perm, block, is_chain, n_elems, n_ops);
	}
}

/**
 * Updates nodes statistics.
 */
static void do_nodes_stat(ir_node *irn, void *env) {
	be_stat_phase_t  *phase = env;
	ir_mode          *mode;
	ir_opcode        opc;
	arch_irn_class_t irn_class;

	if (is_Block(irn))
		return;

	mode = get_irn_mode(irn);
	opc  = get_irn_opcode(irn);

	phase->num_nodes++;

	/* check for nodes we want to ignore */
	if (be_is_Keep(irn)     ||
		be_is_CopyKeep(irn) ||
		opc == iro_Start    ||
		opc == iro_End)
		return;

	if (is_Proj(irn) && (mode != mode_X)) {
		phase->num_proj++;
		return;
	}
	else if (is_Phi(irn)) {
		phase->num_phi++;
		return;
	}
	else if (mode_is_datab(mode) || ((mode == mode_T) && ! is_be_node(irn)) || (is_Proj(irn) && (mode == mode_X)))
		phase->num_data++;

	if (opc == iro_Load)
		phase->num_load++;
	else if (opc == iro_Store)
		phase->num_store++;

	irn_class = arch_irn_classify(phase->arch_env, irn);
	if (irn_class & arch_irn_class_spill)
		phase->num_spill++;
	else if (irn_class & arch_irn_class_reload)
		phase->num_reload++;
	else if (irn_class & arch_irn_class_stackparam)
		phase->num_load++;
	else if (irn_class & arch_irn_class_load)
		phase->num_load++;
	else if (irn_class & arch_irn_class_store)
		phase->num_store++;
}

/**
 * Collects node statistics.
 *
 * @param irg      the to do statistics for
 * @param phase    the phase to collect the statistic for
 */
void be_do_stat_nodes(ir_graph *irg, const char *phase) {
	be_stat_irg_t   *irg_entry;
	be_stat_phase_t *phase_entry, phase_key;

	irg_entry = find_stat_irg_entry(irg);

	if (! irg_entry)
		return;

	phase_key.phase = phase;
	phase_entry     = pset_find_ptr(irg_entry->phases, &phase_key);

	if (! phase_entry) {
		phase_entry = obstack_alloc(&irg_entry->obst, sizeof(*phase_entry));
		phase_entry = pset_insert(irg_entry->phases, phase_entry, HASH_PTR(phase));
	}
	memset(phase_entry, 0, sizeof(*phase_entry));

	phase_entry->phase    = phase;
	phase_entry->arch_env = irg_entry->arch_env;

	irg_walk_blkwise_graph(irg_entry->irg, NULL, do_nodes_stat, phase_entry);
}

/**
 * Dumps statistics about nodes (called from dump_snapshot)
 */
static void be_dump_node_stat(dumper_t *dmp, graph_entry_t *entry) {
	be_stat_irg_t   *stat_irg = find_stat_irg_entry(entry->irg);
	be_stat_phase_t *phase;

	if (! stat_irg || ! stat_irg->phases)
		return;

	fprintf(dmp->f, "===> BE NODE STATISTIC BEGIN <===\n");

	foreach_pset(stat_irg->phases, phase) {
		fprintf(dmp->f, "--> Phase: %s\n", phase->phase);
		fprintf(dmp->f, "# nodes:      %ld\n", phase->num_nodes);
		fprintf(dmp->f, "# data nodes: %ld\n", phase->num_data);
		fprintf(dmp->f, "# Proj:       %ld\n", phase->num_proj);
		fprintf(dmp->f, "# Phi:        %ld\n", phase->num_phi);
		fprintf(dmp->f, "# Load:       %ld\n", phase->num_load);
		fprintf(dmp->f, "# Store:      %ld\n", phase->num_store);
		fprintf(dmp->f, "# Spill:      %ld\n", phase->num_spill);
		fprintf(dmp->f, "# Reload:     %ld\n", phase->num_reload);
	}

	fprintf(dmp->f, "===> BE NODE STATISTIC END <===\n");
}

/**
 * Returns a be statistic object for the given irg.
 */
void be_stat_init_irg(const arch_env_t *arch_env, ir_graph *irg) {
	static int reg_func  = 1;

	if (stat_is_active()) {
		be_stat_irg_t *stat_irg;

		if (! be_stat_data)
			be_stat_data = new_set(cmp_stat_data, 8);

		stat_irg           = get_stat_irg_entry(irg);
		stat_irg->irg      = irg;
		stat_irg->phases   = new_pset(cmp_stat_phase, 8);
		stat_irg->arch_env = arch_env;
		obstack_init(&stat_irg->obst);

		if (reg_func) {
			/* first init: register dumper */
			stat_register_dumper_func(be_dump_node_stat);
			reg_func = 0;
		}
	}
}
#endif /* FIRM_STATISTICS */

typedef struct _estimate_irg_costs_env_t {
	const arch_env_t *arch_env;
	ir_exec_freq     *execfreqs;
	double           costs;
} estimate_irg_costs_env_t;

static void estimate_block_costs(ir_node *block, void *data)
{
	estimate_irg_costs_env_t *env = data;
	ir_node *node;
	double  costs = 0.0;

	sched_foreach(block, node) {
		costs += arch_get_op_estimated_cost(env->arch_env, node);
	}

	env->costs += costs * get_block_execfreq(env->execfreqs, block);
}

double be_estimate_irg_costs(ir_graph *irg, const arch_env_t *arch_env, ir_exec_freq *execfreqs)
{
	estimate_irg_costs_env_t env;

	env.arch_env  = arch_env;
	env.execfreqs = execfreqs;
	env.costs     = 0.0;

	irg_block_walk_graph(irg, estimate_block_costs, NULL, &env);

	return env.costs;
}

#ifdef FIRM_STATISTICS

const char *be_stat_tags[STAT_TAG_LAST];
FILE       *be_stat_file = NULL;

void be_init_stat_file(const char *stat_file_name, const char *sourcefilename)
{
	static char time_str[32];

	assert(be_stat_file == NULL);

	/* if we want to do some statistics, push the environment. */
	if (strlen(stat_file_name) == 0)
		return;

	be_stat_file = fopen(stat_file_name, "at");
	if (be_stat_file == NULL) {
		fprintf(stderr, "Warning couldn't open statfile '%s'\n", stat_file_name);
		return;
	}

	/* initialize the statistics tags */
	ir_snprintf(time_str, sizeof(time_str),"%u", time(NULL));

	be_stat_tags[STAT_TAG_FILE] = sourcefilename;
	be_stat_tags[STAT_TAG_TIME] = time_str;
	be_stat_tags[STAT_TAG_IRG]  = "<all>";
	be_stat_tags[STAT_TAG_CLS]  = "<all>";

	be_stat_ev_push(be_stat_tags, STAT_TAG_LAST, be_stat_file);
}

void be_close_stat_file()
{
	be_stat_ev_pop();
	if (be_stat_file != NULL) {
		fclose(be_stat_file);
		be_stat_file = NULL;
	}
}

#else /* FIRM_STATISTICS */

void (be_stat_init_irg)(const arch_env_t *arch_env, ir_graph *irg) {}
void (be_do_stat_nodes)(ir_graph *irg, const char *phase) {}
void (be_do_stat_reg_pressure)(be_irg_t *birg) {}
void (be_do_stat_sched_ready)(ir_node *block, ir_nodeset_t *ready_set) {}
void (be_do_stat_perm)(const char *class_name, int n_regs, ir_node *perm, ir_node *block, int n, int real_size) {}

#endif /* FIRM_STATISTICS */
