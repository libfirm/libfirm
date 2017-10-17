/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Compute register pressure in loops.
 * @author      Christian Wuerdig
 * @date        19.02.2007
 */
#include "beloopana.h"

#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "besched.h"
#include "debug.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "set.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define HASH_LOOP_INFO(info) (hash_ptr((info)->loop) ^ hash_ptr((info)->cls))

typedef struct be_loop_info_t {
	ir_loop                     *loop;
	const arch_register_class_t *cls;
	unsigned                     max_pressure;
} be_loop_info_t;

struct be_loopana_t {
	set *data;
};

static int cmp_loop_info(const void *a, const void *b, size_t size)
{
	(void)size;
	const be_loop_info_t *i1 = (const be_loop_info_t*)a;
	const be_loop_info_t *i2 = (const be_loop_info_t*)b;
	return i1->loop != i2->loop || i1->cls != i2->cls;
}

/**
 * Compute the highest register pressure in a block.
 * @param block     The block to compute pressure for.
 * @param cls       The register class to compute pressure for.
 * @return The highest register pressure in the given block.
 */
static unsigned be_compute_block_pressure(ir_node *const block, arch_register_class_t const *const cls)
{

	DBG((dbg, LEVEL_1, "Processing Block %+F\n", block));

	/* determine largest pressure with this block */
	ir_nodeset_t live_nodes;
	ir_nodeset_init(&live_nodes);
	be_lv_t *const lv = be_get_irg_liveness(get_irn_irg(block));
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	unsigned max_pressure = 0;

	// use iterator to sum up register widths
	ir_nodeset_iterator_t iterator;
	ir_nodeset_iterator_init(&iterator, &live_nodes);
	for (ir_node *next = ir_nodeset_iterator_next(&iterator); next != NULL; next = ir_nodeset_iterator_next(&iterator)) {
		max_pressure += arch_get_irn_register_req(next)->width;
	}

	sched_foreach_non_phi_reverse(block, irn) {
		be_liveness_transfer(cls, irn, &live_nodes);
		unsigned cnt = 0;
		ir_nodeset_iterator_init(&iterator, &live_nodes);
		for (ir_node *next = ir_nodeset_iterator_next(&iterator); next != NULL; next = ir_nodeset_iterator_next(&iterator)) {
			cnt += arch_get_irn_register_req(next)->width;
		}
		max_pressure = MAX(cnt, max_pressure);
	}
	DBG((dbg, LEVEL_1, "Finished with Block %+F (%s %zu)\n", block, cls->name,
	     max_pressure));

	ir_nodeset_destroy(&live_nodes);
	return max_pressure;
}

/**
 * Compute the highest register pressure in a loop and its sub-loops.
 * @param loop_ana  The loop ana object.
 * @param loop      The loop to compute pressure for.
 * @param cls       The register class to compute pressure for.
 * @return The highest register pressure in the given loop.
 */
static unsigned be_compute_loop_pressure(be_loopana_t *loop_ana, ir_loop *loop,
                                         const arch_register_class_t *cls)
{
	DBG((dbg, LEVEL_1, "\nProcessing Loop %ld\n", loop->loop_nr));

	/* determine maximal pressure in all loop elements */
	unsigned pressure = 0;
	for (size_t i = 0, max = get_loop_n_elements(loop); i < max; ++i) {
		unsigned     son_pressure;
		loop_element elem = get_loop_element(loop, i);

		if (*elem.kind == k_ir_node) {
			son_pressure = be_compute_block_pressure(elem.node, cls);
		} else {
			assert(*elem.kind == k_ir_loop);
			son_pressure = be_compute_loop_pressure(loop_ana, elem.son, cls);
		}

		pressure = MAX(pressure, son_pressure);
	}
	DBG((dbg, LEVEL_1, "Done with loop %ld, pressure %u for class %s\n", loop->loop_nr, pressure, cls->name));

	/* update info in set */
	be_loop_info_t key;
	key.loop            = loop;
	key.cls             = cls;
	key.max_pressure    = 0;
	be_loop_info_t *entry
		= set_insert(be_loop_info_t, loop_ana->data, &key, sizeof(key), HASH_LOOP_INFO(&key));
	entry->max_pressure = MAX(entry->max_pressure, pressure);

	return pressure;
}

be_loopana_t *be_new_loop_pressure(ir_graph *const irg, arch_register_class_t const *const cls)
{
	be_loopana_t *loop_ana = XMALLOC(be_loopana_t);
	loop_ana->data = new_set(cmp_loop_info, 16);

	DBG((dbg, LEVEL_1, "Computing loop register pressure for class %s:\n",
	     cls->name));

	assure_loopinfo(irg);

	be_compute_loop_pressure(loop_ana, get_irg_loop(irg), cls);

	return loop_ana;
}

unsigned be_get_loop_pressure(be_loopana_t *loop_ana, const arch_register_class_t *cls, ir_loop *loop)
{
	be_loop_info_t key;
	key.loop = loop;
	key.cls  = cls;
	be_loop_info_t *entry
		= set_find(be_loop_info_t, loop_ana->data, &key, sizeof(key), HASH_LOOP_INFO(&key));

	return entry->max_pressure;
}

void be_free_loop_pressure(be_loopana_t *loop_ana)
{
	del_set(loop_ana->data);
	free(loop_ana);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_loopana)
void be_init_loopana(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.loopana");
}
