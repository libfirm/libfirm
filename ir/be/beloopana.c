/**
 * Author:      Christian Wuerdig
 * Date:        2007/02/19
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 *
 * Compute register pressure in loops
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "set.h"
#include "pset.h"
#include "irnode.h"
#include "irtools.h"
#include "irloop_t.h"
#include "error.h"
#include "debug.h"

#include "bearch_t.h"
#include "belive.h"
#include "besched.h"
#include "beloopana.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL);

#define HASH_LOOP_INFO(info) (HASH_PTR((info)->loop) ^ HASH_PTR((info)->cls))

typedef struct _be_loop_info_t {
	ir_loop                     *loop;
	const arch_register_class_t *cls;
	unsigned                    max_pressure;
} be_loop_info_t;

struct _be_loopana_t {
	set      *data;
	be_irg_t *birg;
};

static int cmp_loop_info(const void *a, const void *b, size_t sz) {
	const be_loop_info_t *i1 = a;
	const be_loop_info_t *i2 = b;

	return ! (i1->loop == i2->loop && i1->cls == i2->cls);
}

/**
 * Compute the highest register pressure in a block.
 * @param loop_ana  The loop ana object.
 * @param block     The block to compute pressure for.
 * @param cls       The register class to compute pressure for.
 * @return The highest register pressure in the given block.
 */
static unsigned be_compute_block_pressure(be_loopana_t *loop_ana, ir_node *block, const arch_register_class_t *cls) {
	const be_irg_t   *birg       = loop_ana->birg;
	const arch_env_t *aenv       = be_get_birg_arch_env(birg);
	be_lv_t          *lv         = be_get_birg_liveness(birg);
	pset             *live_nodes = pset_new_ptr_default();
	ir_node          *irn;
	int              max_live;

	DBG((dbg, LEVEL_1, "Processing Block %+F\n", block));

	/* determine largest pressure with this block */
	live_nodes = be_liveness_end_of_block(lv, aenv, cls, block, live_nodes);
	max_live   = pset_count(live_nodes);

	sched_foreach_reverse(block, irn) {
		int cnt;

		if (is_Phi(irn))
			break;

		live_nodes = be_liveness_transfer(aenv, cls, irn, live_nodes);
		cnt        = pset_count(live_nodes);
		max_live   = MAX(cnt, max_live);
	}

	DBG((dbg, LEVEL_1, "Finished with Block %+F (%s %u)\n", block, cls->name, max_live));

	del_pset(live_nodes);
	return max_live;
}

/**
 * Compute the highest register pressure in a loop and it's sub-loops.
 * @param loop_ana  The loop ana object.
 * @param loop      The loop to compute pressure for.
 * @param cls       The register class to compute pressure for.
 * @return The highest register pressure in the given loop.
 */
static unsigned be_compute_loop_pressure(be_loopana_t *loop_ana, ir_loop *loop, const arch_register_class_t *cls) {
	int            i, max;
	unsigned       pressure;
	be_loop_info_t *entry, key;

	DBG((dbg, LEVEL_1, "\nProcessing Loop %d\n", loop->loop_nr));
	assert(get_loop_n_elements(loop) > 0);
	pressure = 0;

	/* determine maximal pressure in all loop elements */
	for (i = 0, max = get_loop_n_elements(loop); i < max; ++i) {
		unsigned     son_pressure;
		loop_element elem = get_loop_element(loop, i);

		switch (*elem.kind) {
			case k_ir_node:
				son_pressure = be_compute_block_pressure(loop_ana, elem.node, cls);
				break;
			case k_ir_loop:
				son_pressure = be_compute_loop_pressure(loop_ana, elem.son, cls);
				break;
			default:
				panic("Unknown element found in loop");
				break;
		}

		pressure = MAX(pressure, son_pressure);
   	}
	DBG((dbg, LEVEL_1, "Done with loop %d, pressure %u for class %s\n", loop->loop_nr, pressure, cls->name));

	/* update info in set */
	key.loop            = loop;
	key.cls             = cls;
	key.max_pressure    = 0;
	entry               = set_insert(loop_ana->data, &key, sizeof(key), HASH_LOOP_INFO(&key));
	entry->max_pressure = MAX(entry->max_pressure, pressure);

	return pressure;
}

/**
 * Compute the register pressure for a class of all loops in the birg.
 * @param birg  The backend irg object
 * @param cls   The register class to compute the pressure for
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure_cls(be_irg_t *birg,
                                       const arch_register_class_t *cls) {
	ir_graph     *irg      = be_get_birg_irg(birg);
	be_loopana_t *loop_ana = xmalloc(sizeof(*loop_ana));

	loop_ana->data = new_set(cmp_loop_info, 16);
	loop_ana->birg = birg;

	DBG((dbg, LEVEL_1, "\n=====================================================\n", cls->name));
	DBG((dbg, LEVEL_1, " Computing register pressure for class %s:\n", cls->name));
	DBG((dbg, LEVEL_1, "=====================================================\n", cls->name));

	be_compute_loop_pressure(loop_ana, get_irg_loop(irg), cls);

	return loop_ana;
}

/**
 * Compute the register pressure for all classes of all loops in the birg.
 * @param birg  The backend irg object
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure(be_irg_t *birg) {
	ir_graph         *irg      = be_get_birg_irg(birg);
	be_loopana_t     *loop_ana = xmalloc(sizeof(*loop_ana));
	ir_loop          *irg_loop = get_irg_loop(irg);
	const arch_env_t *arch_env = be_get_birg_arch_env(birg);
	const arch_isa_t *isa      = arch_env->isa;
	int               i;

	loop_ana->data = new_set(cmp_loop_info, 16);
	loop_ana->birg = birg;

	for (i = arch_isa_get_n_reg_class(isa) - 1; i >= 0; --i) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		DBG((dbg, LEVEL_1, "\n=====================================================\n", cls->name));
		DBG((dbg, LEVEL_1, " Computing register pressure for class %s:\n", cls->name));
		DBG((dbg, LEVEL_1, "=====================================================\n", cls->name));
		be_compute_loop_pressure(loop_ana, irg_loop, cls);
	}

	return loop_ana;
}

/**
 * Returns the computed register pressure for the given class and loop.
 * @return The pressure or INT_MAX if not found
 */
unsigned be_get_loop_pressure(be_loopana_t *loop_ana, const arch_register_class_t *cls, ir_loop *loop) {
	unsigned pressure = INT_MAX;
	be_loop_info_t *entry, key;

	assert(cls && loop);

	key.loop = loop;
	key.cls  = cls;
	entry    = set_find(loop_ana->data, &key, sizeof(key), HASH_LOOP_INFO(&key));

	if (entry)
		pressure = entry->max_pressure;
	else
		assert(0 && "Pressure not computed for given class and loop object.");

	return pressure;
}

/**
 * Frees the loop analysis object.
 */
void be_free_loop_pressure(be_loopana_t *loop_ana) {
	del_set(loop_ana->data);
	xfree(loop_ana);
}

void be_init_loopana(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.loopana");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_loopana);
