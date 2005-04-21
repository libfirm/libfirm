/**
 * @author Daniel Grund
 * @date 19.04.2005
 */
#include <stdlib.h>
#include "list.h"
#include "irprog.h"
#include "beutil.h"
#include "becopystat.h"

struct _irg_stat_t {
	struct list_head chain;
	ir_graph *irg;
	const char *irg_name;
	int interferers, lb, max;
	int copies[3];
	//TODO double ilp_time;
};

typedef struct _all_stat_t {
	const char *unit_name;
	struct list_head irgs;
} all_stat_t;

static all_stat_t *all_stats = NULL;

#define irg_list_entry(lh) list_entry(lh, irg_stat_t, chain)

irg_stat_t *new_irg_stat(copy_opt_t *co) {
	irg_stat_t *is, *curr;

	if (!all_stats) {
		all_stats = calloc(1, sizeof(*all_stats));
		all_stats->unit_name = get_irp_prog_name();
		INIT_LIST_HEAD(&all_stats->irgs);
		is = calloc(1, sizeof(*is));
		is->irg_name = "CUMULATIVE";
		list_add_tail(&is->chain, &all_stats->irgs);
	}

	/* look if we had this irg already */
	list_for_each_entry(irg_stat_t, curr, &all_stats->irgs, chain)
		if (curr->irg == co->irg)
			return curr;

	/* else create a new entry */
	is = calloc(1, sizeof(*is));
	is->irg = co->irg;
	is->irg_name = get_entity_name(get_irg_entity(co->irg));
	list_add_tail(&is->chain, &all_stats->irgs);
	return is;
}

void irg_stat_count(irg_stat_t *is, copy_opt_t *co, int phase) {
	unit_t *curr;
	int max = 0, copies = 0;

	list_for_each_entry(unit_t, curr, &co->units, units) {
		int i;
		max += curr->interf + curr->node_count - 1;
		const ir_node *root = curr->nodes[0];
		int root_color = get_irn_color(root);
		copies += curr->interf;
		for (i=1; i<curr->node_count; ++i) {
			const ir_node *arg = curr->nodes[i];
			if (root_color != get_irn_color(arg))
				copies++;
		}
	}

	is->copies[phase] += copies;
	if (phase == 0)
		is->max += max;
	if (phase == 1) {
		is->interferers += co_get_interferer_count(co);
		is->lb += co_get_lower_bound(co);
	}
}

void irg_stat_print(irg_stat_t *is) {
	printf("Irg %s: %3d %3d %3d %3d %3d %3d", is->irg_name, is->interferers, is->lb, is->copies[0], is->copies[1], is->copies[2], is->max);
}

void all_stat_dump(void) {
	FILE *out;
	irg_stat_t *cuml, *curr;
	/* Compute cumulative values */
	cuml = irg_list_entry(all_stats->irgs.next);
	cuml->interferers = 0;
	cuml->lb = 0;
	cuml->max = 0;
	cuml->copies[0] = 0;
	cuml->copies[1] = 0;
	cuml->copies[2] = 0;
	list_for_each_entry(irg_stat_t, curr, &all_stats->irgs, chain) {
		int i = 0;
		cuml->interferers += curr->interferers;
		cuml->lb += curr->lb;
		cuml->max += curr->max;
		for (i=0; i<3; ++i)
			cuml->copies[i] += curr->copies[i];
	}

	/* dump to file */
	out = ffopen(all_stats->unit_name, "stats", "wt");
	list_for_each_entry(irg_stat_t, curr, &all_stats->irgs, chain)
		fprintf(out, "%15s  %3d %3d %3d %3d %3d %3d", curr->irg_name, curr->interferers, curr->lb, curr->copies[0], curr->copies[1], curr->copies[2], curr->max);
	fclose(out);
}
