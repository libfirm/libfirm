/* own includes */
#include "ces_agu_emulator.h"
#include "ces_agu_simple_row.h"
#include "ces_agu_adv_row.h"
#include "ces_agu_twovar_row.h"
#include "ces_si_tools.h"

/* framework includes */

/* private declarations */

/* private functions / forward declarations */
int ces_compare_deltas(int actual, int expected);
typedef void ces_agu_next_addr(struct agu_params* agu_params);

void ces_agu_restore_params(struct agu_params* current);
void ces_agu_save_params(struct agu_params* current);


/* private definitions */
static struct agu_params agu_params_save;
ir_nodemap* ces_load_base;
static const struct agu_strategy strategies[] = {
	{simple_row_init, simple_row_advance_addr, simple_row_match_memop, simple_row_adjust_params, "simple row based"},
	{adv_row_init, adv_row_advance_addr, adv_row_match_memop, adv_row_adjust_params, "adv row based"},
	{twovar_row_init, twovar_row_advance_addr, twovar_row_match_memop, twovar_row_adjust_params, "two variables row based"},
	{NULL, NULL, NULL, NULL, ""}
};

/*
 * simulates the iCore AGU modes and checks if all given memops fit into one of the possible strategies
 * does not change qloads
*/
//TODO:check if really all qloads are matched before strategy is returned
const struct agu_strategy* ces_agu_strategies(plist_t* qloads, struct agu_params* params, ir_nodemap* load_bases) {
	const struct agu_strategy* strategy= strategies;
	const struct agu_strategy* working_strategy = NULL;
	struct agu_params working_params;
	ces_load_base = load_bases;
	plist_element_t* el;
	struct load_base* old_base = NULL;
	struct load_base* current_base = NULL;
	int match, retry;
	int adjust_count=0;
	int skip_first = false;

	while(strategy->adjust_params) {
		DB((ces_dbg, LEVEL_3, "try strategy %s:", strategy->name));
		el = (skip_first)? plist_element_get_next(plist_first(qloads)) : plist_first(qloads);
		current_base = ir_nodemap_get_fast(ces_load_base, plist_element_get_value(el));

		strategy->initialize(params, current_base);
		
		for (el = plist_element_get_next(el); el; el = plist_element_get_next(el)) {
			old_base = current_base;
			current_base = ir_nodemap_get_fast(ces_load_base, plist_element_get_value(el));
			
			if (adjust_count == 0) {
				strategy->adjust_params(params, current_base, old_base);
				adjust_count++;
			}
			
			strategy->advance_addr(params);
			do {
				match = strategy->match_memop(current_base, old_base, params);
				if (!match) {
					retry = strategy->adjust_params(params, current_base, old_base);
					strategy->advance_addr(params);
				}
			} while (!match && retry);
			if (!match)
				break; //next strategy
		}
		if (!el || !plist_element_has_next(el)) {
			DB((ces_dbg, LEVEL_3, "works!\n"));
			DB((ces_dbg, LEVEL_3, "stride:%i, skip:%i, span:%i, total:%i, total skips%i, total strides:%i\n", params->stride, params->skip, params->span, params->total, params->total_skips, params->total_strides));
			working_strategy = strategy;
			memcpy(&working_params, params, sizeof(struct agu_params));
			strategy++;
      //break; //finished - this strategy matches
		} else {
			DB((ces_dbg, LEVEL_3, "nope :(\n"));
			if (skip_first) {
				strategy++;
				skip_first=false;
			} else {
				skip_first = true; //try the same strategy again but skip first elemtent
				DB((ces_dbg, LEVEL_3, "retry strategy %s, skip first element\n",strategy->name));
			}
		}
	}

	memcpy(params, &working_params, sizeof(struct agu_params));
	if (working_strategy) {
		DB((ces_dbg, LEVEL_3, "using strategy:%s\n", working_strategy->name));
		DB((ces_dbg, LEVEL_3, "stride:%i, skip:%i, span:%i, total:%i, total skips%i, total strides:%i\n", params->stride, params->skip, params->span, params->total, params->total_skips, params->total_strides));
	} else
		DB((ces_dbg, LEVEL_3, YELLOW("no strategy works :'\n")));

	return working_strategy;
}


int ces_compare_deltas(int actual, int expected) {
	if (actual == expected)
		return true;
	else
		return false;		
}

void ces_agu_restore_params(struct agu_params* current) {
	memcpy(current, &agu_params_save, sizeof(struct agu_params));
}

void ces_agu_save_params(struct agu_params* current) {
	memcpy(&agu_params_save, current, sizeof(struct agu_params));
}

