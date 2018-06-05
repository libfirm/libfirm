#include "ces_agu_simple_row.h"

static int row_adjust_state=0;

/* extern */
void ces_agu_restore_params(struct agu_params* current);
void ces_agu_save_params(struct agu_params* current);
int ces_compare_deltas(int actual, int expected);

/* code */

int simple_row_adjust_params(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {

	int result = false;
	switch (row_adjust_state) {
	case 0:
		result = simple_row_adjust_stride(params, current_base, old_base);
		break;
	case 1:
		result = simple_row_adjust_skip(params, current_base, old_base);
		break;
	default:
		result = false;
	}
	row_adjust_state++;
	return result;
}

int simple_row_adjust_stride(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->stride != 0) 
		return false;

    ces_agu_restore_params(params);
	params->stride = current_base->c3_value - old_base->c3_value;
	return true;
}

int simple_row_adjust_skip(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->skip != 0)
		return false;
	
	ces_agu_restore_params(params);
	params->span = params->counter;
	params->skip = current_base->c3_value - old_base->c3_value;
	return true;
}

void simple_row_init(struct agu_params* params, struct load_base* current_base) {
	row_adjust_state = 0;

	memset(params, 0, sizeof(struct agu_params));
	params->c1 = current_base->c1_value;
	params->c2 = current_base->c2_value;
	params->c3 = current_base->c3_value;
	params->span=INT_MAX;

	ces_agu_save_params(params);
}

void simple_row_advance_addr(struct agu_params* params) {
	ces_agu_save_params(params);
	
	if (params->counter == params->span) {
		params->counter = 0;
		params->c3 += params->skip;
		params->total_skips++;
	} else {
		params->c3 += params->stride;
		params->counter ++;
		params->total_strides++;
	}
	params->total++;
}


int simple_row_match_memop(struct load_base* current_base, struct load_base* old_base, struct agu_params* params) {
	int el_fits = true;
	if (old_base->c1_value != current_base->c1_value)
		return false;

	if (old_base->c2_value == current_base->c2_value) {
		el_fits &= ces_compare_deltas(params->c3, current_base->c3_value);
	} else if (old_base->c3_value == current_base->c3_value ) {
		el_fits &= ces_compare_deltas(current_base->c2_value, params->c3);
		assert(false && "broken: stride was used in c3 before, cannot be used in c2 now. only exclusive use possible, maybe make this use span or separate strategy");
	}
	return el_fits;
}
