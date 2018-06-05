#include "ces_agu_twovar_row.h"

typedef enum { SKIP_C1, SKIP_C2 } skip_type;
static int skip_variable;
static int count=0;
/* extern */
void ces_agu_restore_params(struct agu_params* current);
void ces_agu_save_params(struct agu_params* current);
int ces_compare_deltas(int actual, int expected);

/* code */

int twovar_row_adjust_params(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	int result = 0;
	switch (count) {
	case 0:
		result = twovar_row_adjust_stride(params, current_base, old_base);
		break;
	case 1:
		result = twovar_row_adjust_skip(params, current_base, old_base);
		break;
	default:
		result = false;
	}
	count++;
	return result;
}

int twovar_row_adjust_stride(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->stride != 0) 
		return false;

	ces_agu_restore_params(params);
	params->stride = current_base->c3_value - old_base->c3_value;
	return true;
}

int twovar_row_adjust_skip(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->skip != 0)
		return false;
	
	if (current_base->c3_value == 0) {
		if (current_base->c2_value != params->c2 && current_base->c1_value == params->c1) {
			ces_agu_restore_params(params);
			params->span = params->counter;
			params->skip = current_base->c2_value - old_base->c2_value;
			skip_variable=SKIP_C2;
		} else if (current_base->c1_value != params->c1 && current_base->c2_value == params->c2) {
			ces_agu_restore_params(params);
			params->span = params->counter;
			params->skip = current_base->c1_value - old_base->c1_value;
			skip_variable=SKIP_C1;
		} else {
			DBG((ces_dbg, LEVEL_DEFAULT, "two variable changes at once"));
			return false; 
		}
	}
	return true;
}

void twovar_row_init(struct agu_params* params, struct load_base* current_base) {
	memset(params, 0, sizeof(struct agu_params));
	params->c1 = current_base->c1_value;
	params->c2 = current_base->c2_value;
	params->c3 = current_base->c3_value;
	params->span=INT_MAX;

	ces_agu_save_params(params);

	skip_variable = SKIP_C1;
	count = 0;
}

void twovar_row_advance_addr(struct agu_params* params) {
	ces_agu_save_params(params);
	
	if (params->counter == params->span) {
		params->counter = 0;
		params->c3 = 0;
		switch (skip_variable) {
		case SKIP_C1:
			params->c1 += params->skip;
			break;
		case SKIP_C2:
			params->c2 += params->skip;
			break;
		default:
			DBG((ces_dbg, LEVEL_DEFAULT,"unkown skip. investigate"));
		}
		params->total_skips++;
	} else {
		params->c3 += params->stride;
		params->counter ++;
		params->total_strides++;
	}
	params->total++;
}


int twovar_row_match_memop(struct load_base* current_base, struct load_base* old_base, struct agu_params* params) {
	UNUSED_PARAM(old_base);
	int el_fits = true;

	switch (skip_variable) {
	case SKIP_C1:
		el_fits &= ces_compare_deltas(params->c1, current_base->c1_value);
		el_fits &= ces_compare_deltas(params->c3, current_base->c3_value);
		break;
	case SKIP_C2:
		el_fits &= ces_compare_deltas(params->c2, current_base->c2_value);
		el_fits &= ces_compare_deltas(params->c3, current_base->c3_value);
		break;
	default:
		DBG((ces_dbg, LEVEL_DEFAULT,"unkown skip. investigate"));
	}
	return el_fits;
}
