#include "ces_agu_adv_row.h"

/* extern */
void ces_agu_restore_params(struct agu_params* current);
void ces_agu_save_params(struct agu_params* current);
int ces_compare_deltas(int actual, int expected);

typedef enum { C1, C2, C3, INVALID } constant;
static constant stride;
static int count=0;

/* code */

int adv_row_adjust_params(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {

	int result = false;
	switch (count) {
	case 0:
		result = adv_row_adjust_stride(params, current_base, old_base);
		break;
	case 1:
		result = adv_row_adjust_skip(params, current_base, old_base);
		break;
	default:
		result = false;
	}
	count++;
	return result;
}

static constant which_const_changed(struct load_base* cu, struct load_base* ol) {
	if ((cu->c1_value != ol->c1_value) && (cu->c2_value == ol->c2_value) && (cu->c3_value == ol->c3_value))
		return C1;
	if ((cu->c1_value == ol->c1_value) && (cu->c2_value != ol->c2_value) && (cu->c3_value == ol->c3_value))
		return C2;
	if ((cu->c1_value == ol->c1_value) && (cu->c2_value == ol->c2_value) && (cu->c3_value != ol->c3_value))
		return C3;
	return INVALID;
}


int adv_row_adjust_stride(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->stride != 0) 
		return false;

	ces_agu_restore_params(params);
	
	switch (which_const_changed(current_base, old_base)) {
	case C1:
		params->stride = current_base->c1_value - old_base->c1_value;
		stride = C1;
		break;
	case C2:
		params->stride = current_base->c2_value - old_base->c2_value;
		stride = C2;
		break;
	case C3:
		params->stride = current_base->c3_value - old_base->c3_value;
		stride = C3;
		break;
	default:
		stride = INVALID;
		DBG((ces_dbg, LEVEL_DEFAULT, "more than one variable changed\n"));
		return false;
	}
	return true;
}

int adv_row_adjust_skip(struct agu_params* params, struct load_base* current_base, struct load_base* old_base) {
	if (params->skip != 0)
		return false;
	
	ces_agu_restore_params(params);
	switch (stride) {
	case C1:
		params->skip = current_base->c1_value - old_base->c1_value;
		break;
	case C2:
		params->skip = current_base->c2_value - old_base->c2_value;
		break;
	case C3:
		params->skip = current_base->c3_value - old_base->c3_value;
		break;
	default:
		DBG((ces_dbg, LEVEL_DEFAULT, "invalid stride setting\n"));
		return false;
	}
	params->span = params->counter;
	return true;
}

void adv_row_init(struct agu_params* params, struct load_base* current_base) {
	memset(params, 0, sizeof(struct agu_params));
	params->c1 = current_base->c1_value;
	params->c2 = current_base->c2_value;
	params->c3 = current_base->c3_value;
	params->span=INT_MAX;

	ces_agu_save_params(params);

	count = 0;
	stride=INVALID;
}

void adv_row_advance_addr(struct agu_params* params) {
	ces_agu_save_params(params);
	
	if (params->counter == params->span) {
		switch (stride) {
		case C1:
			params->c1 += params->skip;
			break;
		case C2:
			params->c2 += params->skip;
			break;
		case C3:
			params->c3 += params->skip;
			break;
		default:
			DBG((ces_dbg, LEVEL_DEFAULT, "invalid stride setting\n")); //happens before first adjust call
//			assert(false);
		}
		params->counter = 0;
		params->total_skips++;

	} else {
		switch (stride) {
		case C1:
			params->c1 += params->stride;
			break;
		case C2:
			params->c2 += params->stride;
			break;
		case C3:
			params->c3 += params->stride;
			break;
		default:
			DBG((ces_dbg, LEVEL_5, "invalid stride setting\n")); //happens before first adjust call
//			assert(false);
		}
		params->counter++;
		params->total_strides++;
	}
	params->total++;
}

int adv_row_match_memop(struct load_base* current_base, struct load_base* old_base, struct agu_params* params) {
	int el_fits = true;

	switch (which_const_changed(current_base, old_base)) {
	case C1:
		el_fits &= ces_compare_deltas(params->c1, current_base->c1_value);
		break;
	case C2:
		el_fits &= ces_compare_deltas(params->c2, current_base->c2_value);
		break;
	case C3:
		el_fits &= ces_compare_deltas(params->c3, current_base->c3_value);
		break;
	default:
		DBG((ces_dbg, LEVEL_DEFAULT, "more than one variable changed\n"));
		return false;
	}
	return el_fits;
}
