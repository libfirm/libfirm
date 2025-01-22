/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements function parameter lowering for the System V AMD64 ABI
 * @author      Andreas Fried
 */
#include "amd64_abi.h"

#include <stdbool.h>

#include "debug.h"
#include "firm.h"
#include "panic.h"
#include "type_t.h"
#include "x86_x87.h"


static const int max_integer_params = 6;
static const int max_sse_params = 8;

static const int max_integer_results = 2;
static const int max_sse_results = 2;

/*
 * We map the "classes" defined in the ABI to firm modes as follows:
 *
 * INTEGER     mode_Lu
 * SSE         mode_D
 * SSEUP       [not supported in firm]
 * X87         x86_mode_E
 * X87UP       [not supported in firm]
 * COMPLEX_X87 [not supported in firm]
 * NO_CLASS    mode_BAD
 * MEMORY      mode_M
 */

static bool try_free_register(unsigned *r, unsigned max)
{
	if (*r < max) {
		(*r)++;
		return true;
	} else {
		return false;
	}
}

/* For the algorithm see the AMD64 ABI, sect. 3.2.3,
 * par. "Classification", no. 4 */
static ir_mode *fold_classes(ir_mode *c1, ir_mode *c2)
{
	if (c1 == c2) {
		return c1;
	} else if (c1 == mode_BAD) {
		return c2;
	} else if (c2 == mode_BAD) {
		return c1;
	} else if (c1 == mode_M || c2 == mode_M) {
		return mode_M;
	} else if (c1 == mode_Lu || c2 == mode_Lu) {
		return mode_Lu;
	} else if (c1 == x86_mode_E || c2 == x86_mode_E) {
		return mode_M;
	} else {
		return mode_D;
	}
}

static ir_mode *classify_slice_for_amd64(ir_type const *tp, unsigned min, unsigned max);

static ir_mode *classify_compound_by_members(ir_type const *const tp, unsigned min, unsigned max)
{
	unsigned n = get_compound_n_members(tp);
	ir_mode *current_class = mode_BAD;
	for (unsigned i = 0; i < n; i++) {
		ir_entity *member = get_compound_member(tp, i);
		ir_type *member_type = get_entity_type(member);
		unsigned member_size = get_type_size(member_type);

		unsigned member_begin = get_entity_offset(member);
		unsigned member_end = member_begin + member_size;

		// Is the member (at least partially) between min and max?
		if (min < member_end && max > member_begin) {
			if (get_entity_aligned(member) == align_non_aligned) {
				return mode_M;
			}
			unsigned min_in_member = min <= member_begin ? 0 : min - member_begin;
			unsigned max_in_member = member_end < max ? member_size : max - member_begin;
			ir_mode *member_class = classify_slice_for_amd64(member_type, min_in_member, max_in_member);
			current_class = fold_classes(current_class, member_class);
		}
	}
	return current_class;
}

static ir_mode *classify_slice_for_amd64(ir_type const *const tp, unsigned min, unsigned max)
{
	switch(get_type_opcode(tp)) {
	case tpo_class:
		/* Classes are not quite like structs. We need to
		 * check whether the class "has either a non-trivial
		 * copy constructor or a non-trivial destructor" (ABI
		 * sect. 3.2.3). */
		/* For now, we treat them as such though to get X10 to work. */

	case tpo_struct:
	case tpo_union:
		return classify_compound_by_members(tp, min, max);

	case tpo_array: {
		ir_type *elem_type = get_array_element_type(tp);
		if (min < get_type_size(tp)) {
			/* We are in the array */
			size_t elem_size = get_type_size(elem_type);
			if (min >= elem_size) {
				/* ... but past the first element. Shift the slice range down. */
				unsigned new_min = min % elem_size;
				unsigned new_max = new_min + (max - min);
				return classify_slice_for_amd64(elem_type, new_min, new_max);
			} else {
				return classify_slice_for_amd64(elem_type, min, max);
			}
		} else {
			return mode_BAD;
		}
	}
	case tpo_primitive: {
		ir_mode *mode = get_type_mode(tp);

		if (min >= get_type_size(tp)) {
			return mode_BAD;
		} else if (get_mode_arithmetic(mode) == irma_x86_extended_float) {
			assert(get_mode_size_bytes(mode) == 10);
			return x86_mode_E;
		} else if (mode_is_float(mode)) {
			return mode_D;
		} else {
			return mode_Lu;
		}
	}
	case tpo_pointer:
		if (min >= get_type_size(tp)) {
			return mode_BAD;
		} else {
			return mode_Lu;
		}

	case tpo_code:
	case tpo_method:
	case tpo_segment:
	case tpo_uninitialized:
	case tpo_unknown:
		break;
	}
	panic("invalid type");
}

static aggregate_spec_t classify_for_amd64(amd64_abi_state *s, ir_type const *const tp, unsigned max_integer, unsigned max_sse)
{
	amd64_abi_state reset = *s;

	aggregate_spec_t result = {
		.length = 0,
		.modes = { },
	};
#ifndef NDEBUG
	bool type_ended = false;
#endif

	/* According to the ABI, sect. 3.2.3, par. "Classification",
	 * no. 1 (note Footnote 12!), types larger than 2 eightbytes
	 * (except __m256 and __m512, which libfirm does not support)
	 * are passed in memory. */
	if (get_type_size(tp) > 2 * 8) {
		goto use_class_memory;
	} else {
		for (unsigned i = 0; i < 2; i++) {
			ir_mode *c = classify_slice_for_amd64(tp, 8 * i, 8 * (i + 1));

			if (c == mode_BAD) {
				/* Check that we do not see anything else after the first mode_BAD */
				DEBUG_ONLY(type_ended = true;)
			} else if (c == mode_Lu) {
				assert(!type_ended);
				if (!try_free_register(&s->integer_params, max_integer)) {
					goto use_class_memory;
				}
			} else if (c == mode_D) {
				assert(!type_ended);
				if (!try_free_register(&s->sse_params, max_sse)) {
					goto use_class_memory;
				}
			} else if (c == x86_mode_E || c == mode_M) {
				assert(!type_ended);
				goto use_class_memory;
			}

			result.modes[i] = c;
			if (c != mode_BAD) {
				result.length++;
			}
		}
		return result;
	}

use_class_memory:
	result.modes[0] = mode_M;
	result.modes[1] = mode_BAD;
	result.length   = 1;
	*s = reset;
	return result;
}

static void notify_amd64_scalar(amd64_abi_state *s, ir_type const *const param_type, unsigned max_integer, unsigned max_sse)
{
	ir_mode *mode = get_type_mode(param_type);
	if (mode == NULL) {
		mode = mode_P;
	}

	if (mode_is_int(mode) || mode_is_reference(mode)) {
		try_free_register(&s->integer_params, max_integer);
	} else if (mode_is_float(mode) && get_mode_arithmetic(mode) != irma_x86_extended_float) {
		try_free_register(&s->sse_params, max_sse);
	}
	/* Otherwise, the argument is passed in memory. Nothing to do. */
}

void amd64_reset_abi_state(void *param_env, void *result_env) {
	amd64_abi_state *param_state = (amd64_abi_state*) param_env;
	amd64_abi_state *result_state = (amd64_abi_state*) result_env;

	param_state->integer_params = 0;
	param_state->sse_params = 0;
	result_state->integer_params = 0;
	result_state->sse_params = 0;
}

aggregate_spec_t amd64_lower_parameter(void *env, ir_type const *const type)
{
	amd64_abi_state *state = (amd64_abi_state*)env;

	if (is_aggregate_type(type)) {
		return classify_for_amd64(state, type, max_integer_params, max_sse_params);
	} else {
		notify_amd64_scalar(state, type, max_integer_params, max_sse_params);
		return (aggregate_spec_t) {
			.length = 1,
			.modes = { get_type_mode(type) },
		};
	}
}

static void convert_stack_to_pointer(aggregate_spec_t *spec) {
	if (spec->length == 1 && spec->modes[0] == mode_M) {
		spec->modes[0] = mode_P;
	}
}

aggregate_spec_t amd64_lower_result(void *env, ir_type const *const type)
{
	amd64_abi_state *state = (amd64_abi_state*)env;

	if (is_aggregate_type(type)) {
		aggregate_spec_t result = classify_for_amd64(state, type, max_integer_results, max_sse_results);
		convert_stack_to_pointer(&result);
		return result;
	} else {
		notify_amd64_scalar(state, type, max_integer_results, max_sse_results);
		return (aggregate_spec_t) {
			.length = 1,
			.modes = { get_type_mode(type) },
		};
	}
}
