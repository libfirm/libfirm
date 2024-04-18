/**
 * @file
 * @brief       Implements function parameter lowering for the RISC-V ILP32 ABI
 * @author      Johannes Bucher
 */
#include "riscv_abi.h"

#include "riscv_bearch_t.h"
#include "irmode.h"
#include "type_t.h"

static ir_mode *fold_classes(ir_mode *c1, ir_mode *c2)
{
	if (c1 == c2) {
		return c1;
	} else if (c1 == mode_BAD) {
		return c2;
	} else if (c2 == mode_BAD) {
		return c1;
	} else {
		panic("TODO");
	}
}

static ir_mode *classify_slice_for_ilp32(ir_type const *const tp, unsigned min, unsigned max);

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
			ir_mode *member_class = classify_slice_for_ilp32(member_type, min_in_member, max_in_member);
			current_class = fold_classes(current_class, member_class);
		}
	}
	return current_class;
}

static ir_mode *classify_slice_for_ilp32(ir_type const *const tp, unsigned min, unsigned max)
{
	switch(get_type_opcode(tp)) {
		case tpo_class:
		case tpo_struct:
		case tpo_union:
			return classify_compound_by_members(tp, min, max);

		case tpo_array: {
			ir_type *elem_type = get_array_element_type(tp);
			if (min < get_type_size(tp)) {
				// We are in the array
				size_t elem_size = get_type_size(elem_type);
				if (min >= elem_size) {
					// ... but past the first element. Shift the slice range down.
					unsigned new_min = min % elem_size;
					unsigned new_max = new_min + (max - min);
					return classify_slice_for_ilp32(elem_type, new_min, new_max);
				} else {
					return classify_slice_for_ilp32(elem_type, min, max);
				}
			} else {
				return mode_BAD;
			}
		}
		case tpo_primitive: {
			ir_mode *mode = get_type_mode(tp);
			if (min >= get_type_size(tp)) {
				return mode_BAD;
			} else if (mode_is_float(mode)) {
				panic("RISC-V ILP32 ABI has no hardware floating point support");
			} else {
				return mode_Iu;
			}
		}
		case tpo_pointer:
			if (min >= get_type_size(tp)) {
				return mode_BAD;
			} else {
				return mode_Iu;
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

static aggregate_spec_t classify_for_ilp32(ir_type const *const type) {
	if (get_type_size(type) > 2 * RISCV_REGISTER_SIZE) {
		return (aggregate_spec_t) {
				.length = 1,
				.modes = { mode_P },
		};
	}

	aggregate_spec_t result = {
		.length = 0,
		.modes = { },
	};

	for (unsigned i = 0; i < 2; i++) {
		ir_mode *c = classify_slice_for_ilp32(type, RISCV_REGISTER_SIZE * i, RISCV_REGISTER_SIZE * (i + 1));
		result.modes[i] = c;
		if (c != mode_BAD) {
			result.length++;
		}
	}

	/* if type has 8 byte alignment and result is two (4-byte) mode_Iu slices we have to convert this into
	 * a single 8 byte mode_Lu slice to ensure correct alignment when passing the argument.
	 * (Variadic arguments with 2*XLEN-bit alignment and size at most 2*XLEN bits are passed in an aligned register pair) */
	if (result.length == 2 && result.modes[0] == mode_Iu && result.modes[1] == mode_Iu &&
	    (get_type_alignment(type) == 2 * RISCV_REGISTER_SIZE)) {
		result.length = 1;
		result.modes[0] = mode_Lu;
	}

	return result;
}

aggregate_spec_t riscv_lower_parameter(void *env, ir_type const *type) {
	(void)env;

	if (is_aggregate_type(type)) {
		return classify_for_ilp32(type);
	} else {
		return (aggregate_spec_t) {
				.length = 1,
				.modes = { get_type_mode(type) },
		};
	}
}

aggregate_spec_t riscv_lower_result(void *env, ir_type const *type) {
	return riscv_lower_parameter(env, type);
}
