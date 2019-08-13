/**
 * @file
 * @brief    Additional data modes for VHDL output.
 * @author  Daniel Biester, Johannes Bucher
 *
 * Define signed and unsigned integer modes w/ sizes at bit
 * granularity to allow efficient hardware synthetisation.
 *
 */

#include <assert.h>
#include <stdio.h>
#include <panic.h>
#include "irmode_t.h"
#include "vhdl_modes.h"

ir_mode *signed_vectors[64];
ir_mode *unsigned_vectors[64];
ir_mode *std_logic_vectors[4];
ir_mode *std_logic;

#define MAX_BITS 64

ir_mode *get_mode_signed_vector(unsigned bits) {
	assert(bits < MAX_BITS);
	return signed_vectors[bits - 1];
}

ir_mode *get_mode_unsigned_vector(unsigned bits) {
	assert(bits < MAX_BITS);
	return unsigned_vectors[bits - 1];
}

ir_mode *get_mode_std_logic_vector(unsigned bits) {
	switch (bits) {
		case 8:
			return std_logic_vectors[0];
		case 16:
			return std_logic_vectors[1];
		case 32:
			return std_logic_vectors[2];
		case 64:
			return std_logic_vectors[3];
		default:
			panic("unsupported length for mode standard logic vector");
	}
}

ir_mode *get_mode_std_logic(void) {
	return std_logic;
}

void vhdl_init_modes(void)
{
	for (int i = 0; i < MAX_BITS; i++) {
		char name[4];
		sprintf(name, "V%ds", i + 1);
		signed_vectors[i] = new_int_mode(name, i + 1, 1, 32);
		sprintf(name, "V%du", i + 1);
		unsigned_vectors[i] = new_int_mode(name, i + 1, 0, 32);
	}
	std_logic_vectors[0] = new_non_arithmetic_mode("slv8", 8);
	std_logic_vectors[1] = new_non_arithmetic_mode("slv16", 16);
	std_logic_vectors[2] = new_non_arithmetic_mode("slv32", 32);
	std_logic_vectors[3] = new_non_arithmetic_mode("slv64", 64);

	std_logic = new_non_arithmetic_mode("std_logic", 1);
}
