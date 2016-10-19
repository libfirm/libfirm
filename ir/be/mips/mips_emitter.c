/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

#include "mips_emitter.h"

#include "bearch.h"
#include "beblocksched.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "benode.h"
#include "besched.h"
#include "gen_mips_emitter.h"
#include "gen_mips_regalloc_if.h"
#include "mips_nodes_attr.h"
#include "panic.h"
#include "util.h"

static void emit_register(arch_register_t const *const reg)
{
	be_emit_char('$');
	be_emit_string(reg->name);
}

void mips_emitf(ir_node const *const node, char const *fmt, ...)
{
	BE_EMITF(node, fmt, ap, false) {
		switch (*fmt++) {
		case 'S': {
			if (!is_digit(*fmt))
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			emit_register(arch_get_irn_register_in(node, pos));
			break;
		}

		default:
unknown:
			panic("unknown format conversion");
		}
	}
}

static void mips_register_emitters(void)
{
	be_init_emitters();
	mips_register_spec_emitters();
}

static void mips_gen_block(ir_node *const block)
{
	be_gas_begin_block(block, true);
	sched_foreach(block, node) {
		be_emit_node(node);
	}
}

void mips_emit_function(ir_graph *const irg)
{
	mips_register_emitters();
	be_gas_elf_type_char = '@';

	ir_entity *const entity = get_irg_entity(irg);
	be_gas_emit_function_prolog(entity, 16, NULL);

	ir_node **const blk_sched = be_create_block_schedule(irg);
	size_t    const n_blocks  = ARR_LEN(blk_sched);
	for (size_t i = 0; i != n_blocks; ++i) {
		mips_gen_block(blk_sched[i]);
	}

	be_gas_emit_function_epilog(entity);
}
