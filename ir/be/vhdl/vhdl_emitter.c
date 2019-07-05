/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_emitter.h"

#include "be_t.h"
#include "bearch.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "benode.h"
#include "besched.h"
#include "irgwalk.h"
#include "gen_vhdl_emitter.h"

#include "panic.h"




void vhdl_emitf(ir_node const *const node, char const *fmt, ...)
{
}

static void vhdl_register_emitters(void)
{
	be_init_emitters();
	vhdl_register_spec_emitters();
}

void vhdl_emit_node(ir_node *node, void *env) {

}

void vhdl_emit_function(ir_graph *const irg)
{
	vhdl_register_emitters();

	irg_walk_blkwise_graph(irg, 0, vhdl_emit_node, NULL);
}
