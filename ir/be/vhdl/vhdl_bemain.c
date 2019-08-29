/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

/**
 * @file
 * @brief   main backend functions for VHDL backend
 * @author  Johannes Bucher, Daniel Biester
 */

#include "vhdl_bemain.h"

#include "beinfo.h"
#include "beirg.h"
#include "belive.h"
#include "irprog_t.h"
#include "irverify.h"
#include "obst.h"
#include "vhdl_bearch_t.h"

static struct obstack obst;
static be_main_env_t env;

/**
 * Prepare a backend graph for code generation and initialize its irg
 */
static void initialize_birg(be_irg_t *birg, ir_graph *irg, be_main_env_t *env)
{
	/* don't duplicate locals in backend when dumping... */
	ir_remove_dump_flags(ir_dump_flag_consts_local);

	be_dump(DUMP_INITIAL, irg, "begin");

	assure_irg_properties(irg,
	                      IR_GRAPH_PROPERTY_NO_BADS
	                      | IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
	                      | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
	                      | IR_GRAPH_PROPERTY_MANY_RETURNS);

	memset(birg, 0, sizeof(*birg));
	birg->main_env = env;
	obstack_init(&birg->obst);
	irg->be_data = birg;

	be_info_init_irg(irg);
	birg->lv = be_liveness_new(irg);

	/* Verify the initial graph */
	bool fine = irg_verify(irg);
	be_check_verify_result(fine, irg);
}

void vhdl_be_begin(const char *cup_name)
{
	obstack_init(&obst);
	vhdl_init();

	be_info_init();

	memset(&env, 0, sizeof(env));
	env.cup_name = cup_name;

	/* First: initialize all birgs */
	size_t num_birgs = 0;
	/* we might need 1 birg more for instrumentation constructor */
	//TODO: Allocating too many birgs as only birgs for special instructions are needed
	be_irg_t *const birgs = OALLOCN(&obst, be_irg_t, get_irp_n_irgs() + 1);
	foreach_irp_irg(i, irg) {
		ir_entity *entity = get_irg_entity(irg);
		if (!(mtp_special_instruction & get_entity_additional_properties(entity)))
			continue;
		initialize_birg(&birgs[num_birgs++], irg, &env);
		be_dump(DUMP_INITIAL, irg, "prepared");
	}

}

void vhdl_be_step_last(ir_graph *irg)
{
	be_dump(DUMP_FINAL, irg, "final");
	be_free_birg(irg);
}

void vhdl_be_finish(void)
{
	be_info_free();
	vhdl_finish();
	obstack_free(&obst, NULL);
}
