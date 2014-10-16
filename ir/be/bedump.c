/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Code for dumping backend datastructures (i.e. interference graphs)
 * @author      Matthias Braun
 */
#include "bedump.h"

#include "irdump_t.h"
#include "irgwalk.h"
#include "beifg.h"
#include "becopyopt_t.h"
#include "belive.h"

static void dump_ifg_nodes(FILE *F, const be_ifg_t *ifg)
{
	be_ifg_foreach_node(ifg, node) {
		dump_node(F, node);
	}
}

static void dump_ifg_edges(FILE *F, const be_ifg_t *ifg)
{
	be_ifg_foreach_node(ifg, node) {
		neighbours_iter_t neigh_iter;

		be_ifg_foreach_neighbour(ifg, &neigh_iter, node, neighbour) {
			/* interference is bidirectional, but it's enough to dump 1
			 * direction */
			if (get_irn_node_nr(node) >= get_irn_node_nr(neighbour))
				continue;

			fprintf(F, "edge: {sourcename: ");
			print_nodeid(F, node);
			fprintf(F, " targetname: ");
			print_nodeid(F, neighbour);
			fprintf(F, " arrowstyle:none class:1}\n");
		}
	}
}

void be_dump_ifg(FILE *F, ir_graph *irg, const be_ifg_t *ifg)
{
	ir_fprintf(F,
		"graph: { title: \"interference graph of %+F\"\n"
		"layoutalgorithm: mindepth //$ \"circular\"\n"
		"classname 1: \"interference\"\n"
		, irg);
	dump_vcg_infonames(F);
	dump_vcg_header_colors(F);

	dump_ifg_nodes(F, ifg);
	dump_ifg_edges(F, ifg);

	fprintf(F, "}\n");
}

static void dump_affinity_edges(FILE *F, const copy_opt_t *co,
                                bool dump_costs, bool dump_colors)
{
	co_gs_foreach_aff_node(co, a) {
		co_gs_foreach_neighb(a, n) {
			/* edges are bidirection, dumping one direction is enough */
			if (get_irn_node_nr(a->irn) >= get_irn_node_nr(n->irn))
				continue;

			fprintf(F, "edge: {sourcename: ");
			print_nodeid(F, a->irn);
			fprintf(F, " targetname: ");
			print_nodeid(F, n->irn);
			fprintf(F, " arrowstyle:none");

			if (dump_costs)
				fprintf(F, " label:\"%d\"", n->costs);
			if (dump_colors) {
				const arch_register_t *ar = arch_get_irn_register(a->irn);
				const arch_register_t *nr = arch_get_irn_register(n->irn);
				const char *color = nr == ar ? "blue" : "red";
				fprintf(F, " color:%s", color);
			}
			fprintf(F, " linestyle:dashed class:2");
			fprintf(F, "}\n");
		}
	}
}

void be_dump_ifg_co(FILE *F, const copy_opt_t *co, bool dump_costs,
                    bool dump_colors)
{
	ir_graph *irg = co->irg;
	be_ifg_t *ifg = co->cenv->ifg;

	ir_fprintf(F,
		"graph: { title: \"interference graph of %+F\"\n"
		"layoutalgorithm: mindepth //$ \"circular\"\n"
		"classname 1: \"interference\"\n"
		"classname 2: \"affinity\"\n"
		, irg);
	dump_vcg_infonames(F);
	dump_vcg_header_colors(F);

	dump_ifg_nodes(F, ifg);
	dump_ifg_edges(F, ifg);
	dump_affinity_edges(F, co, dump_costs, dump_colors);

	fprintf(F, "}\n");
}

static const char *lv_flags_to_str(unsigned flags)
{
	static const char *states[] = {
		"---",
		"i--",
		"-e-",
		"ie-",
		"--o",
		"i-o",
		"-eo",
		"ieo"
	};

	return states[flags & 7];
}

void be_dump_liveness_block(be_lv_t *lv, FILE *F, const ir_node *bl)
{
	be_lv_info_t *info = ir_nodehashmap_get(be_lv_info_t, &lv->map, bl);

	fprintf(F, "liveness:\n");
	if (info != NULL) {
		unsigned n = info[0].head.n_members;
		unsigned i;

		for (i = 0; i < n; ++i) {
			be_lv_info_node_t *n = &info[i+1].node;
			ir_fprintf(F, "%s %+F\n", lv_flags_to_str(n->flags), n->node);
		}
	}
}

typedef struct lv_walker_t {
	be_lv_t *lv;
	FILE    *out;
} lv_walker_t;

static void lv_dump_block_walker(ir_node *irn, void *data)
{
	lv_walker_t *w = (lv_walker_t*)data;
	if (!is_Block(irn))
		return;
	be_dump_liveness_block(w->lv, w->out, irn);
}

void be_liveness_dump(FILE *F, const be_lv_t *lv)
{
	lv_walker_t w;

	w.lv  = (be_lv_t *) lv;
	w.out = F;
	irg_block_walk_graph(lv->irg, lv_dump_block_walker, NULL, &w);
}

/**
 * Print information about a register requirement in human readable form
 * @param F   output stream/file
 * @param req The requirements structure to format.
 */
static void dump_register_req(FILE *const F, arch_register_req_t const *const req)
{
	if (req == NULL || req->type == arch_register_req_type_none) {
		fprintf(F, "n/a");
		return;
	}

	fprintf(F, "%s", req->cls->name);

	if (arch_register_req_is(req, limited)) {
		unsigned n_regs = req->cls->n_regs;
		unsigned i;

		fprintf(F, " limited to");
		for (i = 0; i < n_regs; ++i) {
			if (rbitset_is_set(req->limited, i)) {
				const arch_register_t *reg = &req->cls->regs[i];
				fprintf(F, " %s", reg->name);
			}
		}
	}

	if (arch_register_req_is(req, should_be_same)) {
		const unsigned other = req->other_same;
		int i;

		fprintf(F, " same as");
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_fprintf(F, " #%d", i);
			}
		}
	}

	if (arch_register_req_is(req, must_be_different)) {
		const unsigned other = req->other_different;
		int i;

		fprintf(F, " different from");
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_fprintf(F, " #%d", i);
			}
		}
	}

	if (req->width != 1) {
		fprintf(F, " width:%d", req->width);
	}
	if (arch_register_req_is(req, aligned)) {
		fprintf(F, " aligned");
	}
	if (arch_register_req_is(req, ignore)) {
		fprintf(F, " ignore");
	}
	if (arch_register_req_is(req, produces_sp)) {
		fprintf(F, " produces_sp");
	}
}

void be_dump_reqs_and_registers(FILE *F, const ir_node *node)
{
	backend_info_t *const info = be_get_info(node);
	/* don't fail on invalid graphs */
	if (!info || (!info->in_reqs && get_irn_arity(node) != 0) || !info->out_infos) {
		fprintf(F, "invalid register requirements!!!\n");
		return;
	}

	foreach_irn_in(node, i, op) {
		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		fprintf(F, "inreq #%d = ", i);
		dump_register_req(F, req);
		arch_register_t const *const reg = be_get_info(skip_Proj_const(op))->out_infos ? arch_get_irn_register(op) : NULL;
		fprintf(F, " [%s]\n", reg ? reg->name : "n/a");
	}
	be_foreach_out(node, o) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, o);
		fprintf(F, "outreq #%u = ", o);
		dump_register_req(F, req);
		const arch_register_t *reg = arch_get_irn_register_out(node, o);
		fprintf(F, " [%s]\n", reg != NULL ? reg->name : "n/a");
	}

	fprintf(F, "flags =");
	arch_irn_flags_t flags = arch_get_irn_flags(node);
	if (flags == arch_irn_flags_none) {
		fprintf(F, " none");
	} else {
		if (flags & arch_irn_flag_dont_spill)
			fprintf(F, " unspillable");
		if (flags & arch_irn_flag_rematerializable)
			fprintf(F, " remat");
		if (flags & arch_irn_flag_modify_flags)
			fprintf(F, " modify_flags");
		if (flags & arch_irn_flag_simple_jump)
			fprintf(F, " simple_jump");
		if (flags & arch_irn_flag_not_scheduled)
			fprintf(F, " not_scheduled");
		if (flags & arch_irn_flag_schedule_first)
			fprintf(F, " schedule_first");
	}
	fprintf(F, " (0x%x)\n", (unsigned)flags);
}
