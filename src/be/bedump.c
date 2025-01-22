/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Code for dumping backend data structures (i.e. interference graphs)
 * @author      Matthias Braun
 */
#include "bedump.h"

#include "becopyopt_t.h"
#include "beifg.h"
#include "belive.h"
#include "irdump_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "target_t.h"
#include "util.h"

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
		for (unsigned i = 0, n = info->n_members; i < n; ++i) {
			be_lv_info_node_t *const n = &info->nodes[i];
			ir_fprintf(F, "%s %+F\n", lv_flags_to_str(n->flags), n->node);
		}
	}
}

static void dump_bitmask(FILE *const F, char const *const ctx, unsigned mask)
{
	fputs(ctx, F);
	for (unsigned i = 0; mask != 0; ++i, mask >>= 1) {
		if (mask & 1)
			ir_fprintf(F, " #%u", i);
	}
}

/**
 * Print information about a register requirement in human readable form
 * @param F   output stream/file
 * @param req The requirements structure to format.
 */
static void dump_register_req(FILE *const F, arch_register_req_t const *const req)
{
	if (!req) {
		fputs("n/a", F);
		return;
	}

	arch_register_class_t const *const cls = req->cls;
	fputs(cls->name, F);

	if (req->limited != NULL) {
		fputs(" limited to", F);
		for (unsigned i = 0, n_regs = cls->n_regs; i < n_regs; ++i) {
			if (rbitset_is_set(req->limited, i))
				fprintf(F, " %s", cls->regs[i].name);
		}
	}

	if (req->should_be_same != 0)
		dump_bitmask(F, " same as", req->should_be_same);
	if (req->must_be_different != 0)
		dump_bitmask(F, " different from", req->must_be_different);

	if (req->width > 1)
		fprintf(F, " width:%d", req->width);
	if (req->ignore)
		fputs(" ignore", F);
	if (req->kills_value)
		fputs(" kills_value", F);
}

static void dump_req_reg(FILE *const F, char const *const ctx, unsigned const idx, arch_register_req_t const *const req, arch_register_t const *const reg)
{
	fprintf(F, "%s #%u = ", ctx, idx);
	dump_register_req(F, req);
	fprintf(F, " [%s]\n", be_dump_reg_name(reg));
}

static void dump_req_reg_out(FILE *const F, ir_node const *const node, unsigned const pos)
{
	arch_register_req_t const *const req = arch_get_irn_register_req_out(node, pos);
	arch_register_t     const *const reg = arch_get_irn_register_out(node, pos);
	dump_req_reg(F, "outreq", pos, req, reg);
}

static const char *get_flag_name(arch_irn_flags_t flags)
{
	assert(is_po2_or_zero(flags));
	switch (flags) {
	case arch_irn_flags_none:            return "none";
	case arch_irn_flag_dont_spill:       return "dont_spill";
	case arch_irn_flag_rematerializable: return "rematerializable";
	case arch_irn_flag_modify_flags:     return "modify_flags";
	case arch_irn_flag_simple_jump:      return "simple_jump";
	case arch_irn_flag_not_scheduled:    return "not_scheduled";
	case arch_irn_flag_schedule_first:   return "schedule_first";
	case arch_irn_flag_spill:            return "spill";
	case arch_irn_flag_reload:           return "reload";
	case arch_irn_flag_fallthrough:      return "fallthrough";
	case arch_irn_flag_backend:
		break;
	}
	return NULL;
}

void be_dump_reqs_and_registers(FILE *const F, ir_node const *const node)
{
	if (is_Proj(node)) {
		ir_node *const pred = get_Proj_pred(node);
		if (!is_Proj(pred)) {
			backend_info_t const *const info = be_get_info(pred);
			if (info && info->out_infos) {
				unsigned const num = get_Proj_num(node);
				dump_req_reg_out(F, pred, num);
			}
		}
		return;
	}

	backend_info_t const *const info = be_get_info(node);
	/* don't fail on invalid graphs */
	if (!info) {
		fputs("backend information missing\n", F);
		return;
	}

	if (info->in_reqs) {
		foreach_irn_in(node, i, op) {
			arch_register_req_t const *const req  = arch_get_irn_register_req_in(node, i);
			reg_out_info_t      const *const info = be_get_info(skip_Proj_const(op))->out_infos;
			arch_register_t     const *const reg  = info ? arch_get_irn_register(op) : NULL;
			dump_req_reg(F, "inreq", i, req, reg);
		}
	} else if (get_irn_arity(node) != 0) {
		fputs("input requirements missing\n", F);
	}

	if (info->out_infos) {
		be_foreach_out(node, o) {
			dump_req_reg_out(F, node, o);
		}
	} else {
		fputs("output requirements missing\n", F);
	}

	for (size_t i = 0; i != ARRAY_SIZE(info->add_pressure); ++i) {
		be_add_pressure_t const add = info->add_pressure[i];
		if (add != 0) {
			char const *const name  = ir_target.isa->register_classes[i].name;
			char const *const where = add > 0 ? "before" : "after";
			fprintf(F, "additional pressure %s = %d %s\n", name, abs(add), where);
		}
	}

	fputs("flags =", F);
	arch_irn_flags_t const flags = arch_get_irn_flags(node);
	if (flags == arch_irn_flags_none) {
		fputs(" none", F);
	} else {
		arch_irn_flags_t flags_left = flags;
		while (flags_left != 0) {
			arch_irn_flags_t const        flag = floor_po2(flags_left);
			char             const *const name = get_flag_name(flag);
			if (name != NULL) {
				fputc(' ', F);
				fputs(name, F);
			}
			flags_left &= ~flag;
		}
	}
	fprintf(F, " (0x%X)\n", (unsigned)flags);
}
