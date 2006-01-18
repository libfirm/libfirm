/**
 * Author:      Daniel Grund
 * Date:		17.01.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Implementation of the RA-Interface for an external, (non-SSA) register allocator
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pmap.h"
#include "pset.h"

#include "irprintf_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "phiclass.h"

#include "beraextern.h"
#include "bearch.h"
#include "benode_t.h"
#include "besched.h"
#include "beutil.h"

typedef struct _be_raext_env_t {
	arch_env_t *aenv;
	const arch_register_class_t *cls;
	ir_graph *irg;

	FILE *f;
	int next_var_nr;
	pmap *vars;
	pmap *blocks;
} be_raext_env_t;

/* Helpers */
#define pmap_insert_sth(pmap, key, val) pmap_insert(pmap, (void *)key, (void *)val)
#define pmap_get_sth(pmap, key)			pmap_get(pmap, (void *)key)

#define set_var_nr(irn, nr)				set_irn_link(irn, INT_TO_PTR(nr))
#define get_var_nr(irn)					(PTR_TO_INT(get_irn_link(irn)))

#define is_res_in_reg_class(irn) arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)

static INLINE int is_sth_in_reg_class(be_raext_env_t *raenv, const ir_node *irn) {
	int max, i;

	/* check arguments */
	for (i=0, max=get_irn_arity(irn); i<max; ++i)
		if (arch_irn_has_reg_class(raenv->aenv, get_irn_n(irn, i), -1, raenv->cls))
			return 1;

	/* check result(s) */
	if (get_irn_mode(irn) == mode_T) {
		ir_node *proj;
		for (proj = sched_next(irn); is_Proj(proj); proj = sched_next(proj))
			if (arch_irn_has_reg_class(raenv->aenv, proj, -1, raenv->cls))
				return 1;
		return 0;
	} else {
		return arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls);
	}

	assert(0 && "Where did you come from???");
}


#ifdef WITH_LIBCORE
static void be_ra_extern_register_options(lc_opt_entry_t *grp) {
	/* TODO */
}
#endif


/**
 * Perform simple SSA-destruction with copies
 * TODO: Phi-Swap-Problem
 */
static void ssa_destr_simple(ir_node *blk, void *env) {
	be_raext_env_t *raenv = env;
	ir_node *phi;

	/* for all phi nodes (which are scheduled at first) */
	sched_foreach(blk, phi) {
		int i, max;
		const arch_register_class_t *cls;

		if (!is_Phi(phi))
			break;

		cls = arch_get_irn_reg_class(raenv->aenv, phi, -1);

		/* for all args of these phis */
		for (i=0, max=get_irn_arity(phi); i<max; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			ir_node *pred_blk = get_Block_cfgpred_block(blk, i);
			ir_node *cpy = be_new_Copy(cls, raenv->irg, pred_blk, arg);
			set_irn_n(phi, i, cpy);
			sched_add_before(pred_blk, cpy);
		}
	}
}


/**
 * Define variables (numbers) for all SSA-values.
 * All values in a phi class get assigned the same variable name.
 * The link field maps values to the var-name
 */
static void values_to_vars(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	ir_node *n;
	pset *vals;

	if (!is_sth_in_reg_class(raenv, irn))
		return;

	vals = get_phi_class(irn);

	if (!vals) {
		/* not a phi class member, value == var */
		vals = pset_new_ptr(1);
		pset_insert_ptr(vals, irn);
	}

	/* value to var mapping */
	for (n=pset_first(vals); n; n=pset_next(vals))
		set_var_nr(irn, raenv->next_var_nr);

	/* var to values mapping */
	pmap_insert_sth(raenv->vars, raenv->next_var_nr, vals);
	raenv->next_var_nr++;
}


/**
 * Dump all blocks and instructions in that block
 */
static void dump_blocks(ir_node *blk, void *env) {
	be_raext_env_t *raenv = env;
	ir_node *irn;
	FILE *f = raenv->f;
	int nr = get_irn_node_nr(blk);
	pmap_insert_sth(raenv->blocks, nr, blk);

	/* begin block scope */
	fprintf(f, "\n");
	fprintf(f, "  block %d {\n", nr);

	/* for each instruction */
	for(irn=sched_first(blk); !sched_is_end(irn); irn=sched_next(irn)) {
		int max, i;
		if (is_Phi(irn) || !is_sth_in_reg_class(raenv, irn))
			continue;

		fprintf(f, "    insn %d {\n", get_var_nr(irn));

			/*
			 * print all defs
			 */
			fprintf(f, "      def");
			if (get_irn_mode(irn) == mode_T) {
				for (irn = sched_next(irn); is_Proj(irn); irn = sched_next(irn))
					if (arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls))
						fprintf(f, " %d", get_var_nr(irn));
				irn = sched_prev(irn); /* for outer loop */
			} else {
				if (arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls))
					fprintf(f, " %d", get_var_nr(irn));
			}
			fprintf(f,"\n");

			/*
			 * print all uses
			 */
			fprintf(f, "      use");
			for (i=0, max=get_irn_arity(irn); i<max; ++i)
				if (arch_irn_has_reg_class(raenv->aenv, get_irn_n(irn, i), -1, raenv->cls))
					fprintf(f, " %d", get_var_nr(irn));
			fprintf(f,"\n");

		fprintf(f, "    }\n");
	}

	/* end the block scope */
	fprintf(f, "  }\n", nr);
}


/**
 * Dump all control flow edges of this irg
 */
static void dump_edges(ir_node *blk, void *env) {
	be_raext_env_t *raenv = env;
	int i, max;

	/* dump cf edges in the flow-order "pred succ" */
	for (i=0, max=get_irn_arity(blk); i<max; ++i) {
		ir_node *pred = get_Block_cfgpred_block(blk, i);
		fprintf(raenv->f, "  cf_edge %d %d\n", get_irn_node_nr(pred), get_irn_node_nr(blk));
	}
}


/**
 * Dump all information needed by the external
 * register allocator to a single file.
 */
static void dump_file(be_raext_env_t *raenv, char *filename) {
	FILE *f;

	if (!(f = fopen(filename, "wt"))) {
		fprintf(stderr, "Could not open file %s\n", filename);
		exit(1);
	}

	raenv->f = f;
	fprintf(f, "regs %d\n", arch_register_class_n_regs(raenv->cls));
	fprintf(f, "cfg %s {\n", "noname");

	fprintf(f, "  variables %d\n", pmap_count(raenv->vars));
	irg_block_walk_graph(raenv->irg, dump_blocks, NULL, raenv);
	irg_block_walk_graph(raenv->irg, dump_edges, NULL, raenv);

	fprintf(f, "}\n");

	fclose(f);
}


/**
 * Allocate registers with an external program using a text-file interface.
 *
 * Do some computations (SSA-destruction and mapping of values--vars)
 * Write file
 * Execute external program
 * Read in results and apply them
 *
 */
static void be_ra_extern_main(const be_main_env_t *env, ir_graph *irg) {
	be_raext_env_t raenv;
	int clsnr, clss;

	raenv.irg = irg;
	raenv.aenv = env->arch_env;
	raenv.vars = pmap_create();
	raenv.blocks = pmap_create();
	raenv.next_var_nr = 0;

	/* SSA destruction */
	be_clear_links(irg);
	irg_block_walk_graph(irg, ssa_destr_simple, NULL, &raenv);
	phi_class_compute(irg);
	irg_walk_graph(irg, values_to_vars, NULL, &raenv);

	dump_ir_block_graph_sched(irg, "-extern-ssadestr");

	/* For all register classes */
	for(clsnr = 0, clss = arch_isa_get_n_reg_class(raenv.aenv->isa); clsnr < clss; ++clsnr) {
		char out[256], in[256];

		raenv.cls = arch_isa_get_reg_class(raenv.aenv->isa, clsnr);

		/* Write file */
		ir_snprintf(out, sizeof(out), "%F-%s.ra", irg, raenv.cls->name);
		dump_file(&raenv, out);

		/* Call */
		//execute(out, in);

		/* Read in results and apply them */
		//apply_results(&raenv, in);
		//NOTE: free pmap entries (the psets) pmap_foreach(raenv.vars, pme)	del_pset(pme->value);


	}

	/* Clean up */
	pmap_destroy(raenv.blocks);
	pmap_destroy(raenv.vars);
}


const be_ra_t be_ra_external_allocator = {
#ifdef WITH_LIBCORE
	be_ra_extern_register_options,
#endif
	be_ra_extern_main
};
