/**
 * Author:      Daniel Grund
 * Date:		17.01.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Implementation of the RA-Interface for an external, (non-SSA) register allocator.
 *
 * The external register allocator is a program taking 2 arguments:
 *   1) An input file in which the cfg is defined
 *   2) An output file containing the essential actions performed during allocation
 */

#if 0

The input file format
----------------------

inputfile	::= regs cfg .

regs		::= 'regs' regcount .						// Anzahl der register (0..regcount-1), die zur Verfuegung stehen

cfg			::= 'cfg' ident '{' block* edge* '}' .		// Steuerflussgraph der Prozedur

block		::= 'block' block-nr '{' insn* '}' .		// Grundblock im cfg versehen mit einer nummer

edge		::= 'cf-edge' block-nr block-nr .			// Steuerflusskante src-->tgt

insn		::= gen-insn 								// Befehl in einem block
			  | copy-insn

gen-insn	::= 'insn' insn-nr '{' uses defs '}' .
copy-insn	::= 'copy' insn-nr '{' uses defs '}' .

defs		::= 'def' var-list .						// Liste der definierten/verwendeten Variablen
uses		::= 'use' var-list .

var-list	::= var-ref
			  | var-ref var-list

var-ref		::= var-nr
			  | var-nr '<' reg-nr '>' .					// reg-nr gibt register constraint an.


ident		::= non-whitespace-char*
regcount, block-nr, insn-nr, reg-nr, var-nr ::= integer


The output file format
-----------------------

outputfile	::= 'actions' '{' action-list '}'
TODO

#endif /* documentation of file formats */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#endif

#include "pmap.h"
#include "pset.h"
#include "bitset.h"

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

/**
 * Environment with all the needed stuff
 */
typedef struct _be_raext_env_t {
	arch_env_t *aenv;
	const arch_register_class_t *cls;
	ir_graph *irg;

	FILE *f;		/**< file handle used for out- and input file */
	pmap *vars;		/**< maps variable numbers (int) to the corresponding SSA-values (pset of irns) */
	pmap *blocks;	/**< maps block numbers (int) to the block (ir_node*) having that node_nr */
} be_raext_env_t;

/******************************************************************************
     ____        _   _
    / __ \      | | (_)
   | |  | |_ __ | |_ _  ___  _ __  ___
   | |  | | '_ \| __| |/ _ \| '_ \/ __|
   | |__| | |_) | |_| | (_) | | | \__ \
    \____/| .__/ \__|_|\___/|_| |_|___/
          | |
          |_|
 *****************************************************************************/


static void ssa_destr_simple(be_raext_env_t *);
static void ssa_destr_rastello(be_raext_env_t *);
static void be_ra_extern_main(const be_main_env_t *env, ir_graph *irg);

static void (*ssa_destr)(be_raext_env_t*) = ssa_destr_simple;
static char callee[128] = "echo";

#ifdef WITH_LIBCORE

static const lc_opt_enum_const_ptr_items_t ssa_destr_items[] = {
	{ "simple",    ssa_destr_simple },
	{ "rastello",  ssa_destr_rastello },
	{ NULL,      NULL }
};

static lc_opt_enum_const_ptr_var_t ssa_destr_var = {
	(const void **) &ssa_destr, ssa_destr_items
};

static const lc_opt_table_entry_t be_ra_extern_options[] = {
	LC_OPT_ENT_ENUM_FUNC_PTR("ssa_destr", "SSA destruction flavor", &ssa_destr_var),
	LC_OPT_ENT_STR("callee", "The external program to call", callee, sizeof(callee)),
	{ NULL }
};

static void be_ra_extern_register_options(lc_opt_entry_t *root) {
	lc_opt_entry_t *grp = lc_opt_get_grp(root, "ext");

	lc_opt_add_table(grp, be_ra_extern_options);
}

#endif /* WITH_LIBCORE */

const be_ra_t be_ra_external_allocator = {
#ifdef WITH_LIBCORE
	be_ra_extern_register_options,
#endif
	be_ra_extern_main
};

/******************************************************************************
    _    _      _
   | |  | |    | |
   | |__| | ___| |_ __   ___ _ __ ___
   |  __  |/ _ \ | '_ \ / _ \ '__/ __|
   | |  | |  __/ | |_) |  __/ |  \__ \
   |_|  |_|\___|_| .__/ \___|_|  |___/
                 | |
                 |_|
 *****************************************************************************/

#define mark_as_done(irn, pos)			set_irn_link(irn, INT_TO_PTR(pos+1))
#define has_been_done(irn, pos)			(PTR_TO_INT(get_irn_link(irn)) > pos)

#define pmap_insert_sth(pmap, key, val) pmap_insert(pmap, (void *)key, (void *)val)
#define pmap_get_sth(pmap, key)			pmap_get(pmap, (void *)key)
#define set_var_nr(irn, nr)				set_irn_link(irn, INT_TO_PTR(nr))
#define get_var_nr(irn)					PTR_TO_INT(get_irn_link(irn))


/**
 * Checks if _the_ result of the irn belongs to the
 * current register class (raenv->cls)
 * NOTE: Only the first result is checked.
 */
#define is_res_in_reg_class(irn) arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)


/**
 * Checks if the irn uses or defines values of the
 * current register class (raenv->cls)
 */
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

/******************************************************************************
     _____ _____              _____            _
    / ____/ ____|  /\        |  __ \          | |
   | (___| (___   /  \ ______| |  | | ___  ___| |_ _ __
    \___ \\___ \ / /\ \______| |  | |/ _ \/ __| __| '__|
    ____) |___) / ____ \     | |__| |  __/\__ \ |_| |
   |_____/_____/_/    \_\    |_____/ \___||___/\__|_|

 *****************************************************************************/


/**
 * Insert a copy for the argument of @p start_phi found at position @p pos.
 * Also searches a phi-loop of arbitrary length to detect and resolve
 *   the class of phi-swap-problems. To search for a loop recursion is used.
 *
 * 1) Simplest case (phi with a non-phi arg):
 *     A single copy is inserted.
 *
 * 2) Phi chain (phi (with phi-arg)* with non=phi arg):
 *     Several copies are placed, each after returning from recursion.
 *
 * 3) Phi-loop:
 *     On detection a loop breaker is inserted, which is a copy of the start_phi.
 *     This copy then pretends beeing the argumnent of the last phi.
 *     Now case 2) can be used.
 *
 * The values of @p start_phi and @p pos never change during recursion.
 *
 * @p raenv      Environment with all the stuff needed
 * @p start_phi  Phi node to process
 * @p pos        Argument position to insert copy/copies for
 * @p curr_phi   Phi node currently processed during recursion. Equals start_phi on initial call
 *
 * @return NULL  If no copy is necessary
 *         NULL  If the phi has already been processed at this pos
 *               Link field is used to keep track of processed positions
 *         In all other cases the ir_node *copy which was placed is returned.
 */
static ir_node *insert_copies(be_raext_env_t *raenv, ir_node *start_phi, int pos, ir_node *curr_phi) {
	ir_node *arg = get_irn_n(curr_phi, pos);
	ir_node *arg_blk = get_nodes_block(arg);
	ir_node *pred_blk = get_Block_cfgpred_block(get_nodes_block(curr_phi), pos);
	ir_node *curr_cpy, *last_cpy;

	assert(is_Phi(start_phi) && is_Phi(curr_phi));

	if (has_been_done(start_phi, pos))
		return NULL;

	/* In case this is a 'normal' phi we insert into
	 * the schedule before the pred_blk irn */
	last_cpy = pred_blk;

	/* If we detect a loop stop recursion. */
	if (arg == start_phi) {
		ir_node *loop_breaker;
		if (start_phi == curr_phi) {
			/* Phi directly uses itself. No copy necessary */
			return NULL;
		}

		/* At least 2 phis are involved */
		/* Insert a loop breaking copy (an additional variable T) */
		loop_breaker = be_new_Copy(raenv->cls, raenv->irg, pred_blk, start_phi);
		sched_add_before(pred_blk, loop_breaker);

		arg = loop_breaker;
	}

	/* If arg is a phi in the same block we have to continue search */
	if (is_Phi(arg) && arg_blk == get_nodes_block(start_phi))
		last_cpy = insert_copies(raenv, start_phi, pos, arg);

	/* Insert copy of argument (may be the loop-breaker) */
	curr_cpy = be_new_Copy(raenv->cls, raenv->irg, pred_blk, arg);
	set_irn_n(curr_phi, pos, curr_cpy);
	mark_as_done(curr_phi, pos);
	sched_add_before(last_cpy, curr_cpy);
	return curr_cpy;
}


/**
 * Perform simple SSA-destruction with copies.
 * The order of processing _must_ be
 *  for all positions {
 *    for all phis {
 *      doit
 *    }
 *  }
 * else the magic to keep track of processed phi-positions will fail in
 * function 'insert_copies'
 */
static void ssa_destr_simple_walker(ir_node *blk, void *env) {
	be_raext_env_t *raenv = env;
	int pos, max;
	ir_node *phi;

	/* for all argument positions of the phis */
	for (pos=0, max=get_irn_arity(blk); pos<max; ++pos) {

		/* for all phi nodes (which are scheduled first) */
		sched_foreach(blk, phi) {
			if (!is_Phi(phi))
				break;

			raenv->cls = arch_get_irn_reg_class(raenv->aenv, phi, -1);
			insert_copies(raenv, phi, pos, phi);
		}
	}
}


static void ssa_destr_simple(be_raext_env_t *raenv) {
	be_clear_links(raenv->irg);
	irg_block_walk_graph(raenv->irg, ssa_destr_simple_walker, NULL, raenv);
}


static void ssa_destr_rastello(be_raext_env_t *raenv) {
	phi_class_compute(raenv->irg);
	//TODO irg_block_walk_graph(irg, ssa_destr_rastello, NULL, &raenv);
}

/******************************************************************************
   __      __   _                   ___   __      __
   \ \    / /  | |                 |__ \  \ \    / /
    \ \  / /_ _| |_   _  ___  ___     ) |  \ \  / /_ _ _ __
     \ \/ / _` | | | | |/ _ \/ __|   / /    \ \/ / _` | '__|
      \  / (_| | | |_| |  __/\__ \  / /_     \  / (_| | |
       \/ \__,_|_|\__,_|\___||___/ |____|     \/ \__,_|_|
 *****************************************************************************/

/**
 * Define variables (numbers) for all SSA-values.
 * All values in a phi class get assigned the same variable name.
 * The link field maps values to the var-name
 */
static void values_to_vars(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	ir_node *n;
	int nr;
	pset *vals;

	vals = get_phi_class(irn);

	if (!vals) {
		/* not a phi class member, value == var */
		vals = pset_new_ptr(1);
		pset_insert_ptr(vals, irn);
	}

	/* value to var mapping */
	n = pset_first(vals);
	nr = get_irn_node_nr(n);
	for (; n; n=pset_next(vals))
		set_var_nr(irn, nr);

	/* var to values mapping */
	pmap_insert_sth(raenv->vars, nr, vals);
}

/******************************************************************************
    _____
   |  __ \
   | |  | |_   _ _ __ ___  _ __   ___ _ __
   | |  | | | | | '_ ` _ \| '_ \ / _ \ '__|
   | |__| | |_| | | | | | | |_) |  __/ |
   |_____/ \__,_|_| |_| |_| .__/ \___|_|
                          | |
                          |_|
 *****************************************************************************/

/**
 * Check if node irn has a limited-constraint at position pos.
 * If yes, dump it to FILE raenv->f
 */
static INLINE void dump_constraint(be_raext_env_t *raenv, ir_node *irn, int pos) {
	bitset_t *bs = bitset_alloca(raenv->cls->n_regs);
	arch_register_req_t req;

	arch_get_register_req(raenv->aenv, &req, irn, pos);
	if (arch_register_req_is(&req, limited)) {
		int reg_nr;
		req.limited(irn, pos, bs);
		reg_nr = bitset_next_set(bs, 0);
		fprintf(raenv->f, " <%d>", reg_nr);
		assert(-1 == bitset_next_set(bs, reg_nr+1) && "Constraints with more than 1 possible register are not supported");
	}
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

		fprintf(f, "    insn %ld {\n", get_irn_node_nr(irn));

			/*
			 * print all uses
			 */
			fprintf(f, "      use");
			for (i=0, max=get_irn_arity(irn); i<max; ++i) {
				ir_node *arg = get_irn_n(irn, i);
				if (arch_irn_has_reg_class(raenv->aenv, arg, -1, raenv->cls)) {
					fprintf(f, " %d", get_var_nr(arg));
					dump_constraint(raenv, irn, i);
				}
			}
			fprintf(f,"\n");

			/*
			 * print all defs
			 */
			fprintf(f, "      def");
			/* special handling of projs */
			if (get_irn_mode(irn) == mode_T) {
				for (irn = sched_next(irn); is_Proj(irn); irn = sched_next(irn))
					if (arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)) {
						fprintf(f, " %d", get_var_nr(irn));
						dump_constraint(raenv, irn, -1);
					}
				irn = sched_prev(irn); /* for outer loop */
			} else {
				if (arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)) {
					fprintf(f, " %d", get_var_nr(irn));
					dump_constraint(raenv, irn, -1);
				}
			}
			fprintf(f,"\n");

		fprintf(f, "    }\n");
	}

	/* end the block scope */
	fprintf(f, "  }\n");
}


/**
 * Dump all control flow edges of this irg
 */
static void dump_edges(ir_node *blk, void *env) {
	be_raext_env_t *raenv = env;
	int i, max;

	if (get_irg_start_block(get_irn_irg(blk)) == blk)
		return;

	/* dump cf edges in the flow-order "pred succ" */
	for (i=0, max=get_irn_arity(blk); i<max; ++i) {
		ir_node *pred = get_Block_cfgpred_block(blk, i);
		fprintf(raenv->f, "  cf_edge %ld %ld\n", get_irn_node_nr(pred), get_irn_node_nr(blk));
	}
}


/**
 * Dump all information needed by the external
 * register allocator to a single file.
 */
static void dump_to_file(be_raext_env_t *raenv, char *filename) {
	FILE *f;

	if (!(f = fopen(filename, "wt"))) {
		fprintf(stderr, "Could not open file %s for writing\n", filename);
		exit(0xdeadbeef);
	}
	raenv->f = f;

	fprintf(f, "regs %d\n", arch_register_class_n_regs(raenv->cls));
	fprintf(f, "cfg %s {\n", filename);

	irg_block_walk_graph(raenv->irg, NULL, dump_blocks, raenv);
	irg_block_walk_graph(raenv->irg, NULL, dump_edges, raenv);

	fprintf(f, "}\n");

	fclose(f);
}


/******************************************************************************
    ______                     _
   |  ____|                   | |
   | |__  __  _____  ___ _   _| |_ ___
   |  __| \ \/ / _ \/ __| | | | __/ _ \
   | |____ >  <  __/ (__| |_| | ||  __/
   |______/_/\_\___|\___|\__,_|\__\___|
 *****************************************************************************/

/**
 * Execute the external register allocator specified in the
 * firm-option firm.be.ra.ext.callee
 */
static void execute(char *out_file, char *result_file) {
	char cmd_line[1024];
	int ret_status;

	snprintf(cmd_line, sizeof(cmd_line), "%s %s %s", callee, out_file, result_file);

	ret_status = system(cmd_line);
	assert(ret_status != -1 && "Invokation of external register allocator failed");
}

/******************************************************************************
                         _         _____                 _ _
       /\               | |       |  __ \               | | |
      /  \   _ __  _ __ | |_   _  | |__) |___  ___ _   _| | |_
     / /\ \ | '_ \| '_ \| | | | | |  _  // _ \/ __| | | | | __|
    / ____ \| |_) | |_) | | |_| | | | \ \  __/\__ \ |_| | | |_
   /_/    \_\ .__/| .__/|_|\__, | |_|  \_\___||___/\__,_|_|\__|
            | |   | |       __/ |
            |_|   |_|      |___/
 *****************************************************************************/

/**
 * Read in the actions performed by the external allocator.
 * Apply these transformations to the irg->
 */
static void read_and_apply_results(be_raext_env_t *raenv, char *filename) {
	FILE *f;

	if (!(f = fopen(filename, "rt"))) {
		fprintf(stderr, "Could not open file %s for reading\n", filename);
		exit(0xdeadbeef);
	}
	raenv->f = f;

	//TODO: free pmap entries (the psets) pmap_foreach(raenv.vars, pme)	del_pset(pme->value);

	fclose(f);
}

/******************************************************************************
    __  __       _
   |  \/  |     (_)
   | \  / | __ _ _ _ __
   | |\/| |/ _` | | '_ \
   | |  | | (_| | | | | |
   |_|  |_|\__,_|_|_| |_|
 *****************************************************************************/

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

	/* SSA destruction */
	ssa_destr(&raenv);

	be_clear_links(irg);
	phi_class_compute(irg);
	irg_walk_graph(irg, values_to_vars, NULL, &raenv);

	dump_ir_block_graph_sched(irg, "-extern-ssadestr");

	/* For all register classes */
	for(clsnr = 0, clss = arch_isa_get_n_reg_class(raenv.aenv->isa); clsnr < clss; ++clsnr) {
		char out[256], in[256];

		raenv.cls = arch_isa_get_reg_class(raenv.aenv->isa, clsnr);
		ir_snprintf(out, sizeof(out), "%F-%s.ra", irg, raenv.cls->name);
		ir_snprintf(in, sizeof(in), "%F-%s.ra.res", irg, raenv.cls->name);

		dump_to_file(&raenv, out);

		execute(out, in);

		read_and_apply_results(&raenv, in);
	}

	/* Clean up */
	pmap_destroy(raenv.blocks);
	pmap_destroy(raenv.vars);
}
