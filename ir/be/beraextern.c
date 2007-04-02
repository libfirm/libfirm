/**
 * Author:      Daniel Grund
 * Date:		17.01.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Implementation of the RA-Interface for an external, (non-SSA) register allocator.
 *
 * The external register allocator is a program:
 *    PROG -i INPUTFILE -o OUTPUTFILE
 *
 *   1) Input file defines the interference graph
 *   2) Output file contains the instructions to perform
 *


The input file format
----------------------

inputfile	::= regs nodes interf affinities .

regs		::= 'regs' regcount .						// Anzahl der register (0..regcount-1), die zur Verfuegung stehen

nodes		::= 'nodes' '{' node* '}' .					// All nodes in the graph

node		::= node-info
			  | node-info '<' reg-nr '>' .				// Reg-nr is present in case of constraints

node-info	::= node-nr spill-costs .

interf		::= 'interferences' '{' i-edge* '}' .		// Interference edges of the graph

i-edge		::= '(' node-nr ',' node-nr ')' .

affinities	::= 'affinities' '{' a-edge* '}' .			// Affinity edges of the graph

a-edge		::= '(' node-nr ',' node-nr ',' weight ')' .


weight, regcount, node-nr ::= int32 .
spill-costs ::= int32 .									// negative spill costs indicate unspillable

The output file format
-----------------------

outputfile	::= spills | allocs .

spills		::= 'spills' node-nr+ .

allocs		::= 'allocs' alloc* .

alloc		::= node-nr reg-nr .


******** End of file format docu ********/
#ifdef NOT_PORTED
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#include "set.h"
#include "pset.h"
#include "pmap.h"
#include "bitset.h"
#include "raw_bitset.h"
#include "xmalloc.h"

#include "irprintf_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "irdom_t.h"
#include "phiclass.h"

#include "bemodule.h"
#include "beraextern.h"
#include "beabi.h"
#include "bearch.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "beinsn_t.h"

#include "bessadestrsimple.h"

#define DBG_LEVEL 2

/**
 * Environment with all the needed stuff
 */
typedef struct _be_raext_env_t {
	arch_env_t *aenv;
	const arch_register_class_t *cls;
	be_irg_t *birg;
	ir_graph *irg;

	FILE *f;				/**< file handle used for out- and input file */
	set *vars;				/**< contains all be_var_info_t */
	int n_cls_vars;			/**< length of the array cls_vars */
	be_var_info_t **cls_vars;	/**< only the var_infos for current cls. needed for double iterating */
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} be_raext_env_t;



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


#define pset_foreach(pset, irn)  for(irn=pset_first(pset); irn; irn=pset_next(pset))
#define set_foreach(set, e)  for(e=set_first(set); e; e=set_next(set))

/**
 * Checks if _the_ result of the irn belongs to the
 * current register class (raenv->cls)
 * NOTE: Only the first result is checked.
 */
#define is_res_in_reg_class(irn) arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)

static INLINE ir_node *get_first_non_phi(pset *s) {
	ir_node *irn;

	pset_foreach(s, irn)
		if (!is_Phi(irn)) {
			pset_break(s);
			return irn;
		}

	assert(0 && "There must be a non-phi-irn in this");
	return NULL;
}

static INLINE ir_node *get_first_phi(pset *s) {
	ir_node *irn;

	pset_foreach(s, irn)
		if (is_Phi(irn)) {
			pset_break(s);
			return irn;
		}

	assert(0 && "There must be a phi in this");
	return NULL;
}

static int get_loop_weight(ir_node *irn) {
	int cost = 0;
	ir_loop *loop = get_irn_loop(get_nodes_block(irn));

	if (loop) {
		int d = get_loop_depth(loop);
		cost = d*d;
	}
	return cost+1;
}

#define get_const_weight(irn) (1)

#define get_spill_weight(irn)    get_loop_weight(irn)
#define get_reload_weight(irn)   get_loop_weight(irn)
#define get_affinity_weight(irn) get_loop_weight(irn)

/******************************************************************************
    _____                _            _____            _
   / ____|              | |          / ____|          (_)
  | |     ___  _ __  ___| |_ _ __   | |     ___  _ __  _  ___  ___
  | |    / _ \| '_ \/ __| __| '__|  | |    / _ \| '_ \| |/ _ \/ __|
  | |___| (_) | | | \__ \ |_| |     | |___| (_) | |_) | |  __/\__ \
   \_____\___/|_| |_|___/\__|_|      \_____\___/| .__/|_|\___||___/
                                                | |
                                                |_|
 *****************************************************************************/

static void handle_constraints_insn(be_raext_env_t *env, be_insn_t *insn)
{
	ir_node *bl = get_nodes_block(insn->irn);
	int i;

	for(i = 0; i < insn->use_start; ++i) {
		be_operand_t *op = &insn->ops[i];

		if(op->has_constraints) {
			ir_node *cpy = be_new_Copy(op->req->cls, env->irg, bl, op->carrier);
			sched_add_before(insn->next_insn, cpy);
			edges_reroute(op->carrier, cpy, env->irg);
		}
	}

	for(i = insn->use_start; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];

		if(op->has_constraints) {
			ir_node *cpy = be_new_Copy(op->req->cls, env->irg, bl, op->carrier);
			sched_add_before(insn->irn, cpy);
			set_irn_n(insn->irn, op->pos, cpy);
			be_set_constr_limited(cpy, BE_OUT_POS(0), op->req);
		}
	}
}

static void handle_constraints_block(ir_node *bl, void *data)
{
	be_raext_env_t *raenv = data;
	int active            = bl != get_irg_start_block(raenv->irg);

	ir_node *irn;
	be_insn_env_t ie;
	struct obstack obst;

	ie.cls           = raenv->cls;
	ie.aenv          = raenv->aenv;
	ie.obst          = &obst;
	ie.ignore_colors = NULL;
	obstack_init(&obst);

	irn = sched_first(bl);
	while(!sched_is_end(irn)) {
		be_insn_t *insn = be_scan_insn(&ie, irn);

		if(insn->has_constraints)
			handle_constraints_insn(raenv, insn);

		if(be_is_Barrier(irn))
			active = !active;

		irn = insn->next_insn;
		obstack_free(&obst, insn);
	}
}

static void handle_constraints(be_raext_env_t *raenv) {
	irg_block_walk_graph(raenv->irg, NULL, handle_constraints_block, raenv);
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


static void extract_vars_of_cls(be_raext_env_t *raenv) {
	int count = 0;
	be_var_info_t *vi;

	raenv->cls_vars = xmalloc(set_count(raenv->vars) * sizeof(*raenv->cls_vars));
	assert(raenv->cls_vars);

	set_foreach(raenv->vars, vi)
		if (is_res_in_reg_class(get_first_non_phi(vi->values)))
			raenv->cls_vars[count++] = vi;

	raenv->cls_vars = realloc(raenv->cls_vars, count * sizeof(*raenv->cls_vars));
	assert(raenv->cls_vars);

	raenv->n_cls_vars = count;
}


/**
 * Check if node irn has a limited-constraint at position pos.
 * If yes, dump it to FILE raenv->f
 */
static INLINE void dump_constraint(be_raext_env_t *raenv, ir_node *irn, int pos) {
	const arch_register_req_t *req;

	req = arch_get_register_req(raenv->aenv, irn, pos);
	if (arch_register_req_is(req, limited)) {
		unsigned reg_nr;

		reg_nr = rbitset_next(req->limited, 0, 1);
		fprintf(raenv->f, "<%d>", reg_nr);
		assert(rbitset_popcnt(req->limited, raenv->cls->n_regs) <= 1
				&& "Constraints with more than 1 possible register are not supported");
	}
}

#define UNSPILLABLE -1

static INLINE int get_spill_costs(be_raext_env_t *raenv, be_var_info_t *vi) {
	ir_node *irn;
	int c_spills=0, c_reloads=0;

	pset_foreach(vi->values, irn) {
		if (arch_irn_is(raenv->aenv, irn, ignore) || be_is_Reload(irn)) {
			pset_break(vi->values);
			return UNSPILLABLE;
		}

		if (is_Phi(irn)) {
			/* number of reloads is the number of non-phi uses of all values of this var */
			const ir_edge_t *edge;
			foreach_out_edge(irn, edge)
				if (!is_Phi(edge->src))
					c_reloads += get_reload_weight(edge->src);
		} else {
			/* number of spills is the number of non-phi values for this var */
			c_spills += get_spill_weight(irn);
		}
	}

	return c_spills + c_reloads;
}

static void dump_nodes(be_raext_env_t *raenv) {
	FILE *f = raenv->f;
	int i;

	fprintf(f, "\nnodes {\n");

	for (i=0; i<raenv->n_cls_vars; ++i) {
		be_var_info_t *vi = raenv->cls_vars[i];

		if (vi->var_nr == SET_REMOVED)
			continue;

		fprintf(f, "%d %d", vi->var_nr, get_spill_costs(raenv, vi));
		dump_constraint(raenv, get_first_non_phi(vi->values), -1);
		fprintf(f, "\n");
	}

	fprintf(f, "}\n");
	fflush(f);
}


static void dump_interferences(be_raext_env_t *raenv) {
	int i,o;
	be_var_info_t *vi1, *vi2;
	ir_node *irn1, *irn2;
	FILE *f = raenv->f;
	be_lv_t *lv = raenv->birg->lv;

	fprintf(f, "\ninterferences {\n");

	for (i=0; i<raenv->n_cls_vars; ++i) {
		vi1 = raenv->cls_vars[i];

		if (vi1->var_nr == SET_REMOVED)
			continue;

		for (o=i+1; o<raenv->n_cls_vars; ++o) {
			vi2 = raenv->cls_vars[o];

			if (vi2->var_nr == SET_REMOVED)
				continue;

			pset_foreach(vi1->values, irn1)
				pset_foreach(vi2->values, irn2)
					if (values_interfere(lv, irn1, irn2)) {
						pset_break(vi1->values);
						pset_break(vi2->values);
						fprintf(f, "(%d, %d)\n", vi1->var_nr, vi2->var_nr);
						goto NextVar;
					}

NextVar: ;
		}
	}
	fprintf(f, "}\n");
}

static void dump_affinities_walker(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	const arch_register_req_t *req;
	int pos, max;
	be_var_info_t *vi1, *vi2;

	if (arch_get_irn_reg_class(raenv->aenv, irn, -1) != raenv->cls || arch_irn_is(raenv->aenv, irn, ignore))
		return;

	vi1 = be_get_var_info(irn);

	/* copies have affinities */
	if (arch_irn_class_is(raenv->aenv, irn, copy)) {
		ir_node *other = be_get_Copy_op(irn);

		if (! arch_irn_is(raenv->aenv, other, ignore)) {
			vi2 = be_get_var_info(other);

			fprintf(raenv->f, "(%d, %d, %d)\n",  vi1->var_nr, vi2->var_nr, get_affinity_weight(irn));
		}
	}


	/* should_be_equal constraints are affinites */
	for (pos = 0, max = get_irn_arity(irn); pos<max; ++pos) {
		req = arch_get_register_req(raenv->aenv, irn, pos);

		if (arch_register_req_is(req, should_be_same)) {
			ir_node *other = get_irn_n(irn, req->other_same);
			if(arch_irn_is(raenv->aenv, other, ignore)) {
				vi2 = be_get_var_info(other);

				fprintf(raenv->f, "(%d, %d, %d)\n",  vi1->var_nr, vi2->var_nr, get_affinity_weight(irn));
			}
		}
	}
}


static void dump_affinities(be_raext_env_t *raenv) {
	fprintf(raenv->f, "\naffinities {\n");
	irg_walk_graph(raenv->irg, NULL, dump_affinities_walker, raenv);
	fprintf(raenv->f, "}\n");
}

/**
 * Dump all information needed by the external
 * register allocator to a single file.
 */
static void dump_to_file(be_raext_env_t *raenv, char *filename) {
	FILE *f;

	if (!(f = fopen(filename, "wt"))) {
		fprintf(stderr, "Could not open file %s for writing\n", filename);
		assert(0);
		exit(0xdeadbeef);
	}
	raenv->f = f;

	/* dump register info */
	fprintf(f, "regs %d\n", arch_register_class_n_regs(raenv->cls));

	/* dump the interference graph */
	dump_nodes(raenv);
	dump_interferences(raenv);
	dump_affinities(raenv);

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
static void execute(char *prog_to_call, char *out_file, char *result_file) {
	char cmd_line[1024];
	int ret_status;

	snprintf(cmd_line, sizeof(cmd_line), "%s -i %s -o %s", prog_to_call, out_file, result_file);
	cmd_line[sizeof(cmd_line) - 1] = '\0';

	ret_status = system(cmd_line);
	assert(ret_status != -1 && "Invokation of external register allocator failed");
	assert(ret_status == 0 && "External register allocator is unhappy with sth.");
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
 * Spill a variable and add reloads before all uses.
 */
static INLINE void var_add_spills_and_reloads(be_raext_env_t *raenv, int var_nr) {
	be_var_info_t *vi = be_var_find(raenv->vars, var_nr);
	ir_node *spill=NULL, *ctx, *irn;
	ir_mode *mode;
	const ir_edge_t *edge, *ne;
	pset *spills  = pset_new_ptr(4);	/* the spills of this variable */
	pset *reloads = pset_new_ptr(4);	/* the reloads of this variable */
	be_lv_t *lv = raenv->birg->lv;
	be_dom_front_info_t *dom_front = raenv->birg->dom_front;
	int new_size, n_spills, n_reloads;

	assert(vi && "Variable nr does not exist!");
	assert(pset_count(vi->values) && "There are no values associated to this variable");

	/* the spill context is set to an arbitrary node of the phi-class,
	 * or the node itself if it is not member of a phi class
	 */
	if (pset_count(vi->values) == 1)
		ctx = get_first_non_phi(vi->values);
	else
		ctx = get_first_phi(vi->values);

	DBG((raenv->dbg, LEVEL_2, "Spill context: %+F\n", ctx));

	/* for each value of this variable insert the spills */
	pset_foreach(vi->values, irn) {
		if (is_Phi(irn)) {
			sched_remove(irn);
			continue;
		}

		/* all ordinary nodes must be spilled */
		DBG((raenv->dbg, LEVEL_2, "  spilling %+F\n", irn));
		spill = be_spill(raenv->aenv, irn);

		/* remember the spill */
		pset_insert_ptr(spills, spill);
	}

	assert(spill && "There must be at least one non-phi-node");

	mode = get_irn_mode(get_irn_n(spill, be_pos_Spill_val));

	/* insert reloads and wire them arbitrary*/
	pset_foreach(vi->values, irn) {
		foreach_out_edge_safe(irn, edge, ne) {
			ir_node *reload, *src = edge->src;
			if (is_Phi(src) || be_is_Spill(src))
				continue;

			/* all real uses must be reloaded */
			DBG((raenv->dbg, LEVEL_2, "  reloading before %+F\n", src));
			reload = be_reload(raenv->aenv, raenv->cls, edge->src, mode, spill);
			set_irn_n(edge->src, edge->pos, reload);

			/* remember the reload */
			pset_insert_ptr(reloads, reload);
		}
	}

	/* correct the reload->spill pointers... */
	be_ssa_constr_set_ignore(dom_front, lv, spills, NULL);


	/****** correct the variable <--> values mapping: ******
	 *
	 *  - if we had a phi class it gets split into several new variables
	 *  - all reloads are new variables
	 */
	n_spills = pset_count(spills);
	n_reloads = pset_count(reloads);

	/* first make room for new pointers in the cls_var array */
	new_size = raenv->n_cls_vars + n_reloads + ((n_spills>1) ? n_spills : 0);
	raenv->cls_vars = realloc(raenv->cls_vars, (new_size) * sizeof(*raenv->cls_vars));
	assert(raenv->cls_vars && "Out of mem!?");

	/* if we had a real phi-class, we must... */
	if (pset_count(spills) > 1) {
		/* ...remove the old variable corresponding to the phi class */
		vi->var_nr = SET_REMOVED;

		/* ...add new vars for each non-phi-member */
		pset_foreach(spills, irn) {
			ir_node *spilled = get_irn_n(irn, be_pos_Spill_val);
			raenv->cls_vars[raenv->n_cls_vars++] = be_var_add_value(raenv->vars, get_irn_node_nr(spilled), spilled);
		}
	}

	/* add new variables for all reloads */
	pset_foreach(reloads, irn) {
		assert(get_irn_node_nr(irn) != 1089);
		raenv->cls_vars[raenv->n_cls_vars++] = be_var_add_value(raenv->vars, get_irn_node_nr(irn), irn);
	}

	del_pset(spills);
	del_pset(reloads);
}

#define INVALID_FILE_FORMAT assert(0 && "Invalid file format.")
#define BUFLEN 32
#define BUFCONV " %32s "

/**
 * Read in the actions performed by the external allocator.
 * Apply these transformations to the irg.
 * @return 1 if an allocation was read in. 0 otherwise.
 */
static int read_and_apply_results(be_raext_env_t *raenv, char *filename) {
	FILE *f;
	char buf[BUFLEN];
	int is_allocation = 0;

	if (!(f = fopen(filename, "rt"))) {
		fprintf(stderr, "Could not open file %s for reading\n", filename);
		assert(0);
		exit(0xdeadbeef);
	}
	raenv->f = f;

	/* read the action */
	if (fscanf(f, BUFCONV, buf) != 1)
		INVALID_FILE_FORMAT;

	/* do we spill */
	if (!strcmp(buf, "spills")) {
		int var_nr;
		while (fscanf(f, " %d ", &var_nr) == 1)
			var_add_spills_and_reloads(raenv, var_nr);
	} else

	/* or do we allocate */
	if (!strcmp(buf, "allocs")) {
		int var_nr, reg_nr;

		is_allocation = 1;
		while (fscanf(f, " %d %d ", &var_nr, &reg_nr) == 2) {
			ir_node *irn;
			pset *vals = be_get_var_values(raenv->vars, var_nr);

			assert(vals && "Variable nr does not exist!");
			pset_foreach(vals, irn)
				arch_set_irn_register(raenv->aenv, irn, arch_register_for_index(raenv->cls, reg_nr));
		}
	} else
		INVALID_FILE_FORMAT;

	if (!feof(f))
		INVALID_FILE_FORMAT;

	fclose(f);

	return is_allocation;
}

static void check_allocation(be_raext_env_t *raenv) {
	int i, o;
	be_lv_t *lv = raenv->birg->lv;

	for (i=0; i<raenv->n_cls_vars; ++i) {
		be_var_info_t *vi1 = raenv->cls_vars[i];

		if (vi1->var_nr == SET_REMOVED)
			continue;

		for (o=0; o<i; ++o) {
			be_var_info_t *vi2 = raenv->cls_vars[o];
			ir_node *irn1, *irn2;

			if (vi2->var_nr == SET_REMOVED)
				continue;

			pset_foreach(vi1->values, irn1)
				pset_foreach(vi2->values, irn2)
					if (values_interfere(lv, irn1, irn2) && arch_get_irn_register(raenv->aenv, irn1) == arch_get_irn_register(raenv->aenv, irn2)) {
						dump_ir_block_graph_sched(raenv->irg, "ERROR");
						ir_fprintf(stdout, "SSA values %+F and %+F interfere. They belong to variable %d and %d respectively.\n", irn1, irn2, vi1->var_nr, vi2->var_nr);
						assert(0 && "ERROR graph dumped");
					}
		}
	}
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
 * Default values for options
 */
static char callee[128] = "\"E:/user/kimohoff/public/register allocator\"";
//static char callee[128] = "/ben/kimohoff/ipd-registerallocator/register_allocator";


/**
 * Allocate registers with an external program using a text-file interface.
 *
 * Do some computations (SSA-destruction and mapping of values--vars)
 * Write file
 * Execute external program
 * Read in results and apply them
 *
 */
static void be_ra_extern_main(be_irg_t *birg) {
	be_main_env_t *env = birg->main_env;
	ir_graph *irg = birg->irg;

 	be_raext_env_t raenv;
	int clsnr, clss;

	be_assure_dom_front(birg);
	be_assure_liveness(birg);
	edges_assure(irg);

	raenv.irg      = irg;
	raenv.birg     = birg;
	raenv.aenv     = env->arch_env;
	FIRM_DBG_REGISTER(raenv.dbg, "firm.be.raextern");

	/* Insert copies for constraints */
	for(clsnr = 0, clss = arch_isa_get_n_reg_class(raenv.aenv->isa); clsnr < clss; ++clsnr) {
		raenv.cls = arch_isa_get_reg_class(raenv.aenv->isa, clsnr);
		handle_constraints(&raenv);
	}

	be_dump(irg, "-extern-constr", dump_ir_block_graph_sched);

	/* SSA destruction respectively transformation into "Conventional SSA" */
	raenv.vars = be_ssa_destr_simple(irg, env->arch_env);
	be_dump(irg, "-extern-ssadestr", dump_ir_block_graph_sched);


	/* For all register classes */
	for(clsnr = 0, clss = arch_isa_get_n_reg_class(raenv.aenv->isa); clsnr < clss; ++clsnr) {
		int done, round = 1;
		char out[256], in[256];

		raenv.cls = arch_isa_get_reg_class(raenv.aenv->isa, clsnr);

		extract_vars_of_cls(&raenv);

		do {
			ir_snprintf(out, sizeof(out), "%F-%s-%d.ra", irg, raenv.cls->name, round);
			ir_snprintf(in, sizeof(in), "%F-%s-%d.ra.res", irg, raenv.cls->name, round);

			be_liveness(irg);

			dump_to_file(&raenv, out);
			execute(callee, out, in);
			done = read_and_apply_results(&raenv, in);
			be_abi_fix_stack_nodes(birg->abi);

			ir_snprintf(in, sizeof(in), "-extern-%s-round-%d", raenv.cls->name, round);
			be_dump(irg, in, dump_ir_block_graph_sched);

			round++;
		} while (!done);

		check_allocation(&raenv);

		free(raenv.cls_vars);
	}

	be_dump(irg, "-extern-alloc", dump_ir_block_graph_sched);

	/* Clean up */
	free_ssa_destr_simple(raenv.vars);

	be_invalidate_liveness(birg);
}

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

static const lc_opt_enum_func_ptr_items_t ssa_destr_items[] = {
	{ "simple",     (int (*)(void)) be_ssa_destr_simple }, /* TODO make (void*) casts nicer */
	{ NULL,      NULL }
};

static set* (*ssa_destr)(ir_graph*,const arch_env_t*) = be_ssa_destr_simple;

static lc_opt_enum_func_ptr_var_t ssa_destr_var = {
	 (int (**)(void)) &ssa_destr, ssa_destr_items
};

static const lc_opt_table_entry_t be_ra_extern_options[] = {
	LC_OPT_ENT_ENUM_FUNC_PTR("ssa_destr", "SSA destruction flavor", &ssa_destr_var),
	LC_OPT_ENT_STR("callee", "The external program to call", callee, sizeof(callee)),
	{ NULL }
};

static be_ra_t be_ra_external_allocator = {
	be_ra_extern_main
};

void be_init_raextern(void) {
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *blocksched_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *ext_grp = lc_opt_get_grp(blocksched_grp, "ext");

	lc_opt_add_table(ext_grp, be_ra_extern_options);

	be_register_allocator("ext", &be_ra_external_allocator);
}
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_raextern);

#endif
