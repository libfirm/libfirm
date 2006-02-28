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

interf		::= 'interferences' '{' edge* '}' .			// Interference edges of the graph

affinities	::= 'affinities' '{' edge* '}' .			// Affinity edges of the graph

edge		::= '(' node-nr ',' node-nr ')' .


spill-costs, regcount, node-nr ::= integer .


The output file format
-----------------------

outputfile	::= spills | allocs .

spills		::= 'spills' node-nr+ .

allocs		::= 'allocs' alloc* .

alloc		::= node-nr reg-nr .


******** End of file format docu ********/

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

#include "set.h"
#include "pset.h"
#include "pmap.h"
#include "bitset.h"

#include "irprintf_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "irdom_t.h"
#include "phiclass.h"

#include "beraextern.h"
#include "bearch.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched.h"
#include "beutil.h"
#include "belive_t.h"

typedef struct _var_info_t var_info_t;

/**
 * Environment with all the needed stuff
 */
typedef struct _be_raext_env_t {
	arch_env_t *aenv;
	const arch_register_class_t *cls;
	ir_graph *irg;
	dom_front_info_t *dom_info;

	FILE *f;				/**< file handle used for out- and input file */
	set *vars;				/**< contains all var_info_t */
	int n_cls_vars;			/**< length of the array cls_vars */
	var_info_t **cls_vars;	/**< only the var_infos for current cls. needed for double iterating */
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

static void handle_constraints_walker(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	arch_register_req_t req;
	int pos, max;

	/* handle output constraints
	 * user -> irn    becomes    user -> cpy -> irn
	 */
	arch_get_register_req(raenv->aenv, &req, irn, -1);
	if (arch_register_req_is(&req, limited)) {
		ir_node *cpy = be_new_Copy(req.cls, raenv->irg, get_nodes_block(irn), irn);
		const ir_edge_t *edge;

		/* all users of the irn use the copy instead */
		sched_add_after(irn, cpy);
		foreach_out_edge(irn, edge)
			set_irn_n(edge->src, edge->pos, cpy);
	}


	/* handle input constraints by converting them into output constraints
	 * of copies of the former argument
	 * irn -> arg   becomes  irn -> copy -> arg
     */
	for (pos = 0, max = get_irn_arity(irn); pos<max; ++pos) {
		arch_get_register_req(raenv->aenv, &req, irn, pos);
		if (arch_register_req_is(&req, limited)) {
			ir_node *arg = get_irn_n(irn, pos);
			ir_node *cpy = be_new_Copy(req.cls, raenv->irg, get_nodes_block(irn), arg);

			/* use the copy instead */
			sched_add_before(irn, cpy);
			set_irn_n(irn, pos, cpy);

			/* set an out constraint for the copy */
			be_set_constr_limited(cpy, -1, &req);
		}
	}
}

static void handle_constraints(be_raext_env_t *raenv) {
	irg_block_walk_graph(raenv->irg, NULL, handle_constraints_walker, raenv);
}


/******************************************************************************
     _____ _____              _____            _
    / ____/ ____|  /\        |  __ \          | |
   | (___| (___   /  \ ______| |  | | ___  ___| |_ _ __
    \___ \\___ \ / /\ \______| |  | |/ _ \/ __| __| '__|
    ____) |___) / ____ \     | |__| |  __/\__ \ |_| |
   |_____/_____/_/    \_\    |_____/ \___||___/\__|_|

 *****************************************************************************/

#define mark_as_done(irn, pos)			set_irn_link(irn, INT_TO_PTR(pos+1))
#define has_been_done(irn, pos)			(PTR_TO_INT(get_irn_link(irn)) > pos)

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
	assert(0 && "NYI");
	exit(0xDeadBeef);
	/*
	phi_class_compute(raenv->irg);
	irg_block_walk_graph(irg, ssa_destr_rastello, NULL, &raenv);
	*/
}

/******************************************************************************
   __      __   _       ___   __      __
   \ \    / /  | |     |__ \  \ \    / /
    \ \  / /_ _| |___     ) |  \ \  / /_ _ _ __ ___
     \ \/ / _` | / __|   / /    \ \/ / _` | '__/ __|
      \  / (_| | \__ \  / /_     \  / (_| | |  \__ \
       \/ \__,_|_|___/ |____|     \/ \__,_|_|  |___/
 *****************************************************************************/

/**
 * This struct maps a variable (nr) to the values belonging to this variable
 */
struct _var_info_t {
	int var_nr;		/* the key */
	pset *values;	/* the ssa-values belonging to this variable */
};

#define SET_REMOVED -1

/**
 * The link field of an irn points to the var_info struct
 * representing the corresponding variable.
 */
#define set_var_info(irn, vi)				set_irn_link(irn, vi)
#define get_var_info(irn)					((var_info_t *)get_irn_link(irn))

#define HASH_VAR_NR(var_nr) var_nr

static int compare_var_infos(const void *e1, const void *e2, size_t size) {
	const var_info_t *v1 = e1;
	const var_info_t *v2 = e2;

	if (v1->var_nr == SET_REMOVED || v2->var_nr == SET_REMOVED)
		return 1;

	return v1->var_nr != v2->var_nr;
}

static INLINE var_info_t *var_find(set *vars, int var_nr) {
	var_info_t vi;
	vi.var_nr = var_nr;

	return set_find(vars, &vi, sizeof(vi), HASH_VAR_NR(var_nr));
}

static INLINE var_info_t *var_find_or_insert(set *vars, int var_nr) {
	var_info_t vi, *found;
	memset(&vi, 0, sizeof(vi));
	vi.var_nr = var_nr;

	found = set_insert(vars, &vi, sizeof(vi), HASH_VAR_NR(var_nr));

	if (!found->values)
		found->values  = pset_new_ptr(1);

	return found;
}

/**
 * Adds a value to a variable. Sets all pointers accordingly.
 */
static INLINE var_info_t *var_add_value(be_raext_env_t *raenv, int var_nr, ir_node *irn) {
	var_info_t *vi = var_find_or_insert(raenv->vars, var_nr);

	/* var 2 value mapping */
	pset_insert_ptr(vi->values, irn);

	/* value 2 var mapping */
	set_var_info(irn, vi);

	return vi;
}

static INLINE pset *get_var_values(be_raext_env_t *raenv, int var_nr) {
	var_info_t *vi = var_find(raenv->vars, var_nr);
	assert(vi && "Variable does not exist");
	return vi->values;
}

/**
 * Define variables (numbers) for all SSA-values.
 * All values in a phi class get assigned the same variable name.
 * The link field maps values to the var-name
 */
static void values_to_vars(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	int nr;
	pset *vals;

	vals = get_phi_class(irn);

	if (vals) {
		nr = get_irn_node_nr(get_first_phi(vals));
	} else {
		/* not a phi class member, value == var */
		nr = get_irn_node_nr(irn);
		vals = pset_new_ptr(1);
		pset_insert_ptr(vals, irn);
	}

	/* values <--> var mapping */
	pset_foreach(vals, irn)
		var_add_value(raenv, nr, irn);
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
	var_info_t *vi;

	raenv->cls_vars = malloc(set_count(raenv->vars) * sizeof(*raenv->cls_vars));
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
	bitset_t *bs = bitset_alloca(raenv->cls->n_regs);
	arch_register_req_t req;

	arch_get_register_req(raenv->aenv, &req, irn, pos);
	if (arch_register_req_is(&req, limited)) {
		int reg_nr;
		req.limited(req.limited_env, bs);
		reg_nr = bitset_next_set(bs, 0);
		fprintf(raenv->f, "<%d>", reg_nr);
		assert(-1 == bitset_next_set(bs, reg_nr+1) && "Constraints with more than 1 possible register are not supported");
	}
}

static INLINE int get_spill_costs(var_info_t *vi) {
	ir_node *irn;
	int n_spills=0, n_reloads=0;

	pset_foreach(vi->values, irn) {
		if (is_Phi(irn)) {
			/* number of reloads is the number of non-phi uses of all values of this var */
			const ir_edge_t *edge;
			foreach_out_edge(irn, edge)
				if (!is_Phi(edge->src))
					n_reloads++;
		} else {
			/* number of spills is the number of non-phi values for this var */
			n_spills++;
		}
	}

	return n_spills + n_reloads;
}

static void dump_nodes(be_raext_env_t *raenv) {
	FILE *f = raenv->f;
	int i;

	fprintf(f, "\nnodes {\n");

	for (i=0; i<raenv->n_cls_vars; ++i) {
		var_info_t *vi = raenv->cls_vars[i];

		if (vi->var_nr == SET_REMOVED)
			continue;

		fprintf(f, "%d %d", vi->var_nr, get_spill_costs(vi));
		dump_constraint(raenv, get_first_non_phi(vi->values), -1);
		fprintf(f, "\n");
	}

	fprintf(f, "}\n");
}


static void dump_interferences(be_raext_env_t *raenv) {
	int i,o;
	var_info_t *vi1, *vi2;
	ir_node *irn1, *irn2;
	FILE *f = raenv->f;

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
					if (values_interfere(irn1, irn2)) {
						pset_break(vi1->values);
						pset_break(vi2->values);
						fprintf(f, "(%d, %d)\n", vi1->var_nr, vi2->var_nr);
					}
		}
	}
	fprintf(f, "}\n");
}


static void dump_affinities_walker(ir_node *irn, void *env) {
	be_raext_env_t *raenv = env;
	arch_register_req_t req;
	int pos, max;
	var_info_t *vi1, *vi2;

	vi1 = get_var_info(irn);

	/* copies have affinities */
	/* TODO? remove this case by adding should_be_equal requirements */
	if (arch_irn_classify(raenv->aenv, irn) == arch_irn_class_copy) {
		vi2 = get_var_info(get_irn_n(irn, 0));

		fprintf(raenv->f, "(%d, %d)\n",  vi1->var_nr, vi2->var_nr);
	}


	/* should_be_equal constraints are affinites */
	for (pos = 0, max = get_irn_arity(irn); pos<max; ++pos) {
		arch_get_register_req(raenv->aenv, &req, irn, pos);

		if (arch_register_req_is(&req, should_be_same)) {
			vi2 = get_var_info(req.other_same);

			fprintf(raenv->f, "(%d, %d)\n",  vi1->var_nr, vi2->var_nr);
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
 * Spill a variable and add reloads before all uses.
 */
static INLINE void var_add_spills_and_reloads(be_raext_env_t *raenv, int var_nr) {
	var_info_t *vi = var_find(raenv->vars, var_nr);
	ir_node *spill=NULL, *ctx, *irn;
	const ir_edge_t *edge;
	pset *spills  = pset_new_ptr(4);	/* the spills of this variable */
	pset *reloads = pset_new_ptr(4);	/* the reloads of this variable */
	int new_size, n_spills, n_reloads;

	assert(vi && "Variable nr does not exist!");
	assert(pset_count(vi->values) && "There are no values associated to this variable");

	/* the spill context is set to an arbitrary node of the phi-class */
	ctx = get_first_phi(vi->values);

	/* for each value of this variable insert the spills */
	pset_foreach(vi->values, irn) {
		if (is_Phi(irn))
			continue;

		/* all ordinary nodes must be spilled */
		spill = be_new_Spill(raenv->cls, raenv->irg, get_nodes_block(irn), irn, ctx);
		sched_add_after(irn, spill);

		/* remember the spill */
		pset_insert_ptr(spills, spill);
	}

	assert(spill && "There must be at least one non-phi-node");

	/* insert reloads and wire them arbitrary*/
	pset_foreach(vi->values, irn)
		foreach_out_edge(irn, edge) {
			ir_node *reload, *src = edge->src;
			if (is_Phi(src))
				continue;

			/* all real uses must be reloaded */
			reload = be_new_Reload(raenv->cls, raenv->irg, get_nodes_block(src), get_irn_mode(get_irn_n(spill, 0)), spill);
			sched_add_before(src, reload);

			/* remember the reload */
			pset_insert_ptr(reloads, reload);
		}

	/* correct the reload->spill pointers... */
	be_ssa_constr_sets(raenv->dom_info, spills, reloads);


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
			ir_node *spilled = get_irn_n(irn, 0);
			raenv->cls_vars[raenv->n_cls_vars++] = var_add_value(raenv, get_irn_node_nr(spilled), spilled);
		}
	}

	/* add new variables for all reloads */
	pset_foreach(reloads, irn)
		raenv->cls_vars[raenv->n_cls_vars++] = var_add_value(raenv, get_irn_node_nr(irn), irn);



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
			pset *vals = get_var_values(raenv, var_nr);

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
static void (*ssa_destr)(be_raext_env_t*) = ssa_destr_simple;
static char callee[128] = "/ben/kimohoff/ipd-registerallocator/register_allocator";


/**
 * Allocate registers with an external program using a text-file interface.
 *
 * Do some computations (SSA-destruction and mapping of values--vars)
 * Write file
 * Execute external program
 * Read in results and apply them
 *
 */
static void be_ra_extern_main(const be_irg_t *bi) {
	be_main_env_t *env = bi->main_env;
	ir_graph *irg = bi->irg;

	be_raext_env_t raenv;
	int clsnr, clss;
	var_info_t *vi;

	compute_doms(irg);
	be_liveness(irg);

	raenv.irg      = irg;
	raenv.aenv     = env->arch_env;
	raenv.dom_info = be_compute_dominance_frontiers(irg);
	raenv.vars     = new_set(compare_var_infos, 64);

	/* Insert copies for constraints */
	handle_constraints(&raenv);
	dump_ir_block_graph_sched(irg, "-extern-constr");

	/* SSA destruction respectively transformation into "Conventional SSA" */
	ssa_destr(&raenv);
	dump_ir_block_graph_sched(irg, "-extern-ssadestr");


	/* Mapping of SSA-Values <--> Variables */
	phi_class_compute(irg);
	be_clear_links(irg);
	irg_walk_graph(irg, values_to_vars, NULL, &raenv);

	/* For all register classes */
	for(clsnr = 0, clss = arch_isa_get_n_reg_class(raenv.aenv->isa); clsnr < clss; ++clsnr) {
		int done = 0;
		char out[256], in[256];

		raenv.cls = arch_isa_get_reg_class(raenv.aenv->isa, clsnr);
		ir_snprintf(out, sizeof(out), "%F-%s.ra", irg, raenv.cls->name);
		ir_snprintf(in, sizeof(in), "%F-%s.ra.res", irg, raenv.cls->name);

		extract_vars_of_cls(&raenv);

		while (!done) {
			dump_to_file(&raenv, out);
			execute(callee, out, in);
			done = read_and_apply_results(&raenv, in);
		}

		free(raenv.cls_vars);
	}

	dump_ir_block_graph_sched(irg, "-extern-alloc");

	/* Clean up */
	set_foreach(raenv.vars, vi)
		del_pset(vi->values);
	del_set(raenv.vars);
	be_free_dominance_frontiers(raenv.dom_info);
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

#ifdef WITH_LIBCORE


static const lc_opt_enum_func_ptr_items_t ssa_destr_items[] = {
	{ "simple",     (int (*)()) ssa_destr_simple }, /* TODO make (void*) casts nicer */
	{ "rastello",   (int (*)()) ssa_destr_rastello },
	{ NULL,      NULL }
};

static lc_opt_enum_func_ptr_var_t ssa_destr_var = {
	 (int (**)()) &ssa_destr, ssa_destr_items
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
