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
 *


The input file format
----------------------

inputfile	::= regs cfg .

regs		::= 'regs' regcount 'saved-by-caller' reg-list 'saved-by-callee' reg-list .						// Anzahl der register (0..regcount-1), die zur Verfuegung stehen

reg-list	::= reg-nr
			  | reg-nr reg-list

cfg			::= 'cfg' ident '{' block* edge* '}' .		// Steuerflussgraph der Prozedur

block		::= 'block' block-nr '{' insn* '}' .		// Grundblock im cfg versehen mit einer nummer

edge		::= 'cf-edge' block-nr block-nr .			// Steuerflusskante src-->tgt

insn		::= gen-insn 								// Befehl in einem block
			  | copy-insn .

gen-insn	::= 'insn' insn-nr '{' uses defs '}' .
copy-insn	::= 'copy' insn-nr '{' uses defs '}' .
call-insn	::= 'call' insn-nr '{' uses defs '}' .

defs		::= 'def' var-list .						// Liste der definierten/verwendeten Variablen
uses		::= 'use' var-list .

var-list	::= var-ref
			  | var-ref var-list .

var-ref		::= var-nr
			  | var-nr '<' reg-nr '>' .					// reg-nr gibt register constraint an.


ident		::= non-whitespace-char* .
regcount, block-nr, insn-nr, reg-nr, var-nr ::= integer .


The output file format
-----------------------

outputfile	::= action* assign*.

action		::= 'spill'  loc var-nr					// insert a spill    spill(var-nr);
			  | 'reload' loc new-var-nr  var-nr		// insert a reload   new-var-nr := reload(var-nr);
			  | 'setarg' insn-nr pos var-nr			// change a usage to insn(..., var-nr, ...)  (equals set_irn_n)
			  | 'copy'   loc var-nr var-nr			// insert a copy     var-nr[1] := var-nr[2];

assign		::= 'assign' var-nr reg-nr .			// assign var-nr the register reg-nr

loc			::= 'before' insn-nr
			  | 'after'  insn-nr .


Constraints for output file
---------------------------
1) The returned actions must result in a non-ssa-program
   equivalent to the former input.
2) All names (numbers) must be defined before their first use.
   Numbers already occuring in the input file are implicitly defined in the output file.
3) All spills of a variable must precede the first reload of this variable
4) Each reload must define a new variable name



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
#include "irdom_t.h"
#include "phiclass.h"

#include "beraextern.h"
#include "bearch.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched.h"
#include "beutil.h"

typedef struct _var_info_t var_info_t;

/**
 * Environment with all the needed stuff
 */
typedef struct _be_raext_env_t {
	arch_env_t *aenv;
	const arch_register_class_t *cls;
	ir_graph *irg;
	dom_front_info_t *dom_info;

	FILE *f;		/**< file handle used for out- and input file */
	set *vars;		/**< contains all var_info_t */
	pmap *nodes;	/**< maps nodes numbers (int) to the node (ir_node*) having that node_nr */
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
	/* TODO
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
 * This struct maps a variable (nr) to
 *  1) the values belonging to this variable
 *  2) the spills of this variable
 */
struct _var_info_t {
	int var_nr;		/* the key for comparesion */
	pset *values;	/* the ssa-values belonging to this variable */
	pset *spills;	/* the spills of this variable */
	pset *reloads;	/* the relaods of this variable */
	unsigned reload_phase:1;	/* 0 initially, 1 if a reload of this var has been inserted */
};

/**
 * The link field of an irn points to the var_info struct
 * representing the corresponding variable.
 */
#define set_var_info(irn, vi)				set_irn_link(irn, vi)
#define get_var_info(irn)					((var_info_t *)get_irn_link(irn))

/* TODO insn-nr handling if ext defines new ones */
#define pmap_insert_sth(pmap, key, val)	pmap_insert(pmap, (void *)key, (void *)val)
#define pmap_get_sth(pmap, key)			pmap_get(pmap, (void *)key)


#define HASH_VAR_NR(var_nr) var_nr



static int compare_var_infos(const void *e1, const void *e2, size_t size) {
	const var_info_t *v1 = e1;
	const var_info_t *v2 = e2;

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

	if (!found->values) {
		found->values  = pset_new_ptr(1);
		found->spills  = pset_new_ptr(1);
		found->reloads = pset_new_ptr(1);
	}

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

/**
 * Adds a spill to a variable. Sets all pointers accordingly.
 */
static INLINE var_info_t *var_add_spill(be_raext_env_t *raenv, int var_to_spill, ir_node *before) {
	var_info_t *vi = var_find_or_insert(raenv->vars, var_to_spill);
	ir_node *blk, *tospill, *spill;

	assert(pset_count(vi->values) && "There are no values associated to this variable (yet?)");
	assert(!vi->reload_phase && "I have already seen a reload for this variable, so you cant spill anymore!");

	/* add spill to graph and schedule */
	blk     = get_nodes_block(before);
	tospill = dom_up_search(vi->values, before); /* which value gets spilled */
	spill   = be_new_Spill(raenv->cls, raenv->irg, blk, tospill, tospill); /* the corresponding spill node */

	sched_add_before(before, spill);

	/* the spill also points to the var_info of the spilled node */
	set_var_info(spill, vi);

	/* remember the spill */
	pset_insert_ptr(vi->spills, spill);

	return vi;
}

/**
 * Adds a reload to a variable. Sets all pointers accordingly.
 */
static INLINE var_info_t *var_add_reload(be_raext_env_t *raenv, int var_to_reload, int var_nr_for_reload, ir_node *before) {
	var_info_t *vi = var_find_or_insert(raenv->vars, var_to_reload);
	ir_node *blk, *spill, *reload;

	assert(pset_count(vi->spills) && "There are no spills associated to this variable (yet?)");
	/* now we enter the reload phase, so no more spills are allowed */
	vi->reload_phase = 1;

	/* add reload to graph and schedule */
	blk    = get_nodes_block(before);
	spill  = pset_first(vi->spills); /* For now use an arbitrary spill node. This is corrected later in fix_reloads */
	pset_break(vi->spills);
	reload = be_new_Reload(raenv->cls, raenv->irg, blk, get_irn_mode(get_irn_n(spill, 0)), spill);

	sched_add_before(before, reload);

	/* create a new variable for the result of the reload */
	assert(!var_find(raenv->vars, var_nr_for_reload) && "Each reload must define a new variable");
	var_add_value(raenv, var_nr_for_reload, reload);

	/* remember the reload */
	pset_insert_ptr(vi->reloads, reload);

	return vi;
}

static INLINE pset *get_var_values(be_raext_env_t *raenv, int var_nr) {
	var_info_t *vi = var_find(raenv->vars, var_nr);
	assert(vi && "Variable does not (yet?) exist");
	return vi->values;
}

static INLINE pset *get_var_spills(be_raext_env_t *raenv, int var_nr) {
	var_info_t *vi = var_find(raenv->vars, var_nr);
	assert(vi && "Variable does not (yet?) exist");
	return vi->spills;
}

static INLINE pset *get_var_reloads(be_raext_env_t *raenv, int var_nr) {
	var_info_t *vi = var_find(raenv->vars, var_nr);
	assert(vi && "Variable does not (yet?) exist");
	return vi->reloads;
}

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

	/* values <--> var mapping */
	n = pset_first(vals);
	nr = get_irn_node_nr(n);
	for (; n; n=pset_next(vals))
		var_add_value(raenv, nr, n);
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

#define INVALID_FILE_FORMAT assert(0 && "Invalid file format.")

static INLINE int is_before(const char *s, size_t len) {
	if (!strncmp(s, "before", len))
		return 1;
	if (!strncmp(s, "after", len))
		return 0;
	INVALID_FILE_FORMAT;
	return -1;
}

static void fix_reloads(be_raext_env_t *raenv) {
	var_info_t *vi;
	set_foreach(raenv->vars, vi)
		be_introduce_copies_pset(raenv->dom_info, vi->spills);
}

/**
 * Read in the actions performed by the external allocator.
 * Apply these transformations to the irg.
 */
static void read_and_apply_results(be_raext_env_t *raenv, char *filename) {
	FILE *f;
	var_info_t *vi;
	int phase=0;

	if (!(f = fopen(filename, "rt"))) {
		fprintf(stderr, "Could not open file %s for reading\n", filename);
		exit(0xdeadbeef);
	}
	raenv->f = f;



	/* parse the file */
	while (phase == 0) {
		int loc, var_use_ident, var_def_ident, pos;
		char where[16];

		/* handle a spill */
		if (fscanf(f, " spill %6s %d %d ", &where, &loc, &var_use_ident) == 3) {

			/* determine the node to insert the spill before */
			ir_node *anchor = pmap_get_sth(raenv->nodes, loc);
			assert(anchor && "insn-nr does not exist");
			if (!is_before(where, sizeof(where)))
				anchor = sched_next(anchor);

			var_add_spill(raenv, var_use_ident, anchor);
		}

		/* handle a reload */
		else if (fscanf(f, " reload %s %d %d %d ", &where, &loc, &var_def_ident, &var_use_ident) == 4) {

			/* determine the node to insert the spill before */
			ir_node *anchor = pmap_get_sth(raenv->nodes, loc);
			assert(anchor && "insn-nr does not exist");
			if (!is_before(where, sizeof(where)))
				anchor = sched_next(anchor);

			var_add_reload(raenv, var_use_ident, var_def_ident, anchor);
		}

		/* handle a set_irn_n */
		else if (fscanf(f, " setarg %d %d %d ", &loc, &pos, &var_use_ident) == 3) {
			ir_node *to_change, *new_arg;
			var_info_t *vi = var_find(raenv->vars, var_use_ident);
			assert(vi && vi->values && "New argument does not exist");

			to_change = pmap_get_sth(raenv->nodes, loc);
			assert(to_change && "insn-nr does not exist");

			new_arg = dom_up_search(vi->values, to_change);
			set_irn_n(to_change, pos, new_arg);
		}

		/* handle a copy insertion */
		else if (fscanf(f, " copy %s %d %d %d ", &where, &loc, &var_def_ident, &var_use_ident) == 4) {
			/* TODO
				Ziel der Kopie ist Variable die bereits existiert
				Ziel der Kopie ist eine neue Variable
			*/
		}

		else
			phase++;
	}

	fix_reloads(raenv);

	while (phase == 1) {
		int var_use_ident, reg_nr;

		/* assign register */
		if (fscanf(f, " assign %d %d ", &var_use_ident, &reg_nr) == 2) {
			pset *vals = get_var_values(raenv, var_use_ident);
			ir_node *irn;

			assert(vals && "Variable does not (yet?) exist!");
			pset_foreach(vals, irn)
				arch_set_irn_register(raenv->aenv, irn, arch_register_for_index(raenv->cls, var_use_ident));
		}
		else
			phase++;
	}

	if (!feof(f))
		INVALID_FILE_FORMAT;

	fclose(f);

	/* Free the psets held in the variable-infos */
	set_foreach(raenv->vars, vi) {
		del_pset(vi->values);
		del_pset(vi->spills);
		del_pset(vi->reloads);
	}
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
		req.limited(req.limited_env, bs);
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

	pmap_insert_sth(raenv->nodes, nr, blk);

	/* begin block scope */
	fprintf(f, "\n");
	fprintf(f, "  block %d {\n", nr);

	/* for each instruction */
	for(irn=sched_first(blk); !sched_is_end(irn); irn=sched_next(irn)) {
		int max, i;
		int node_nr;

		if (is_Phi(irn) || !is_sth_in_reg_class(raenv, irn))
			continue;

		/* kind of instruction */
		if (arch_irn_classify(raenv->aenv, irn) == arch_irn_class_copy)
			fprintf(f, "    copy");
		else if (arch_irn_classify(raenv->aenv, irn) == arch_irn_class_call)
			fprintf(f, "    call");
		else
			fprintf(f, "    insn");

		/* number */
		node_nr = get_irn_node_nr(irn);
		fprintf(f, " %ld {\n", node_nr);

		pmap_insert_sth(raenv->nodes, node_nr, irn);


		/*
		 * print all uses
		 */
		fprintf(f, "      use");
		for (i=0, max=get_irn_arity(irn); i<max; ++i) {
			ir_node *arg = get_irn_n(irn, i);
			if (arch_irn_has_reg_class(raenv->aenv, arg, -1, raenv->cls)) {
				fprintf(f, " %d", get_var_info(arg)->var_nr);
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
					fprintf(f, " %d", get_var_info(irn)->var_nr);
					dump_constraint(raenv, irn, -1);
				}
			irn = sched_prev(irn); /* for outer loop */
		} else {
			if (arch_irn_has_reg_class(raenv->aenv, irn, -1, raenv->cls)) {
				fprintf(f, " %d", get_var_info(irn)->var_nr);
				dump_constraint(raenv, irn, -1);
			}
		}
		fprintf(f,"\n");

		/* end of insn scope */
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
	int i, reg_count;

	if (!(f = fopen(filename, "wt"))) {
		fprintf(stderr, "Could not open file %s for writing\n", filename);
		exit(0xdeadbeef);
	}
	raenv->f = f;

	/* dump register info */
	reg_count = arch_register_class_n_regs(raenv->cls);
	fprintf(f, "regs %d", reg_count);

	fprintf(f, " saved-by-caller");
	for (i=0; i<reg_count; ++i)
		if (arch_register_type_is(arch_register_for_index(raenv->cls, i), caller_saved))
			fprintf(f, " %d", i);

	fprintf(f, " saved-by-callee");
	for (i=0; i<reg_count; ++i)
		if (arch_register_type_is(arch_register_for_index(raenv->cls, i), callee_saved))
			fprintf(f, " %d", i);

	fprintf(f, "\n");

	/* dump the cfg */

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
static void execute(char *prog_to_call, char *out_file, char *result_file) {
	char cmd_line[1024];
	int ret_status;

	snprintf(cmd_line, sizeof(cmd_line), "%s %s %s", prog_to_call, out_file, result_file);

	ret_status = system(cmd_line);
	assert(ret_status != -1 && "Invokation of external register allocator failed");
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
static char callee[128] = "echo";


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

	compute_doms(irg);

	raenv.irg      = irg;
	raenv.aenv     = env->arch_env;
	raenv.dom_info = be_compute_dominance_frontiers(irg);
	raenv.vars     = new_set(compare_var_infos, 64);
	raenv.nodes    = pmap_create();
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

		execute(callee, out, in);

		read_and_apply_results(&raenv, in);
	}

	/* Clean up */
	pmap_destroy(raenv.nodes);
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

/* TODO: explicit cast to void * is ugly, but ISO-C forbids
   assignment from func ptr to void ptr. This should probably be fixed
   in libcore. (cw) */
static const lc_opt_enum_const_ptr_items_t ssa_destr_items[] = {
	{ "simple",    (void *)ssa_destr_simple },
	{ "rastello",  (void *)ssa_destr_rastello },
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
