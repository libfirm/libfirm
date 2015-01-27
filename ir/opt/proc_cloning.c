/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Procedure cloning.
 * @author  Beyhan Veliev, Michael Beck
 * @brief
 *
 * The purpose is first to find and analyze functions, that are called
 * with constant parameter(s).
 * The second step is to optimize the function that are found from our
 * analyze. Optimize mean to make a new function with parameters, that
 * aren't be constant. The constant parameters of the function are placed
 * in the function graph. They aren't be passed as parameters.
 */
#include "iroptimize.h"
#include "tv.h"
#include "set.h"
#include "irprog_t.h"
#include "hashptr.h"
#include "irgwalk.h"
#include "analyze_irg_args.h"
#include "irprintf.h"
#include "ircons.h"
#include "irouts_t.h"
#include "irnode_t.h"
#include "irtools.h"
#include "irgmod.h"
#include "array.h"
#include "panic.h"

/**
 * This struct contains the information quadruple for a Call, which we need to
 * decide if this function must be cloned.
 */
typedef struct quadruple {
	ir_entity *ent;   /**< The entity of our Call. */
	size_t     pos;   /**< Position of a constant argument of our Call. */
	ir_tarval *tv;    /**< The tarval of this argument if Const node. */
	ir_node  **calls; /**< The list of all calls with the same characteristics */
} quadruple_t;

/**
 * The quadruplets are hold in a sorted list
 */
typedef struct entry {
	quadruple_t   q;      /**< the quadruple */
	float         weight; /**< its weight */
	struct entry *next;   /**< link to the next one */
} entry_t;

typedef struct q_set {
	struct obstack  obst;       /**< an obstack containing all entries */
	pset           *map;        /**< a hash map containing the quadruples */
	entry_t        *heavy_uses; /**< the ordered list of heavy uses */
} q_set;

/**
 * Compare two quadruplets.
 *
 * @return zero if they are identically, non-zero else
 */
static int entry_cmp(const void *elt, const void *key)
{
	const entry_t *e1 = (const entry_t*)elt;
	const entry_t *e2 = (const entry_t*)key;

	return (e1->q.ent != e2->q.ent) || (e1->q.pos != e2->q.pos) || (e1->q.tv != e2->q.tv);
}

/**
 * Hash an element of type entry_t.
 *
 * @param entry  The element to be hashed.
 */
static unsigned hash_entry(const entry_t *entry)
{
	return hash_ptr(entry->q.ent) ^ hash_ptr(entry->q.tv) ^ (unsigned)(entry->q.pos * 9);
}

/**
 * Free memory associated with a quadruplet.
 */
static void kill_entry(entry_t *entry)
{
	if (entry->q.calls) {
		DEL_ARR_F(entry->q.calls);
		entry->q.calls = NULL;
	}
}

/**
 * Copies a node to a new irg. The Ins of the new node point to
 * the predecessors on the old irg.  n->link points to the new node.
 *
 * @param n    The node to be copied
 * @param irg  the new irg
 *
 * Does NOT copy standard nodes like Start, End etc that are fixed
 * in an irg. Instead, the corresponding nodes of the new irg are returned.
 * Note further, that the new nodes have no block.
 */
static void copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
	/* do not copy standard nodes */
	ir_node *nn = NULL;
	switch (get_irn_opcode(n)) {
	case iro_NoMem:
		nn = get_irg_no_mem(irg);
		break;

	case iro_Block: {
		ir_graph *old_irg = get_irn_irg(n);
		if (n == get_irg_start_block(old_irg))
			nn = get_irg_start_block(irg);
		else if (n == get_irg_end_block(old_irg))
			nn = get_irg_end_block(irg);
		break;
	}

	case iro_Start:
		nn = get_irg_start(irg);
		break;

	case iro_End:
		nn = get_irg_end(irg);
		break;

	case iro_Proj: {
		ir_graph *old_irg = get_irn_irg(n);
		if (n == get_irg_frame(old_irg))
			nn = get_irg_frame(irg);
		else if (n == get_irg_initial_mem(old_irg))
			nn = get_irg_initial_mem(irg);
		else if (n == get_irg_args(old_irg))
			nn = get_irg_args(irg);
		break;
	}
	}

	if (nn) {
		set_irn_link(n, nn);
		return;
	}

	nn = new_ir_node(get_irn_dbg_info(n),
	                 irg,
	                 NULL,            /* no block yet, will be set later */
	                 get_irn_op(n),
	                 get_irn_mode(n),
	                 get_irn_arity(n),
	                 get_irn_in(n) + 1);


	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	copy_node_attr(irg, n, nn);
	set_irn_link(n, nn);
}

/**
 * Process a call node.
 *
 * @param call    A ir_node to be checked.
 * @param callee  The entity of the callee
 * @param hmap    The quadruple-set containing the calls with constant parameters
 */
static void process_call(ir_node *call, ir_entity *callee, q_set *hmap)
{
	/* TODO
	 * Beware: We cannot clone variadic parameters as well as the
	 * last non-variadic one, which might be needed for the va_start()
	 * magic. */

	/* In this for loop we collect the calls, that have
	   an constant parameter. */
	size_t const n_params = get_Call_n_params(call);
	for (size_t i = n_params; i-- > 0;) {
		ir_node *const call_param = get_Call_param(call, i);
		if (is_Const(call_param)) {
			/* we have found a Call to collect and we save the informations,
			 * which we need.*/
			if (!hmap->map)
				hmap->map = new_pset(entry_cmp, 8);

			entry_t *const key = OALLOC(&hmap->obst, entry_t);
			key->q.ent   = callee;
			key->q.pos   = i;
			key->q.tv    = get_Const_tarval(call_param);
			key->q.calls = NULL;
			key->weight  = 0.0F;
			key->next    = NULL;

			/* We insert our information in the set, where we collect the calls.*/
			entry_t *const entry = (entry_t*)pset_insert(hmap->map, key, hash_entry(key));
			if (entry != key)
				obstack_free(&hmap->obst, key);

			/* add the call to the list */
			if (!entry->q.calls) {
				entry->q.calls = NEW_ARR_F(ir_node*, 1);
				entry->q.calls[0] = call;
			} else {
				ARR_APP1(ir_node *, entry->q.calls, call);
			}
		}
	}
}

/**
 * Collect all calls in a ir_graph to a set.
 *
 * @param call   A ir_node to be checked.
 * @param env   The quadruple-set containing the calls with constant parameters
 */
static void collect_irg_calls(ir_node *call, void *env)
{
	q_set *const hmap = (q_set*)env;

	/* We collect just "Call" nodes */
	if (!is_Call(call))
		return;
	ir_entity *const callee = get_Call_callee(call);
	if (callee == NULL)
		return;
	ir_graph *const callee_irg = get_entity_linktime_irg(callee);
	if (callee_irg == NULL)
		return;

	process_call(call, callee, hmap);
}

/**
 * Make a name for a clone. The clone name is
 * the name of the original method suffixed with "_cl_pos_nr".
 * pos is the pos from our quadruplet and nr is a counter.
 *
 * @param id  The ident of the cloned function.
 * @param pos The "pos" from our quadruplet.
 * @param nr  A counter for the clones.
 */
static ident *get_clone_ident(ident *id, size_t pos, size_t nr)
{
	return new_id_fmt("%s_cl_%zu_%zu", id, pos, nr);
}

static inline ir_node *get_irn_copy(ir_node *const irn)
{
	return (ir_node*)get_irn_link(irn);
}

/**
 * Pre-Walker: Copies blocks and nodes from the original method graph
 * to the cloned graph. Fixes the argument projection numbers for
 * all arguments behind the removed one.
 *
 * @param irn  A node from the original method graph.
 * @param env  The clone graph.
 */
static void copy_nodes(ir_node *irn, void *env)
{
	ir_graph *const clone_irg = (ir_graph*)env;
	ir_node  *const arg       = (ir_node*)get_irg_link(clone_irg);
	ir_node  *const irg_args  = get_Proj_pred(arg);

	/* Copy all nodes except the arg. */
	if (irn != arg)
		copy_irn_to_irg(irn, clone_irg);

	/* Fix argument numbers */
	ir_node *const irn_copy = get_irn_copy(irn);
	if (is_Proj(irn) && get_Proj_pred(irn) == irg_args) {
		unsigned const proj_nr = get_Proj_num(irn);
		if (get_Proj_num(arg) < proj_nr)
			set_Proj_num(irn_copy, proj_nr - 1);
	}
}

/**
 * Post-walker: Set the predecessors of the copied nodes.
 * The copied nodes are set as link of their original nodes. The links of
 * "irn" predecessors are the predecessors of copied node.
 */
static void set_preds(ir_node *irn, void *env)
{
	ir_graph *const clone_irg = (ir_graph*)env;

	/* Arg is the method argument, that we have replaced by a constant.*/
	ir_node *const arg = (ir_node*)get_irg_link(clone_irg);
	if (arg == irn)
		return;

	ir_node  *const irn_copy = get_irn_copy(irn);

	if (is_Block(irn)) {
		ir_graph *const irg       = get_irn_irg(irn);
		ir_node  *const end_block = get_irg_end_block(irg);
		for (int i = get_Block_n_cfgpreds(irn); i-- > 0;) {
			ir_node *const pred = get_Block_cfgpred(irn, i);
			/* "End" block must be handled extra, because it is not matured. */
			if (end_block == irn)
				add_immBlock_pred(get_irg_end_block(clone_irg), get_irn_copy(pred));
			else
				set_Block_cfgpred(irn_copy, i, get_irn_copy(pred));
		}
	} else {
		/* First we set the block our copy if it is not a block.*/
		set_nodes_block(irn_copy, get_irn_copy(get_nodes_block(irn)));
		if (is_End(irn)) {
			/* Handle the keep-alives. This must be done separately, because
			 * the End node was NOT copied */
			for (int i = 0, n = get_End_n_keepalives(irn); i < n; ++i)
				add_End_keepalive(irn_copy, get_irn_copy(get_End_keepalive(irn, i)));
		} else {
			foreach_irn_in_r(irn, i, pred) {
				set_irn_n(irn_copy, i, get_irn_copy(pred));
			}
		}
	}
}

/**
 * Get the method argument at the position "pos".
 *
 * @param irg  irg that must be cloned.
 * @param pos  The position of the argument.
 */
static ir_node *get_irg_arg(ir_graph *irg, size_t pos)
{
	/* Call algorithm that computes the out edges */
	assure_irg_outs(irg);

	/* Search the argument with the number pos.*/
	ir_node *arg      = NULL;
	ir_node *irg_args = get_irg_args(irg);
	foreach_irn_out_r(irg_args, i, proj) {
		if (pos == get_Proj_num(proj)) {
			assert(arg == NULL);
			if (arg != NULL)
				panic("multiple projs for the same argument");
			arg = proj;
		}
	}
	assert(arg && "Argument not found");
	return arg;
}

/**
 * Create a new graph for the clone of the method,
 * that we want to clone.
 *
 * @param ent The entity of the method that must be cloned.
 * @param q   Our quadruplet.
 */
static void create_clone_proc_irg(ir_entity *ent, const quadruple_t *q)
{
	ir_graph *const method_irg = get_entity_linktime_irg(ent);

	/* We create the skeleton of the clone irg.*/
	ir_graph *const clone_irg  = new_ir_graph(ent, 0);

	ir_node *const arg        = get_irg_arg(get_entity_irg(q->ent), q->pos);
	/* we will replace the argument in position "q->pos" by this constant. */
	ir_node *const const_arg  = new_r_Const(clone_irg, q->tv);

	/* args copy in the cloned graph will be the const. */
	set_irn_link(arg, const_arg);

	/* Store the arg that will be replaced here, so we can easily detect it. */
	set_irg_link(clone_irg, arg);

	/* We copy the blocks and nodes, that must be in
	the clone graph and set their predecessors. */
	irg_walk_graph(method_irg, copy_nodes, set_preds, clone_irg);

	/* The "cloned" graph must be matured. */
	irg_finalize_cons(clone_irg);
}

/**
 * The function create a new entity type
 * for our clone and set it to clone entity.
 *
 * @param q   Contains information for the method to clone.
 * @param ent The entity of the clone.
 * @param nr  A pointer to the counter of clones.
 **/
static void change_entity_type(const quadruple_t *q, ir_entity *ent)
{
	ir_type *const mtp      = get_entity_type(q->ent);
	size_t   const n_params = get_method_n_params(mtp);
	size_t   const n_ress   = get_method_n_ress(mtp);

	/* Create the new type for our clone. It must have one parameter
	   less then the original.*/
	ir_type *const new_mtp  = new_type_method(n_params - 1, n_ress);

	/* We must set the type of the methods parameters.*/
	for (size_t i = 0, j = 0; i < n_params; ++i) {
		/* This is the position of the argument, that we have replaced. */
		if (i == q->pos)
			continue;
		ir_type *const tp = get_method_param_type(mtp, i);
		set_method_param_type(new_mtp, j++, tp);
	}
	/* Copy the methods result types. */
	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *const tp = get_method_res_type(mtp, i);
		set_method_res_type(new_mtp, i, tp);
	}
	set_entity_type(ent, new_mtp);
}

/**
 * Make a clone of a method.
 *
 * @param q   Contains information for the method to clone.
 */
static ir_entity *clone_method(const quadruple_t *q)
{
	/* A counter for the clones.*/
	static size_t nr = 0;

	/* We get a new ident for our clone method.*/
	ident     *const clone_ident = get_clone_ident(get_entity_ident(q->ent), q->pos, nr);
	/* We get our entity for the clone method. */
	ir_entity *const new_entity  = copy_entity_name(q->ent, clone_ident);

	/* a cloned entity is always local */
	set_entity_visibility(new_entity, ir_visibility_local);

	/* set a ld name here: Should we mangle this ? */
	set_entity_ld_ident(new_entity, get_entity_ident(new_entity));

	/* set a new type here. */
	change_entity_type(q, new_entity);

	/* We need now a new ir_graph for our clone method. */
	create_clone_proc_irg(new_entity, q);

	/* The "new_entity" don't have this information. */
	new_entity->attr.mtd_attr.param_access = NULL;
	new_entity->attr.mtd_attr.param_weight = NULL;

	return new_entity;
}

/**
 * Creates a new "cloned" Call node and return it.
 *
 * @param call        The call that must be cloned.
 * @param new_entity  The entity of the cloned function.
 * @param pos         The position of the replaced parameter of this call.
 **/
static ir_node *new_cl_Call(ir_node *call, ir_entity *new_entity, size_t pos)
{
	size_t    const n_params = get_Call_n_params(call);
	ir_node **const in       = ALLOCAN(ir_node*, n_params - 1);

	/* we save the parameters of the new call in the array "in" without the
	 * parameter in position "pos", that is replaced with a constant.*/
	size_t new_params = 0;
	for (size_t i = 0; i < n_params; ++i) {
		if (pos != i)
			in[new_params++] = get_Call_param(call, i);
	}
	/* Create and return the new Call. */
	ir_node  *const bl     = get_nodes_block(call);
	ir_node  *const mem    = get_Call_mem(call);
	ir_graph *const irg    = get_irn_irg(call);
	ir_node  *const callee = new_r_Address(irg, new_entity);
	ir_type  *const type   = get_entity_type(new_entity);
	return new_r_Call(bl, mem, callee, n_params - 1, in, type);
}

/**
 * Exchange all Calls stored in the quadruplet to Calls of the cloned entity.
 *
 * @param q             The quadruple
 * @param cloned_ent    The entity of the new function that must be called
 *                      from the new Call.
 */
static void exchange_calls(quadruple_t *q, ir_entity *cloned_ent)
{
	/* We iterate the list of the "call".*/
	size_t const pos = q->pos;
	for (size_t i = 0; i < ARR_LEN(q->calls); ++i) {
		ir_node *const call = q->calls[i];
		/* A clone exist and the copy of "call" in this
		 * clone graph must be exchanged with new one. */
		ir_node *const new_call = new_cl_Call(call, cloned_ent, pos);
		exchange(call, new_call);
	}
}

/**
 * The weight formula:
 * We save one instruction in every caller and param_weight instructions
 * in the callee.
 */
static float calculate_weight(const entry_t *entry)
{
	return ARR_LEN(entry->q.calls) *
		(float)(get_method_param_weight(entry->q.ent, entry->q.pos) + 1);
}

/**
 * After we exchanged all calls, some entries on the list for
 * the next cloned entity may get invalid, so we have to check
 * them and may even update the list of heavy uses.
 */
static void reorder_weights(q_set *hmap, float threshold)
{
restart:
	;
	entry_t *const entry = hmap->heavy_uses;
	if (!entry)
		return;

	size_t len = ARR_LEN(entry->q.calls);
	for (size_t i = 0; i < len;) {
		/* might be exchanged, so skip Id nodes here. */
		ir_node   *const call   = skip_Id(entry->q.calls[i]);
		/* we know, that an Address is here */
		ir_entity *const callee = get_Call_callee(call);
		if (callee != entry->q.ent) {
			/* This call is already changed because of a previous
			 * optimization. Remove it from the list. */
			--len;
			entry->q.calls[i]   = entry->q.calls[len];
			entry->q.calls[len] = NULL;

			/* the new call should be processed */
			process_call(call, callee, hmap);
		} else {
			++i;
		}
	}

	/* the length might be changed */
	ARR_SHRINKLEN(entry->q.calls, len);

	/* recalculate the weight and resort the heavy uses map */
	entry->weight = calculate_weight(entry);

	if (len <= 0 || entry->weight < threshold) {
		hmap->heavy_uses = entry->next;
		kill_entry(entry);

		/* we have changed the list, check the next one */
		goto restart;
	}

	entry_t **adr = NULL;
	for (entry_t *p = entry->next; p && entry->weight < p->weight; p = p->next) {
		adr = &p->next;
	}

	if (adr) {
		hmap->heavy_uses = entry->next;
		entry->next      = *adr;
		*adr             = entry;

		/* we have changed the list, check the next one */
		goto restart;
	}
}

void proc_cloning(float threshold)
{
	DEBUG_ONLY(firm_dbg_module_t *dbg;)

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.proc_cloning");

	q_set hmap;
	obstack_init(&hmap.obst);
	hmap.map        = NULL;
	hmap.heavy_uses = NULL;

	/* initially fill our map by visiting all irgs */
	foreach_irp_irg(i, irg) {
		irg_walk_graph(irg, collect_irg_calls, NULL, &hmap);
	}

	/* We have the "Call" nodes to optimize in set "set_entries". Our algorithm
	   replace one constant parameter and make a new "Call" node for all found "Calls". It exchange the
	   old one with the new one and the algorithm is called with the new "Call".
	 */
	while (hmap.map || hmap.heavy_uses) {
		/* We iterate the set and arrange the element of the set in a list.
		   The elements are arranged dependent of their value descending.*/
		if (hmap.map) {
			foreach_pset(hmap.map, entry_t, entry) {
				entry->weight = calculate_weight(entry);

				/*
				 * Do not put entry with a weight < threshold in the list
				 */
				if (entry->weight < threshold) {
					kill_entry(entry);
					continue;
				}

				/* put entry in the heavy uses list */
				for (entry_t **anchor = &hmap.heavy_uses;; anchor = &(*anchor)->next) {
					if (!*anchor || entry->weight >= (*anchor)->weight) {
						entry->next = *anchor;
						*anchor     = entry;
						break;
					}
				}
			}
			del_pset(hmap.map);
			hmap.map = NULL;
		}

#ifdef DEBUG_libfirm
		/* Print some information about the list. */
		DB((dbg, LEVEL_2, "-----------------\n"));
		for (entry_t *entry = hmap.heavy_uses; entry; entry = entry->next) {
			DB((dbg, LEVEL_2, "\nweight: is %f\n", entry->weight));
			DB((dbg, LEVEL_2, "Call for Method %E\n", entry->q.ent));
			DB((dbg, LEVEL_2, "Position %zu\n", entry->q.pos));
			DB((dbg, LEVEL_2, "Value %T\n", entry->q.tv));
		}
#endif
		entry_t *const entry = hmap.heavy_uses;
		if (entry) {
			quadruple_t *const qp  = &entry->q;
			ir_entity   *const ent = clone_method(qp);
			DB((dbg, LEVEL_1, "Cloned <%+F, %zu, %T> info %+F\n", qp->ent, qp->pos, qp->tv, ent));

			hmap.heavy_uses = entry->next;

			/* We must exchange the copies of this call in all clones too.*/
			exchange_calls(&entry->q, ent);
			kill_entry(entry);

			/*
			 * after we exchanged all calls, some entries on the list for
			 * the next cloned entity may get invalid, so we have to check
			 * them and may even update the list of heavy uses.
			 */
			reorder_weights(&hmap, threshold);
		}
	}
	obstack_free(&hmap.obst, NULL);
}
