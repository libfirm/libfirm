/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   parallelizing Load/Store optimisation
 * @author  Christoph Mallon
 * @version $Id: $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iroptimize.h"

#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "irgraph.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irnode.h"
#include "irnodeset.h"
#include "obst.h"
#include "irdump.h"
#include "irflag_t.h"

#define OPTIMISE_LOAD_AFTER_LOAD


#define UNIMPLEMENTED abort();


DEBUG_ONLY(static firm_dbg_module_t *dbg);


static struct obstack obst;
static size_t count_addrs;
static ir_node** addrs;


static void AddressCollector(ir_node* node, void* env)
{
	ir_nodeset_t* addrs_set = env;
	ir_node* addr;
	if (is_Load(node)) {
		addr = get_Load_ptr(node);
	} else if (is_Store(node)) {
		addr = get_Store_ptr(node);
	} else {
		return;
	}
	ir_nodeset_insert(addrs_set, addr);
}


/* Collects all unique addresses used by load and store nodes of a graph and
 * puts them into an array for later use */
static void CollectAddresses(ir_graph* irg)
{
	ir_nodeset_t addrs_set;

	ir_nodeset_init(&addrs_set);
	irg_walk_graph(irg, AddressCollector, NULL, &addrs_set);

	count_addrs = ir_nodeset_size(&addrs_set);
	DB((dbg, LEVEL_1, "===> %+F uses %u unique addresses\n", irg, (unsigned int)count_addrs));
	if (count_addrs != 0) {
		ir_nodeset_iterator_t addr_iter;
		size_t i;

		addrs = NEW_ARR_D(ir_node*, &obst, count_addrs);
		ir_nodeset_iterator_init(&addr_iter, &addrs_set);
		for (i = 0; i < count_addrs; i++) {
			ir_node* addr = ir_nodeset_iterator_next(&addr_iter);
			assert(addr != NULL);
			set_irn_link(addr, (void *)i);
			addrs[i] = addr;
			DB((dbg, LEVEL_2, "===> Collected unique symbolic address %+F\n", addr));
		}
	}
}


static void AliasSetAdder(ir_node* block, void* env)
{
	ir_nodeset_t* alias_set;
	size_t i;
	(void) env;

	alias_set = NEW_ARR_D(ir_nodeset_t, &obst, count_addrs);
	for (i = 0; i < count_addrs; i++) {
		ir_nodeset_init(&alias_set[i]);
	}
	set_irn_link(block, alias_set);
}


static void SetStartAddressesTop(ir_graph* irg)
{
	ir_node* initial_mem;
	ir_node* start_block;
	ir_nodeset_t* start_addrs;
	size_t i;

	initial_mem = get_irg_initial_mem(irg);
	start_block = get_irg_start_block(irg);
	start_addrs = get_irn_link(start_block);
	for (i = 0; i < count_addrs; i++) {
		ir_nodeset_insert(&start_addrs[i], initial_mem);
	}
	mark_Block_block_visited(start_block);
}


static void AliasSetDestroyer(ir_node* block, void* env)
{
	ir_nodeset_t* alias_set = get_irn_link(block);
	size_t i;
	(void) env;

	for (i = 0; i < count_addrs; i++) {
		ir_nodeset_destroy(&alias_set[i]);
	}
}


static ir_alias_relation AliasTest(ir_graph* irg, ir_node* addr, ir_mode* mode, ir_node* other)
{
	ir_node* other_addr;
	ir_mode* other_mode;

	if (is_Proj(other)) other = get_Proj_pred(other);

	if (is_Load(other)) {
		other_addr = get_Load_ptr(other);
	} else if (is_Store(other)) {
		other_addr = get_Store_ptr(other);
	} else {
		return may_alias;
	}

	other_mode = get_irn_mode(other);
	return get_alias_relation(irg, addr, mode, other_addr, other_mode);
}


static ir_node* GenerateSync(ir_graph* irg, ir_node* block, ir_nodeset_t* after_set)
{
	size_t set_size = ir_nodeset_size(after_set);
	ir_nodeset_iterator_t iter;

	assert(set_size != 0);

	ir_nodeset_iterator_init(&iter, after_set);
	if (set_size == 1) {
		return ir_nodeset_iterator_next(&iter);
	} else {
		ir_node** in;
		size_t i;

		NEW_ARR_A(ir_node*, in, set_size);
		for (i = 0; i < set_size; i++) {
			in[i] = ir_nodeset_iterator_next(&iter);
		}
		return new_r_Sync(irg, block, set_size, in);
	}
}


static ir_node** unfinished_phis;


static void PlaceMemPhis(ir_graph* irg, ir_node* block, ir_node* phi)
{
	int unfinished = 0;
	size_t block_n_preds = get_Block_n_cfgpreds(block);
	ir_nodeset_t* thissets;
	ir_node** in;
	size_t i;
	size_t j;

	thissets = get_irn_link(block);
	NEW_ARR_A(ir_node*, in, block_n_preds);
	for (j = 0; j < count_addrs; j++) {
		ir_node* new_phi;

		for (i = 0; i < block_n_preds; i++) {
			ir_node* pred_block = get_nodes_block(get_Phi_pred(phi, i)); // TODO get_Block_cfgpred_block(block, i);
			ir_nodeset_t* predsets = get_irn_link(pred_block);
			size_t predset_size = ir_nodeset_size(&predsets[j]);

			if (predset_size == 0) {
				in[i] = new_r_Unknown(irg, mode_M);
				unfinished = 1;
			} else {
				in[i] = GenerateSync(irg, pred_block, &predsets[j]);
			}
		}
		new_phi = new_r_Phi(irg, block, block_n_preds, in, mode_M);
		if (unfinished) {
			set_irn_link(new_phi, unfinished_phis[j]);
			unfinished_phis[j] = new_phi;
		}
		ir_nodeset_insert(&thissets[j], new_phi);
	}
}


static int WalkMem(ir_graph* irg, ir_node* node, ir_node* last_block);


static void WalkMemPhi(ir_graph* irg, ir_node* block, ir_node* phi)
{
	size_t n = get_Phi_n_preds(phi);
	size_t i;

	for (i = 0; i < n; i++) {
		WalkMem(irg, get_Phi_pred(phi, i), block);
	}

	PlaceMemPhis(irg, block, phi);
	exchange(phi, new_Bad());
}


static void PlaceLoad(ir_graph* irg, ir_node* block, ir_node* load, ir_node* memory)
{
	ir_node* addr = get_Load_ptr(load);
	size_t addr_idx = (size_t)get_irn_link(addr);
	ir_nodeset_t* interfere_sets = get_irn_link(block);
	ir_nodeset_t* interfere_set = &interfere_sets[addr_idx];
	size_t size = ir_nodeset_size(interfere_set);
	ir_nodeset_iterator_t interfere_iter;
	size_t i;

	assert(size > 0);
	ir_nodeset_iterator_init(&interfere_iter, interfere_set);
	if (size == 1) {
		ir_node* after = ir_nodeset_iterator_next(&interfere_iter);
		assert(!is_Proj(after) || !is_Load(get_Proj_pred(after)));
		DB((dbg, LEVEL_3, "===> %+F must be executed after %+F\n", load, after));
		set_Load_mem(load, after);
	} else {
		ir_node** after_set;
		ir_node* after;
		ir_node* mem;
		size_t i;

		NEW_ARR_A(ir_node*, after_set, size);
		i = 0;
		while ((mem = ir_nodeset_iterator_next(&interfere_iter)) != NULL) {
			if (is_Proj(mem)) {
				ir_node* pred = get_Proj_pred(mem);
				if (is_Load(pred)) {
#ifdef OPTIMISE_LOAD_AFTER_LOAD
					if (get_Load_ptr(pred) == addr && get_Load_mode(pred) == get_Load_mode(load)) {
						exchange(load, pred);
						return;
					}
#endif
					continue;
				}
			}
			DB((dbg, LEVEL_3, "===> %+F must be executed after %+F\n", load, mem));
			after_set[i++] = mem;
		}
		assert(i != 0);
		if (i == 1) {
			after = after_set[0];
		} else {
			after = new_r_Sync(irg, block, i, after_set);
		}
		set_Load_mem(load, after);
	}

	for (i = 0; i < count_addrs; i++) {
		ir_mode* mode = get_Load_mode(load);
		ir_node* other_addr = addrs[i];
		ir_mode* other_mode = mode; // XXX second mode is nonsense
		ir_alias_relation rel = get_alias_relation(irg, addr, mode, other_addr, other_mode);

		DB((dbg, LEVEL_3, "===> Testing for alias between %+F and %+F. Relation is %d\n", addr, other_addr, rel));
		if (rel == no_alias) {
			continue;
		}
		DB((dbg, LEVEL_3, "===> %+F potentially aliases address %+F\n", load, other_addr));

		ir_nodeset_insert(&interfere_sets[i], memory);
	}
}


static void PlaceStore(ir_graph* irg, ir_node* block, ir_node* store, ir_node* memory)
{
	ir_node* addr = get_Store_ptr(store);
	size_t addr_idx = (size_t)get_irn_link(addr);
	ir_nodeset_t* interfere_sets = get_irn_link(block);
	ir_nodeset_t* interfere_set = &interfere_sets[addr_idx];
	ir_node* after;
	size_t i;

	after = GenerateSync(irg, block, interfere_set);
	set_Store_mem(store, after);

	for (i = 0; i < count_addrs; i++) {
		ir_nodeset_iterator_t interfere_iter;
		ir_mode* mode = get_irn_mode(get_Store_value(store));
		ir_node* other_addr = addrs[i];
		ir_mode* other_mode = mode; // XXX second mode is nonsense
		ir_alias_relation rel = get_alias_relation(irg, addr, mode, other_addr, other_mode);
		ir_node* other_node;

		DB((dbg, LEVEL_3, "===> Testing for alias between %+F and %+F. Relation is %d\n", addr, other_addr, rel));
		if (rel == no_alias) {
			continue;
		}
		DB((dbg, LEVEL_3, "===> %+F potentially aliases address %+F\n", store, other_addr));

		ir_nodeset_iterator_init(&interfere_iter, &interfere_sets[i]);
		while ((other_node = ir_nodeset_iterator_next(&interfere_iter)) != NULL) {
			if (AliasTest(irg, addr, mode, other_node) != no_alias) {
				DB((dbg, LEVEL_3, "===> Removing %+F from execute-after set of %+F due to %+F\n", other_node, addrs[i], store));
				ir_nodeset_remove_iterator(&interfere_sets[i], &interfere_iter);
			}
		}

		ir_nodeset_insert(&interfere_sets[i], memory);
	}
}


static int WalkMem(ir_graph* irg, ir_node* node, ir_node* last_block)
{
	int block_change = 0;
	ir_node* block = get_nodes_block(node);
	ir_node* pred;
	ir_node* memory = node;
	ir_nodeset_t* addr_sets;

	if (block != last_block) {
		DB((dbg, LEVEL_3, "===> Changing block from %+F to %+F\n", last_block, block));
		block_change = 1;
		if (Block_not_block_visited(block)) {
			mark_Block_block_visited(block);
		} else {
			DB((dbg, LEVEL_2, "===> Hit already visited block at %+F\n", node));
			return block_change;
		}
	}

	// Skip projs
	if (is_Proj(node)) node = get_Proj_pred(node);

	if (is_Phi(node)) {
		WalkMemPhi(irg, block, node);
		return block_change;
	} else if (is_Sync(node)) {
		UNIMPLEMENTED
	} else if (is_Return(node)) {
		pred = get_Return_mem(node);
	} else {
		pred = get_fragile_op_mem(node);
	}

	if (WalkMem(irg, pred, block)) {
		// There was a block change
		size_t block_arity = get_Block_n_cfgpreds(block);

		DB((dbg, LEVEL_3, "===> There is a block change before %+F\n", node));
		if (block_arity == 1) {
			// Just one predecessor, inherit its alias sets
			ir_node* pred_block = get_nodes_block(pred);
			ir_nodeset_t* predsets = get_irn_link(pred_block);
			ir_nodeset_t* thissets = get_irn_link(block);
			size_t i;

			DB((dbg, LEVEL_3, "===> Copying the only predecessor's address sets\n"));

			if (ir_nodeset_size(&predsets[0]) == 0) {
				ir_node* unknown;

				DB((dbg, LEVEL_3, "===> The predecessor was not finished yet\n"));
				assert(!Block_not_block_visited(pred_block));

				unknown = new_r_Unknown(irg, mode_M);
				for (i = 0; i < count_addrs; i++) {
					ir_node* phi_unk = new_r_Phi(irg, block, 1, &unknown, mode_M);
					DB((dbg, LEVEL_3, "===> Placing unfinished %+F for %+F in %+F\n", phi_unk, addrs[i], block));
					set_irn_link(phi_unk, unfinished_phis[i]);
					unfinished_phis[i] = phi_unk;
					ir_nodeset_insert(&thissets[i], phi_unk);
				}
			} else {
				for (i = 0; i < count_addrs; i++) {
					ir_nodeset_iterator_t prediter;
					ir_node* addr;

					ir_nodeset_iterator_init(&prediter, &predsets[i]);
					while ((addr = ir_nodeset_iterator_next(&prediter)) != NULL) {
						ir_nodeset_insert(&thissets[i], addr);
					}
				}
			}
		}
	}

	DB((dbg, LEVEL_3, "===> Detotalising %+F\n", node));

	addr_sets = get_irn_link(block);

	if (is_Load(node)) {
		PlaceLoad(irg, block, node, memory);
	} else if (is_Store(node)) {
		PlaceStore(irg, block, node, memory);
	} else {
		ir_nodeset_t sync_set;
		size_t i;
		ir_node* after;

		DB((dbg, LEVEL_3, "===> Fallback: %+F aliases everything\n", node));

		ir_nodeset_init(&sync_set);
		for (i = 0; i < count_addrs; i++) {
			ir_nodeset_iterator_t iter;
			ir_node* mem;

			ir_nodeset_iterator_init(&iter, &addr_sets[i]);
			while ((mem = ir_nodeset_iterator_next(&iter)) != NULL) {
				ir_nodeset_insert(&sync_set, mem);
			}
		}

		after = GenerateSync(irg, block, &sync_set);
		set_irn_n(node, 0, after); // XXX unnice way to set the memory input

		for (i = 0; i < count_addrs; i++) {
			ir_nodeset_iterator_t iter;
			ir_nodeset_iterator_init(&iter, &addr_sets[i]);
			while (ir_nodeset_iterator_next(&iter) != NULL) {
				ir_nodeset_remove_iterator(&addr_sets[i], &iter);
			}
			ir_nodeset_insert(&addr_sets[i], memory);
		}
	}

	return block_change;
}


static void FinalisePhis(ir_graph* irg)
{
	size_t i;

	for (i = 0; i < count_addrs; i++) {
		ir_node* next_phi;
		ir_node* phi;

		for (phi = unfinished_phis[i]; phi != NULL; phi = next_phi) {
			ir_node* block = get_nodes_block(phi);
			size_t block_n_preds = get_Block_n_cfgpreds(block);

			next_phi = get_irn_link(phi);

			DB((dbg, LEVEL_4, "===> Finialising phi %+F in %+F\n", phi, block));

			if (block_n_preds == 1) {
				ir_node* pred_block = get_Block_cfgpred_block(block, 0);
				ir_nodeset_t* pred_sets = get_irn_link(pred_block);
				ir_node* after = GenerateSync(irg, pred_block, &pred_sets[i]);

				assert(is_Unknown(get_Phi_pred(phi, 0)));
				exchange(phi, after);
			} else {
				ir_node** in;
				size_t j;

				NEW_ARR_A(ir_node*, in, block_n_preds);
				for (j = 0; j < block_n_preds; j++) {
					ir_node* pred_block = get_Block_cfgpred_block(block, j);
					ir_nodeset_t* pred_sets = get_irn_link(pred_block);

					if (is_Unknown(get_Phi_pred(phi, j))) {
						set_Phi_pred(phi, j, GenerateSync(irg, pred_block, &pred_sets[i]));
					}
				}
			}
		}
	}
}


static void Detotalise(ir_graph* irg)
{
	ir_node* end_block = get_irg_end_block(irg);
	size_t npreds = get_Block_n_cfgpreds(end_block);
	size_t i;

	unfinished_phis = xmalloc(sizeof(*unfinished_phis) * count_addrs);
	for (i = 0; i < count_addrs; i++) {
		unfinished_phis[i] = NULL;
	}

	for (i = 0; i < npreds; i++) {
		ir_node* pred = get_Block_cfgpred(end_block, i);
		assert(is_Return(pred));
		DB((dbg, LEVEL_2, "===> Starting memory walk at %+F\n", pred));
		WalkMem(irg, pred, NULL);
	}

	FinalisePhis(irg);
	xfree(unfinished_phis);
}


static void AddSyncPreds(ir_nodeset_t* preds, ir_node* sync)
{
	size_t n = get_Sync_n_preds(sync);
	size_t i;

	for (i = 0; i < n; i++) {
		ir_node* pred = get_Sync_pred(sync, i);
		if (is_Sync(pred)) {
			AddSyncPreds(preds, pred);
		} else {
			ir_nodeset_insert(preds, pred);
		}
	}
}


static void NormaliseSync(ir_node* node, void* env)
{
	ir_nodeset_t preds;
	ir_nodeset_iterator_t iter;
	ir_node** in;
	size_t count_preds;
	size_t i;
	(void) env;

	if (!is_Sync(node)) return;

	ir_nodeset_init(&preds);
	AddSyncPreds(&preds, node);

	count_preds = ir_nodeset_size(&preds);
	if (count_preds != (unsigned)get_Sync_n_preds(node)) {
		NEW_ARR_A(ir_node*, in, count_preds);
		ir_nodeset_iterator_init(&iter, &preds);
		for (i = 0; i < count_preds; i++) {
			ir_node* pred = ir_nodeset_iterator_next(&iter);
			assert(pred != NULL);
			in[i] = pred;
		}
		set_irn_in(node, count_preds, in);
	}

	ir_nodeset_destroy(&preds);
}


void opt_ldst2(ir_graph* irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.ldst2");
	DB((dbg, LEVEL_1, "===> Performing load/store optimisation on %+F\n", irg));

	normalize_one_return(irg);
	dump_ir_block_graph(irg, "-prefluffig");

	obstack_init(&obst);

	if (1 /* XXX */ || get_opt_alias_analysis()) {
		assure_irg_address_taken_computed(irg);
		assure_irp_globals_address_taken_computed();
	}


	CollectAddresses(irg);
	if (count_addrs == 0) return;

	irg_block_walk_graph(irg, AliasSetAdder, NULL, NULL);
	inc_irg_block_visited(irg);
	SetStartAddressesTop(irg);
	Detotalise(irg);
	dump_ir_block_graph(irg, "-fluffig");

	irg_block_walk_graph(irg, AliasSetDestroyer, NULL, NULL);
	obstack_free(&obst, NULL);

	normalize_proj_nodes(irg);
	irg_walk_graph(irg, NormaliseSync, NULL, NULL);
  optimize_graph_df(irg);
	irg_walk_graph(irg, NormaliseSync, NULL, NULL);
	dump_ir_block_graph(irg, "-postfluffig");
}
