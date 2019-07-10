#ifndef FACTOR_H

ir_node *find_biggest_block(ir_graph *irg);

/* Subset predicate type */
typedef bool subset_predicate(ir_node *node, void *data);

/* Create new method and IRG from nodes satisfying PREDICATE and
 * replace the subset's nodes with a Call.  A pointer to the Call
 * node is stored in the entity's link. */
ir_graph *factor_subset(ir_graph *irg,
			subset_predicate *predicate,
			void *data);

/* Wrapper around factor_subset with a predicate that compares node's
   blocks with source_block. */
ir_graph *factor_bb(ir_graph *irg, ir_node *source_block);

/* Wrapper around factor_bb with a predicate that checks presence of
   nodes in a nodeset. */
ir_graph *factor_nodeset(ir_graph *irg, ir_nodeset_t *set);


void factor_subset_core(struct factor_env* env, const char* name);

struct factor_env {
	subset_predicate *predicate;
	void *clientdata; /* client data for predicate */
	ir_graph *new_irg;
	ir_graph *old_irg;
	ir_entity *new_method;
	ir_nodeset_t *inputs; /* data mode inputs */
	ir_nodeset_t *outputs;  /* data mode outputs */
	ir_nodeset_t *keepalives;  /* keepalives */
	ir_node *call;
	ir_node *mem_in;
	ir_node *mem_out;
	ir_node *block; /* block to place the call in */
};
void dupe_nodes_walker(ir_node *node, void *data);

#define FACTOR_H
#endif
