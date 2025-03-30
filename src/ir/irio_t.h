#ifndef FIRM_IR_IRIO_T_H
#define FIRM_IR_IRIO_T_H

#include "irio.h"

#include "irnode_t.h"
#include "obst.h"
#include "pdeq.h"
#include "set.h"
#include "type_t.h"
#include "typerep.h"
#include <stdio.h>

typedef struct delayed_initializer_t {
	ir_initializer_t *initializer;
	long              node_nr;
} delayed_initializer_t;

typedef struct delayed_pred_t {
	ir_node *node;
	int      n_preds;
	long     preds[];
} delayed_pred_t;

typedef struct read_env_t {
	int            c;           /**< currently read char */
	FILE          *file;
	const char    *inputname;
	unsigned       line;

	ir_graph      *irg;
	set           *idset;       /**< id_entry set, which maps from file ids to
	                                 new Firm elements */
	ir_type      **fixedtypes;
	bool           read_errors;
	struct obstack obst;
	struct obstack preds_obst;
	delayed_initializer_t *delayed_initializers;
	const delayed_pred_t **delayed_preds;
} read_env_t;

typedef struct write_env_t {
	FILE *file;
	deq_t write_queue;
	deq_t entity_queue;
} write_env_t;

void write_align(write_env_t *env, ir_align align);
void write_builtin_kind(write_env_t *env, ir_builtin_kind kind);
void write_cond_jmp_predicate(write_env_t *env, cond_jmp_predicate pred);
void write_entity_ref(write_env_t *env, ir_entity *entity);
void write_ident(write_env_t *env, ident *id);
void write_ident_null(write_env_t *env, ident *id);
void write_initializer(write_env_t *env, ir_initializer_t const *ini);
void write_int(write_env_t *env, int value);
void write_long(write_env_t *env, long value);
void write_loop(write_env_t *env, bool loop);
void write_mode_ref(write_env_t *env, ir_mode *mode);
void write_node_nr(write_env_t *env, const ir_node *node);
void write_node_ref(write_env_t *env, const ir_node *node);
void write_pin_state(write_env_t *env, op_pin_state state);
void write_pred_refs(write_env_t *env, const ir_node *node, int from);
void write_relation(write_env_t *env, ir_relation relation);
void write_size_t(write_env_t *env, size_t value);
void write_string(write_env_t *env, const char *string);
void write_switch_table_ref(write_env_t *env, const ir_switch_table *table);
void write_symbol(write_env_t *env, const char *symbol);
void write_tarval_ref(write_env_t *env, ir_tarval *tv);
void write_throws(write_env_t *env, bool throws);
void write_type_ref(write_env_t *env, ir_type *type);
void write_unsigned(write_env_t *env, unsigned value);
void write_visibility(write_env_t *env, ir_visibility visibility);
void write_volatility(write_env_t *env, ir_volatility vol);

/**
 * Read a node reference and return the node for it. This assumes that the node
 * was previously read. This is fine for all normal nodes.
 * (Note: that we "break" loops by having special code for phi, block or anchor
 *  nodes in place, firm guarantees us that a loop in the graph always contains
 *  a phi, block or anchor node)
 */
ir_node *read_node_ref(read_env_t *env);
ir_mode *read_mode_ref(read_env_t *env);
ir_type *read_type_ref(read_env_t *env);
ir_switch_table *read_switch_table_ref(read_env_t *env);
ir_entity *read_entity_ref(read_env_t *env);
unsigned read_unsigned(read_env_t *env);
size_t read_size_t(read_env_t *env);
int read_int(read_env_t *env);
ir_builtin_kind read_builtin_kind(read_env_t *env);
cond_jmp_predicate read_cond_jmp_predicate(read_env_t *env);
ir_align read_align(read_env_t *env);
bool read_pinned(read_env_t *env);
int read_preds(read_env_t *env);
bool read_throws(read_env_t *env);
bool read_loop(read_env_t *env);
ir_relation read_relation(read_env_t *env);
ir_tarval *read_tarval_ref(read_env_t *env);
ir_volatility read_volatility(read_env_t *env);

typedef ir_node* read_node_func(read_env_t *env);
void register_node_reader(char const *const name, read_node_func *const func);

typedef void write_node_func(write_env_t *env, ir_node const *node);
void register_node_writer(ir_op *op, write_node_func *func);

void register_generated_node_writers(void);
void register_generated_node_readers(void);

#endif
