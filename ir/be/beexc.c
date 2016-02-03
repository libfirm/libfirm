#include "irnode_t.h"
#include "irnodehashmap.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "be_t.h"
#include "beemitter.h"
#include "besched.h"
#include "begnuas.h"
#include "beexc.h"
#include "util.h"

/** Representation of an entry in the exception table. */
typedef struct exc_entry {
	ir_node    *instr;          /** The instruction that can issue an exception. */
	ir_node    *x_except_block; /** The instruction's X_except block */
	ir_label_t  label;          /** The exception label. */
} exc_entry;

/* Counter; the next exception label to be assigned. */
static ir_label_t next_exc_label;
/* Current graph we're dealing with. */
static ir_entity const *irg_entity = NULL;
/* The graph's exception entry list. */
static exc_entry *exc_list = NULL;
/* Map of ir_node* -> exc_entry (actually bijective). */
static ir_nodehashmap_t node_exc_map;

static void check_initialized(void)
{
	assert(irg_entity != NULL && "beexc not initialized");
}

/**
 * Collect list of exception entries in ascending instruction address ordering.
 */
static void collect_exception_entries(ir_node *const *const schedule)
{
	/* We MUST walk this in schedule order (by contract) */
	for (size_t i = 0, n = ARR_LEN(schedule); i < n; ++i) {
		ir_node *const block = schedule[i];
		sched_foreach(block, node) {
			if (!needs_exc_label(node))
				continue;

			/* Find X_except block. */
			ir_node const *const proj = get_Proj_for_pn(node, node->op->pn_x_except);
			assert(get_irn_n_edges(proj) == 1);
			assert(get_irn_n_edges_kind(proj, EDGE_KIND_BLOCK) == 0);
			ir_node *const except_block = get_edge_src_irn(get_irn_out_edge_first(proj));

			if (!is_x_except_block(except_block))
				/* End block */
				continue;

			/* Add exc_entry to list. */
			exc_entry e = {
				.instr = node,
				.x_except_block = except_block,
				.label = next_exc_label++
			};
			ARR_APP1(exc_entry, exc_list, e);
		}
	}
}

/** Emit "__foobar_LSDA". */
static void emit_function_lsda_name(const ir_entity *const function_entity)
{
	be_emit_cstring("__");
	be_gas_emit_entity(function_entity);
	be_emit_cstring("_LSDA");
}

void be_exc_emit_function_prolog(void)
{
	if (!ARR_LEN(exc_list))
		/* No X_except blocks in this irg */
		return;

	be_emit_cstring("\t.cfi_personality 0x0, " FIRM_PERSONALITY_NAME "\n");
	be_emit_write_line();
	be_emit_cstring("\t.cfi_lsda 0x0, ");
	emit_function_lsda_name(irg_entity);
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Emit an exception label name ("LE123").
 */
static void emit_label_name(const ir_label_t label)
{
	be_emit_string(be_gas_insn_label_prefix());
	be_emit_irprintf("%lu", label);
}

void be_exc_emit_irn_label(const ir_node *const node)
{
	check_initialized();

	exc_entry* entry = ir_nodehashmap_get(exc_entry, &node_exc_map, node);
	if (entry != NULL) {
		emit_label_name(entry->label);
		be_emit_char(':');
		be_emit_write_line();
	}
}

void be_exc_emit_table(void)
{
	check_initialized();

	size_t n_exc = ARR_LEN(exc_list);

	if (!n_exc)
		/* No X_except blocks in this irg */
		return;

	emit_function_lsda_name(irg_entity);
	be_emit_cstring(":\n");
	be_emit_write_line();

	if (be_options.verbose_asm) {
		be_emit_cstring("\t/* Number of entries */\n");
		be_emit_write_line();
	}
	be_emit_irprintf("\t.quad %lu\n", n_exc);
	be_emit_write_line();

	for (size_t i = 0; i < ARR_LEN(exc_list); ++i) {
		if (be_options.verbose_asm) {
			be_emit_irprintf("\t/* Handler for %+F: %+F */\n", exc_list[i].instr, exc_list[i].x_except_block);
			be_emit_write_line();
		}
		be_emit_cstring("\t.quad ");
		emit_label_name(exc_list[i].label);
		be_emit_char('\n');
		be_emit_cstring("\t.quad ");
		be_gas_emit_block_name(exc_list[i].x_except_block);
		be_emit_char('\n');
	}
}

void be_exc_init(ir_entity const* irg_entity2, ir_node *const *const schedule)
{
	irg_entity = irg_entity2;

	exc_list = NEW_ARR_F(exc_entry, 0);
	collect_exception_entries(schedule);

	/* Build node -> exc_entry map.  Note that the exc_list MUST NOT be changed
	 * (e.g., sorted) after building this map; otherwise the exc_entry*
	 * pointers stored in the map may point to the wrong entry or be invalid
	 * entirely. */
	ir_nodehashmap_init(&node_exc_map);
	for (size_t i = 0; i < ARR_LEN(exc_list); ++i) {
		ir_nodehashmap_insert(&node_exc_map, exc_list[i].instr, &exc_list[i]);
	}
}

void be_exc_finish(void)
{
	check_initialized();
	DEL_ARR_F(exc_list);
	ir_nodehashmap_destroy(&node_exc_map);
	irg_entity = NULL;
	exc_list = NULL;
}
