#ifndef __CES_SI_TOOLS_H__
#define __CES_SI_TOOLS_H__

#include "pset_new.h"
#include "plist.h"
#include "firm.h"
#include "obstack.h"
#include "irnodemap.h"
#include "debug.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#define BOLD(bla) "\x1b[1m"bla"\x1b[0m"
#define YELLOW(bla) "\x1b[33;1m"bla"\x1b[0m"
#define RED(bla) "\x1b[31;1m"bla"\x1b[0m"
#define SKULL "\xE2\x98\xA0"
#define PIRATE(bla) "\x1b[1;37m"SKULL bla SKULL"\x1b[0m"

#ifdef UNUSED_PARAM
#undef UNUSED_PARAM
#endif 
#define UNUSED_PARAM(x) (void)(x)

extern firm_dbg_module_t *ces_dbg;
#define DBG_DO(mod, lvl, func) if(firm_dbg_get_mask(mod) & lvl) func;

typedef enum { WHITE,        BLUE=1,    RED,      GREEN,      YELLOW,      PINK,      CYAN,      
							 DARK_WHITE,   DARK_BLUE, DARK_RED, DARK_GREEN, DARK_YELLOW, DARK_PINK, DARK_CYAN  } color_names;

struct memLists {
 plist_t* loadList;
 plist_t* storeList;
};

struct pattern {
	ir_node* start;
	ir_graph* irg;
	ir_nodemap* map;
	unsigned int pattern_height;
	unsigned int new_height;
	pset_new_t* pattern;
	pset_new_t* in;
	pset_new_t* out;
	pset_new_t* pred;
	pset_new_t* disc;
	pset_new_t* inInput;
  pset_new_t* perInput;
	pset_new_t* allNodes;
};


struct env {
	pset_new_t* in;
	pset_new_t* fns;
	ir_node* block;
};

typedef enum ces_stat_tag_t {
        CES_STAT_FIRST,
        CES_STAT_PHIS = CES_STAT_FIRST, /**< phi count (excluding mem-phis) */
        CES_STAT_MEM_PHIS,             /**< memory-phi count */
        CES_STAT_MEMOPS,
		CES_STAT_LOADS,
		CES_STAT_STORES,
		CES_STAT_SYNC,
		CES_STAT_CALL,
        CES_STAT_BLOCKS,
		CES_STAT_NORMALS,
		CES_STAT_ILLEGAL,
		CES_STAT_CFG_NODES,                /**< cfg nodes */
        CES_STAT_TOTAL,
				
		CES_STAT_MAX /* keep as extra value for loops & array declaration*/
} ces_stat_tag_t; 
ENUM_COUNTABLE(ces_stat_tag_t)
typedef unsigned long ces_node_stats_t[CES_STAT_MAX];

struct load_base {
	ir_node* base;
	ir_node* c1;
	ir_node* x;
	ir_node* c2;
	ir_node* y;
	ir_node* c3;
	int c1_value;
	int c2_value;
	int c3_value;
	//ir_node* new_ptr;
	size_t size;
	ir_mode* mode;
};

/************ command line parameters *********/
typedef struct ces_si_params_t {
	int      example;
	unsigned      phases;
} ces_si_params_t;

/* SI Identification parameters */
extern ces_si_params_t ces_si_params;

enum {
	CES_PHASE_SIPART = 1,
	CES_PHASE_XIAO   = 2,
	CES_PHASE_LDST   = 4,
};

static const lc_opt_enum_mask_items_t ces_phase_items[] = {
        { "sipart",  CES_PHASE_SIPART },
        { "xiao",    CES_PHASE_XIAO },
        { "ldst",    CES_PHASE_LDST  },
        { NULL,         0 }
};

static lc_opt_enum_mask_var_t ces_phases_var = {
        &ces_si_params.phases, ces_phase_items
};

static const lc_opt_table_entry_t si_options[] = {
	LC_OPT_ENT_INT("example", "example param in si group", &(ces_si_params.example)),
	LC_OPT_ENT_ENUM_MASK("phases", "select ces-si phases", &ces_phases_var),
	LC_OPT_LAST
};
/************** END command line parameters **********/

//uses private ces_obstack
extern struct obstack* ces_obstack;

void ces_pattern_new(struct pattern* pattern, ir_nodemap* topo_map);
struct pattern* ces_pattern_copy(struct pattern* old);
void ces_pattern_add(struct pattern* pattern, ir_node* node);
void ces_disc(ir_node* current, void* envPtr);
void pset_new_clone(pset_new_t* new, pset_new_t* old);
void pset_new_union(pset_new_t* set1, const pset_new_t* set2);
pset_new_t* pset_new_intersect(pset_new_t* set1, pset_new_t* set2);
void ces_walker(ir_graph* graph, ir_node* start, void (*irg_walk_func)(ir_node *, void *), void* env, int up);
int ces_pattern_get_height(struct pattern* pattern);

void ces_collect_node_stats(ces_node_stats_t *new_stats, ir_graph *irg);
void ces_emit_node_stats(ces_node_stats_t *stats, const char *prefix);
void ces_print_node_stats(ces_node_stats_t *stats);
void ces_node_stat_walker(ir_node *irn, void *data);

void ces_set_color_map(ir_nodemap* map);
void ces_dump_colored_graph(ir_graph* irg, const char * const title );

ir_node* ces_load_get_proj(ir_node* load);

void ces_dump_irg(ir_node* memop, const char const *title);
void ces_print_irg(ir_node* node, int indent);
void ces_print_irg_max(ir_node* node, int indent, int limit);
ir_mode* get_memop_mode(const ir_node* node);
ir_align get_memop_unaligned(const ir_node* node);
ir_volatility get_memop_volatility(const ir_node* node);

int is_ProjP(ir_node* node);

void ces_plist_remove_and_advance(plist_t* list, plist_element_t** el);
void plist_insert_all(plist_t* into, plist_t* from);

void ces_nodemap_list(ir_nodemap* map);

void stat_ev_ull_fmt(char const * fmt, ...);

int ces_exchange_edge(ir_node* from1, ir_node* to1, ir_node* from2, ir_node* to2);
int ces_find_edge(ir_node* from, ir_node* to);

ir_node* get_memop_ptr(ir_node* memop);
//ir_node* ces_get_original(ir_nodemap* ces_copy_map, ir_node* copy);


//--------------------------  implementation in ces_duplicate_subgraph.c ---------------
ir_graph* ces_dup_subgraph(ir_node* node);

#endif//__CES_SI_TOOLS_H__
