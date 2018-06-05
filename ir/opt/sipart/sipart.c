/* #define NDEBUG */

#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <firm.h>
#include <irtools.h>
#include <irnodemap.h>
#include <debug.h>
#include <stdint.h>
#include <array.h>
#include <obstack.h>
#include <time.h>

#include <sys/time.h>
#include <sys/resource.h>

#include <arpa/inet.h>

#include "irnodemap.h"
#include "constbits.h"
#include "dca.h"
#include "irop_t.h"
#include "irgraph.h"
#include "irgraph_t.h"
#include "opt_init.h"

#include "sipart.h"
#define DATA SIDATA
#define STORE_DATA STORE_SIDATA
#include "contraction.h"
#include "aggregate.h"
#include "sitools.h"
#include "factor.h"
#include "myvrp.h"

#include "si2xml.h"

void remove_irp_irg(ir_graph *irg);

#define MIN(x,y) ((x<y)?x:y)
#define MAX(x,y) ((x>y)?x:y)

/* /\* types only *\/ */
/* #define TYPE_ACCOUNTING */
/* #define BOUNDED(cost, bound)  (cost < bound) */
/* #define COST(types, sets) (types) */
/* #define INITIAL_BOUND COST(9,0)		/\* initial bound  *\/ */

/* combined types + sets bound */
#define TYPE_ACCOUNTING
#define BOUNDED(cost, bound)  (cost < bound)
#define COST(types, sets) (types*1000 +sets)
#define INITIAL_BOUND COST(5,0)		/* initial bound  */

/* /\* /\\* combined types + sets bound *\\/ *\/ */
/* #define TYPE_ACCOUNTING */
/* #define BOUNDED(cost, bound)  (cost < bound) */
/* #define COST(types, sets) (types +sets*1000) */
/* #define INITIAL_BOUND COST(0,80)		/\* initial bound  *\/ */

/* /\* sets as bound *\/ */
/* #undef TYPE_ACCOUNTING */
/* #define BOUNDED(cost, bound)  (cost < bound) */
/* #define COST(types, sets) (sets) */
/* #define INITIAL_BOUND COST(8,80)	/\* initial bound  *\/ */

static struct options {
	int max_in;
	int max_out;
	int max_area;
	int multilevel;
	int bound;
	int max_path;
	int max_in_imbalance;
	int max_out_imbalance;
	int max_in_width;
	int max_out_width;
	int discout;
	/* int max_ac; */
	/* int bc_in; */
	/* int bc_out; */
	/* int bc_width; */
} options = {
	.max_in = 4,
	.max_out = 4,
	.max_area = 1000,
	.multilevel = true,
	.bound = INITIAL_BOUND,
	.max_path = 7,
	.max_in_imbalance = 3,
	.max_out_imbalance = 3,
	.max_in_width = 64,
	.max_out_width = 64,
	.discout = 0,
	/* .max_ac = 10, */
	/* .bc_width = 32, */
	/* .bc_in = 2, */
	/* .bc_out = 2, */
};

#define my_LC_OPT_ENT_INT(member, doc) LC_OPT_ENT_INT(#member, doc, &(options.member))
#define my_LC_OPT_ENT_BOOL(member, doc) LC_OPT_ENT_BOOL(#member, doc, &(options.member))

static const lc_opt_table_entry_t option_table[] = {
	my_LC_OPT_ENT_INT(max_out, " Maximum number of DF outputs of Atoms"),
	my_LC_OPT_ENT_INT(max_in, " Maximum number of DF inputs of Atoms"),
	my_LC_OPT_ENT_INT(max_area, "Maximum area for Atoms"),
	my_LC_OPT_ENT_BOOL(multilevel, "Coarsen the graph before partitioning"),
	my_LC_OPT_ENT_INT(bound, "The initial bound"),
	my_LC_OPT_ENT_INT(max_path, "Maximum path length in Atoms"),
	my_LC_OPT_ENT_INT(max_in_imbalance, "Maximum imbalance of Atom inputs (-1 to disable)"),
	my_LC_OPT_ENT_INT(max_out_imbalance, "Maximum imbalance of Atom outputs (-1 to disable)"),
	my_LC_OPT_ENT_INT(max_in_width, "Total number of input bits (0 to disable)"),
	my_LC_OPT_ENT_INT(max_out_width, "Total number of output bits (0 to disable)"),
	my_LC_OPT_ENT_BOOL(discout, "Allow pattern outputs from disconnected set"),
	/* LC_OPT_ENT_INT("bc_out", "Number of output bus connectors", &options.bc_out), */
	/* LC_OPT_ENT_INT("bc_in", "Number of input bus connectors", &options.bc_in), */
	/* LC_OPT_ENT_INT("bc_width", "Bit width of a bus connector", &options.bc_width), */
	/* LC_OPT_ENT_INT("max_types", "Maximum number of atom types to generate", &options.max_ac), */
	LC_OPT_LAST
};
static lc_opt_entry_t *sipart_opt_grp;

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
#define BS_SIZE (env->height + 1)

// #define dump_ir_graph(...)
/* #define dump_ir_graph(irg, str) do { dump_graph_stats(irg); dump_ir_graph(irg, str); } while(0) */

#define dump_ir_graph(irg, str) if ((firm_dbg_get_mask(dbg) & LEVEL_5)) dump_ir_graph(irg, str)

#define EVICT_POINTER_ARITH
#undef EVICT_CONTROL_ARITH
#define NODEFILTER_ITERATOR bitset_foreach_rev
#undef EXPELL_LOOP_INCREMENT

#define DUMP_BS(x)								\
	fprintf(stderr, #x ":\t");					\
	bitset_fprint_hex(stderr, x);		\
	fputs("\n", stderr);				\

#define DUMP_BS_NODES(x)								\
	fprintf(stderr, #x ": ");							\
	bitset_fprint_node(stderr, x, env);				\
	fputs("\n", stderr);				\

#define DUMP_BS_FIRM(x)								\
	fprintf(stderr, #x ": ");						\
	bitset_fprint_firm(stderr, x, env);				\
	fputs("\n", stderr);				\

#define DUMP_BS_TOPO(x)								\
	fprintf(stderr, #x ": ");						\
	bitset_fprint(stderr, x);		\
	fputs("\n", stderr);				\

#define DUMP_DIFF(x)							\
	fprintf(stderr, #x ": ");						\
	bitset_fprint_node(stderr, x, env);				\
	fprintf(stderr, " -> ");							\
	bitset_fprint_node(stderr, new_ ## x,env);		\
	fputs("",stderr);

/* The default stack limit of 8M on Linux is not sufficient to
 * partition the sadsixteen.  Luckily, in contrast to other resources,
 * this one can be increased... */
static void adjust_stack_limit(void)
{

	static struct rlimit rlimit = {
		.rlim_cur = 134217728,
		.rlim_max = 134217728
	};

	setrlimit(RLIMIT_STACK, &rlimit);
}

static void analyze_type(pattern_t *p, si_env_t *env)
{
	p->popcount = (int) bitset_popcount(p->nodes);

	p->cover = bitset_obstack_alloc(&env->obst, BS_SIZE);	
	p->cover_in = bitset_obstack_alloc(&env->obst, BS_SIZE);	
	p->cover_out = bitset_obstack_alloc(&env->obst, BS_SIZE);	

	for (pattern_t *i=p; i; i=i->type_next) {
		p->instances++;
		bitset_or(p->cover, i->nodes);
		bitset_or(p->cover_in, i->in);
		bitset_or(p->cover_out, i->out);
	}
}

static unsigned pattern_ninputs(bitset_t *pat, si_env_t *env)
{
	bitset_t *inputs = bitset_alloca(1+ env->height);
	bitset_foreach(pat, n) {
		bitset_or(inputs, DATA(topo2node(n))->pattern->ipred);
	}
	bitset_andnot(inputs, pat);
	return bitset_popcount(inputs);
}

static int pattern_input_balance(const pattern_t *sys, si_env_t *env)
{
	int min = env->height;
	int max = 0;

	if (sys->inin) {
		bitset_foreach(sys->inin, bit) {
			max = MAX(max, topo2data(bit)->height);
			min = MIN(min, topo2data(bit)->height);
		}
		bitset_foreach(sys->perin, bit) {
			max = MAX(max, topo2data(bit)->height);
			min = MIN(min, topo2data(bit)->height);
		}
	} else {
		bitset_foreach(sys->in, bit) {
			max = MAX(max, topo2data(bit)->height);
			min = MIN(min, topo2data(bit)->height);
		}
	}
	return max - min + 1;
}

static int pattern_output_balance(const pattern_t *sys, si_env_t *env)
{
	int min = env->height;
	int max = 0;

	bitset_foreach(sys->out, bit) {
		max = MAX(max, topo2data(bit)->height);
		min = MIN(min, topo2data(bit)->height);
	}
	return max - min + 1;
}

static void dump_pattern(const pattern_t *sys, si_env_t *env) {
	(void) env;
	fprintf(stderr, "%3dn ", sys->popcount);
	fprintf(stderr, "%4da ", sys->area);
	fprintf(stderr, "%2dl ", sys->maxpath);
	fprintf(stderr, "%d/%db ", pattern_input_balance(sys, env), pattern_output_balance(sys, env));
	fprintf(stderr, "%d/%d ", pattern_ninputs(sys->nodes, env), (int)bitset_popcount(sys->out));
	fprintf(stderr, "%08x ", (unsigned)sys->hash);
	bitset_fprint_hex(stderr, sys->nodes);
	fputc('\n', stderr);
}


/* List of all patterns, sorted by type hash. */

static void dump_type(pattern_t *p, si_env_t *env)
{
	if (! (firm_dbg_get_mask(dbg) & LEVEL_3)) return;

	pattern_t *type = pmap_get(pattern_t, env->type_map, (void *)p->hash);
	if (!type) type=p;
	dump_pattern(p, env);
	DUMP_BS(p->cover);
	DUMP_BS(p->cover_in);
	DUMP_BS(p->cover_out);
}

static inline void firm_bitset_fprint_hex(FILE *file, const bitset_t *bs, si_env_t *env)
{
	assert(8==BITS_PER_ELEM/4);
	for(size_t elt = 0; elt < (BITS_PER_ELEM + bs->size)/BITS_PER_ELEM; elt++) {
		ir_fprintf(file, "%08lx", htonl(bs->data[elt]));
	}
	ir_fprintf(file, " ");
	bitset_foreach(bs, elt) {
		ir_fprintf(file, "%F<", topo2node(elt));
	}
}

static unsigned get_irn_width(const ir_node *node, si_env_t *env) {
  ir_mode *mode = get_irn_mode(node);

  /* TODO: figure out why this function returns 0 for boolean mode nodes... */
  if (mode_b == mode)
	  return 1;

  if (!mode_is_num(mode))
	  return 1;
  int result = get_mode_size_bits(mode);

  ir_tarval *dca = ir_nodemap_get(ir_tarval, &env->dca, node);

  result = MIN(result, 1+get_tarval_highest_bit(dca));

  bitinfo *constbits = get_bitinfo(node);
  if (constbits) {
	  ir_tarval *not_const_zero_mask = tarval_or(constbits->o, constbits->z);
	  result = MIN(result, 1+get_tarval_highest_bit(not_const_zero_mask));
  }

  myvrpdata_t *vrp = ir_nodemap_get(myvrpdata_t, &env->vrp, node);
  if (vrp) {
	  int sign =
		  tarval_cmp(vrp->bottom, get_tarval_null(mode)) == ir_relation_less ?
		  1 : 0;

	  ir_tarval *abstop = tarval_abs(vrp->top);
	  ir_tarval *absbottom = tarval_abs(vrp->bottom);

	  result = MIN(result,
				   sign + 1+ MAX(get_tarval_highest_bit(abstop),
								 get_tarval_highest_bit(absbottom)));
  }

  /* TODO: value range analysis yields additional information, but
   * libfirm's implementation needs more work in order to be useful */

  DB3("width of %+F: %u\n", node, result);
  return result;
}

static unsigned int get_irn_area(const ir_node *node, si_env_t *env) {
  ir_mode *mode = get_irn_mode(node);
  if (is_Contraction(node))
	  return DATA(node)->contracted->area;
  if (iro_Confirm == get_irn_opcode(node))
	  return 0;
  if (is_Conv(node))
	  return 0;
  if (!mode_is_data(mode))
	  return 0;

  unsigned size = DATA(node)->width;

  return size;
}

static bool valid_system(const pattern_t *sys, si_env_t *env) {
	bitset_t *unpartitioned = bitset_alloca(1+ env->height);
	bitset_copy(unpartitioned, env->internal);

	do {
		bitset_andnot(unpartitioned, sys->nodes);
	} while((sys = sys->partition));

	if (bitset_is_empty(unpartitioned))
		return true;
	puts("unpartitioned internal nodes: ");
	bitset_fprint_hex(stderr, sys->nodes);
	return false;
}

#define PRIME16 65537
static long hash_node(ir_node *node, const bitset_t *pattern, si_env_t *env)
{
	long hash = 0;

	for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (bitset_is_set(pattern, DATA(pred)->topo)) {
			if (is_op_commutative(get_irn_op(node)))
				hash *= PRIME16;
			hash += hash_node(pred, pattern, env);
		}
	}

	hash *= PRIME16;

	switch (get_irn_opcode(node)) {
	case iro_Const:
		hash += get_tarval_long(get_Const_tarval(node));
		break;
	case iro_Slice:
		hash *= 137;
		hash += get_Slice_from(node);
		hash *= 137;
		hash += get_Slice_to(node);
		break;
	case iro_Contraction:
		hash += DATA(node)->contracted->hash;
		break;
	default:
		hash += get_irn_opcode(node);
	}

	return hash;
}

static uintptr_t hash_pattern(const bitset_t *pattern, const bitset_t *outputs, si_env_t *env)
{
	uintptr_t hash = 0;

	bitset_foreach(outputs, n) {
	  hash += hash_node(topo2node(n), pattern, env);
	}
	return hash;
}

bool sipart_coarsen_cb(si_env_t *env)
{
	pattern_t *pat = env->result;

	pat->hash = hash_pattern(pat->nodes, pat->out, env);

	if (env->maxout > 1) {
		assert(bitset_popcount(pat->out) <= (size_t)env->maxout);
	}

	pattern_t *p = pmap_get(pattern_t, env->type_map, (void *)pat->hash);
	pat->type_next = p;
	pmap_insert(env->type_map, (void *)pat->hash, pat);

	bitset_foreach(pat->out, n) {
		pat->node_next = topo2data(n)->pattern_list;
		topo2data(n)->pattern_list = pat;
	}
	/* assert_constraints(pat, env); */

	return false;
}

#define INDENT(level) for(int i = level; i; i--) putchar(' ')

static struct {
	int out;
	int out_bw;
	int perin;
	int perin_bw;
	int in;
	int in_bw;
	int bound;
	int area;
	int invocations;
	int pathlen;
	int in_balance;
	int out_balance;
	int cycles;
} stat;

static void dump_parts(const pattern_t *sys, si_env_t *env) {
	fprintf(stderr, "BEGIN SYSTEM\n");
	do {
		dump_pattern(sys, env);
	} while((sys = sys->partition));
	fprintf(stderr, "END SYSTEM\n");
}

static void apply_parts(const pattern_t *sys, si_env_t *env) {
	unsigned number = 1;
	do {
		unsigned order = bitset_next_set(sys->nodes, 0);
		bitset_foreach(sys->nodes, n) {
			topo2data(n)->partition = number;
			topo2data(n)->order = order;
		}
		number++;
	} while((sys = sys->partition));
}

static dump_node_info_cb_t partition_dumper;
static void partition_dumper(void *data, FILE *f, const ir_node *node)
{
  si_env_t *env = (si_env_t *)data;
  (void) env;
  fprintf(f, "part=%u (order=%u)\n", DATA(node)->partition, DATA(node)->order);
}

/* dump_node_vcgattr_func my_print_vcg_color; */
static int my_print_vcg_color_part(FILE *out, const ir_node *node, const ir_node *local)
{
	(void) local;
	si_env_t *env = get_irg_link(get_irn_irg(node));
	fprintf(out, " color:%u ", (DATA(node)->partition%32));
	return 0;
}

static bool is_internal_node(const ir_node *node, si_env_t *env) {

	ir_opcode op = get_irn_opcode(node);
	ir_mode *mode = get_irn_mode(node);

	if (! (mode_is_int(mode) || mode == mode_b))
		return false;
	if (is_Block(node)) return false;

	if (env->block && (env->block != get_nodes_block(node)))
		return false;

	if (get_nodes_block(node) == get_irg_start(get_irn_irg(node)))
		return false;
	if (get_nodes_block(node) == get_irg_end(get_irn_irg(node)))
		return false;

	switch(op) {

	case iro_Return:
	case iro_Load:
	case iro_Store:
	/* case iro_Phi: */
	case iro_Block:
	case iro_Proj:
		return false;

	case iro_Const:
		return false;

	default:
		return true;
	}
}


/* dump_node_vcgattr_func my_print_vcg_color; */
static int my_print_vcg_color_internal(FILE *out, const ir_node *node, const ir_node *local)
{
	si_env_t *env = get_irg_link(get_irn_irg(node));
	(void) local;
	fprintf(out, " color:%u \n", 2+ bitset_is_set(env->internal, DATA(node)->topo));
	return 0;
}

static irg_walk_func clear_partinfo;
static void clear_partinfo(ir_node *node, void *data) {
	si_env_t *env = data;
	DATA(node)->partition = 0;
	DATA(node)->order = 0;
}

static void vcg_dump_parts(pattern_t *sys, const char *prefix, si_env_t *env) {
	dump_parts(sys, env);
	irg_walk_graph(env->si, clear_partinfo, NULL, env);
	apply_parts(sys, env);
	set_dump_node_vcgattr_hook(my_print_vcg_color_part);

	void *handle = dump_add_node_info_callback(partition_dumper, env);

	dump_ir_graph(env->si, prefix);
	dump_remove_node_info_callback(handle);
	set_dump_node_vcgattr_hook(NULL);
}

/* #define vcg_dump_parts(x, y, z) */

/* void set_dump_node_vcgattr_hook(dump_node_vcgattr_func hook) */

static irg_walk_func toposort_walker;
static void toposort_walker(ir_node *node, void *data) {
	si_env_t *env = data;
	if (!node) {
		env->height = 0;
		env->highest = 0;
		return;
	}
	if (is_Block(node))
		return;
	sidata *nd = DATA(node);
	nd->topo = env->height++;
	nd->next = env->highest;
	env->highest = node;
}

static dump_node_info_cb_t topo_dumper;
static void topo_dumper(void *data, FILE *f, const ir_node *node)
{
  si_env_t *env = (si_env_t *)data;
  (void) env;
  if(!data) return;
  if(is_Block(node)) return;
  fprintf(f, "topo=%u\n", DATA(node)->topo);
}

static dump_node_info_cb_t bitset_dumper;
static void bitset_dumper(void *data, FILE *f, const ir_node *node)
{
  si_env_t *env = (si_env_t *)data;
  sidata *nd = DATA(node);

  if (is_Block(node)) return;

  fprintf(f, "is_internal=%u\n", bitset_is_set(env->internal, nd->topo));

  fprintf(f, "perinput=");
  bitset_fprint(f, nd->pattern->perin);
  fprintf(f, "\n");
  fprintf(f, "ininput=");
  bitset_fprint(f, nd->pattern->inin);
  fprintf(f, "\n");
  fprintf(f, "pred=");
  bitset_fprint(f, nd->pattern->pred);
  fprintf(f, "\n");
  fprintf(f, "succ=");
  bitset_fprint(f, nd->pattern->succ);
  fprintf(f, "\n");

  ir_tarval *dca = ir_nodemap_get(ir_tarval, &env->dca, node);
  if (dca)
	  ir_fprintf(f, "dca: %T\n", dca);
  bitinfo *constbits = get_bitinfo(node);
  myvrpdata_t *vrp = ir_nodemap_get(myvrpdata_t, &env->vrp, node);

  if (constbits)
  	  ir_fprintf(f, "constbits: o=%F, z=%F\n",  constbits->o, constbits->z);

  if (vrp)
  	  ir_fprintf(f, "VRP: [%F, %F]\n",  vrp->bottom, vrp->top);

  if (dca&&constbits)
  	  ir_fprintf(f, "width: %u\n",  get_irn_width(node, env));

  ir_fprintf(f, "myheight: %d\n",  nd->height);

  ir_fprintf(f, "area: %u\n",  get_irn_area(node, env));

}

static int true_p(ir_node *sel, ir_node *mux_false, ir_node *mux_true)
{
  (void) sel;
  (void) mux_false;
  (void) mux_true;
  return 1;
}

static void my_init_heights(si_env_t *env) {
	bitset_t *pred = bitset_alloca(BS_SIZE);

	bitset_foreach(env->internal, elt) {
		bitset_or(pred, topo2data(elt)->pattern->isucc);
	}
	bitset_andnot(pred, env->internal);

	int height = 1;

	while(!bitset_is_empty(pred)) {
		bitset_foreach(pred, elt) {
			bitset_or(pred, topo2data(elt)->pattern->ipred);
			topo2data(elt)->height = height;
			bitset_clear(pred, elt);
		}
		height++;
	}
}

static irg_walk_func init_node_data;
static void init_node_data(ir_node *node, void *data) {
  si_env_t *env = data;
  void *mem = obstack_alloc(&env->obst, sizeof(struct sidata));
  memset(mem, 0, sizeof(struct sidata));
  STORE_DATA(node, mem);
  DATA(node)->partition = 0;
  DATA(node)->topo = 0;
  DATA(node)->env = env;
  DATA(node)->width = get_irn_width(node, env);
  DATA(node)->pattern = obstack_alloc(&env->obst, sizeof(struct sipart_pattern));
}

static void compute_succ(bitset_t *tgt, ir_node *succ, si_env_t *env) {
	ir_graph *irg = get_irn_irg(succ);

	if (irn_visited(succ)) return;

	bitset_foreach (DATA(succ)->pattern->isucc, elm) {
		if (!bitset_is_set(env->internal, elm)) continue;
		bitset_set(tgt, elm);
		set_irn_visited(succ, irg->visited);
		compute_succ(tgt, topo2node(elm), env);
	}
}

static void compute_pred(bitset_t *tgt, ir_node *pred, si_env_t *env) {
	ir_graph *irg = get_irn_irg(pred);

	if (irn_visited(pred)) return;

	bitset_foreach (DATA(pred)->pattern->ipred, elm) {
		bitset_set(tgt, elm);
		set_irn_visited(pred, irg->visited);
		compute_pred(tgt, topo2node(elm), env);
	}
}

static void init_bitsets (ir_graph *irg, si_env_t *env) {
	ir_node *node = env->highest;
	assure_edges(irg);

	env->internal = bitset_obstack_alloc(&env->obst, BS_SIZE);
	env->touched = bitset_obstack_alloc(&env->obst, BS_SIZE);
	env->contracted = bitset_obstack_alloc(&env->obst, BS_SIZE);

	do {
		sidata *nd = DATA(node);

		env->topo2node[nd->topo] = node;

		if (is_internal_node(node, env))
			bitset_set(env->internal, nd->topo);
		else
			bitset_clear(env->internal, nd->topo);

		nd->pattern->ipred = bitset_obstack_alloc(&env->obst, BS_SIZE);
		nd->pattern->inin = bitset_obstack_alloc(&env->obst, BS_SIZE);
		nd->pattern->perin = bitset_obstack_alloc(&env->obst, BS_SIZE);
		for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(node, i);
			if (is_Const(pred)) continue;
			if (mode_M == get_irn_mode(pred)) continue;
			if (is_Phi(node)) continue;
			if (is_internal_node(pred, env))
				bitset_set(nd->pattern->inin, DATA(pred)->topo);
			else
				bitset_set(nd->pattern->perin, DATA(pred)->topo);
			bitset_set(nd->pattern->ipred, DATA(pred)->topo);
		}

		nd->pattern->isucc = bitset_obstack_alloc(&env->obst, BS_SIZE);
		foreach_out_edge_safe(node, edge) {
			ir_node *out = get_edge_src_irn(edge);
			assert(out);
			if (!DATA(out)) continue;
			if (is_Block(out)) continue;
			if (is_Phi(out)) continue;
			bitset_set(nd->pattern->isucc, DATA(out)->topo);
		}

		nd->pattern->nodes = bitset_obstack_alloc(&env->obst, BS_SIZE);
		bitset_set(nd->pattern->nodes, nd->topo);

	} while ((node = DATA(node)->next));

	/* Edge walker can start from block nodes only, so compute
	 * nd->succ from isucc instead. */

	node = env->highest;
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	do {
		sidata *nd = DATA(node);
		nd->pattern->succ = bitset_obstack_alloc(&env->obst, BS_SIZE);
		nd->pattern->pred = bitset_obstack_alloc(&env->obst, BS_SIZE);
		inc_irg_visited(irg);
		compute_succ(nd->pattern->succ, node, env);
		inc_irg_visited(irg);
		compute_pred(nd->pattern->pred, node, env);
	} while ((node = DATA(node)->next));
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

#ifdef EVICT_POINTER_ARITH

	DB2("%u internal nodes BEFORE ptr arith eviction.\n", bitset_popcount(env->internal));
	/* Evict Load/Store pointer predecessors from internal set. */
	node = env->highest;
	do {
		sidata *nd = DATA(node);
		if ((get_irn_mode(node) == mode_P)) {
		  bitset_andnot(env->internal, nd->pattern->pred);
		} 
	} while ((node = DATA(node)->next));
#endif /* EVICT_POINTER_ARITH */

#ifdef EVICT_CONTROL_ARITH

	DB2("%u internal nodes BEFORE control arith. eviction.\n", bitset_popcount(env->internal));
	/* Evict Load/Store pointer predecessors from internal set. */
	node = env->highest;
	do {
		sidata *nd = DATA(node);
		if (is_Cond(node)) {
		  bitset_andnot(env->internal, nd->pattern->pred);
		}
	} while ((node = DATA(node)->next));
#endif /* EVICT_CONTROL_ARITH */
}


static void assert_convexity_1(const bitset_t *pattern, ir_node *node, bool flag, si_env_t *env) {
	sidata *nd = DATA(node);
	ir_graph *irg = get_irn_irg(node);
	ir_mode *mode = get_irn_mode(node);
	if (is_Block(node)) return;
	if (!(mode_is_data(mode) || mode == mode_b)) return;
	if (irn_visited(node)) return;
	set_irn_visited(node, irg->visited);

	if (bitset_is_set(pattern, nd->topo)) {
		if (flag) {
			DB2("\tbogusly inside pattern: %n %N\n", node, node);
			pattern_t p;
			p.nodes = (bitset_t *)pattern;
			p.partition = 0;
			vcg_dump_parts(&p, "failed-convexity", env);
			assert(!"convex pattern");
		}
	} else {
		flag=1;
	}

	for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		assert_convexity_1(pattern, pred, flag, env);
	}
}

static void assert_convexity(const bitset_t *pattern, si_env_t *env)
{
	ir_graph *irg = env->si;
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
	bitset_foreach(pattern, n) {
		inc_irg_visited(irg);
		assert_convexity_1(pattern, topo2node(n), 0, env);
	}
	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

}

#if 0
static void assert_constraints(bitset_t *pattern, si_env_t *env)
{
	/* if (pattern_ninputs(pattern,env) > INP */
	/* 	|| pattern_noutputs(pattern,env) > OUTP ) { */
	/* 	pattern_t p = {.this=pattern, .next=0}; */
	/* 	vcg_dump_parts(&p, __func__, env); */
	/* 	assert(false); */
	/* } */
	assert_convexity(pattern, env);

}
#endif

static void nodefilter(
	bitset_t *filtered_set, /* result bitset */
	const bitset_t *pivot,	/* the pivot partition */
	const bitset_t *ininput,	/* set of internal inputs of pivot */
	const bitset_t *perinput,/* set of permanent inputs of pivot */
	const bitset_t *pred,	/* set of predecessors of pivot */
	const bitset_t *outputs, /* set of outputs of pivot */
	const bitset_t *done, /* set of partitioned nodes */
	int order, /* order of pivot pattern */
	 si_env_t *env) {
	(void) pivot, (void)pred, (void)outputs, (void)order;

	if (options.discout && (env->maxout > 1))
		{
			int width = 0;
			if (env->outwidth) {
				bitset_foreach(outputs, elm) {
					width += topo2data(elm)->width;
				}
			}
			if (( 0 == env->outwidth ) || (width < env->outwidth)) {
				bitset_copy(filtered_set, env->internal);
				bitset_andnot(filtered_set, pred);
				bitset_andnot(filtered_set, pivot);
				bitset_andnot(filtered_set, done);
				bitset_clear_range(filtered_set, order, env->height+1);
			}
		}

	/* DUMP_BS(filtered_set) */

	bitset_foreach(ininput, n) {
		/* size_t order = bitset_next_set(pivot, 0); */
		/* if (n > order) continue; */
		if (bitset_intersect(topo2data(n)->pattern->succ, ininput)
			||bitset_intersect(topo2data(n)->pattern->succ, perinput))
			continue;

		bitset_set(filtered_set, n);
	}

	bitset_andnot(filtered_set, done);
	/* bitset_foreach(filtered_set, n) { */
	/* 	assert(bitset_is_set(env->internal, n)); */
	/* } */
}

#if 0
void verify_bitset_iterator(bitset_t *b) {
	int f = 0;
	int r = 0;

	bitset_foreach(b, e) f++;
	bitset_foreach_rev(b, e) r++;
	assert(f==r);
}
#endif

static void dumpstats(si_env_t *env) 
{
	/* bitset_t *scratch = bitset_alloca(1+ env->height); */
	/* bitset_copy(scratch, env->internal); */
	/* bitset_andnot(scratch, env->touched); */
	/* if (bitset_popcount(scratch) < 5 && bitset_popcount(scratch) > 0) { */
	/* 	bitset_foreach (scratch, elm) { */
	/* 		ir_printf("%F %N ", topo2node(elm) ,topo2node(elm)); */
	/* 	} */
	/* } */

	DB2("=== "
			  "%10u invocations "
			  "%3u bound "
			  "%5u systems "
			  "%3u maxlevel \n",
			  stat.invocations,
			  env->bound,
			  env->nsystems,
			  env->maxlevel
		);
}

static long cycle_detection_counter;

#define SEEN (cycle_detection_counter-1)
#define FINISHED (cycle_detection_counter)

static bool detect_cycles_1(pattern_t *pat, pattern_t *partitions) {
	if (pat->visited == FINISHED)
		return false;
	if (pat->visited == SEEN)
		return true;
	pat->visited = SEEN;
	bitset_foreach(pat->inin, elm) {
		pattern_t *pat2 = partitions;
		do {
			if (bitset_is_set(pat2->nodes, elm))
				if (detect_cycles_1(pat2, partitions))
					return true;
		} while((pat2 = pat2->partition));
	}

	pat->visited = FINISHED;
	return false;
}

#undef SEEN
#undef FINISHED

/* check a partitioning for cycles */
static bool detect_cycles(pattern_t *sys)
{
	pattern_t *pat = sys;
	cycle_detection_counter += 2;
	do {
		if (detect_cycles_1(pat, sys))
			return true;
	} while((pat = pat->partition));
	return false;
}

/* Recursive partition generator. */
static void rpg(
	const pattern_t *sys, /* linked list of past pivots */
	const bitset_t *pivot,	/* the pivot partition */
	const bitset_t *ininput,	/* set of internal inputs of pivot */
	const bitset_t *perinput,/* set of permanent inputs of pivot */
	const bitset_t *pred,	/* set of predecessors of pivot */
	const bitset_t *output,  /* set of output nodes of pivot */
	const bitset_t *done,	    /* set of partitioned nodes */
	unsigned level, /* search tree level == # of partitioned nodes */
	unsigned sets, /* # of sets produced during descend */
	unsigned types, /* # of types produced during descend */
	unsigned area, /* area of pivot */
	unsigned popcount, /* population count of pivot */
	unsigned order,  /* order of pivot */
	unsigned low,  /* lowest height of a node in pivot */
	unsigned high,  /* highest height of a node in pivot */
	si_env_t *env) {

	stat.invocations++;

	if (level > env->maxlevel)
		env->maxlevel = level;

#if 1
	/* Output some progress indication */
	if ((0 == (stat.invocations)%1000000)) {
		DB2("%d sets %d level\t", sets, level);
		dumpstats(env);
		/* pattern_t final; */
		/* final.partition = sys; */
		/* final.nodes = pivot; */
		/* final.area = area; */
		/* final.out = output; */
		/* final.inin = ininput; */
		/* final.perin = perinput; */
		/* final.popcount = popcount; */
		/* dump_parts(&final, env); */

	}
#endif


	/* if (bitset_intersect(ininput, done)) */
	/* 	return; */

	/* size_t order = bitset_next_set(pivot, 0); */

	if (!env->patterns_only)
		if (!BOUNDED(COST(types,sets),env->bound)) {
			stat.bound++;
			return;
		}

	if ((long)area > options.max_area) {
		stat.area++;
		return;
	}

	if  (options.max_path) {
		if (high - low + 1 > (unsigned)options.max_path) {
			stat.pathlen++;
			return;
		}
	}

	if (!env->patterns_only) {

/* 0. if all nodes are partitioned, enumerate partitioning */
		if (level == env->popcount) {
			env->nsystems++;
			pattern_t final;
			types++;

#ifdef TYPE_ACCOUNTING
			final.hash = hash_pattern(pivot, output, env);
			for(const pattern_t *p = sys; p; p=p->partition) {
				if (p->hash == final.hash) {
					types--;
					break;
				}
			}
#else
			final.hash = 0;
#endif

			if (BOUNDED(COST(types,sets),env->bound)) {
				final.partition = (pattern_t *)sys;
				final.nodes = (bitset_t *)pivot;
				final.area = area;
				final.popcount = popcount;
				final.out = (bitset_t *)output;
				final.inin = (bitset_t *)ininput;
				final.perin = (bitset_t *)perinput;
				final.maxpath = high - low + 1;
				/* dump_parts(&final); */

				/* check one last constraint */
				if (detect_cycles(&final)) {
					stat.cycles++;
					return;
				}

				env->bound = COST(types, sets);

#ifndef TYPE_ACCOUNTING
				for(pattern_t *p = &final; p; p=p->partition) {
					p->hash = hash_pattern(p->nodes, p->out, env);
				}
#endif

				env->result = &final;
				DB2("Enumerating partitioning at " );
				dumpstats(env);
				if (env->callback(env)) longjmp(env->done, 1);
			}
			return;
		}
	}
/* 	DEBUG_ONLY( */
/* 	fprintf(stderr, "=== lvl %u out %u inin %u perin %u order %u done %u\n", */
/* 		   level, */
/* 		   bitset_popcount(output), */
/* 		   bitset_popcount(ininput), */
/* 		   bitset_popcount(perinput), */
/* 		   order, */
/* 		   bitset_popcount(done) + bitset_popcount(pivot) */

/* ); */

	/* INDENT(level);DUMP_BS(pivot) */

/* 		); */

	/* ∀ n ∈ nodefilter(pivot), union, prune, recurse. */

	bitset_t *filtered_set = bitset_alloca(1 + env->height);
    nodefilter(filtered_set, pivot, ininput, perinput, pred, output, done, order, env);

	/* DEBUG_ONLY( */
	/* 	INDENT(level);DUMP_BS_TOPO(filtered_set) */
	/* 	INDENT(level);DUMP_BS(pred) */
	/* 	) */
	NODEFILTER_ITERATOR(filtered_set, n) {
		bitset_set(env->touched, n);

		bitset_t *new_pivot = bitset_alloca(1 + env->height);
		bitset_copy(new_pivot, pivot);
		bitset_set(new_pivot, n);

		bitset_t *new_ininput = bitset_alloca(1 + env->height);
		bitset_copy(new_ininput, ininput);
		bitset_clear_range(new_ininput, n, env->height+1);
		bitset_or(new_ininput, topo2data(n)->pattern->inin);

		bitset_t *new_perinput = bitset_alloca(1 + env->height);
		bitset_copy(new_perinput, ininput);
		bitset_clear_range(new_perinput, 0, n+1);
		bitset_or(new_perinput, topo2data(n)->pattern->perin);
		bitset_or(new_perinput, perinput);

		if (bitset_popcount(new_perinput) > (size_t)env->maxin) {
			stat.in++;
			continue;
		}

		/* { */
		/* 	int width = 0; */
		/* 	bitset_foreach(new_perinput, elm) { */
		/* 		width += topo2data(elm)->width; */
		/* 	} */
		/* 	if (width > env->inwidth) */
		/* 		continue; */
		/* } */

		bitset_t *new_output;

		if (bitset_is_set(pred, n)) {
			if (bitset_contains(topo2data(n)->pattern->isucc, new_pivot)) {
				new_output = (bitset_t *)output;
			} else {
				new_output = bitset_alloca(1 + env->height);
				bitset_copy(new_output, output);
				bitset_set(new_output, n);
			}
		} else {
			new_output = bitset_alloca(1 + env->height);
			bitset_copy(new_output, output);
			bitset_set(new_output, n);
		}

		if (bitset_popcount(new_output) > (size_t)env->maxout) {
			stat.out++;
			continue;
		}

		if ((((env->maxout > 1) && env->outwidth)
			 || options.max_out_imbalance > -1
			 || options.max_in_imbalance > -1) && (env->maxout > 1))
		{
			int width = 0;
			int output_low = env->height;
			int output_high = 0;

			bitset_foreach(new_output, elm) {
				width += topo2data(elm)->width;
				output_high = MAX(output_high, topo2data(elm)->height);
				output_low = MIN(output_low, topo2data(elm)->height);
			}
			if ((env->maxout > 1) && env->outwidth && (width > env->outwidth)) {
				stat.out_bw++;
				continue;
			}
			if (options.max_out_imbalance > -1 && (output_high - output_low > options.max_out_imbalance)) {
				stat.out_balance++;
				continue;
			}
		}

		/* INDENT(level); ir_printf("picked %+F", topo2node(n)); */

		bitset_t *new_pred = bitset_alloca(1 + env->height);
		bitset_copy(new_pred, pred);

		if (bitset_is_set(new_pred, n)) {
			bitset_clear(new_pred, n);
		} else {
			bitset_or(new_pred, topo2data(n)->pattern->pred);
		}

		/* DEBUG_ONLY( */
		/* /\* ir_printf("branch: %u - %n %N\n", n, topo2node(n), topo2node(n)); *\/ */
		/* 	INDENT(level);DUMP_DIFF(output) */
		/* 	INDENT(level);DUMP_DIFF(ininput) */
		/* 	INDENT(level);DUMP_DIFF(perinput) */
		/* 	) */
		rpg(sys, new_pivot, new_ininput, new_perinput, new_pred, new_output, done, level+1, sets, types,
		    area + get_irn_area(topo2node(n), env),
		    1 + popcount, MIN(order,n),
		    MIN(low, topo2data(n)->height),
		    MAX(high, topo2data(n)->height),
		    env);

		/* Bound might have changed. */
		if (!env->patterns_only)
			if (!BOUNDED(COST(types,sets), env->bound)) {
				stat.bound++;
				return;
			}
	}

	int new_types = types + 1;

#ifdef TYPE_ACCOUNTING
	uintptr_t hash = hash_pattern(pivot, output, env);

	for(const pattern_t *p = sys; p; p=p->partition) {
		if (p->hash == hash) {
			new_types--;
			break;
		}
	}
#else
	uintptr_t hash = 0;
#endif

	/* Would creating a new partition cross the current bound? */
	if (!env->patterns_only)
		if (!BOUNDED(COST(new_types,sets+1),env->bound)) {
			stat.bound++;
			return;
		}

#ifdef FUZZY
	{
		if (sets / types > FUZZY)
			return;
	}
#endif

	if ((long)area > options.max_area) {
		stat.area++;
		return;
	}

	/* If constraints are satisfied, branch topo-highest, not yet
	 * partitioned internal node as new pivot. */

	int width = 0;
	int population = 0;

	int input_low = env->height;
	int input_high = 0;

	bitset_foreach(perinput, elm) {
		width += topo2data(elm)->width;
		population++;
		input_high = MAX(input_high, topo2data(elm)->height);
		input_low = MIN(input_low, topo2data(elm)->height);
	}
	bitset_foreach(ininput, elm) {
		width += topo2data(elm)->width;
		population++;
		input_high = MAX(input_high, topo2data(elm)->height);
		input_low = MIN(input_low, topo2data(elm)->height);
	}

	if (population > env->maxin) {
		stat.perin++;
		return;
	}
	if (env->inwidth && (width > env->inwidth)) {
		stat.perin_bw++;
		return;
	}
	if (options.max_in_imbalance > -1 && (input_high - input_low > options.max_in_imbalance)) {
		stat.in_balance++;
		return;
	}

	if (env->patterns_only)
	{
		/* Just enumerate the pattern */
		env->npatterns++;
		struct obstack *obst = &env->obst;
		pattern_t *pattern = obstack_alloc(obst, sizeof(pattern_t));

		pattern->nodes = bitset_obstack_alloc(obst, 1 + env->height);
		bitset_copy(pattern->nodes, pivot);

		pattern->in = bitset_obstack_alloc(obst, 1 + env->height);
		bitset_copy(pattern->in, ininput);
		bitset_or(pattern->in, perinput);

		pattern->out = bitset_obstack_alloc(obst, 1 + env->height);
		bitset_copy(pattern->out, output);

		pattern->pred = bitset_obstack_alloc(obst, 1 + env->height);
		bitset_copy(pattern->pred, pred);
		pattern->area = area;
		pattern->maxpath = high - low + 1;

		pattern->inin = 0;
		pattern->perin = 0;

		env->result = pattern;
		env->callback(env);
		return;
	}

		bitset_t *tmp = bitset_alloca(1 + env->height);

		bitset_copy(tmp, env->internal);
		bitset_andnot(tmp, done);
		bitset_andnot(tmp, pivot);

		size_t toponext = 0;
		toponext = bitset_prev_set(tmp, 1 + env->height);

		if (toponext) {
			bitset_set(env->touched, toponext);
			sidata *nd = topo2data(toponext);
			bitset_t *new_done = tmp;
			bitset_copy(new_done, done);
			bitset_or(new_done, pivot);
			/* INDENT(level); */
			/* DUMP_BS(pivot) */
			/* INDENT(level); */
			/* ir_printf("new pivot: %u - %n %N\n", toponext, topo2node(toponext), topo2node(toponext)); */
			pattern_t new_sys;
			new_sys.partition = (pattern_t *)sys;
			new_sys.nodes = (bitset_t *)pivot;
			new_sys.popcount = popcount;
			new_sys.area = area;
			new_sys.maxpath = high - low + 1;
			new_sys.out = (bitset_t *)output;
			new_sys.inin = (bitset_t *)ininput;
			new_sys.perin = (bitset_t *)perinput;
			new_sys.hash = hash;

			rpg(
				&new_sys,                /* const system *sys */
				nd->pattern->nodes,              	/* const bitset_t *pivot */
				nd->pattern->inin,		   	/* const bitset_t *ininput   */
				nd->pattern->perin,		   	/* const bitset_t *perinput */
				nd->pattern->pred,			   	/* const bitset_t *pred  */
				nd->pattern->nodes,			   	/* const bitset_t *output   */
				new_done,			   	/* const bitset_t *done */
				level + 1,
				sets + 1,               /* # of sets finished */
				new_types ,               /* # of types created */
				get_irn_area(topo2node(toponext), env),  /* area */
				1,   /* popcount */
				MIN(order,toponext),
				nd->height,
				nd->height,
				env);
		}
}

#if 0
static void init_succ(pattern_t *p, si_env_t *env) {
	struct obstack *obst = &env->obst;
	if (p->succ) return;
	p->succ = bitset_obstack_alloc(obst, 1 + env->height);
	bitset_foreach(p->out, elm) {
		bitset_or(p->succ, topo2data(elm)->succ);
	}
}

/* Check if the union of two convex patterns is convex. */
static bool convex_union_p(pattern_t *p1, pattern_t *p2, si_env_t *env) {
	bitset_t *intersect = bitset_alloca(1 + env->height);
	bitset_copy(intersect, p1->pred);
	init_succ(p1, env);
	init_succ(p2, env);
	bitset_and(intersect, p2->succ);

	if (!bitset_is_empty(intersect))
		return false;

	bitset_copy(intersect, p1->succ);
	bitset_and(intersect, p2->pred);

	if (!bitset_is_empty(intersect))
		return false;

	return true;
}

static bool constrained_p(pattern_t *p, si_env_t *env) {
	int inwidth = 0;
	int outwidth = 0;
	bitset_foreach(p->in, elt) {
		inwidth += topo2data(elt)->width;
	}
	bitset_foreach(p->out, elt) {
		outwidth += topo2data(elt)->width;
	}
	if (env->inwidth && (inwidth > env->inwidth)) return false;
	if (env->outwidth && (outwidth > env->outwidth)) return false;
	return true;
}

/* Compute convex hull of two convex patterns. */
static pattern_t *convex_hull(pattern_t *p1, pattern_t *p2, si_env_t *env) {
	struct obstack *obst = &env->obst;
	pattern_t *result = obstack_alloc(obst, sizeof(pattern_t));
	result->nodes = bitset_obstack_alloc(obst, 1 + env->height);

	bitset_copy(result->nodes, p1->nodes);
	bitset_or(result->nodes, p2->nodes);

	init_succ(p1, env);
	init_succ(p2, env);

	bitset_t *intersect1 = bitset_alloca(1 + env->height);
	bitset_copy(intersect1, p1->pred);
	bitset_and(intersect1, p2->succ);

	bitset_t *intersect2 = bitset_alloca(1 + env->height);
	bitset_copy(intersect2, p1->succ);
	bitset_and(intersect2, p2->pred);

	bitset_or(intersect1, intersect2);
	intersect2 = NULL;

	bitset_or(result->nodes, intersect1);

	result->in = bitset_obstack_alloc(obst, 1 + env->height);
	bitset_or(result->in, p1->in);
	bitset_or(result->in, p2->in);
	bitset_foreach(intersect1, elm) {
		bitset_or(result->in, topo2data(elm)->ipred);
	}

	result->out = bitset_obstack_alloc(obst, 1 + env->height);
	bitset_foreach(intersect1, elm) {
		bitset_or(result->out, topo2data(elm)->isucc);
	}
	bitset_foreach(result->out, elm) {
		bitset_or(result->out, topo2data(elm)->ipred);
	}

	bitset_and(result->out, result->nodes);
	bitset_or(result->out, p1->out);
	bitset_or(result->out, p2->out);

	return result;
}

void greedypart(bitset_t *outputs, int level, pattern_t *result, si_env_t *env)
{
  bitset_t *scratch = bitset_alloca(BS_SIZE);
  pqueue_t *q = new_pqueue();
  DUMP_BS(outputs);
  assert(level <100);
  if (bitset_is_empty(outputs)) {
	  DB1("all outputs satisfied!\n");
	  env->result = result;
	  dump_partitions(env);
	  return;
  }

  foreach_pmap(env->type_map, entry) {
    pattern_t *p = entry->value;

    if (bitset_intersect(outputs, p->cover_out)) {
		pqueue_put(q, p, p->popcount*p->instances);
    }
  }

  bitset_t *new_out = bitset_alloca(BS_SIZE);

  while(!pqueue_empty(q)) {
	  pattern_t *type = pqueue_pop_front(q);
	  bitset_copy(new_out, outputs);

	  /* for (pattern_t *p=type; p; p=p->type_next) { */
		  bitset_or(new_out, type->cover_in);
		  bitset_andnot(new_out, type->cover);
		  bitset_and(new_out, env->internal);
		  type->partition = result;
		  result = type;
		/* if (bitset_intersect(p->out, outputs)) { */
		/* 	bitset_andnot(new_out, p->out); */
		/* 	bitset_or(new_out, p->in); */
		/* 	bitset_and(new_out, env->internal); */
		/* 	p->partition = result; */
		/* 	result = p; */
		/* } */
	  /* } */
	  greedypart(new_out, level+1, result, env);
  }


  exit(0);
  while(!bitset_is_empty(outputs)) {
    DUMP_BS(outputs);
    bitset_foreach_rev(outputs, out) {
      ir_node *node = topo2node(out);
      DATA(node)->pattern_queue = new_pqueue();
      for (pattern_t *p = DATA(node)->pattern_list; p; p = p->node_next) {
	pattern_t *type = pmap_get(pattern_t, env->type_map, (void *)p->hash);
	pqueue_put(DATA(node)->pattern_queue, p, type->instances * type->popcount);
      }
      pattern_t *p = pqueue_pop_front(DATA(node)->pattern_queue);
      /* DUMP_BS(p->nodes); */

      p->partition = result;
      result = p;
      /* bitset_andnot(outputs, p->out); */
      bitset_clear(outputs, out);
      bitset_or(outputs, p->in);
      bitset_and(outputs, env->internal);
    }
  }

  /* for (pattern_t *p = new_patterns; p; p = p->node_next) { */
  /* 	pattern_t *type = pmap_get(pattern_t, env->type_map, (void *)p->hash);		 */
  /* 	analyze_type(type, env); */
  /* 	dump_pattern(type, env); */
  /* } */
  env->result = result;
  dump_partitions(env);
  /* generate_output_adt(env); */
}

#endif

static bool type_has_overlapping_instances(pattern_t *type, si_env_t *env)
{
	ir_reserve_resources(env->si, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(env->si);
	ir_visited_t visited = get_irg_visited(env->si);

	for (pattern_t *i=type; i; i=i->type_next) {
		bitset_foreach(i->nodes, topo) {
			ir_node *node = topo2node(topo);
			if (get_irn_visited(node) == visited) {
				ir_free_resources(env->si, IR_RESOURCE_IRN_VISITED);
				return true;
			}
			set_irn_visited(node, visited);
		}
	}
	ir_free_resources(env->si, IR_RESOURCE_IRN_VISITED);
	return false;
}

static void sipart_copy_data(ir_node *dst, ir_node *src, si_env_t *env) {
	ir_nodemap_insert(&env->nodemap, dst, ir_nodemap_get(void, &env->nodemap, src));
	ir_nodemap_insert(&env->dca, dst, ir_nodemap_get(void, &env->dca, src));
	bitinfo *constbits = get_bitinfo(src);
	set_bitinfo(dst, constbits->z, constbits->o);
}

static void contract_pattern(pattern_t *pattern, si_env_t *env) {
	int num_inputs = bitset_popcount(pattern->in);
	ir_node **in = ALLOCAN(ir_node*, num_inputs);

	int i = 0;
	bitset_foreach(pattern->in, topo) {
		ir_node *node = topo2node(topo);
		in[i++] = node;
	}

	add_irg_constraints(env->si, IR_GRAPH_CONSTRAINT_CONSTRUCTION);

	static int contraction_counter = 0;
	bitset_foreach(pattern->out, topo) {
		ir_node *contracted = topo2node(topo);
		ir_graph *irg = get_irn_irg(contracted);
		ir_mode *mode = get_irn_mode(contracted);

		/* Need to make a copy of the original node because exchange()
		 * turns the replaced node into an Deleted node. */
		ir_node *graph = irn_copy_into_irg(contracted, env->si);
		sipart_copy_data(graph, contracted, env);
		init_node_data(graph, env);
		ir_node *contraction = new_r_Contraction(get_nodes_block(graph), num_inputs, in, mode, graph);
		add_End_keepalive(get_irg_end(irg), graph);

		sipart_copy_data(contraction, contracted, env);

		DATA(contraction)->contracted = pattern;
		topo2node(topo) = contraction;
		exchange(contracted, contraction);
		bitset_or(env->contracted, pattern->nodes);
		contraction_counter++;
		assert(count_contractions(irg) == contraction_counter);		
	}
 	clear_irg_constraints(env->si, IR_GRAPH_CONSTRAINT_CONSTRUCTION);

}

static void contract_type(pattern_t *type, si_env_t *env) {
	DB2("contracting type %p.\n", type->hash);
	for (pattern_t *i=type; i; i=i->type_next) {
		if (bitset_intersect(i->nodes, env->contracted)) {
			DB2("skipping pattern due to already-contracted nodes\n", type->hash);
			continue;
		}
		contract_pattern(i, env);
	}
}

static void multilevel_contract(si_env_t *env) {
	pqueue_t *q =  new_pqueue();
	foreach_pmap(env->type_map, entry) {
		pattern_t *type = entry->value;
		if (bitset_popcount(type->nodes) < 2) {
			continue;
		}
		if (type_has_overlapping_instances(type, env)) {
			DB2("skipping type %p: overlapping instances\n", type->hash);
			continue;
		}
		pqueue_put(q, type, type->instances*type->popcount);
	}

	irg_walk_graph(env->si, firm_clear_link, 0, 0);

	/* Find a type with only non-overlapping instances and
	 * not-yet-contracted nodes. */

	while(!pqueue_empty(q)) {
		pattern_t *type = pqueue_pop_front(q);
		if (bitset_intersect(type->cover, env->contracted)) {
			DB2("skipping type %p: cover contains contracted nodes\n", type->hash);
			continue;
		}
		contract_type(type, env);
		dump_ir_graph(env->si, "contract1");
		irg_assert_verify(env->si);
	}

	{
	  int count = count_contractions(env->si);
	  remove_contraction_keepalives(env->si);
	  assert(count_contractions(env->si) == count);
	}
}

static void pattern_enum(si_env_t *env, rpg_callback *cb) {
	ir_node *out = env->highest;
	env->callback = cb;
	env->patterns_only = true;

	memset(&stat, 0, sizeof(stat));
	do {
		while(out && !bitset_is_set(env->internal, DATA(out)->topo))
			out = DATA(out)->next;

		if (out) {

			sidata *nd = DATA(out);
			rpg(
				0,                              /* linked list of past pivots */
				nd->pattern->nodes,						/* the pivot partition */
				nd->pattern->inin,					/* set of internal inputs of pivot */
				nd->pattern->perin,					/* set of permanent inputs of pivot */
				nd->pattern->pred,						/* set of predecessors of pivot */
				nd->pattern->nodes,						/* set of output nodes of pivot */
				bitset_alloca(BS_SIZE),	/* set of partitioned nodes */
				1,								/* search tree level == # of partitioned nodes */
				0,
				1,								/* # of sets produced during descend */
				get_irn_area(out, env),				/* area of pivot */
				1,								/* population count of pivot */
				nd->topo,                       /* order of pivot pattern */
				nd->height,
				nd->height,
				env);

			DB2("%u patterns found at node %+F.\n", env->npatterns, out);
			out = nd->next;
		}
	} while(out);

	DB1("%u patterns found for %+F.\n", env->npatterns, env->si);
}

static void sipart_coarsen(si_env_t *env) {

	env->type_map = pmap_create();

	pattern_enum(env, sipart_coarsen_cb);

	int ntypes = 0;

	pqueue_t *q = new_pqueue();

	foreach_pmap(env->type_map, entry) {
		ntypes++;
		pattern_t *p = entry->value;
		analyze_type(p, env);
		pqueue_put(q, p, p->popcount*p->instances - bitset_popcount(p->cover_in));
	}
	DB1("%u pattern types found.\n", ntypes, env->si);

	while(!pqueue_empty(q)) {
	  /* if (limit++>25) break; */
	  dump_type(pqueue_pop_front(q), env);
	}
	env->outputs = bitset_alloca(BS_SIZE);
	bitset_set_range(env->outputs, 0, env->height);
	bitset_andnot(env->outputs, env->internal);
	bitset_foreach(env->outputs, elt) {
		bitset_or(env->outputs, topo2data(elt)->pattern->ipred);
	}
	bitset_and(env->outputs, env->internal);
	DB1("SI outputs %d values.\n", bitset_popcount(env->outputs));
	DUMP_BS_FIRM(env->outputs);

	env->inputs = bitset_alloca(BS_SIZE);
	bitset_copy(env->inputs, env->internal);
	bitset_foreach(env->inputs, elt) {
		bitset_or(env->inputs, topo2data(elt)->pattern->ipred);
	}
	bitset_andnot(env->inputs, env->internal);
	DB1("SI has %d inputs.\n", bitset_popcount(env->inputs));
	DUMP_BS_FIRM(env->inputs);

	multilevel_contract(env);

	return;
}

static void partition_enum(si_env_t *env, rpg_callback *cb) {

	ir_node *ret = env->highest;
	env->patterns_only = false;
	env->callback = cb;

	/* invoke callback once with empty result so user can access
	 * jmp_buf to terminate the algorithm */
	env->result = 0;
	env->callback(env);

	/* Start with topo-highest internal node. */

	while(ret && !bitset_is_set(env->internal, DATA(ret)->topo))
		ret = DATA(ret)->next;
	if (!ret) {
		DB1("only external nodes in graph.");
		return;
	}

	DB((dbg,LEVEL_2,"picked initial pivot: %F %N\n", ret ,ret));

	sidata *nd = DATA(ret);
	bitset_set(env->touched, nd->topo);
	if (setjmp(env->done)) {
		return;
	}
	memset(&stat, 0, sizeof(stat));
	rpg(
		0,                              /* collection of past pivots */
		nd->pattern->nodes,						/* the pivot partition */
	    nd->pattern->inin,					/* set of internal inputs of pivot */
	    nd->pattern->perin,					/* set of permanent inputs of pivot */
		nd->pattern->pred,						/* set of predecessors of pivot */
		nd->pattern->nodes,						/* set of output nodes of pivot */
		bitset_alloca(BS_SIZE),	/* set of partitioned nodes */
		1, /* level */					/* search tree level == # of partitioned nodes */
		1,								/* # of sets produced during descend */
		0,								/* # of types produced during descend */
		get_irn_area(ret, env),
		1,
		nd->topo /* order */,
		nd->height,
		nd->height,
		env);
}

static irg_walk_func assert_safe_opcodes;
static void assert_safe_opcodes(ir_node *node, void *data) {
	(void) data;
	assert(iro_Cond != get_irn_opcode(node));
}

static irg_walk_func count_unsafe_opcodes;
static void count_unsafe_opcodes(ir_node *node, void *data) {
	int *count = data;
	ir_mode *mode = get_irn_mode(node);
	if (is_Cond(node)
		|| (is_Phi(node) && mode_M == mode)) {
		(*count)++;
		DB((dbg, LEVEL_2, "unsafe node: %+F\n", node));
	}
}

static void fill_nodemap(ir_node *node, void *data) {
	ir_nodemap *map = data;
	ir_nodemap_insert_fast(map, node, get_irn_link(node));
}

static void sipart_transform_graph_for_partitioning(ir_graph *irg)
{
	/* lower_mux(irg, &true_p); */
	/* dump_ir_graph(irg, "lower_mux"); */
	/* construct_confirms(irg); */
	/* dump_ir_graph(irg, "confirm"); */

	optimize_load_store(irg);
	dump_ir_graph(irg, "loadstore");
 	opt_parallelize_mem(irg);
	dump_ir_graph(irg, "parallelize_mem");
 	irg_walk_graph(irg, unpin_loadstore, 0, 0);
	opt_if_conv_cb(irg, &true_p);
	dump_ir_graph(irg, "ifconv");
 	opt_parallelize_mem(irg);
	optimize_cf(irg); /* unrolling doesn't grok sad_loop otherwise */
	dump_ir_graph(irg, "cfopt");

	irg_assert_verify(irg);
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	firm_init_loop_opt();
	do_loop_unrolling(irg);
	clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	dump_ir_graph(irg, "unrolled");

    /* On complete unrolling, the loop Cond and related Phi is still
	 * in the graph as a tautology.  Running vrp and optimize_cf gets
	 * rid of it. */
	/* fixpoint_vrp(irg); */
	/* dump_ir_graph(irg, "fp-vrp"); */

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	constbits_analyze(irg);
	optimize_graph_df(irg);
	dump_ir_graph(irg, "local");

	optimize_cf(irg);
	dump_ir_graph(irg, "cfopt");
	optimize_graph_df(irg);
	dump_ir_graph(irg, "local");
	remove_bads(irg);
	dump_ir_graph(irg, "remove_bads");
	optimize_cf(irg);
	dump_ir_graph(irg, "cfopt");
	optimize_graph_df(irg);
	dump_ir_graph(irg, "local");

	irg_assert_verify(irg);
}

static ir_graph *factor_biggest_block(ir_graph *irg)
{
	/* Restrict internal nodes to biggest block in IRG */
	ir_node *block = find_biggest_block(irg);
	assert(block);
	DB2("biggest block: %N\n", block);

#ifdef EXPELL_LOOP_INCREMENT
	/* We usually don't want to increment the loop counter
	 * within the SI, expell it. */
	DB2("expelling loop increment...\n");
	expell_loop_increment(block);
#endif
	ir_graph *factor = factor_bb(irg, block);

	dump_ir_graph(irg, "fraction");
	dump_ir_graph(factor, "factor");

	add_entity_additional_properties(get_irg_entity(factor),
		mtp_special_instruction);
	return factor;
}

/* Perform initialization required for first RPG run only. */
static void init_si_env_1(ir_graph *irg, si_env_t *env)
{
	adjust_stack_limit();

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	ir_nodemap_init(&env->nodemap, irg);

	/* set_vrp_data(irg); */
	/* dump_ir_graph(irg, "vrp"); */

	irg_walk_graph(irg, firm_clear_link, 0, 0);
	myvrp_analyze(irg, &env->obst);
	ir_nodemap_init(&env->vrp, irg);
	irg_walk_graph(irg, fill_nodemap, 0, &env->vrp);

	irg_walk_graph(irg, firm_clear_link, 0, 0);
	constbits_analyze(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	irg_walk_graph(irg, firm_clear_link, 0, 0);
	dca_analyze(irg);
	ir_nodemap_init(&env->dca, irg);
	irg_walk_graph(irg, fill_nodemap, 0, &env->dca);

 	irg_walk_blkwise_graph(irg, init_node_data, 0, env);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* env->heights = heights_new(irg); */
 	/* irg_walk_graph(irg, init_heights, 0, env); */
}

/* Perform initialization required for each RPG run. */
static void init_si_env_2(si_env_t *env) {
	ir_graph *irg = env->si;

	DB2("toposort... ");
	toposort_walker(0, env);
 	irg_walk_blkwise_graph(irg, /* init_node_data */ 0, toposort_walker, env);
	void *topo_handle = dump_add_node_info_callback(topo_dumper, env);

	env->topo2node = obstack_alloc(&env->obst, (1+ env->height) * sizeof(ir_node*));
	DB2("%u toposorted nodes\n", env->height);

	dump_ir_graph(irg, "toposort");

	init_bitsets(irg, env);
	void *bitset_handle = dump_add_node_info_callback(bitset_dumper, env);
	dump_ir_graph(irg, "bitsets");

	bitset_foreach(env->internal, topo) {
		assert(is_internal_node(topo2node(topo), env));
	}

	bitset_foreach(env->internal, topo) {
		assert(is_internal_node(topo2node(topo), env));
	}

 	set_irg_link(irg, env);
	set_dump_node_vcgattr_hook(my_print_vcg_color_internal);
	dump_ir_graph(irg, "internal_nodes");
	set_dump_node_vcgattr_hook(NULL);

	env->popcount = bitset_popcount(env->internal);

	DEBUG_ONLY(
	if (firm_dbg_get_mask(dbg) & LEVEL_1) {
		ir_printf("%u internal nodes.  bitset: ", env->popcount);
		bitset_fprint_hex(stderr, env->internal);
		fputs("", stderr);
	}
		)

	dump_remove_node_info_callback(topo_handle);
	dump_remove_node_info_callback(bitset_handle);
}

void sipart(ir_graph *irg, rpg_callback callback, void *data) {

	FIRM_DBG_REGISTER(dbg, "firm.si.part");

	si_env_t env = {
		.nsystems = 0,
		.maxlevel = 0,
		.height = 0,
		.highest = 0,
		.si = irg,
		.block = NULL,
		.callback = callback,
		.userdata = data,
		.maxin = options.max_in,
		.maxout = options.max_out,
		.inwidth = options.max_in_width,
		.outwidth = options.max_out_width,
	};

	DB((dbg, LEVEL_1, "===> Performing sipart on %+F\n", irg));

	sipart_transform_graph_for_partitioning(irg);

	{
		int unsafe = 0;
		irg_walk_graph(irg, count_unsafe_opcodes, 0, &unsafe);

		DBG((dbg,LEVEL_1,"warning paper mode engaged\n"));
		if (1) {
			DB2("unsafe nodes found - factoring out biggest block");
			env.si = irg = factor_biggest_block(irg);
			set_current_ir_graph(env.si);
		}
	}

	irg_assert_verify(irg);
 	irg_walk_graph(irg, assert_safe_opcodes, 0, 0);

	optimize_graph_df(irg);
	dump_ir_graph(irg, "local");


#ifdef LOWER_SLICE_PACK
	/* /\*** Custom nodes in graph beyond this line. ***\/ */

	conv_opt(irg); dump_ir_graph(irg, "conv");

	{
		unsigned long before = count_nodes(irg);
		lower_slice_pack(irg);
		dump_ir_graph(irg, "lower_slice_pack");
		unsigned long after = count_nodes(irg);
	}
/* 	aggregate_transform(irg); */
/* 	dump_ir_graph(irg, "aggregates"); */

	optimize_graph_df(irg);
	dump_ir_graph(irg, "local");

#endif

	opt_osr(irg, 0);
	dump_ir_graph(irg, "strength_reduction");

	combo(irg);
	dump_ir_graph(irg, "combo");

	/*** no modification of irg beyond this line ***/
	obstack_init(&env.obst);
	init_si_env_1(irg, &env);
	init_si_env_2(&env);

	env.bound = options.bound;

	my_init_heights(&env);

	if (options.multilevel) {
		DB2("Enumerating Patterns of %N...\n", env.si);
		env.maxout = 1;
		env.maxin = 2;
		sipart_coarsen(&env);
		dump_ir_graph(irg, "coarsened");
		edges_deactivate(irg);
		edges_activate(irg);

		init_si_env_2(&env);

		env.bound = options.bound;
	}

	DB2("Enumerating Partitions of %N...\n", env.si);
	env.maxout = options.max_out;
	env.maxin = options.max_in;
	partition_enum(&env, callback);

	DB2("Enumeration finished for %N...\n", env.si);
	dumpstats(&env);
	obstack_free(&env.obst, NULL);
}

/* Signal handler to allow stopping the enumeration with SIGINT. */
static si_env_t *client_si_env;
static void user_interrupt(int sig)
{
	(void) sig;
	if (client_si_env)
		longjmp(client_si_env->done, 1);
}

/* This callback just dumps the partitioning. */
bool dump_partitions(struct si_env* si_env)
{
	if (!si_env->result) {
		/* The callback is invoked once at the start of the algorithm,
		 * so the client can glance at the fully initialized si_part struct. */
		client_si_env = si_env;
		signal(SIGINT, user_interrupt);
		signal(SIGXCPU, user_interrupt);
		return false;
	}
	vcg_dump_parts(si_env->result, "new_bound", si_env);
	if (!si_env->patterns_only)
		assert(valid_system(si_env->result, si_env));
	return false;
}

irg_walk_func plain_dumper;
void plain_dumper(ir_node *node, void *data) {
	(void)data;
	DB2("%s: %+F\n", __func__, node);
}

struct atom_type {
	struct atom_type *next;
	ir_entity *entity;
	long hash;
};

static irg_walk_func serialize_mem_walk;
static void serialize_mem_walk(ir_node *node, void *data)
{
	ir_node **last = data;

	if (is_Load(node) || is_Store(node)) {
		set_irn_link(node, *last);
		DB2("memory walk: %+F -> %+F\n", node, *last);
		*last = node;
	}
}

static void add_fake_memory_schedule(ir_graph *irg)
{
	ir_node *node = NULL;
	irg_walk_graph(irg, firm_clear_link, serialize_mem_walk, &node);
	for(; node; node = get_irn_link(node)) {
		ir_node *next = get_irn_link(node);
		ir_node *proj = 0;
		DB2("faking memory dep: %+F -> %+F\n", node, next);
		if (!next) {
			proj = new_r_NoMem(irg);
		} else if (is_Load(next)) {
			proj = new_r_Proj(next, mode_M, pn_Load_M);
		} else if (is_Store(next)) {
			proj = new_r_Proj(next, mode_M, pn_Store_M);
		}

		assert(proj);

		if (is_Load(node)) {
			set_Load_mem(node, proj);
		} else if (is_Store(node)) {
			set_Store_mem(node, proj);
		}
	}
}

static irg_walk_func contraction2nodeset;
static void contraction2nodeset(ir_node *node, void *data)
{
	ir_nodeset_t *nodeset = data;
	DB3("%s: inserting %+F\n", __func__, node);
	ir_nodeset_insert(nodeset, node);
}

/* Translates the result of the algorithm from internal data
 * structures to the official output ADT. */
bool generate_output_adt(struct si_env* env)
{
	static int result_count;
	char buf[100];

	if (!env->result) {
		/* The callback is invoked once at the start of the algorithm,
		 * so the client can glance at the fully initialized si_part struct. */
		client_si_env = env;
		signal(SIGINT, user_interrupt);
		signal(SIGXCPU, user_interrupt);
		result_count = 0;
		return false;
	}

	dump_partitions(env);
	ir_entity *si_entity = get_irg_entity(env->si);
	ident *si_id = get_entity_ident(si_entity);
	snprintf(buf, sizeof(buf), "_result_%d", result_count);

	edges_deactivate(env->si);

/* 	irg_walk_graph(env->si, uncontract_walker, 0, 0); */

	ir_graph *partitioned = create_contracted_irg_copy(env->si);
	set_current_ir_graph(partitioned);
	set_irg_entity(partitioned,
				   copy_entity_name(get_irg_entity(env->si),
									id_mangle3("", si_id, buf)));
	void add_irp_irg(ir_graph *irg);
	add_irp_irg(partitioned); /* Dumper doesn't appear to support irgs
							   * that are not part of the IRP... */

	/* ir_remove_dump_flags(ir_dump_flag_blocks_as_subgraphs); */
	irg_assert_verify(partitioned);

/* 	clear_irg_constraints(partitioned, IR_GRAPH_CONSTRAINT_CONSTRUCTION); */

	struct atom_type *atom_type = 0;
	pattern_t *sys = env->result;
	intptr_t atom_no = 1;
	do {
		ir_nodeset_t *nodeset = ir_nodeset_new(100);
		long hash = hash_pattern(sys->nodes, sys->out, env);
		DB2("now factoring partition %d (hash: %lx) \n", atom_no, hash);
		assert_convexity(sys->nodes, env);
		bitset_foreach(sys->nodes, n) {
			ir_node *node = get_irn_link(topo2node(n));

			ir_nodeset_insert(nodeset, node);
			if (is_Contraction(node)) {
				walk_contraction(node, contraction2nodeset, nodeset);
			}
		}
		add_contraction_keepalives(partitioned);
		ir_graph *atom = factor_nodeset(partitioned, nodeset);
		remove_contraction_keepalives(partitioned);
		ir_nodeset_destroy(nodeset);
		fix_contractions_after_irg_copy(atom);
		remove_contraction_keepalives(atom);
		irg_walk_graph(atom, uncontract_walker, 0, 0);
		ir_entity *atom_entity = get_irg_entity(atom);

		/* Reuse entity of same atom type. */

		for(struct atom_type *t = atom_type; t; t=t->next) {
			if (hash == t->hash) {
				set_Call_ptr(get_entity_link(atom_entity),
							 new_r_Address(partitioned, t->entity));
				remove_irp_irg(atom);
				DB3("Reusing atom type %+F \n", t->entity);
				goto type_collision;
			}
		}

		snprintf(buf, sizeof(buf), "_result_%d_atom_0x%016lx", (int)result_count, hash);
		ident *atom_id = id_mangle3("", si_id, buf);
		set_entity_ident(atom_entity, atom_id);
		struct atom_type *new_atom_type = alloca(sizeof(struct atom_type));
		new_atom_type->next = atom_type;
		new_atom_type->hash = hash;
		new_atom_type->entity = atom_entity;
		atom_type = new_atom_type;
		DB2("Creating atom type %+F \n", atom_entity);
		dump_ir_graph(atom, "new");
		atom_no++;
	type_collision:
		continue;
	} while((sys = sys->partition));


	add_fake_memory_schedule(partitioned);
	dump_ir_graph(partitioned, "partitioned");
	remove_irp_irg(partitioned);

	{
		snprintf(buf, sizeof(buf), "_result_%d.xml", (int)result_count);
		ident *filename = id_mangle3("", si_id, buf);
		FILE *output = fopen(get_id_str(filename), "w");
		si2xml(output, partitioned);
		fclose(output);
	}

	result_count++;
	return false;
}

/* enumerate patterns */
void xiao_casseau_enumerator_bitsets(ir_graph* irg, rpg_callback *cb, void *data)
{
	FIRM_DBG_REGISTER(dbg, "firm.si.part");

	si_env_t env = {
		.patterns_only = true,
		.userdata = data,
		.maxin = options.max_in,
		.maxout = options.max_out,
		.inwidth = 0,
		.outwidth = 0,
		.si = irg
	};

	obstack_init(&env.obst);
	init_si_env_1(irg, &env);
	init_si_env_2(&env);
	pattern_enum(&env, cb);
	obstack_free(&env.obst, NULL);
	constbits_clear(irg);
}

bool pattern_dump_cb(si_env_t *env)
{
	pattern_t *pat = env->result;
	DUMP_BS(pat->nodes);

	return false; /* continue enumeration */
}

void sipart_opt(ir_graph *irg, struct stream_description* streams)
{
	set_op_dump(op_Contraction, node_dump_Contraction);
	/* sipart(irg, dump_partitions, NULL); */
	sipart(irg, generate_output_adt, NULL);
}

void sipart_register_opt(void)
{
	lc_opt_entry_t *opt_grp   = lc_opt_get_grp(firm_opt_get_root(), "opt");
	sipart_opt_grp = lc_opt_get_grp(opt_grp, "sipart");
	lc_opt_add_table(sipart_opt_grp, option_table);
}
