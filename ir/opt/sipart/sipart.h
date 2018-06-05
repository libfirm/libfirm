#ifndef __SIPART_H__
#define __SIPART_H__

#include <bitset.h>
#include <firm.h>
#include <setjmp.h>
#include <adt/pqueue.h>
#include <irnodemap.h>
#include "heights.h"
#include "error.h"
#include "ces_convex_graph.h"

#define SIDATA(x) (ir_nodemap_get(sidata, &env->nodemap, x))
#define STORE_SIDATA(x,y) ir_nodemap_insert(&env->nodemap,x,y)

#define topo2node(x) env->topo2node[x]
#define topo2data(x) SIDATA(topo2node(x))

#if !defined(panic)
#define panic(x) do { fputs(stderr, x); exit(-1);  } while(0)
#endif

void sipart_opt(ir_graph *irg, struct stream_description* streams);
void sipart_register_opt(void);

typedef struct sipart_pattern {
/* valid from pattern enumeration */
	bitset_t *nodes; 
	bitset_t *in;    /* valid on env.patterns_only=true */
	bitset_t *inin;  /* valid on env.patterns_only=false */
	bitset_t *perin; /* valid on env.patterns_only=false */
	bitset_t *out;
	bitset_t *pred;
    bitset_t *ipred; /* only valid on single-node patterns */
    bitset_t *isucc; /* only valid on single-node patterns */

	int area;
	int maxpath; /* maximum path length in pattern */

/* valid after postprocessing */
	bitset_t *succ;

	uintptr_t hash; /* type hash value */
	ir_node *node; /* node the RPG was called on */
	struct sipart_pattern *node_next; /* linked list of patterns enumerated for this node */
	struct sipart_pattern *type_next; /* linked list of type collisions */
	struct sipart_pattern *partition; /* linked list forming a partition */

	/* The following members are only valid for patterns representing a type. */

	int instances; /* Length of type_next list. */
	int popcount; /* Population count of representative pattern. */
	bitset_t *cover; /* union of nodes of patterns of this type. */
	bitset_t *cover_in; /* union of inputs of patterns of this type. */
	bitset_t *cover_out; /* union of outputs of patterns of this type. */
	long visited; /* visited counter for cycle detection */
} pattern_t;

struct si_env;

typedef bool rpg_callback(struct si_env *si_env);

typedef struct si_env {
    struct obstack obst;
    unsigned bound;      /* bound */
    bitset_t *internal;         /* bitset of nodes to be partitioned */
    bitset_t *outputs;          /* bitset of outputs of the SI */
    bitset_t *inputs;          /* bitset of inputs of the SI */
    bitset_t *touched;          /* debugging */
    bitset_t *contracted;          /* contracted nodes */
    ir_node *highest;           /* the topologically highest node inside. */
    unsigned height;            /* the size of the topo-order. */
    unsigned nsystems;          /* # of candidates */
    unsigned maxlevel;          /* deepest level reached yet */
    unsigned popcount;          /* # of nodes to be partitioned */
    ir_node **topo2node;        /* map toposort number (index) to node */
    unsigned npatterns;  /* number of patterns found when patterns_only is set */
    ir_graph *si;     /* the graph to be partitioned */
    ir_node *block;  /* restrict partitioning to contents of block */
	rpg_callback *callback; /* the user-supplied callback */
	void *userdata; /* anonymous data for the user-supplied callback */
	pattern_t *result; /* The partitioning. only valid during
						* callback invocation. */
	jmp_buf done; /* jump here to terminate the partition algorithm early. */
	ir_nodemap nodemap; /* node-specific data goes here (to spare
						 * links for clients) */

	pmap *type_map; /* map from hash value to representative pattern */
 	ir_nodemap dca; /* don't care bit analysis result */
	ir_nodemap vrp; /* value range bit analysis result */
	ir_heights_t *heights;  /* node heights in si */
	bool patterns_only; /* makes rpg() enumerate patterns instead of partitions */

	/* Some constraints for the pattern and partition enumerator are
	   duplicated here from the user options since they are
	   temporarily modified during multilevel-partitioning. */
	int maxout; /* max number of output nodes of patterns */
	int maxin; /* max number of input nodes of patterns */
	int inwidth; /* maximum number of accumulated input bits to patterns */
	int outwidth; /* maximum number of accumulated output bits to patterns */
} si_env_t;

/*
  The sipart_callback is invoked in the following cases:

   - With a NULL si_env->result before the recursive algorithm starts.
     All precomputation is finished and available for the client.
     env->done is set to a jmp_buf the client may longjmp() to in
     order to terminate the algorithm early (e.g., from a SIGALRM
     handler to implement a timeout.  Or a SIGINT one for when the
     user gets impatient)
   - When a first partitioning is found satisfying the constraints
     with the partitions in si_env->result.
   - Each time a valid partitioning is found with a better bound.

   The callback may also terminate the partitioning by returning a
   non-zero value.
 */

rpg_callback generate_output_adt;
rpg_callback dump_partitions;
rpg_callback sipart_coarsen_cb;
rpg_callback pattern_dump_cb;

typedef struct sidata {
    unsigned topo;      /* Topological order of node */
    ir_node *next;      /* The topologically lower node */

	pattern_t *pattern; /* single-node pattern for this node */

    unsigned partition; /* Conclusive partition of this node */
    unsigned order;     /* Order of conclusive partition */
    si_env_t *env;          /* Accessed during graph coloring */
    pattern_t *pattern_list; /* list of patterns computing this node */
	pqueue_t *pattern_queue; /* pqueue of above */
	unsigned width; /* bit width of the node */
	long height;
	pattern_t *contracted; /* Contraction nodes reference the contracted pattern.
							  note that bitsets might be bogus. */
} sidata;

/* Run the branch and bound partitioning algorithm on IRG.
   Userdata is avaiilable to the callback in si_env->userdata. */
void sipart(ir_graph *irg, rpg_callback callback, void *data);

/* Perform pattern enumeration ONLY.  Invoke callback for each pattern found.
   Userdata is avaiilable to the callback in si_env->userdata. */
void xiao_casseau_enumerator_bitsets(ir_graph* irg, rpg_callback *cb, void *data);

/**
 * Print a bitset to a stream in hex.
 * The bitset is printed as a hexadecimal number.
 * @param file The stream.
 * @param bs The bitset.
 */
static inline void bitset_fprint_hex(FILE *file, const bitset_t *bs)
{
	assert(8==BITS_PER_ELEM/4);
	for(size_t elt = 0; elt < (BITS_PER_ELEM + bs->size)/BITS_PER_ELEM; elt++) {
		ir_fprintf(file, "%08lx", (bs->data[elt]));
	}
}

/* Like above, but print dots instead of zeros. */
static inline void bitset_fprint_dots(FILE *file, const bitset_t *bs)
{
	(void) file;
	char *str = alloca(BITSET_SIZE_BYTES(bs->size)*2 + 1);
	char *buf = str;

	assert(8==BITS_PER_ELEM/4);
	for(size_t elt = 0; elt < (BITS_PER_ELEM + bs->size)/BITS_PER_ELEM; elt++) {
		uintptr_t nibbles = bs->data[elt];
#define n2c(x) "0123456789abcdef"[x%16]
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
		*buf++ = n2c(nibbles); nibbles /=16;
#undef n2c
		*buf = 0;
	}
	buf = str;
	while (*buf) {
		if (*buf == '0') *buf='.';
		buf++;
	}
	fprintf(file, "%s", str);
}

static inline void bitset_fprint_node(FILE *file, const bitset_t *bs, si_env_t *env)
{
	const char *prefix = "";
	size_t i;

	putc('{', file);
	for(i = bitset_next_set(bs, 0); i != (size_t)-1; i = bitset_next_set(bs, i + 1)) {
	  ir_fprintf(file, "%s%N", prefix, topo2node(i));
		prefix = ",";
	}
	putc('}', file);
}

static inline void bitset_fprint_firm(FILE *file, const bitset_t *bs, si_env_t *env)
{
	const char *prefix = "";
	size_t i;

	putc('{', file);
	for(i = bitset_next_set(bs, 0); i != (size_t)-1; i = bitset_next_set(bs, i + 1)) {
	  ir_fprintf(file, "%s%+F", prefix, topo2node(i));
		prefix = ",";
	}
	putc('}', file);
}

#define bitset_fprint_hex bitset_fprint_dots

#endif//__SIPART_H__
