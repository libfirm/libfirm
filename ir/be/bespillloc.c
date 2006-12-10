/*	be_spill_loc (ation) -> BY Sven Polk*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "obst.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "debug.h"
#include "xmalloc.h"

#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bechordal_t.h"

#include "obst.h"
#include "set.h"
#include "pdeq.h"
#include "pset.h"
#include "bitset.h"

#include "irprintf_t.h"

#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irprintf.h"



#include "irprog.h"
#include "irgopt.h"
#include "irdump.h"
#include "phiclass.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irtools.h"
#include "return.h"

#include "bearch.h"
#include "firm/bearch_firm.h"
#include "ia32/bearch_ia32.h"
#include "arm/bearch_arm.h"
#include "ppc32/bearch_ppc32.h"
#include "mips/bearch_mips.h"

#include "be_t.h"
#include "benumb_t.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"

#include "bespillbelady.h"
#include "bera.h"
#include "beraextern.h"
#include "bechordal_t.h"
#include "beifg.h"
#include "beifg_impl.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "beabi.h"
#include "belower.h"
#include "bestat.h"

#include "error.h"

#include "irnode_t.h"
#include "entity_t.h"

#include "irprintf.h"

#include "irphase.h"
#include "irphase_t.h"

#include "typewalk.h"

//#include "bespillloc_t.h"
#include "bespillloc.h"

#define LPP_SERVER "i44pc52"
#define LPP_SOLVER "cplex"

// "cat /proc/cpuinfo" cache size : 512 KB => wahrscheinlich L2
// arranged into 32-byte cache lines



#define LINESIZE_BYTES (32)

#define L1SIZE   (64)
#define L2SIZE   (512)

#define L1SIZE_BYTES   (L1SIZE * 1024)
#define L2SIZE_BYTES   (L2SIZE * 1024)

#define L1LINECOUNT (L1SIZE_BYTES / LINESIZE_BYTES)
#define L2LINECOUNT (L2SIZE_BYTES / LINESIZE_BYTES)

#define ASSOCIATIVE   (1)
#define BLOCKFACTOR   (2)
#define CONTEMP_RANGE (10)




typedef struct _spilloc_env_t {

	struct obstack ob;

	ir_graph		 *irg;
	const arch_env_t *arch;

	pset *ents;
	pset *nodes;
	pmap *ent_nodes;

	pmap *afgraph;
	pset *afgraph_vs;
	pset *afgraph_es;

	int   ent_cnt;
	int   node_cnt;

	int   blk_cnt;
	int	  cli_cnt;

	int	  intval_cnt;
	int	  intval[100];

	pmap *Cli_ents;
	pmap *position;
	pset *coals;

	pset *chilimbi_Cli_ents;

	//lpp_t *lpp;

} spilloc_env_t;



typedef struct _cacheline {
	int   start;
	int   end;
	int   nr;		// (1,...,n)
	pset *ents;
} cacheline;



typedef struct _vertex {
	entity		*ent;
	int			offset_bits;
	int			offset_bytes;

	pset		*neighbors;		/* The neighboring entities */
	int         ela;			/* entity layout affinity */
    cacheline   *Cli;
	int         is_coal;		/* singled/doubled position */
} vertex;



typedef struct _edge {
	entity *src;
	entity *tgt;
	vertex *vtx_src;
	vertex *vtx_tgt;
	int     aff;
} edge;



typedef struct _Node { /*Prototype Row of Graph-Matrix*/
	vertex *vtx;
	pset   *edges;	/*The outgoin' edges of vtx*/
	int     status;
} Node;











typedef struct _ent_position {
	int			is_set;
	int			offset;
	cacheline	*Cli;
	int			nr;
	entity		*ent;
} ent_position;








typedef struct _Coalesce {
	entity *ent_1;
	entity *ent_2;
} Coalesce;







static void _ents(entity*, void*);
static void _ent_nodes(ir_node*, void*);

static void create_Vertex(void*);
static void create_Edge(entity*, entity*, void*);

int		values_contemp(ir_node*, ir_node*);
int		count_Edge_Affinity(pset*, pset*);

static void	Cli_ents(int, int, int, void*);
static cacheline *create_Cli_ent(struct obstack*);

int		wt(entity*, entity*);
int		get_edge_affinity(entity*, entity*, void*);
int		compute_Entity_Cache_Affinity(entity*, pset*, void*);

void	ir_printf_ent_nodes(void*);
void	ir_printf_ent_nodes_aff(void*) ;

int		Entity_values_interfere(entity*, entity*, void*);
int		Ents_Coalesce (entity*, entity*, void*);
void	 Check_Coalesce(void*);

int		delta_configuration_locality(entity*, cacheline*, void*);

int		check_offset(void*);
int		is_filled(pset*, pset*);

entity *min_configuration_locality(cacheline*, void*);
entity *max_configuration_locality(cacheline*, void*);
void	bbcache_REORDER (void*);

int		is_pasteable(entity*, pset*);
entity *MAX_affinity_ents(pset*, pset*, void*);
int		bbcache_CHILIMBI (void*);





static void _ents(entity *_entity, void *env) {
	/* GET all ents from stack-frame of the current irg*/

	spilloc_env_t *spi = env;
	entity *ent = obstack_alloc(&spi->ob, sizeof(*ent));

	ent = _entity;
	pset_insert_ptr(spi->ents, ent);
	spi->ent_cnt++;
}

static void _ent_nodes(ir_node *irn, void *env) {
	/* GET ir_node(s) for each found entity */

	spilloc_env_t *spi = env;
	entity *ir_ent, *ent;
	ir_node *node;
	pset *nodes;

	ir_ent = arch_get_frame_entity(spi->arch, irn);
	if (ir_ent)
	{
		foreach_pset(spi->ents, ent)
		{
			if(ir_ent == ent)
			{
				if(!pmap_find(spi->ent_nodes, ent)) {
					nodes = pset_new_ptr_default();
					pmap_insert(spi->ent_nodes, ent, nodes);

				}
				nodes = pmap_get(spi->ent_nodes, ent);
				node = obstack_alloc(&spi->ob, sizeof(*node));
				node = irn;

				pset_insert_ptr(nodes, node);		/* entity and their ir_node(set)*/

				pset_insert_ptr(spi->nodes, node);	/* 'only' all ir_node(s)*/

				spi->node_cnt++;
			}
		}
	}
}

static void create_Vertex(void *env) {

	spilloc_env_t *spi = env;
	entity *ent;
	Node *node;


	foreach_pset(spi->ents, ent)
	{
		/* alloc */

		node   = obstack_alloc(&spi->ob, sizeof(*node));
		node->vtx = obstack_alloc(&spi->ob, sizeof(*(node->vtx)));
		node->edges = pset_new_ptr_default();

		/* init */
		node->vtx->ent			= ent;
		node->vtx->offset_bits  = 0;
		node->vtx->offset_bytes = 0;
		node->vtx->ela			= 0;
		node->vtx->neighbors	= pset_new_ptr_default();
		/*
		node->vtx->Cli->end	= 0;
		node->vtx->Cli->start	= 0;
		node->vtx->Cli->nr		= 0;
		node->vtx->Cli->ents   = pset_new_ptr_default();
		*/
		node->status			= 0;

		/* write */
		pmap_insert(spi->afgraph, ent, node);
		pset_insert_ptr(spi->afgraph_vs, node);
	}
}

int values_contemp(ir_node *a, ir_node *b) {

	/* check, if 2 nodes lying within range */

	int i;
	ir_node *irn = a;
	ir_node *prev;
	ir_node *next;
	int range = CONTEMP_RANGE;

	// backward (until block-border)
	for(i = 0; i < range; i++) {
		if(sched_has_prev(irn)) {
			prev = sched_prev(irn);
			if(prev == b) return -1;
			irn = prev;
		}
	}

	// forward (until block-border)
	for(i = 0, irn = a; i < range; i++) {
		if(sched_has_next(irn)) {

			next = sched_next(irn);
			if(next == b) return 1;
			irn = next;
		}
	}
	return 0;
}

int count_Edge_Affinity(pset *a, pset *b) {

	/* count entity *a and entity *b */

	ir_node *a_node, *cpy;
	pset *copy = pset_new_ptr(pset_count(b));
	int cnt=0;

	foreach_pset(b, cpy) {pset_insert_ptr(copy, cpy);}
	foreach_pset(a, a_node) {
		foreach_pset(copy, cpy) {
			 if (values_contemp(a_node, cpy) != 0)
				 cnt++;
		}
	}
	del_pset(copy);
	return cnt;

}

static void create_Edge(entity *a, entity *b, void *env) {

	/* create a node in afgraph */

	spilloc_env_t *spi = env;

	pset *nodes_a,*nodes_b;
	int aff;
	Node *node;
	edge *edg;

	nodes_a = ((pset *)pmap_get(spi->ent_nodes, a));
	nodes_b = ((pset *)pmap_get(spi->ent_nodes, b));
	aff = 0;
	if(nodes_a && nodes_b)aff = count_Edge_Affinity(nodes_a, nodes_b);

	if(aff != 0)
	{
		// alloc
		edg = obstack_alloc(&spi->ob, sizeof(*edg));

		// init
		edg->src = a;
		node = (Node *)pmap_get(spi->afgraph, a);
		edg->vtx_src = node->vtx;
		edg->tgt = b;
		node = (Node *)pmap_get(spi->afgraph, b);
		edg->vtx_tgt = node->vtx;
		edg->aff = aff;

		// write
		node = pmap_get(spi->afgraph, a);
		pset_insert_ptr(node->edges, b);
		node = pmap_get(spi->afgraph, b);
		pset_insert_ptr(node->edges, a);
		pset_insert_ptr(spi->afgraph_es, edg);
	}
}

static void Cli_ents(int start, int end, int nr, void *env) {

	spilloc_env_t *spi = env;
	pset *surrounder = pset_new_ptr_default();
	entity *ent;

	cacheline *cli;

	foreach_pset(spi->ents, ent)
	{
			if((start < get_entity_offset_bytes(ent)) && (get_entity_offset_bytes(ent) < end)) {
				pset_insert_ptr(surrounder, ent);
			}
	}

	cli = obstack_alloc(&spi->ob, sizeof(*cli));

	cli->start 	= start;
	cli->end   	= end;
	cli->nr 	= nr;
	cli->ents 	= pset_new_ptr_default();

	foreach_pset(surrounder, ent) {pset_insert_ptr(cli->ents,ent);}
	pmap_insert(spi->Cli_ents, (void *)nr, cli);
	del_pset(surrounder);
}

static cacheline *create_Cli_ent(struct obstack *ob) {

	cacheline *cli;

	cli = obstack_alloc(ob, sizeof(*cli));

	cli->start 	= 0;
	cli->end   	= 0;
	cli->nr 	= 0;
	cli->ents 	= pset_new_ptr_default();

	return cli;
}






int wt(entity *ent1, entity *ent2) {
	/*Relative Distance of 2 Ents == from start to start*/

	int o1, o2, cache_blk_size, dist, wt;

	o1 = get_entity_offset_bytes(ent1);
	o2 = get_entity_offset_bytes(ent2);

	if (o1 < o2) {
		ent1->type->size;
		dist = (o2 - o1);
	}
	if (o1 > o2) dist = (o1 - o2);

	cache_blk_size = LINESIZE_BYTES;
	wt = ((cache_blk_size - dist) / cache_blk_size);
	if(wt != 0) return wt;

	// default
	return 0;
}

int get_edge_affinity(entity *a, entity *b, pmap *graph) {

	Node *node;
	edge *edg;

	if((pmap_find(graph,a))) {
		node = pmap_get(graph, a);
		foreach_pset(node->edges, edg) {
			if(edg->src == a && edg->tgt == b) {
				return edg->aff;
			}
		}
	}
	return 0;
}

int compute_Entity_Cache_Affinity(entity *ent, pset *surrs, void *env) {

	spilloc_env_t *spi = env;
	entity *sur;
	int ela = 0;
	int aff = 0;

	if(pset_count(surrs) != 0) {
		foreach_pset(surrs, sur)
		{
			aff = get_edge_affinity(ent, sur, spi->afgraph);
			ela += (wt(ent, sur) * aff);
		}
	}
	return ela;
}


void ir_printf_ent_nodes(void *env) {

	spilloc_env_t *spi = env;

	entity *ent;
	pset *nodes;
	ir_node *node;

	printf("\n\n\n");
	// printf("%c", get_irg_dump_name(spi->irg));

	nodes = pset_new_ptr_default();
	foreach_pset(spi->ents, ent) {
		printf("\n <%d>",ent->nr);
		nodes = pmap_get(spi->ent_nodes, ent);
		if(nodes) {foreach_pset(nodes, node) {printf(" %d, ", node->node_nr);}}
	}
}


void ir_printf_ent_nodes_aff(void *env) {

	spilloc_env_t *spi = env;
	edge *edg;
	entity *ent;
	pset *nodes;
	ir_node *node;

	printf("\n\n\n");

	nodes = pset_new_ptr_default();
	foreach_pset(spi->afgraph_es, edg) {
		printf("\n <%d-%d> :",edg->src->nr, edg->tgt->nr);
		printf(" %d \n", edg->aff);
	}
}

void ir_printf_chilimbi_cachelines(spilloc_env_t env) {

	spilloc_env_t spi = env;
	cacheline *cline;
	entity *ent;
	int i=0;

	printf("<< ");
	foreach_pset(spi.chilimbi_Cli_ents, cline) {
		printf("%d", i);
		foreach_pset(cline->ents, ent)
		{
			printf("%d,", ent->nr);
		}
		i++;
	}
	printf(" >>\n");
}












int Entity_values_interfere(entity *ent_A, entity *ent_B, void *env) {

	spilloc_env_t *spi = env;

	ir_node *node_A, *node_B;
	pset *nodes_A = pmap_get(spi->ent_nodes, ent_A);
	pset *nodes_B = pmap_get(spi->ent_nodes, ent_B);

	foreach_pset(nodes_A, node_A) {
		foreach_pset(nodes_B, node_B) {
			if(values_interfere(node_A, node_B)) {
				return 0;
			}
		}
	}
	return 1;
}

int Ents_Coalesce (entity *a, entity *b, void *env) {

	spilloc_env_t *spi = env;
	edge *edg;
	Node *aNode,*bNode;

			/* check  (ent,ent)-interfere */

	if(Entity_values_interfere(a,b,&spi) == 1) {

		aNode = pmap_get(spi->afgraph, a);
		bNode = pmap_get(spi->afgraph, b);

		if(pset_find_ptr(aNode->edges,b) && pset_find_ptr(bNode->edges,a))
		{

			// find THE EDGE(a,b) - if exists

			pmap_foreach(spi->afgraph_es, edg) {
				if((edg->src == a || edg->tgt == b) || (edg->src == b || edg->tgt == a)) pmap_break(spi->afgraph_es);
			}
			// (a,b) are coalescable

			if ((edg != NULL) && (edg->aff > 0))
			{
				return 1;
			}
		}
	}
			// (a,b) not coalescable

	return 0;

}


void Check_Coalesce(void *env) {

	/* return a subset of (ptr_set) = all potential coalescing pairs */

	spilloc_env_t *spi = env;
	pset *these = pset_new_ptr_default();
	pset *those = pset_new_ptr_default();

	entity *cpy, *ea, *eb;
	Coalesce *coal;

	foreach_pset(spi->ents, cpy) {
		pset_insert_ptr(these, cpy);
		pset_insert_ptr(those, cpy);
	}

	foreach_pset(these, ea) {		/* (ena,enb) */
		foreach_pset(those, eb) {

			if(Ents_Coalesce(ea, eb, &spi) == 1)
			{
				/* markieren */


				/* alloc and set new information*/
				coal = obstack_alloc(&spi->ob, sizeof(*coal));
				coal->ent_1 = ea;
				coal->ent_2 = eb;

				/* sammeln */
				pset_insert_ptr(spi->coals, coal);
			}
		}
	}
}







ir_node *find_phi_ent() {
	return NULL;
}

ir_node *looptroop() {
	return NULL;
}









int delta_configuration_locality(entity *ent, cacheline *cli_env, void *env) {

	spilloc_env_t *spi = env;
	cacheline *cli;
	Node *node;

	cli->ents = pset_new_ptr_default();

	// teste ent in allen cachelines ...
	pmap_foreach(spi->Cli_ents, cli) {

		/* ausser der gegebenen cacheline(ent)*/
		if(cli != cli_env) {
			node = pmap_get(spi->afgraph, ent);
			//node->vtx->ela = compute_Entity_Cache_Affinity(ent,rel,&spi);
			node->vtx->ela = compute_Entity_Cache_Affinity(ent,(cli->ents),&spi);
		}
	}

	return 0;
}



int is_pasteable(entity *ent, pset *co) {

	/* 	RETURNs 1, IF ent could be moved to CACHELine co
	*/

	int align;
	int size;
	int step,curr_pos,curr_end, etc_pos, etc_end;
	entity *etc;

	ir_type *stype = get_entity_type(ent);

	align = get_type_alignment_bytes(stype);
	size  = get_type_size_bytes(stype);

	// check all possible alignment-constrainted positions
	for(step=0; (step*align) >= LINESIZE_BYTES; step++) {
		curr_pos = (step*align);
		curr_end = curr_pos + size;

		// collision with prev and/or next neighbor
		foreach_pset(co, etc) {
		 	etc_pos = get_entity_offset_bytes(get_entity_type(etc));
			etc_end = (get_entity_offset_bytes(get_entity_type(etc)) + get_type_size_bytes(get_entity_type(etc)));

			if((etc_end < curr_pos) || (curr_end < etc_pos)) { /* (etc,ent) is OK  */ }
			else if((etc_pos < curr_pos) && (curr_pos < etc_end) && (etc_end < curr_end)) {return 0;}
			else if((curr_pos < etc_pos) && (etc_end < curr_end)) {return 0;}
			else if((etc_pos < curr_pos) && (curr_end < etc_end)) {return 0;}
			else if((curr_pos < etc_pos) && (etc_pos < curr_end) && (etc_end > curr_end)) {return 0;}
		}

		// overlapping to next LINE
		if(get_entity_offset_bytes(ent)+get_type_size_bytes(ent) > LINESIZE_BYTES) {return 0;}
	}
	return 1;
}










entity *min_configuration_locality(cacheline *cli_env, void *env) {

	spilloc_env_t *spi = env;

	cacheline *cli = cli_env;
	entity *ent,*ret;
	Node *node;
	float min = 9999;

	if(cli->ents == NULL) return NULL;

	foreach_pset(cli->ents, ent) {
		node = pmap_get(spi->afgraph, ent);
		if(node->vtx->ela < min) {min = (float)node->vtx->ela; ret = node->vtx->ent;}
	}

	return ret;
}

void bbcache_REORDER (void *env) {

	spilloc_env_t *spi = env;
	int  curr_ela, line_nr;
	Node *node;
	entity *ent,*cpy;
	pset *Cli_ents;
	pset *ws = pset_new_ptr_default();
	pset *copy;
	cacheline *cli, *mv2cli;

	do{	/* first, find 'some' (= 1 ?) bad for each CACHELINE */
		pmap_foreach(spi->Cli_ents, cli) {

			/* ents(cacheline) holen - jeweils kleinste insert(ws) und remove(cacheline(i) = cli(i)) */

			if(min_configuration_locality(&cli, &spi) != NULL) {

				ent = min_configuration_locality(&cli, &spi);
				pset_insert_ptr(ws, ent);								// insert(ws,ent)
				//pset_remove_ptr(Cli_ents,ent);							// cacheline_remove(i,ent)

			}
		}



		/* second, PASTE them AGAIN hopefully elsewhere*/
		copy=pset_new_ptr(pset_count(ws));
		foreach_pset(ws, ent) {pset_insert_ptr(copy,ent);}

		foreach_pset(copy,cpy){

			node = pmap_get(spi->afgraph,cpy);
			curr_ela = node->vtx->ela;
			mv2cli = NULL;
			line_nr = -1;

			pmap_foreach(spi->Cli_ents, cli) {									/* for each CACHELINE */
				if(delta_configuration_locality(cpy, &cli, &spi) > curr_ela){	    // condition 1
					if((is_pasteable(cpy,&cli) != 0)) {							    // condition 2

						mv2cli = cli;
						line_nr = cli->nr;

					}
				}
			}

			if((mv2cli != NULL) && (line_nr > 0)) {

				cli = pmap_get(spi->Cli_ents, line_nr);
				pset_insert_ptr(cli->ents,cpy);
				pset_remove_ptr(ws,cpy);

			}
		}

		del_pset(copy);

	} while(pset_count(ws) > 0);
}

entity *MAX_affinity_ents(pset *ws, pset *layout_set, spilloc_env_t *env) {

	//spilloc_env_t *spi = env;
	//pset *layout = pset_new_ptr_default();
	entity *ent, *max;
	int x = 0;
	int y;

	//foreach_pset() {layout}

	foreach_pset(ws, ent) {

		y = compute_Entity_Cache_Affinity(ent, layout_set, env);

		if(x <= y) {
			max = ent;
			x = y;
		}
	}

	return max;

}

entity *max_configuration_locality(cacheline *cli_env, void *env) {

	spilloc_env_t *spi = env;
	cacheline *cli = cli_env;
	entity *ent;
	Node *node;
	float min;
	entity *ret;
/*
	min = 9999;
	if(cli->ents == NULL) return NULL;

	foreach_pset(cli->ents, ent) {
		node = pmap_get(spi->afgraph, ent);
		if(node->vtx->ela < min) {min = (float)node->vtx->ela; ret = node->vtx->ent;}
	}
*/
	return ret;
}

int check_offset(void *env) {

	spilloc_env_t *spi = env;
	pset *cli_ent_set;
	entity *ent;
	int m,n;

	cli_ent_set = pset_new_ptr_default();
	if(is_pasteable(ent, cli_ent_set)) return ;

	return -1;
}

int is_filled(pset *cli, pset *ws) {

	int re = 1;
	entity *ent;

	foreach_pset(ws, ent) {
		if (is_pasteable(ent, cli)) re = 0;
	}

	return re;
}

int bbcache_CHILIMBI (void *env) {

	spilloc_env_t *spi = env;

	entity *ent;
	cacheline *cline;
	pset *ws;
	edge *edg, *max_edge;
	entity *max, *last_max;

	int o,p,q, x,y,z, i,j,k;



		/* get workset: ws == (spi->ents) */
	o = pset_count(spi->ents);
	ws = pset_new_ptr(o);
	foreach_pset(spi->ents,ent) {pset_insert_ptr(ws,ent);}


	do {

		cline = create_Cli_ent(&spi->ob);

		cline->start = 0;
		cline->end   = 0;
		cline->nr    = 0;

		p = 0;

		/*
		   (1) start BY: adding the pair of ents
		       connected by
			   the maxinmum affinity edge
		*/


			/* MAX affinity-edge */
		foreach_pset(spi->afgraph_es, edg)
		{

			if((edg->aff > p) && (pset_find_ptr(ws, edg->src)) && (pset_find_ptr(ws, edg->tgt)))
			{
				p = edg->aff;
				max_edge = edg;
			}
		}

		o = pset_count(spi->afgraph_es);
		if(o == 0) return -1;

		if(is_pasteable(max_edge->src, cline->ents)) {
			pset_insert_ptr(cline->ents, max_edge->src);
			pset_remove_ptr(ws, max_edge->src);
		}

		if(is_pasteable(max_edge->tgt, cline->ents)) {
			pset_insert_ptr(cline->ents, max_edge->tgt);
			pset_remove_ptr(ws, max_edge->tgt);
		}


		/* (2) A single entity
			   is appended to the existing layout,
			   the one
			   that increases configuration locality
			   by the largest amount
		*/
		o = pset_count(ws);
		if(o == 0) return -2;

		o = pset_count(cline->ents);
		if(o != 0)
			max = MAX_affinity_ents(ws, (cline->ents), spi);


		if((o != 0) && (max != NULL) ) { // && (max->value != NULL)

			do {

				last_max = max;

					// paste to layout set
				if(is_pasteable(max, cline->ents)) {
					pset_insert_ptr(cline->ents, max);
					pset_remove_ptr(ws, max);
					last_max = NULL;
				}

					// get the best-fit'in entity
				o = pset_count(cline->ents);
				p = pset_count(ws);
				if(o != 0 && p != 0)
					max = MAX_affinity_ents(ws, (cline->ents), spi);

			} while( ((max != last_max) || (last_max != NULL) || (max == NULL)) && (p != 0));

		}

			/* insert one "filled" cacheline */
		pset_insert_ptr(spi->chilimbi_Cli_ents, cline);

	} while(ws != NULL && pset_count(ws) > 0);

	del_pset(ws);
	return 0;
}

void frame_information (spilloc_env_t spi) {

	int frame_align;
	ir_type *frame;

	frame = get_irg_frame_type(spi.irg);
	frame_align = get_type_alignment_bytes(frame);

}

void be_spill_loc(const be_chordal_env_t *chordal_env) {

	spilloc_env_t spi;
	pset *copy;
	entity *ent,*cpy;
	int max_off, i, start, end;
	Node *node;
	ent_position *pos;

		/* Init */
	obstack_init(&spi.ob);

	spi.irg	  = chordal_env->irg;
	spi.arch	  = chordal_env->birg->main_env->arch_env;

	spi.ents	    = pset_new_ptr_default();
	spi.nodes	    = pset_new_ptr_default();
	spi.ent_nodes	= pmap_create();

	spi.ent_cnt     = 0;
	spi.node_cnt    = 0;
	spi.blk_cnt     = 0;
	spi.cli_cnt	    = 0;

	spi.intval_cnt  = 0;

	spi.afgraph	    = pmap_create();
	spi.afgraph_es  = pset_new_ptr_default();
	spi.afgraph_vs  = pset_new_ptr_default();

	spi.Cli_ents    = pmap_create();
	spi.position    = pmap_create();
	spi.coals       = pset_new_ptr_default();

	spi.chilimbi_Cli_ents = pset_new_ptr_default();



		/* irg -> {ent}  */
	walk_types_entities(get_irg_frame_type(chordal_env->irg), _ents, &spi);
	if(spi.ent_cnt != pset_count(spi.ents))
		spi.ent_cnt = pset_count(spi.ents);



		/* ent -> {irn} */
	irg_walk_blkwise_graph(chordal_env->irg, NULL, _ent_nodes, &spi);
	if(spi.node_cnt != pset_count(spi.nodes))
		spi.ent_cnt = pset_count(spi.ents);


		/* ent -> Node */
	create_Vertex(&spi);



		/* (ent,ent) -> edge */
	copy = pset_new_ptr(spi.ent_cnt);
	foreach_pset(spi.ents, ent)	{pset_insert_ptr(copy,ent);}
	foreach_pset(copy, cpy) {
		foreach_pset(spi.ents, ent) {
			if(ent != cpy)
				create_Edge(ent,cpy,&spi);
		}
	}
	del_pset(copy);



	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/

										/* use the dumb offset */

	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/


		/* {ent} -> max_off */
	max_off = 0;
	foreach_pset(spi.ents, ent) {
		if(get_entity_offset_bytes(ent) > max_off) {max_off = get_entity_offset_bytes(ent);}
	}



			/* max_off -> max_cli */
	i=1;
	while((i * LINESIZE_BYTES) < max_off){
		i++;
	}
	spi.cli_cnt = i;




		/* {ent} -> {({ent},int), ({ent},int), ({ent},int),...,({ent},int)} */
	for(i = 0; i < spi.cli_cnt; i++) {
		start = (i * LINESIZE_BYTES);
		end   = start + LINESIZE_BYTES;
		Cli_ents(start, end, (i + 1), &spi);
	}



		/* (ent,{ent}) -> int */

	for(i = 1; i <= spi.cli_cnt; i++) {

		cacheline *cline;

		cline = pmap_get(spi.Cli_ents, i);
		if(!cline) break;

		foreach_pset(spi.ents, ent) {
			node = pmap_get(spi.afgraph, ent);
			//node->vtx->ela = compute_Entity_Cache_Affinity(ent,rel,&spi);
			node->vtx->ela = compute_Entity_Cache_Affinity(ent,(cline->ents),&spi);
		}
	}


	/* ======================================================================================================*/


			/* {ent} -> {ent_position} (INIT) */

	foreach_pset(spi.ents, ent) {

		ent_position *epos;

		epos = obstack_alloc(&spi.ob, sizeof(*epos));
		epos->Cli			= NULL;
		epos->is_set		= 0;
		epos->offset		= 0;
		epos->nr			= 0;
		epos->ent			= ent;
		pmap_insert(spi.position,ent,epos);
	}

			/* {ent_position->offset} -> {ent_position} (SET) */

	foreach_pset(spi.ents, ent) {
		pos = pmap_get(spi.position, ent);
		pos->offset = get_entity_offset_bytes(pos->ent);
	}





	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/

										/* use the initial offset */

	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/
	/* ======================================================================================================*/








			/* PRINT INFO's */
	/*
	ir_printf_ent_nodes(&spi);		// <ent->nr>: ir_node->node_nr, ir_node->node_nr ....
	ir_printf_ent_nodes_aff(&spi);	// <ent->nr,ent->nr>: affinity
	*/

		// CHILIMBI => BBCache - algorithm

	bbcache_CHILIMBI(&spi);


			/* PRINT INFO's */

	ir_printf_chilimbi_cachelines(spi);


		// HIGH affinity  &&  NO values_interfere

	//Check_Coalesce(&spi);









			/* CLEAN */

	del_pset(spi.ents);
	del_pset(spi.nodes);
	pmap_destroy(spi.ent_nodes);

	pmap_destroy(spi.afgraph);
	del_pset(spi.afgraph_es);
	del_pset(spi.afgraph_vs);

	pmap_destroy(spi.Cli_ents);
	pmap_destroy(spi.position);

	del_pset(spi.coals);

	del_pset(spi.chilimbi_Cli_ents);

	obstack_free(&spi.ob, NULL);

}
