/* A disjoint-set forest ADT.
   TODO: path compression / ranking
 */

typedef struct djset {
  unsigned nparts; /* # of partitions. */
  unsigned bound;  /* lower bound for # of partitions. */
  unsigned nelts; /* # of elements in flexible array member. */
  struct {
    unsigned color:2;
    unsigned in:15; /* Disjoint set forest. */
    unsigned head:15; /* List of heads for set enumeration. */
  } links[]; /* C99 flexible array member. */
} djset;

static inline djset *djs_alloc(unsigned nelts, struct obstack *obst)
{
  size_t links_size = nelts * sizeof(unsigned);
  djset *djs = obstack_alloc(obst, sizeof(djset)+links_size);
  djs->nelts = nelts;
  djs->nparts = nelts;
  memset(djs->links, 0, links_size);
  return djs;
}

static inline djset *djs_dup(djset *djs, struct obstack *obst)
{
  size_t size = djs->nelts * sizeof(unsigned) + sizeof(djset);
  djset *new_djs = obstack_alloc(obst, size);
  memcpy(new_djs, djs, size);
  return new_djs;
}

__attribute__((always_inline))
static inline djset *djs_dupa(djset *djs)
{
  size_t size = djs->nelts * sizeof(unsigned) + sizeof(djset);
  djset *new_djs = alloca(size);
  memcpy(new_djs, djs, size);
  return new_djs;
}

/* Return representative of set in which elt is contained in. */
static inline unsigned djs_find(djset *djs, unsigned elt)
{
  while(djs->links[elt].in)
    elt = djs->links[elt].in;
  return elt;
}

/* Unite two sets identified by their representative elements. */
static inline unsigned djs_union(djset *djs, unsigned to_rep, unsigned from_rep)
{
  assert(!djs->links[to_rep].in /* Representative. */);
  assert(!djs->links[from_rep].in /* Representative. */);

  djs->links[from_rep].in = to_rep;

  {
    /* Concatenate heads list. */
    unsigned head = to_rep;

    while (djs->links[head].head)
      head = djs->links[head].head;

    djs->links[head].head = djs->links[to_rep].head;

  }

  djs->links[from_rep].in = to_rep;

  return to_rep;
}

