/*
 * pattern history
 */
#include <assert.h>
#include <stdlib.h>
#include <limits.h>

#include "ident.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irprog.h"
#include "set.h"
#include "pset.h"
#include "counter.h"
#include "pattern_dmp.h"

/*
 * just be make some things clear :-), the
 * poor man "generics"
 */
#define HASH_MAP(type) pset_##type

typedef pset pset_pattern_entry_t;

typedef unsigned char BYTE;


/**
 * The code buffer
 */
typedef struct _code_buf_t {
  BYTE		*next;		/**< next byte to be written */
  BYTE		*end;		/**< end of the buffer */
  BYTE		*start;		/**< start of the buffer */
  unsigned	hash;		/**< calculates the hash value */
} CODE_BUFFER;

/**
 * VLC codes
 */
enum vlc_code_t {
  VLC_7BIT       = 0x00,	/**< 8 bit code, carrying 7 bits payload */
  VLC_14BIT      = 0x80,	/**< 16 bit code, carrying 14 bits payload */
  VLC_21BIT      = 0xC0,	/**< 24 bit code, carrying 21 bits payload */
  VLC_28BIT      = 0xE0,	/**< 32 bit code, carrying 28 bits payload */
  VLC_32BIT      = 0xF0,	/**< 40 bit code, carrying 32 bits payload */

  VLC_TAG_FIRST  = 0xF1, 	/**< first possible tag value */
  VLC_TAG_ICONST = 0xFB,	/**< encodes an integer constant */
  VLC_TAG_EMPTY  = 0xFC,	/**< encodes an empty entity */
  VLC_TAG_OPTION = 0xFD, 	/**< options exists */
  VLC_TAG_REF    = 0xFE,	/**< special tag, next code is an ID */
  VLC_TAG_END    = 0xFF,	/**< end tag */
};

/*
 * An entry for patterns
 */
typedef struct _pattern_entry_t {
  counter_t   count;		/**< amount of pattern occurance */
  unsigned    len;		/**< lenght of the VLC encoded buffer */
  BYTE        buf[1];		/**< buffer contains the VLC encoded pattern */
} pattern_entry_t;

/**
 * current options
 */
enum options_t {
  OPT_WITH_MODE   = 0x00000001,	/**< use modes */
  OPT_ENC_GRAPH   = 0x00000002,	/**< encode graphs, not terms */
  OPT_WITH_ICONST = 0x00000004, /**< encode integer constants */
};


/**
 * pattern info
 */
typedef struct _pattern_info_t {
  int                       enable;		/**< if non-zero, this module is enabled */
  struct obstack            obst;		/**< obstack containing the counters */
  HASH_MAP(pattern_entry_t) *pattern_hash;	/**< hash map containing the counter for pattern */
  unsigned                  bound;		/**< lowest value for output */
  unsigned                  options;		/**< option mask */
} pattern_info_t;

/*
 * global status
 */
static pattern_info_t _status, *status = &_status;

/**
 * compare two elemnts for counter
 */
static int pattern_count_cmp(const void *elt, const void *key)
{
  int cmp;

  pattern_entry_t **e1 = (pattern_entry_t **)elt;
  pattern_entry_t **e2 = (pattern_entry_t **)key;

  cmp = cnt_cmp(&(*e1)->count, &(*e2)->count);

  /* we want it sorted in descending order */
  return cmp * - 1;
}

/**
 * compare two elements of the pattern hash
 */
static int pattern_cmp(const void *elt, const void *key)
{
  const pattern_entry_t *e1 = elt;
  const pattern_entry_t *e2 = key;
  int diff = e1->len - e2->len;

  if (diff)
    return diff;

  return memcmp(e1->buf, e2->buf, e1->len);
}

/**
 * initialise a code buffer
 */
static void init_buf(CODE_BUFFER *buf, BYTE *data, unsigned len)
{
  buf->start = buf->next = data;
  buf->end   = data + len;
  buf->hash  = 0x2BAD4;
}

/**
 * put a byte into the buffer
 */
static INLINE void put_byte(CODE_BUFFER *buf, BYTE byte)
{
  if (buf->next < buf->end) {
    unsigned hash = buf->hash;

    hash = (hash * 9) ^ byte;
    *buf->next++ = byte;
    buf->hash    = hash;
  }
}

/**
 * returns the current lenght of a buffer
 */
static unsigned buf_lenght(const CODE_BUFFER *buf)
{
  return buf->next - buf->start;
}

/**
 * returns the current lenght of a buffer
 */
static const BYTE *buf_content(const CODE_BUFFER *buf)
{
  return buf->start;
}

/**
 * returns the hash value of a buffer
 */
static unsigned buf_hash(const CODE_BUFFER *buf)
{
  return buf->hash;
}

/**
 * returns the next byte from the buffer WITHOUT dropping
 */
static INLINE BYTE look_byte(CODE_BUFFER *buf)
{
  if (buf->next < buf->end)
    return *buf->next;
  return VLC_TAG_END;
}

/**
 * returns the next byte from the buffer WITH dropping
 */
static INLINE BYTE get_byte(CODE_BUFFER *buf)
{
  if (buf->next < buf->end)
    return *buf->next++;
  return VLC_TAG_END;
}

#define BITS(n)		(1 << (n))

/**
 * put a 32bit value into the buffer
 */
static void put_code(CODE_BUFFER *buf, unsigned code)
{
  if (code < BITS(7)) {
    put_byte(buf, VLC_7BIT | code);
  }
  else if (code < BITS(6 + 8)) {
    put_byte(buf, VLC_14BIT | (code >> 8));
    put_byte(buf, code);
  }
  else if (code < BITS(5 + 8 + 8)) {
    put_byte(buf, VLC_21BIT | (code >> 16));
    put_byte(buf, code >> 8);
    put_byte(buf, code);
  }
  else if (code < BITS(4 + 8 + 8 + 8)) {
    put_byte(buf, VLC_28BIT | (code >> 24));
    put_byte(buf, code >> 16);
    put_byte(buf, code >> 8);
    put_byte(buf, code);
  }
  else {
    put_byte(buf, VLC_32BIT);
    put_byte(buf, code >> 24);
    put_byte(buf, code >> 16);
    put_byte(buf, code >> 8);
    put_byte(buf, code);
  }
}

#define BIT_MASK(n)	((1 << (n)) - 1)

/**
 * get 32 bit from the buffer
 */
static unsigned get_code(CODE_BUFFER *buf)
{
  unsigned code = get_byte(buf);

  if (code < VLC_14BIT)
    return code;
  if (code < VLC_21BIT)
    return ((code & BIT_MASK(6)) << 8) | get_byte(buf);
  if (code < VLC_28BIT) {
    code  = ((code & BIT_MASK(5)) << 16) | (get_byte(buf) << 8);
    code |= get_byte(buf);
    return code;
  }
  if (code < VLC_32BIT) {
    code  = ((code & BIT_MASK(4)) << 24) | (get_byte(buf) << 16);
    code |= get_byte(buf) <<  8;
    code |= get_byte(buf);
    return code;
  }
  if (code == VLC_32BIT) {
    code  = get_byte(buf) << 24;
    code |= get_byte(buf) << 16;
    code |= get_byte(buf) <<  8;
    code |= get_byte(buf);
    return code;
  }
  /* should not happen */
  assert(0 && "Wrong code in buffer");

  return 0;
}

/**
 * put a tag into the buffer
 */
static void put_tag(CODE_BUFFER *buf, BYTE tag)
{
  assert(tag >= VLC_TAG_FIRST && "invalid tag");

  put_byte(buf, tag);
}

/**
 * returns the next tag or zero if the next code isn't a tag
 */
static BYTE next_tag(CODE_BUFFER *buf)
{
  BYTE b = look_byte(buf);

  if (b >= VLC_TAG_FIRST)
    return get_byte(buf);
  return 0;
}

/**
 * environment for the pattern encoder
 */
typedef struct _codec_enc_t {
  CODE_BUFFER      *buf;		/**< the code buffer */
  set              *id_set;		/**< the set containing all already seen nodes */
  unsigned         curr_id;		/**< current node id */
  unsigned         options;		/**< encoding options */
  pattern_dumper_t *dmp;		/**< dumper for the decoder */
} codec_env_t;

typedef struct _addr_entry_t {
  void *addr;		/**< the address */
  unsigned id;		/**< associated ID */
} addr_entry_t;

/**
 * hash value of an address
 */
static INLINE unsigned addr_hash(void *addr)
{
  return ((unsigned)addr) >> 3;
}

/**
 * compare two addresses
 */
static int addr_cmp(const void *p1, const void *p2, size_t size) {
  const addr_entry_t *e1 = p1;
  const addr_entry_t *e2 = p2;

  return e1->addr != e2->addr;
}

/**
 * encodes an IR-node, recursive worker
 *
 * @return reached depth
 */
static int _encode_node(ir_node *node, int max_depth, codec_env_t *env)
{
  addr_entry_t entry, *r_entry;
  set_entry *s_entry;
  int i, preds;
  int res, depth;

  opcode code = get_irn_opcode(node);

  /* insert the node into our ID map */
  entry.addr = node;
  entry.id   = env->curr_id;

  s_entry = set_hinsert(env->id_set, &entry, sizeof(entry), addr_hash(node));
  r_entry = (addr_entry_t *)s_entry->dptr;

  if (r_entry->id != env->curr_id) {
    /* already in the map, add an REF */
    put_tag(env->buf, VLC_TAG_REF);
    put_code(env->buf, r_entry->id);

    return max_depth;
  }
  else {
    /* a new entry, proceed */
    ++env->curr_id;
  }

  put_code(env->buf, (unsigned)code);

  /* do we need the mode ? */
  if (env->options & OPT_WITH_MODE) {
    ir_mode *mode = get_irn_mode(node);

    if (mode)
      put_code(env->buf, (unsigned)mode);
    else
      put_tag(env->buf, VLC_TAG_EMPTY);
  }

  /* do we need integer constants */
  if (env->options & OPT_WITH_ICONST) {
    if (code == iro_Const) {
      tarval *tv = get_Const_tarval(node);

      if (tarval_is_long(tv)) {
	long v = tarval_to_long(tv);

	put_tag(env->buf, VLC_TAG_ICONST);
	put_code(env->buf, v);
      }
    }
  }

  --max_depth;

  if (max_depth <= 0) {
    put_code(env->buf, 0);
    return max_depth;
  }

  preds = get_irn_arity(node);
  put_code(env->buf, preds);

  res = INT_MAX;
  for (i = 0; i < preds; ++i) {
    ir_node *n = get_irn_n(node, i);

    depth = _encode_node(n, max_depth, env);
    if (depth < res)
      res = depth;
  }
  return res;
}

/**
 * encode an IR-node (and its children)
 *
 * @param @node      The root node of the graph
 * @param buf        The code buffer to store the bitstring in
 * @param max_depth  The maximum depth for descending
 *
 * @return The depth of the encoded graph (without cycles)
 */
static int encode_node(ir_node *node, CODE_BUFFER *buf, int max_depth)
{
  codec_env_t env;
  int         res;

  env.buf     = buf;
  env.curr_id = 1;	/* 0 is used for special purpose */
  env.options = status->options;
  env.dmp     = NULL;

  if (env.options & OPT_ENC_GRAPH)
    env.id_set = new_set(addr_cmp, 32);
  else
    env.id_set = NULL;

  /* encode options if any */
  if (env.options) {
    put_tag(buf, VLC_TAG_OPTION);
    put_code(buf, env.options);
  }

  res = _encode_node(node, max_depth, &env);

  if (env.options & OPT_ENC_GRAPH)
    del_set(env.id_set);

  return max_depth - res;
}

/**
 * decode an IR-node, recursive walker
 */
static void _decode_node(unsigned parent, int position, codec_env_t *env)
{
  unsigned code;
  unsigned op_code;
  unsigned mode_code = 0;
  long iconst;
  void *attr = NULL;

  code = next_tag(env->buf);
  if (code == VLC_TAG_REF) { /* it's a REF */
    code = get_code(env->buf);

    /* dump the edge */
    if (parent) {
      int edge_mode = 0;
      /*
       * the mode of a Firm edge can be either computed from its target or
       * from its source and position. We must take the second approach because
       * we dont know the target here, it's a ref.
       */
      pattern_dump_edge(env->dmp, code, parent, position, edge_mode);
    }

    /* dump the node ref */
    pattern_dump_ref(env->dmp, code);

    return;
  }

  /* get the opcode */
  op_code = get_code(env->buf);

  /* get the mode if encoded */
  if (env->options & OPT_WITH_MODE) {
    if (next_tag(env->buf) != VLC_TAG_EMPTY) {
      mode_code = get_code(env->buf);
    }
  }

  /* check, if a ICONST attribute is given */
  if (next_tag(env->buf) == VLC_TAG_ICONST) {
    iconst = get_code(env->buf);
    attr   = &iconst;
  }

  /* dump the edge */
  if (parent) {
    int edge_mode = 0;

    /*
     * the mode of a Firm edge can be either computed from its target or
     * from its source and position. We take the second approach because
     * we need it anyway for ref's.
     */
    pattern_dump_edge(env->dmp, env->curr_id, parent, position, edge_mode);
  }

  /* dump the node */
  parent = env->curr_id;
  pattern_dump_node(env->dmp, parent, op_code, mode_code, attr);

  /* ok, we have a new ID */
  ++env->curr_id;

  code = next_tag(env->buf);
  if (code != VLC_TAG_END) {
    /* more info, do recursion */
    int i, preds;

    preds = get_code(env->buf);
    if (preds > 0) {
      pattern_start_children(env->dmp, parent);
      for (i = 0; i < preds; ++i) {
	_decode_node(parent, i, env);
      }
      pattern_finish_children(env->dmp, parent);
    }
  }
}

/**
 * decode an IR-node
 */
static void decode_node(BYTE *b, unsigned len, pattern_dumper_t *dump)
{
  codec_env_t env;
  CODE_BUFFER buf;
  unsigned code, options = 0;

  init_buf(&buf, b, len);

  env.buf     = &buf;
  env.curr_id = 1;	/* 0 is used for special purpose */
  env.dmp     = dump;

  /* decode options */
  code = next_tag(&buf);
  if (code == VLC_TAG_OPTION) {
    options = get_code(&buf);
  }
  env.options = options;

  _decode_node(0, 0, &env);
}

/**
 * the environment for the pattern calculation
 */
typedef struct _pattern_env {
  int max_depth;		/**< maximum depth for pattern generation */
} pattern_env_t;

/**
 * Returns the associates pattern_entry_t for a CODE_BUF
 */
static pattern_entry_t *pattern_get_entry(CODE_BUFFER *buf, pset *set)
{
  pattern_entry_t *key, *elem;
  unsigned len = buf_lenght(buf);
  unsigned hash;

  key = obstack_alloc(&status->obst, sizeof(*key) + len - 1);
  assert(key);

  key->len = len;
  memcpy(key->buf, buf_content(buf), len);

  hash = buf_hash(buf);

  elem = pset_find(set, key, hash);
  if (elem) {
    obstack_free(&status->obst, key);
    return elem;
  }

  cnt_clr(&key->count);
  assert(key != (void *)4);
  return pset_insert(set, key, hash);
}

/**
 * walker for nodes pattern calculation
 */
static void calc_nodes_pattern(ir_node *node, void *ctx)
{
  BYTE            buffer[1024];
  pattern_env_t   *env = ctx;
  CODE_BUFFER     buf;
  pattern_entry_t *entry;
  int             depth;

  init_buf(&buf, buffer, sizeof(buffer));
  depth = encode_node(node, &buf, env->max_depth);

  /* ignore single node pattern (i.e. constants) */
  if (depth > 1) {
    entry = pattern_get_entry(&buf, status->pattern_hash);

    /* increase count */
    cnt_inc(&entry->count);
  }
}

/**
 * output the collected pattern
 */
static void pattern_output(void)
{
  pattern_entry_t  *entry;
  pattern_entry_t  **pattern_arr;
  pattern_dumper_t *dump;
  int i, count = pset_count(status->pattern_hash);

  printf("\n%d pattern detected\n", count);

  if (count <= 0)
    return;

  /* creates a dumper */
  dump = new_vcg_dumper("pattern.vcg", 100);

  pattern_arr = xmalloc(sizeof(*pattern_arr) * count);
  for (i = 0, entry = pset_first(status->pattern_hash);
       entry && i < count;
       entry = pset_next(status->pattern_hash), ++i) {
    pattern_arr[i] =  entry;
  }
  assert(count == i);
  count = i;

  /* sort it */
  qsort(pattern_arr, count, sizeof(*pattern_arr), pattern_count_cmp);

  for (i = 0; i < count; ++i) {
    entry = pattern_arr[i];
    if (entry->count.cnt[0] < status->bound)
      continue;

    /* dump a pattern */
    pattern_dump_new_pattern(dump, &entry->count);
    decode_node(entry->buf, entry->len, dump);
    pattern_dump_finish_pattern(dump);
  }

  /* destroy it */
  pattern_end(dump);
}

/*
 * calculates the pattern history
 */
void stat_calc_pattern_history(ir_graph *irg)
{
  pattern_env_t env;

  if (! status->enable)
    return;

  /* do NOT count the const code IRG */
  if (irg == get_const_code_irg())
    return;

  env.max_depth = 5;
  irg_walk_graph(irg, calc_nodes_pattern, NULL, &env);
}

/*
 * initialises the pattern history
 */
void stat_init_pattern_history(int enable)
{
  status->enable = enable;
  if (! enable)
    return;

  status->bound   = 10;
  status->options = OPT_WITH_MODE | OPT_ENC_GRAPH | OPT_WITH_ICONST;

  obstack_init(&status->obst);

  /* create the hash-table */
  status->pattern_hash = new_pset(pattern_cmp, 8);
}

/*
 * finishes the pattern history
 */
void stat_finish_pattern_history(void)
{
  if (! status->enable)
    return;

  pattern_output();

  del_pset(status->pattern_hash);
  obstack_free(&status->obst, NULL);

  status->enable = 0;
}
