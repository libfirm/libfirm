/*
 * pattern history
 */
#include <assert.h>
#include <stdlib.h>

#include "ident.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "pset.h"
#include "counter.h"
#include "irprog.h"

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
  OPT_WITH_MODE = 0x00000001,	/**< use modes */
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

/*
 * encodes an IR-node, recursive worker
 */
static void _encode_node(ir_node *node, CODE_BUFFER *buf, int max_depth)
{
  int i, preds;

#if 0
  opcode code = get_irn_opcode(node);
#else
  ir_op *code = get_irn_op(node);
#endif

  put_code(buf, (unsigned)code);

  if (status->options & OPT_WITH_MODE) {
    ir_mode *mode = get_irn_mode(node);

    if (mode)
      put_code(buf, (unsigned)mode);
    else
      put_tag(buf, VLC_TAG_EMPTY);
  }

  --max_depth;

  if (max_depth <= 0) {
    put_code(buf, 0);
    return;
  }

  preds = get_irn_arity(node);
  put_code(buf, preds);

  for (i = 0; i < preds; ++i) {
    ir_node *n = get_irn_n(node, i);

    _encode_node(n, buf, max_depth);
  }
}

/**
 * encode an IR-node
 */
static void encode_node(ir_node *node, CODE_BUFFER *buf, int max_depth)
{
  put_tag(buf, VLC_TAG_OPTION);
  put_code(buf, status->options);
  _encode_node(node, buf, max_depth);
}


/**
 * decode an IR-node, recursive walker
 */
static void _decode_node(CODE_BUFFER *buf, unsigned options)
{
  unsigned op_code = get_code(buf);
  unsigned code    = next_tag(buf);
  ir_op *op = (ir_op *)op_code;

  /* output the opcode-name */
  printf("%s", get_id_str(op->name));

  if (options & OPT_WITH_MODE) {
    if (next_tag(buf) != VLC_TAG_EMPTY) {
      unsigned mode_code = get_code(buf);
      ir_mode *mode = (ir_mode *)mode_code;
      printf("%s", get_mode_name(mode));
    }
  }

  /* enter it into the ID table */

  if (code != VLC_TAG_END) {
    /* more info, do recursion */
    int i, preds;

    preds = get_code(buf);
    if (preds > 0) {
      printf("(");
      for (i = 0; i < preds; ++i) {
	if (i > 0)
	  printf(", ");
	_decode_node(buf, options);
      }
      printf(")");
    }
  }
}

/**
 * decode an IR-node
 */
static void decode_node(BYTE *b, unsigned len)
{
  CODE_BUFFER buf;
  unsigned code, options = 0;

  init_buf(&buf, b, len);

  code = next_tag(&buf);
  if (code == VLC_TAG_OPTION) {
    options = get_code(&buf);
  }
  _decode_node(&buf, options);
}

/**
 * the environment for the pattern calculation
 */
typedef struct _pattern_env {
  int max_depth;		/**< maximum depth for pattern generation */
} pattern_env_t;

/**
 * calculate a hash value for a pattern
 */
static unsigned pattern_hash(pattern_entry_t *key)
{
  return 9 * key->buf[0] + 31 * key->buf[key->len - 1] + 7 * key->buf[key->len >> 1];
}

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

  hash = buf_hash(buf); // pattern_hash(key);

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

  init_buf(&buf, buffer, sizeof(buffer));
  encode_node(node, &buf, env->max_depth);

  entry = pattern_get_entry(&buf, status->pattern_hash);

  /* increase count */
  cnt_inc(&entry->count);
}

/**
 */
static void pattern_output(void)
{
  pattern_entry_t *entry;
  pattern_entry_t **pattern_arr;
  int i, count = pset_count(status->pattern_hash);

  printf("\n%d pattern detected\n", count);

  if (count <= 0)
    return;

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

    printf("%8d\t", entry->count.cnt[0]);
    decode_node(entry->buf, entry->len);
    printf("\n");
  }
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

  env.max_depth = 4;
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

  status->bound   = 3;
  status->options = OPT_WITH_MODE;

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
