/*
 * pattern history
 */
#include <assert.h>

#include "irnode_t.h"

typedef unsigned char BYTE;

/**
 * The code buffer
 */
typedef struct _code_buf_t {
  BYTE		*next;		/**< next byte to be written */
  int           len;		/**< length of current string */
} CODE_BUFFER;

enum vlc_code_t {
  VLC_7BIT      = 0x00,	/**< 8 bit code, carrying 7 bits payload */
  VLC_14BIT     = 0x80,	/**< 16 bit code, carrying 14 bits payload */
  VLC_21BIT     = 0xC0,	/**< 24 bit code, carrying 21 bits payload */
  VLC_28BIT     = 0xE0,	/**< 32 bit code, carrying 28 bits payload */
  VLC_32BIT     = 0xF0,	/**< 40 bit code, carrying 32 bits payload */

  VLC_TAG_FIRST = 0xF1, /**< first possible tag value */
  VLC_TAG_REF   = 0xFE,	/**< special tag, next code is an ID */
  VLC_TAG_END   = 0xFF,	/**< end tag */
};

/**
 * put a byte into the buffer
 */
static INLINE void put_byte(CODE_BUFFER *buf, BYTE byte)
{
  *buf->next++ = byte;
  ++buf->len;
}

/**
 * returns the next byte from the buffer WITHOUT dropping
 */
static INLINE BYTE look_byte(CODE_BUFFER *buf)
{
  return *buf->next;
}

/**
 * returns the next byte from the buffer WITH dropping
 */
static INLINE BYTE get_byte(CODE_BUFFER *buf)
{
  return *buf->next++;
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
    return b;
  return 0;
}

/*
 * encodes an IR-node, recursive worker
 */
static void _encode_node(ir_node *node, CODE_BUFFER *buf, int max_depth)
{
  opcode code;
  int i, preds;

  code = get_irn_opcode(node);
  put_code(buf, code);

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
  _encode_node(node, buf, max_depth);
  put_code(buf, VLC_TAG_END);
}
