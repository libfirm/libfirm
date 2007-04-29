/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Statistics for Firm. Pattern history.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

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
#include "hashptr.h"

/*
 * just be make some things clear :-), the
 * poor man "generics"
 */
#define HASH_MAP(type) pset_##type

typedef pset pset_pattern_entry_t;

typedef unsigned char BYTE;

/** Maximum size of the pattern store. */
#define PATTERN_STORE_SIZE	2048


/**
 * The code buffer.
 */
typedef struct _code_buf_t {
	BYTE     *next;    /**< Next byte address to be written. */
	BYTE     *end;     /**< End address of the buffer. */
	BYTE     *start;   /**< Start address of the buffer. */
	unsigned hash;     /**< The hash value for the buffer content. */
	unsigned overrun;  /**< flag set if the buffer was overrun */
} CODE_BUFFER;

/**
 * Reserved VLC codes.
 */
enum vlc_code_t {
	VLC_7BIT       = 0x00,  /**< 8 bit code, carrying 7 bits payload */
	VLC_14BIT      = 0x80,  /**< 16 bit code, carrying 14 bits payload */
	VLC_21BIT      = 0xC0,  /**< 24 bit code, carrying 21 bits payload */
	VLC_28BIT      = 0xE0,  /**< 32 bit code, carrying 28 bits payload */
	VLC_32BIT      = 0xF0,  /**< 40 bit code, carrying 32 bits payload */

	VLC_TAG_FIRST  = 0xF1,  /**< First possible tag value. */
	VLC_TAG_ICONST = 0xFB,  /**< Encodes an integer constant. */
	VLC_TAG_EMPTY  = 0xFC,  /**< Encodes an empty entity. */
	VLC_TAG_OPTION = 0xFD,  /**< Options exists. */
	VLC_TAG_REF    = 0xFE,  /**< Special tag, next code is an ID. */
	VLC_TAG_END    = 0xFF,  /**< End tag. */
};

/*
 * An entry for holding one pattern.
 */
typedef struct _pattern_entry_t {
	counter_t   count;        /**< Amount of pattern occurance. */
	unsigned    len;          /**< The length of the VLC encoded buffer. */
	BYTE        buf[1];       /**< The buffer containing the VLC encoded pattern. */
} pattern_entry_t;

/**
 * Current options for the pattern matcher.
 */
enum options_t {
	OPT_WITH_MODE       = 0x00000001, /**< use modes */
	OPT_ENC_DAG         = 0x00000002, /**< encode DAGs, not terms */
	OPT_WITH_ICONST     = 0x00000004, /**< encode integer constants */
	OPT_PERSIST_PATTERN = 0x00000008, /**< persistent pattern hash */
};


/**
 * Pattern info.
 */
typedef struct _pattern_info_t {
	int                       enable;         /**< If non-zero, this module is enabled. */
	struct obstack            obst;           /**< An obstack containing the counters. */
	HASH_MAP(pattern_entry_t) *pattern_hash;  /**< A hash map containing the pattern. */
	unsigned                  bound;          /**< Lowest value for pattern output. */
	unsigned                  options;        /**< Current option mask. */
	unsigned                  min_depth;      /**< Minimum pattern depth. */
	unsigned                  max_depth;      /**< Maximum pattern depth. */
} pattern_info_t;

/*
 * global status
 */
static pattern_info_t _status, *status = &_status;

/**
 * Compare two pattern for its occurance counter.
 */
static int pattern_count_cmp(const void *elt, const void *key) {
	int cmp;

	pattern_entry_t **e1 = (pattern_entry_t **)elt;
	pattern_entry_t **e2 = (pattern_entry_t **)key;

	/* we want it sorted in descending order */
	cmp = cnt_cmp(&(*e2)->count, &(*e1)->count);

	return cmp;
}  /* pattern_count_cmp */

/**
 * Compare two pattern for its pattern hash.
 */
static int pattern_cmp(const void *elt, const void *key) {
	const pattern_entry_t *e1 = elt;
	const pattern_entry_t *e2 = key;
	int diff = e1->len - e2->len;

	if (diff)
		return diff;

	return memcmp(e1->buf, e2->buf, e1->len);
}  /* pattern_cmp */

/**
 * Initialize a code buffer.
 *
 * @param buf   the code buffer
 * @param data  a buffer address
 * @param len   the length of the data buffer
 */
static void init_buf(CODE_BUFFER *buf, BYTE *data, unsigned len) {
	buf->start   =
	buf->next    = data;
	buf->end     = data + len;
	buf->hash    = 0x2BAD4;      /* An arbitrary seed. */
	buf->overrun = 0;
}  /* init_buf */

/**
 * Put a byte into the buffer.
 *
 * @param buf   the code buffer
 * @param byte  the byte to write
 *
 * The hash value for the buffer content is updated.
 */
static INLINE void put_byte(CODE_BUFFER *buf, BYTE byte) {
	if (buf->next < buf->end) {
		*buf->next++ = byte;
		buf->hash = (buf->hash * 9) ^ byte;
	} else {
		buf->overrun = 1;
	}  /* if */
}  /* put_byte */

/**
 * Returns the current length of a buffer.
 *
 * @param buf   the code buffer
 *
 * @return  the length of the buffer content
 */
static unsigned buf_lenght(const CODE_BUFFER *buf) {
	return buf->next - buf->start;
}  /* buf_lenght */

/**
 * Returns the current content of a buffer.
 *
 * @param buf   the code buffer
 *
 * @return  the start address of the buffer content
 */
static const BYTE *buf_content(const CODE_BUFFER *buf) {
	return buf->start;
}  /* buf_content */

/**
 * Returns the hash value of a buffer.
 *
 * @param buf   the code buffer
 *
 * @return  the hash value of the buffer content
 */
static unsigned buf_hash(const CODE_BUFFER *buf) {
	return buf->hash;
}  /* buf_hash */

/**
 * Returns non-zero if a buffer overrun has occurred.
 *
 * @param buf   the code buffer
 */
static unsigned buf_overrun(const CODE_BUFFER *buf) {
	return buf->overrun;
}  /* buf_overrun */

/**
 * Returns the next byte from the buffer WITHOUT dropping.
 *
 * @param buf   the code buffer
 *
 * @return  the next byte from the code buffer
 */
static INLINE BYTE look_byte(CODE_BUFFER *buf) {
	if (buf->next < buf->end)
		return *buf->next;
	return VLC_TAG_END;
}  /* look_byte */

/**
 * Returns the next byte from the buffer WITH dropping.
 *
 * @param buf   the code buffer
 *
 * @return  the next byte from the code buffer
 */
static INLINE BYTE get_byte(CODE_BUFFER *buf) {
	if (buf->next < buf->end)
		return *buf->next++;
	return VLC_TAG_END;
}  /* get_byte */

#define BITS(n)   (1 << (n))

/**
 * Put a 32bit value into the buffer.
 *
 * @param buf   the code buffer
 * @param code  the code to be written into the buffer
 */
static void put_code(CODE_BUFFER *buf, unsigned code) {
	if (code < BITS(7)) {
		put_byte(buf, VLC_7BIT | code);
	} else if (code < BITS(6 + 8)) {
		put_byte(buf, VLC_14BIT | (code >> 8));
		put_byte(buf, code);
	} else if (code < BITS(5 + 8 + 8)) {
		put_byte(buf, VLC_21BIT | (code >> 16));
		put_byte(buf, code >> 8);
		put_byte(buf, code);
	} else if (code < BITS(4 + 8 + 8 + 8)) {
		put_byte(buf, VLC_28BIT | (code >> 24));
		put_byte(buf, code >> 16);
		put_byte(buf, code >> 8);
		put_byte(buf, code);
	} else {
		put_byte(buf, VLC_32BIT);
		put_byte(buf, code >> 24);
		put_byte(buf, code >> 16);
		put_byte(buf, code >> 8);
		put_byte(buf, code);
	}  /* if */
}  /* put_code */

#define BIT_MASK(n) ((1 << (n)) - 1)

/**
 * Get 32 bit from the buffer.
 *
 * @param buf   the code buffer
 *
 * @return  next 32bit value from the code buffer
 */
static unsigned get_code(CODE_BUFFER *buf) {
	unsigned code = get_byte(buf);

	if (code < VLC_14BIT)
		return code;
	if (code < VLC_21BIT)
		return ((code & BIT_MASK(6)) << 8) | get_byte(buf);
	if (code < VLC_28BIT) {
		code  = ((code & BIT_MASK(5)) << 16) | (get_byte(buf) << 8);
		code |= get_byte(buf);
		return code;
	}  /* if */
	if (code < VLC_32BIT) {
		code  = ((code & BIT_MASK(4)) << 24) | (get_byte(buf) << 16);
		code |= get_byte(buf) <<  8;
		code |= get_byte(buf);
		return code;
	}  /* if */
	if (code == VLC_32BIT) {
		code  = get_byte(buf) << 24;
		code |= get_byte(buf) << 16;
		code |= get_byte(buf) <<  8;
		code |= get_byte(buf);
		return code;
	}  /* if */
	/* should not happen */
	assert(0 && "Wrong code in buffer");

	return 0;
}  /* get_code */

/**
 * Put a tag into the buffer.
 *
 * @param buf   the code buffer
 * @param tag   the tag to write to the code buffer
 */
static void put_tag(CODE_BUFFER *buf, BYTE tag) {
	assert(tag >= VLC_TAG_FIRST && "invalid tag");

	put_byte(buf, tag);
}  /* put_tag */

/**
 * Returns the next tag or zero if the next code isn't a tag.
 *
 * @param buf   the code buffer
 *
 * @return the next tag in the code buffer
 */
static BYTE next_tag(CODE_BUFFER *buf) {
	BYTE b = look_byte(buf);

	if (b >= VLC_TAG_FIRST)
		return get_byte(buf);
	return 0;
}  /* next_tag */

/**
 * An Environment for the pattern encoder.
 */
typedef struct _codec_enc_t {
	CODE_BUFFER      *buf;      /**< The current code buffer. */
	set              *id_set;   /**< A set containing all already seen Firm nodes. */
	unsigned         curr_id;   /**< The current node id. */
	unsigned         options;   /**< The encoding options. */
	pattern_dumper_t *dmp;      /**< The dumper for the decoder. */
} codec_env_t;

/**
 * An address entry.
 */
typedef struct _addr_entry_t {
	void *addr;     /**< the address */
	unsigned id;    /**< associated ID */
} addr_entry_t;

/**
 * Compare two addresses.
 */
static int addr_cmp(const void *p1, const void *p2, size_t size) {
	const addr_entry_t *e1 = p1;
	const addr_entry_t *e2 = p2;

	return e1->addr != e2->addr;
}  /* addr_cmp */

/**
 * Encodes an IR-node, recursive worker.
 *
 * @return reached depth
 */
static int _encode_node(ir_node *node, int max_depth, codec_env_t *env) {
	addr_entry_t entry, *r_entry;
	set_entry *s_entry;
	int i, preds;
	int res, depth;

	ir_opcode code = get_irn_opcode(node);

	/* insert the node into our ID map */
	entry.addr = node;
	entry.id   = env->curr_id;

	s_entry = set_hinsert(env->id_set, &entry, sizeof(entry), HASH_PTR(node));
	r_entry = (addr_entry_t *)s_entry->dptr;

	if (r_entry->id != env->curr_id) {
		/* already in the map, add an REF */
		put_tag(env->buf, VLC_TAG_REF);
		put_code(env->buf, r_entry->id);

		return max_depth;
	} else {
		/* a new entry, proceed */
		++env->curr_id;
	}  /* if */

	put_code(env->buf, (unsigned)code);

	/* do we need the mode ? */
	if (env->options & OPT_WITH_MODE) {
		ir_mode *mode = get_irn_mode(node);

		if (mode)
			/* FIXME: not 64bit save */
			put_code(env->buf, (unsigned)mode);
		else
			put_tag(env->buf, VLC_TAG_EMPTY);
	}  /* if */

	/* do we need integer constants */
	if (env->options & OPT_WITH_ICONST) {
		if (code == iro_Const) {
			tarval *tv = get_Const_tarval(node);

			if (tarval_is_long(tv)) {
				long v = get_tarval_long(tv);

				put_tag(env->buf, VLC_TAG_ICONST);
				put_code(env->buf, v);
			}  /* if */
		}  /* if */
	}  /* if */

	--max_depth;

	if (max_depth <= 0) {
		put_code(env->buf, 0);
		return max_depth;
	} /* if */

	preds = get_irn_arity(node);
	put_code(env->buf, preds);

	res = INT_MAX;
	if (is_op_commutative(get_irn_op(node))) {
		ir_node *l = get_binop_left(node);
		ir_node *r = get_binop_right(node);
		int opcode_diff = (int)get_irn_opcode(l) - (int)get_irn_opcode(r);

		if (opcode_diff > 0) {
			ir_node *t = l;
			l = r;
			r = t;
		} else if (opcode_diff == 0 && l != r) {
			/* Both nodes have the same opcode, but are different.
			   Need a better method here to decide which goes to the left side. */
		}  /* if */

		/* special handling for commutative operators */
		depth = _encode_node(l, max_depth, env);
		if (depth < res)
			res = depth;
		depth = _encode_node(r, max_depth, env);
		if (depth < res)
			res = depth;
	} else {
		for (i = 0; i < preds; ++i) {
			ir_node *n = get_irn_n(node, i);

			depth = _encode_node(n, max_depth, env);
			if (depth < res)
				res = depth;
		}  /* for */
	}  /* if */
	return res;
}  /* _encode_node */

/**
 * Encode a DAG starting by the IR-node node.
 *
 * @param node       The root node of the graph
 * @param buf        The code buffer to store the bitstring in
 * @param max_depth  The maximum depth for descending
 *
 * @return The depth of the encoded graph (without cycles)
 */
static int encode_node(ir_node *node, CODE_BUFFER *buf, int max_depth) {
	codec_env_t env;
	int         res;

	/* initialize the encoder environment */
	env.buf     = buf;
	env.curr_id = 1;  /* 0 is used for special purpose */
	env.options = status->options;
	env.dmp     = NULL;

	if (env.options & OPT_ENC_DAG)
		env.id_set = new_set(addr_cmp, 32);
	else
		env.id_set = NULL;

	/* encode options if any for the decoder */
	if (env.options) {
		put_tag(buf, VLC_TAG_OPTION);
		put_code(buf, env.options);
	}  /* if */

	res = _encode_node(node, max_depth, &env);

	if (env.id_set != NULL)
		del_set(env.id_set);

	return max_depth - res;
}  /* encode_node */

/**
 * Decode an IR-node, recursive walker.
 */
static void _decode_node(unsigned parent, int position, codec_env_t *env) {
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
			 * we don't know the target here, it's a ref.
			 */
			pattern_dump_edge(env->dmp, code, parent, position, edge_mode);
		}  /* if */

		/* dump the node ref */
		pattern_dump_ref(env->dmp, code);

		return;
	}  /* if */

	/* get the opcode */
	op_code = get_code(env->buf);

	/* get the mode if encoded */
	if (env->options & OPT_WITH_MODE) {
		if (next_tag(env->buf) != VLC_TAG_EMPTY) {
			mode_code = get_code(env->buf);
		}  /* if */
	}  /* if */

	/* check, if a ICONST attribute is given */
	if (next_tag(env->buf) == VLC_TAG_ICONST) {
		iconst = get_code(env->buf);
		attr   = &iconst;
	}  /* if */

	/* dump the edge */
	if (parent) {
		int edge_mode = 0;

		/*
		 * the mode of a Firm edge can be either computed from its target or
		 * from its source and position. We take the second approach because
		 * we need it anyway for ref's.
		 */
		pattern_dump_edge(env->dmp, env->curr_id, parent, position, edge_mode);
	}  /* if */

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
			}  /* for */
			pattern_finish_children(env->dmp, parent);
		}  /* if */
	}  /* if */
}  /* _decode_node */

/**
 * Decode an IR-node.
 */
static void decode_node(BYTE *b, unsigned len, pattern_dumper_t *dump) {
	codec_env_t env;
	CODE_BUFFER buf;
	unsigned code, options = 0;

	init_buf(&buf, b, len);

	env.buf     = &buf;
	env.curr_id = 1;  /* 0 is used for special purpose */
	env.dmp     = dump;

	/* decode options */
	code = next_tag(&buf);
	if (code == VLC_TAG_OPTION) {
		options = get_code(&buf);
	}  /* if */
	env.options = options;

	_decode_node(0, 0, &env);
}  /* decode_node */

/**
 * The environment for the pattern calculation.
 */
typedef struct _pattern_env {
	int max_depth;    /**< maximum depth for pattern generation. */
} pattern_env_t;

/**
 * Returns the associates pattern_entry_t for a CODE_BUF.
 *
 * @param buf  the code buffer
 * @param set  the hash table containing all pattern entries
 *
 * @return   the associated pattern_entry_t for the given code buffer
 *
 * If the code content was never seen before, a new pattern_entry is created
 * and returned.
 */
static pattern_entry_t *pattern_get_entry(CODE_BUFFER *buf, pset *set) {
	pattern_entry_t *key, *elem;
	unsigned len = buf_lenght(buf);
	unsigned hash;

	key = obstack_alloc(&status->obst, offsetof(pattern_entry_t, buf) + len);
	assert(key);

	key->len = len;
	memcpy(key->buf, buf_content(buf), len);

	hash = buf_hash(buf);

	elem = pset_find(set, key, hash);
	if (elem != NULL) {
		obstack_free(&status->obst, key);
		return elem;
	}  /* if */

	cnt_clr(&key->count);
	return pset_insert(set, key, hash);
}  /* pattern_get_entry */

/**
 * Increase the count for a pattern.
 *
 * @param buf    the code buffer containing the pattern
 * @param depth  the pattern depth
 *
 * @note Single node patterns are ignored
 */
static void count_pattern(CODE_BUFFER *buf, int depth) {
	pattern_entry_t *entry;

	/* ignore single node pattern (i.e. constants) */
	if (depth > 1) {
		entry = pattern_get_entry(buf, status->pattern_hash);

		/* increase count */
		cnt_inc(&entry->count);
	}  /* if */
}  /* count_pattern */

/**
 * Pre-walker for nodes pattern calculation.
 */
static void calc_nodes_pattern(ir_node *node, void *ctx) {
	pattern_env_t   *env = ctx;
	BYTE            buffer[PATTERN_STORE_SIZE];
	CODE_BUFFER     buf;
	int             depth;

	init_buf(&buf, buffer, sizeof(buffer));
	depth = encode_node(node, &buf, env->max_depth);

	if (buf_overrun(&buf)) {
		fprintf(stderr, "Pattern store: buffer overrun at size %d. Pattern ignored.\n", sizeof(buffer));
	} else
		count_pattern(&buf, depth);
}  /* calc_nodes_pattern */

/**
 * Store all collected patterns.
 *
 * @param fname  filename for storage
 */
static void store_pattern(const char *fname) {
	FILE *f;
	pattern_entry_t *entry;
	int i, count = pset_count(status->pattern_hash);

	if (count <= 0)
		return;

	f = fopen(fname, "wb");
	if (! f) {
		perror(fname);
		return;
	}  /* if */

	fwrite("FPS1", 4, 1, f);
	fwrite(&count, sizeof(count), 1, f);

	for (i = 0, entry = pset_first(status->pattern_hash);
	     entry && i < count;
	     entry = pset_next(status->pattern_hash), ++i) {
		fwrite(entry, offsetof(pattern_entry_t, buf) + entry->len, 1, f);
	}  /* for */
	fclose(f);
}  /* store_pattern */

/**
 * Read collected patterns from a file.
 *
 * @param fname  filename
 */
static HASH_MAP(pattern_entry_t) *read_pattern(const char *fname) {
	FILE *f;
	pattern_entry_t *entry, tmp;
	int i, count;
	unsigned j;
	char magic[4];
	HASH_MAP(pattern_entry_t) *pattern_hash = new_pset(pattern_cmp, 8);
	BYTE            buffer[PATTERN_STORE_SIZE];
	CODE_BUFFER     buf;

	f = fopen(fname, "rb");
	if (! f) {
		perror(fname);
		return NULL;
	}  /* if */

	fread(magic, 4, 1, f);
	count = 0;
	fread(&count, sizeof(count), 1, f);
	if (memcmp(magic, "FPS1", 4) != 0 || count <= 0) {
		fprintf(stderr, "Error: %s is not a Firm pattern store. Ignored.\n", fname);
		fclose(f);
		return NULL;
	}  /* if */

	/* read all pattern entries and put them into the hash table. */
	for (i = 0; i < count; ++i) {
		init_buf(&buf, buffer, sizeof(buffer));
		fread(&tmp, offsetof(pattern_entry_t, buf), 1, f);
		for (j = 0; j < tmp.len; ++j)
			put_byte(&buf, fgetc(f));
		entry = pattern_get_entry(&buf, pattern_hash);
		memcpy(&entry->count, &tmp.count, sizeof(entry->count));
	}  /* for */
	fclose(f);

	printf("Read %d pattern from %s\n", count, fname);
	assert(pset_count(pattern_hash) == count);

	return pattern_hash;
}  /* read_pattern */

/**
 * Write the collected patterns to a VCG file for inspection.
 *
 * @param fname  name of the VCG file to create
 */
static void pattern_output(const char *fname) {
	pattern_entry_t  *entry;
	pattern_entry_t  **pattern_arr;
	pattern_dumper_t *dump;
	int i, count = pset_count(status->pattern_hash);

	printf("\n%d pattern detected\n", count);

	if (count <= 0)
		return;

	/* creates a dumper */
	dump = new_vcg_dumper(fname, 100);

	pattern_arr = xmalloc(sizeof(*pattern_arr) * count);
	for (i = 0, entry = pset_first(status->pattern_hash);
	     entry && i < count;
	     entry = pset_next(status->pattern_hash), ++i) {
		pattern_arr[i] =  entry;
	}  /* for */
	assert(count == i);
	count = i;

	/* sort it */
	qsort(pattern_arr, count, sizeof(*pattern_arr), pattern_count_cmp);

	for (i = 0; i < count; ++i) {
		entry = pattern_arr[i];
		if (cnt_to_uint(&entry->count) < status->bound)
			continue;

		/* dump a pattern */
		pattern_dump_new_pattern(dump, &entry->count);
		decode_node(entry->buf, entry->len, dump);
		pattern_dump_finish_pattern(dump);
	}  /* for */

	/* destroy it */
	pattern_end(dump);
}  /* pattern_output */

/*
 * Calculates the pattern history.
 */
void stat_calc_pattern_history(ir_graph *irg) {
	pattern_env_t env;
	unsigned      i;

	if (! status->enable)
		return;

	/* do NOT count the const code IRG */
	if (irg == get_const_code_irg())
		return;

	for (i = status->min_depth; i <= status->max_depth; ++i) {
		env.max_depth = i;
		irg_walk_graph(irg, calc_nodes_pattern, NULL, &env);
	}  /* for */
}  /* stat_calc_pattern_history */

/*
 * Initializes the pattern history.
 */
void stat_init_pattern_history(int enable) {
	HASH_MAP(pattern_entry_t) *pattern_hash = NULL;

	status->enable = enable;
	if (! enable)
		return;

	status->bound     = 10;
	status->options   = /* OPT_WITH_MODE | */ OPT_ENC_DAG | OPT_WITH_ICONST | OPT_PERSIST_PATTERN;
	status->min_depth = 3;
	status->max_depth = 5;

	obstack_init(&status->obst);

	/* create the hash-table */
	if (status->options & OPT_PERSIST_PATTERN)
		pattern_hash = read_pattern("pattern.fps");
	if (pattern_hash == NULL)
		pattern_hash = new_pset(pattern_cmp, 8);
	status->pattern_hash = pattern_hash;
}  /* stat_init_pattern_history */

/*
 * Finish the pattern history.
 */
void stat_finish_pattern_history(const char *fname) {
	if (! status->enable)
		return;

	store_pattern("pattern.fps");
	pattern_output("pattern.vcg");

	del_pset(status->pattern_hash);
	obstack_free(&status->obst, NULL);

	status->enable = 0;
}  /* stat_finish_pattern_history */
