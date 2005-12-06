/*
 * Project:     libFIRM
 * File name:   ir/debug/seqnumbers.c
 * Purpose:     Implements simple sequence numbers for Firm debug info.
 * Author:      Michael Beck
 * Modified by:
 * Created:     2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file seqnumbers.c
 *
 * Sequence numbers for Firm.
 *
 * A sequence number is an unique number representing a filename
 * and a line number. The number 0 represents empty information.
 * This module is an optional "snap-in" for the Firm debug info.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "set.h"
#include "hashptr.h"
#include "ident.h"
#include "seqnumbers.h"

/**
 * A entry in the sequence number table.
 */
struct sn_entry {
  ident    *filename;  /**< the filename */
  unsigned lineno;     /**< the line number */
};

static set *seqnos = NULL;

/** hash a seqno entry */
#define HASH(key) (HASH_PTR((key).filename) ^ (key).lineno)

/**
 * Compare two seqno entries.
 */
static int seqno_cmp(const void *elt, const void *key, size_t size)
{
  seqno_t e1 = (seqno_t)elt;
  seqno_t e2 = (seqno_t)key;

  return (e1->filename != e2->filename) | (e1->lineno - e2->lineno);
}

/*
 * Create a new sequence number from a filename and a line number.
 */
seqno_t firm_seqno_enter(const char *filename, unsigned lineno)
{
  struct sn_entry key;

  key.filename = new_id_from_str(filename);
  key.lineno   = lineno;

  return set_insert(seqnos, &key, sizeof(key), HASH(key));
}

/*
 * Create a new sequence number from a filename ident and a line number.
 */
seqno_t firm_seqno_enter_id(ident *filename, unsigned lineno)
{
  struct sn_entry key;

  key.filename = filename;
  key.lineno   = lineno;

  return set_insert(seqnos, &key, sizeof(key), HASH(key));
}

/**
 * Retrieve filename and line number form a sequence number
 */
const char *firm_seqno_retrieve(seqno_t seqno, unsigned *lineno)
{
  if (seqnos && seqno) {
    *lineno = seqno->lineno;
    return get_id_str(seqno->filename);
  }
  *lineno = 0;
  return NULL;
}

/*
 * Creates the seqno pool.
 */
void firm_seqno_init(void)
{
  if (seqnos)
    firm_seqno_term();

  seqnos = new_set(seqno_cmp, 8);
}

/*
 * Terminates the seqno pool.
 * Sequence numbers cannot be resolved anymore.
 */
void firm_seqno_term(void)
{
  if (seqnos) {
    del_set(seqnos);
    seqnos = NULL;
  }
}
