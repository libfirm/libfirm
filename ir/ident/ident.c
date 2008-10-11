/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief     Hash table to store names.
 * @author    Goetz Lindenmaier
 * @version   $Id$
 */
#include "config.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#include "ident_t.h"
#include "set.h"
#include "xmalloc.h"

/* for debugging only, not the real implementation */
struct _ident {
  char reserved[sizeof(unsigned) + sizeof(size_t)];
  char data[1];
};

/** The current ident module implementation. */
static ident_if_t impl;

/**
 * Stores a string in the ident module and returns a handle for the string.
 *
 * @param handle   the handle for the set
 * @param str      the string which shall be stored
 * @param len      length of str in bytes
 *
 * @return id - a handle for the generated ident
 *
 * Default implementation using libfirm sets.
 */
static ident *set_new_id_from_chars(void *handle, const char *str, int len)
{
  set *id_set = handle;

  /* GL: Who added this assert?  And why? */
  //assert(len > 0);
  return (ident *)set_hinsert0(id_set, str, len, ID_HASH(unsigned char, str, len));
}

/**
 * Stores a string in the ident module and returns a handle for the string.
 *
 * @param handle   the handle for the set
 * @param str      the string (or whatever) which shall be stored
 *
 * Default implementation using libfirm sets.
 */
static ident *set_new_id_from_str(void *handle, const char *str)
{
  assert(str);
  return (ident *)set_new_id_from_chars(handle, str, strlen(str));
}

/**
 * Returns a string represented by an ident.
 *
 * @param handle   the handle for the set
 * @param id       the ident
 *
 * Default implementation using libfirm sets.
 */
static const char *set_get_id_str(void *handle, ident *id)
{
  struct set_entry *entry = (struct set_entry *)id;
  (void) handle;

  return (const char *)entry->dptr;
}

/**
 * Returns the length of the string represented by an ident.
 *
 * @param handle   the handle for the set
 * @param id       the ident
 *
 * Default implementation using libfirm sets.
 */
static int set_get_id_strlen(void *handle, ident *id)
{
  struct set_entry *entry = (struct set_entry *)id;
  (void) handle;

  return entry->size;
}

/**
 * Default implementation using libfirm sets.
 */
void set_finish_ident(void *handle) {
  set *id_set = handle;

  del_set(id_set);
}

/**
 * Default implementation if no new_id_from_str() is provided.
 */
static ident *def_new_id_from_str(void *handle, const char *str)
{
  return impl.new_id_from_chars(handle, str, strlen(str));
}

/**
 * Default implementation if no get_id_strlen() is provided.
 */
static int def_get_id_strlen(void *handle, ident *id)
{
  return strlen(impl.get_id_str(handle, id));
}

/* Initialize the ident module. */
void init_ident(ident_if_t *id_if, int initial_n_idents)
{
  if (id_if) {
    memcpy(&impl, id_if, sizeof(impl));

    if (! impl.new_id_from_str)
      impl.new_id_from_str = def_new_id_from_str;
    if (! impl.get_id_strlen)
      impl.get_id_strlen = def_get_id_strlen;
  } else {
   impl.new_id_from_str    = set_new_id_from_str;
   impl.new_id_from_chars  = set_new_id_from_chars;
   impl.get_id_str         = set_get_id_str;
   impl.get_id_strlen      = set_get_id_strlen;
   impl.finish_ident       = set_finish_ident;

   /* it's ok to use memcmp here, we check only strings */
   impl.handle = new_set(memcmp, initial_n_idents);
  }
}

ident *new_id_from_str(const char *str)
{
  assert(str);
  return impl.new_id_from_str(impl.handle, str);
}

ident *new_id_from_chars(const char *str, int len)
{
  assert(len > 0);
  return impl.new_id_from_chars(impl.handle, str, len);
}

const char *get_id_str(ident *id)
{
  return impl.get_id_str(impl.handle, id);
}

int get_id_strlen(ident *id)
{
  return impl.get_id_strlen(impl.handle, id);
}

void finish_ident(void) {
  if (impl.finish_ident)
    impl.finish_ident(impl.handle);
}

int id_is_prefix(ident *prefix, ident *id)
{
  if (get_id_strlen(prefix) > get_id_strlen(id)) return 0;
  return 0 == memcmp(get_id_str(prefix), get_id_str(id), get_id_strlen(prefix));
}

int id_is_suffix(ident *suffix, ident *id)
{
  int suflen = get_id_strlen(suffix);
  int idlen  = get_id_strlen(id);
  const char *part;

  if (suflen > idlen) return 0;

  part = get_id_str(id);
  part = part + (idlen - suflen);

  return 0 == memcmp(get_id_str(suffix), part, suflen);
}

int id_contains_char(ident *id, char c)
{
  return strchr(get_id_str(id), c) != NULL;
}

ident *id_unique(const char *tag)
{
	static unsigned unique_id = 0;
	char buf[256];

	snprintf(buf, sizeof(buf), tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}
