/*
 * Project:     libFIRM
 * File name:   ir/common/ident.c
 * Purpose:     Hash table to store names.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#ifdef FIRM_ENABLE_WCHAR
#include <wchar.h>
#endif

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

#ifdef FIRM_ENABLE_WCHAR
/**
 * Stores a wide character string in the ident module and returns a
 * handle for the string.
 *
 * @param handle   the handle for the set
 * @param wstr     the wide character string which shall be stored
 * @param len      length of wstr
 *
 * @return id - a handle for the generated ident
 *
 * Default implementation using libfirm sets.
 */
static ident *set_new_id_from_wchars(void *handle, const wchar_t *wstr, int len)
{
  set *id_set = handle;
  wchar_t *tmp;

  /* can't use hinsert0 here, so copy and add a 0 */
  tmp = alloca((len + 1) * sizeof(*tmp));
  memcpy(tmp, wstr, len * sizeof(*tmp));
  tmp[len] = L'\0';

  return (ident *)set_hinsert(id_set, tmp, (len + 1) * sizeof(wchar_t), ID_HASH(wchar_t, tmp, len));
}

/**
 * Stores a wide character string in the ident module and
 * returns a handle for the string.
 *
 * @param handle   the handle for the set
 * @param wstr     the wide character string which shall be stored
 *
 * Default implementation using libfirm sets.
 */
static ident *set_new_id_from_wcs(void *handle, const wchar_t *wstr)
{
  assert(wstr);
  return (ident *)set_new_id_from_wchars(handle, wstr, wcslen(wstr));
}

/**
 * Returns a wide character string represented by an ident.
 *
 * @param handle   the handle for the set
 * @param id       the ident
 *
 * Default implementation using libfirm sets.
 */
static const wchar_t *set_get_id_wcs(void *handle, ident *id)
{
  struct set_entry *entry = (struct set_entry *)id;

  return (const wchar_t *)entry->dptr;
}

/**
 * Returns the length of the string represented by an ident.
 *
 * @param handle   the handle for the set
 * @param id       the ident
 *
 * Default implementation using libfirm sets.
 */
static int set_get_id_wcslen(void *handle, ident *id)
{
  struct set_entry *entry = (struct set_entry *)id;

  /* len + \0 is stored for wchar_t */
  return entry->size / sizeof(wchar_t) - 1;
}

/**
 * Default implementation if no new_id_from_wcs() is provided.
 */
static ident *def_new_id_from_wcs(void *handle, const wchar_t *wstr)
{
  return impl.new_id_from_wchars(handle, wstr, wcslen(wstr));
}

/**
 * Default implementation if no new_id_from_wchars() is provided.
 */
static ident *def_new_id_from_wchars(void *handle, const wchar_t *wstr, int len)
{
  return impl.new_id_from_chars(handle, (const char *)wstr, (len + 1) * sizeof(wchar_t));
}

/**
 * Default implementation if no get_id_wcs() is provided.
 */
static const wchar_t *def_get_id_wcs(void *handle, ident *id)
{
  return (const wchar_t *)impl.get_id_str(handle, id);
}

/**
 * Default implementation if no get_id_wcslen() is provided.
 */
static int def_get_id_wcslen(void *handle, ident *id)
{
  return wcslen(impl.get_id_wcs(handle, id));
}
#endif /* FIRM_ENABLE_WCHAR */

/* Initialize the ident module. */
void init_ident(ident_if_t *id_if, int initial_n_idents)
{
  if (id_if) {
    memcpy(&impl, id_if, sizeof(impl));

    if (! impl.new_id_from_str)
      impl.new_id_from_str = def_new_id_from_str;
    if (! impl.get_id_strlen)
      impl.get_id_strlen = def_get_id_strlen;

#ifdef FIRM_ENABLE_WCHAR
    if (! impl.new_id_from_wcs)
      impl.new_id_from_wcs = def_new_id_from_wcs;
    if (! impl.new_id_from_wchars)
      impl.new_id_from_wchars = def_new_id_from_wchars;
    if (! impl.get_id_wcs)
      impl.get_id_wcs = def_get_id_wcs;
    if (! impl.get_id_wcslen)
      impl.get_id_wcslen = def_get_id_wcslen;
#endif /* FIRM_ENABLE_WCHAR */
  }
  else {
   impl.new_id_from_str    = set_new_id_from_str;
   impl.new_id_from_chars  = set_new_id_from_chars;
   impl.get_id_str         = set_get_id_str;
   impl.get_id_strlen      = set_get_id_strlen;
   impl.finish_ident       = set_finish_ident;
#ifdef FIRM_ENABLE_WCHAR
   impl.new_id_from_wcs    = set_new_id_from_wcs;
   impl.new_id_from_wchars = set_new_id_from_wchars;
   impl.get_id_wcs         = set_get_id_wcs;
   impl.get_id_wcslen      = set_get_id_wcslen;
#endif /* FIRM_ENABLE_WCHAR */

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

#ifdef FIRM_ENABLE_WCHAR

ident *new_id_from_wcs (const wchar_t *str)
{
  assert(str);
  return impl.new_id_from_wcs(impl.handle, str);
}

ident *new_id_from_wchars (const wchar_t *str, int len)
{
  assert(len > 0);
  return impl.new_id_from_wchars(impl.handle, str, len);
}

const wchar_t *get_id_wcs(ident *id)
{
  return impl.get_id_wcs(impl.handle, id);
}

int  get_id_wcslen(ident *id)
{
  return impl.get_id_wcslen(impl.handle, id);
}

int id_contains_wchar (ident *id, wchar_t c)
{
  return wcschr(get_id_wcs(id), c) != NULL;
}

#endif /* FIRM_ENABLE_WCHAR */
