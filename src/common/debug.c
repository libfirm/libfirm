/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Debug facility.
 * @author  Michael Beck, Sebastian Hack
 * @date    15.12.2004
 */
#ifdef DEBUG_libfirm

#include <stdarg.h>
#include <string.h>

#include "irprintf.h"
#include "debug.h"

#include "hashptr.h"
#include "obst.h"
#include "set.h"

static struct obstack dbg_obst;
static set *module_set;

/**
 * A debug module.
 */
struct firm_dbg_module_t {
  unsigned mask;
  const char *name;
  FILE *file;
};

/**
 * Compares two modules by comparing their names
 */
static int module_cmp(const void *p1, const void *p2, size_t size)
{
  const firm_dbg_module_t *m1 = (const firm_dbg_module_t*)p1;
  const firm_dbg_module_t *m2 = (const firm_dbg_module_t*)p2;
  (void) size;

  return strcmp(m1->name, m2->name);
}

/**
 * initialize the debug module
 */
static void firm_dbg_init(void)
{
  obstack_init(&dbg_obst);
  module_set = new_set(module_cmp, 16);
}

firm_dbg_module_t *firm_dbg_register(const char *name)
{
  firm_dbg_module_t mod;
  mod.mask = 0;
  mod.name = name;
  mod.file = stderr;

  if (!module_set)
    firm_dbg_init();

  return set_insert(firm_dbg_module_t, module_set, &mod, sizeof(mod), hash_str(name));
}

void firm_dbg_set_mask(firm_dbg_module_t *module, unsigned mask)
{
  module->mask = mask;
}

unsigned firm_dbg_get_mask(const firm_dbg_module_t *module)
{
  return module->mask;
}

void firm_dbg_set_file(firm_dbg_module_t *module, FILE *file)
{
  module->file = file;
}

/**
 * A message info: a pair of debug handle and message
 */
typedef struct msg_info_t {
  const char *msg;
  const firm_dbg_module_t *mod;
} msg_info_t;

/**
 * Formats a message given by a printf-like format and a va_list argument,
 * puts the test on an obstack and return a msg_info.
 */
static void *make_msg_info(const firm_dbg_module_t *mod, const char *fmt, va_list args)
{
  static const char msg_header[] = "%s(%d) %s: ";
  msg_info_t *res = OALLOC(&dbg_obst, msg_info_t);

  obstack_grow(&dbg_obst, msg_header, sizeof(msg_header) - 1);
  ir_obst_vprintf(&dbg_obst, fmt, args);
  obstack_1grow(&dbg_obst, '\0');

  res->msg = (const char*)obstack_finish(&dbg_obst);
  res->mod = mod;
  return res;
}

void *_firm_dbg_make_msg(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...)
{
  void *res = NULL;

  if (mask == 0 || (mod->mask & mask)) {
    va_list args;
    va_start(args, fmt);
    res = make_msg_info(mod, fmt, args);
    va_end(args);
  }

  return res;
}

void _firm_dbg_print_msg(const char *filename, int line, const char *func, void *mi_ptr)
{
  msg_info_t *mi = (msg_info_t*)mi_ptr;
  if (mi) {
    fprintf(mi->mod->file, mi->msg, filename, line, func);
    obstack_free(&dbg_obst, mi);
  }
}

void _firm_dbg_print(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...)
{
  if (mask == 0 || (mod->mask & mask)) {
    va_list args;
    char *res;
    va_start(args, fmt);
    ir_obst_vprintf(&dbg_obst, fmt, args);
    obstack_1grow(&dbg_obst, '\0');
    res = (char*)obstack_finish(&dbg_obst);
    fprintf(mod->file, "%s", res);
    obstack_free(&dbg_obst, res);
    va_end(args);
  }
}

#else /* DEBUG_libfirm */

/* some picky compiler don't allow empty files */
static int __attribute__((unused)) dummy;

#endif /* DEBUG_libfirm */
