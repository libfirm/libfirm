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
 * @brief   Debug facility.
 * @author  Michael Beck, Sebastian Hack
 * @date    15.12.2004
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "firm_config.h"

#ifdef DEBUG_libfirm

#include <stdarg.h>
#include <string.h>

#include "irprintf.h"
#include "debug.h"

#include "hashptr.h"
#include "obst.h"
#include "set.h"

#ifdef WITH_LIBCORE

#include "irargs_t.h"

static void firm_dbg_default_printer(struct obstack *obst, const char *fmt, va_list args)
{
  static lc_arg_env_t *env = NULL;

  if(!env)
    env = firm_get_arg_env();

  lc_evoprintf(env, obst, fmt, args);

}

firm_dbg_module_t *firm_dbg_register(const char *name)
{
  return lc_dbg_register_with_printer(name, firm_dbg_default_printer);
}

#else

static struct obstack dbg_obst;
static set *module_set;

/**
 * A debug module.
 */
struct _firm_dbg_module_t {
  unsigned mask;
  const char *name;
  FILE *file;
};

/**
 * Compares two modules by comparing it's names
 */
static int module_cmp(const void *p1, const void *p2, size_t size)
{
  const firm_dbg_module_t *m1 = p1;
  const firm_dbg_module_t *m2 = p2;
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

  if(!module_set)
    firm_dbg_init();

  return set_insert(module_set, &mod, sizeof(mod), HASH_STR(name, strlen(name)));
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
typedef struct _msg_info_t {
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
  msg_info_t *res = obstack_alloc(&dbg_obst, sizeof(*res));

  obstack_grow(&dbg_obst, msg_header, sizeof(msg_header) - 1);
  ir_obst_vprintf(&dbg_obst, fmt, args);
  obstack_1grow(&dbg_obst, '\0');

  res->msg = obstack_finish(&dbg_obst);
  res->mod = mod;
  return res;
}

void *_firm_dbg_make_msg(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...)
{
  void *res = NULL;

  if(mask == 0 || (mod->mask & mask)) {
    va_list args;
    va_start(args, fmt);
    res = make_msg_info(mod, fmt, args);
    va_end(args);
  }

  return res;
}

void _firm_dbg_print_msg(const char *filename, int line, const char *func, void *mi_ptr)
{
  msg_info_t *mi = mi_ptr;
  if(mi) {
    fprintf(mi->mod->file, mi->msg, filename, line, func);
    obstack_free(&dbg_obst, mi);
  }
}

void _firm_dbg_print(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...)
{
  if(mask == 0 || (mod->mask & mask)) {
    va_list args;
    char *res;
    va_start(args, fmt);
    ir_obst_vprintf(&dbg_obst, fmt, args);
    obstack_1grow(&dbg_obst, '\0');
    res = obstack_finish(&dbg_obst);
    fprintf(mod->file, res);
    obstack_free(&dbg_obst, res);
    va_end(args);
  }
}

#endif /* WITH_LIBCORE */

#else /* DEBUG_libfirm */

/* some picky compiler don't allow empty files */
static int __attribute__((unused)) dummy;

#endif /* DEBUG_libfirm */
