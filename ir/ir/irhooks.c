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

/*
 * Project:     libFIRM
 * File name:   ir/ir/irhooks.c
 * Purpose:     Generic hooks for various libFirm functions.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (C) 1998-2005 Universität Karlsruhe
 */

/**
 * @file irhooks.c
 *
 * Generic hooks for various libFirm functions.
 *
 * @author Michael Beck
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irhooks.h"

#ifdef FIRM_ENABLE_HOOKS

/* the hooks */
hook_entry_t *hooks[hook_last];

/* register a hook */
void register_hook(hook_type_t hook, hook_entry_t *entry) {
  /* check if a hook function is specified. It's a union, so no matter which one */
  if (! entry->hook._hook_turn_into_id)
    return;

  entry->next = hooks[hook];
  hooks[hook] = entry;
}

/* unregister a hook */
void unregister_hook(hook_type_t hook, hook_entry_t *entry) {
  hook_entry_t *p;

  if (hooks[hook] == entry) {
    hooks[hook] = entry->next;
    entry->next = NULL;
    return;
  }

  for (p = hooks[hook]; p && p->next != entry; p = p->next);

  if (p) {
    p->next     = entry->next;
    entry->next = NULL;
  }
}


#else

void register_hook(hook_type_t hook, hook_entry_t *entry) {}
void unregister_hook(hook_type_t hook, hook_entry_t *entry) {}

#endif /* FIRM_ENABLE_HOOKS */

int firm_init_hooks(void) {
  /* this strange code assures that both functions are available
     in a shared library even if none of them is called.
     Meanwhile not needed anymore but ... */
  return (int)register_hook + (int)unregister_hook;
}
