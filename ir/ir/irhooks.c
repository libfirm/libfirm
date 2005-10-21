/*
 * Project:     libFIRM
 * File name:   ir/ir/irhooks.c
 * Purpose:     Generic hooks for various libFirm functions.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (C) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
  /* check if a hook function is specifyed. It's a union, so no matter which one */
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

int init_hooks(void) {
  return (int)register_hook + (int)unregister_hook;
}
