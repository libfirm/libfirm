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
  if (! entry->hook._hook_turn_into_id)
    return;

  entry->next = hooks[hook];
  hooks[hook] = entry;
}

#else

void register_hook(hook_type_t hook, hook_entry_t *entry) {}

#endif /* FIRM_ENABLE_HOOKS */

int init_hooks(void)
{
  return (int)register_hook;
}
