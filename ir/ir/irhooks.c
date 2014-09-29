/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Generic hooks for various libFirm functions.
 * @author   Michael Beck
 */
#include <assert.h>

#include "irhooks.h"

hook_entry_t *hooks[hook_last];

void register_hook(hook_type_t hook, hook_entry_t *entry)
{
	/* check if a hook function is specified. It's a union, so no matter which one */
	if (!entry->hook._hook_turn_into_id)
		return;

	/* hook should not be registered yet */
	assert(entry->next == NULL && hooks[hook] != entry);

	entry->next = hooks[hook];
	hooks[hook] = entry;
}

void unregister_hook(hook_type_t hook, hook_entry_t *entry)
{
	if (hooks[hook] == entry) {
		hooks[hook] = entry->next;
		entry->next = NULL;
		return;
	}

	hook_entry_t *p;
	for (p = hooks[hook]; p && p->next != entry; p = p->next) {
	}

	if (p != NULL) {
		p->next     = entry->next;
		entry->next = NULL;
	}
}
