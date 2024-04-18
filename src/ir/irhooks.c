/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Generic hooks for various libFirm functions.
 * @author   Michael Beck
 */
#include "irhooks.h"

#include <assert.h>

hook_entry_t *hooks[hook_last];

void register_hook(hook_type_t hook, hook_entry_t *entry)
{
	/* check if a hook function is specified. It's a union, so no matter which one */
	if (!entry->hook._hook_node_info)
		return;

	/* hook should not be registered yet */
	assert(entry->next == NULL && hooks[hook] != entry);

	entry->next = hooks[hook];
	hooks[hook] = entry;
}

void unregister_hook(hook_type_t hook, hook_entry_t *entry)
{
	for (hook_entry_t **p = &hooks[hook]; *p; p = &(*p)->next) {
		if (*p == entry) {
			*p          = entry->next;
			entry->next = NULL;
			break;
		}
	}
}
