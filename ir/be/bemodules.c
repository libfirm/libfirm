/*
 * Author:      Matthias Braun
 * Date:        29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "bemodules.h"
#include "xmalloc.h"

typedef struct _constructor_list_entry_t {
	struct _constructor_list_entry_t *next;
	be_module_constructor_func func;
} constructor_list_entry_t;

static constructor_list_entry_t *constructors = NULL;

static void free_constructor_list(void)
{
	constructor_list_entry_t *entry = constructors;

	while(entry != NULL) {
		constructor_list_entry_t *next = entry->next;
		free(entry);
		entry = next;
	}
}

void be_module_add_constructor(be_module_constructor_func func)
{
	static int initialized = 0;
	constructor_list_entry_t *entry;

	if(!initialized) {
		atexit(free_constructor_list);
		initialized = 1;
	}

	entry = xmalloc(sizeof(entry[0]));
	entry->next = constructors;
	entry->func = func;

	constructors = entry;
}

void be_module_call_constructors()
{
	constructor_list_entry_t *entry;

	for(entry = constructors; entry != NULL; entry = entry->next) {
		entry->func();
	}
}
