/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <string.h>

#include "pset.h"
#include "list.h"
#include "iterator.h"


static void *it_pset_start(void *collection)
{
	return pset_first(collection);
}

static void *it_pset_next(void *collection, void *curr)
{
	return pset_next(collection);
}

static void it_pset_finish(void *collection, void *curr)
{
}

static const iterator_t iterator_pset = {
	ITERATOR_MAGIC,
	it_pset_start,
	it_pset_next,
	it_pset_finish
};

const iterator_t *pset_iterator = &iterator_pset;


static void *it_list_next(void *coll, void *it)
{
	struct list_head *head = coll;
	struct list_head *curr = it;
	return curr->next != head ? curr->next : NULL;
}

static void *it_list_start(void *coll)
{
	return it_list_next(coll, coll);
}

static void it_list_finish(void *coll, void *curr)
{
}

static const iterator_t iterator_list = {
	ITERATOR_MAGIC,
	it_list_start,
	it_list_next,
	it_list_finish
};

const iterator_t *list_iterator = &iterator_list;
