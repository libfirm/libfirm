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

/**
 * @file
 * @brief   implementation of pset_new
 * @author  Matthias Braun
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pset_new.h"

/** probing method: quadratic probing */
#define DO_REHASH
#define HashSet                    pset_new_t
#define HashSetIterator            pset_new_iterator_t
#define ValueType                  void*
#define NullValue                  NULL
#define DeletedValue               ((void*)-1)
#define KeysEqual(this,key1,key2)  1
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(HashSetEntry))

#define hashset_init            pset_new_init
#define hashset_init_size       pset_new_init_size
#define hashset_destroy         pset_new_destroy
#define hashset_insert          pset_new_insert
#define hashset_remove          pset_new_remove
#define hashset_find            pset_new_find
#define hashset_size            pset_new_size
#define hashset_iterator_init   pset_new_iterator_init
#define hashset_iterator_next   pset_new_iterator_next
#define hashset_remove_iterator pset_new_remove_iterator

#include "hashset.c"

int pset_new_contains(const pset_new_t *pset_new, const ValueType val)
{
	return pset_new_find(pset_new, val) != NullValue;
}
