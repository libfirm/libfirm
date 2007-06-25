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
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be prefered over a simple pset, because it
              tries to guarantee deterministic behavior.
 * @version   $Id$
 */
#include "config.h"

#include "irnodeset.h"
#include "irnode_t.h"
#include "hashptr.h"

#define DO_REHASH
#define ID_HASH
#define HashSet                   ir_nodeset_t
#define HashSetIterator           ir_nodeset_iterator_t
#define ValueType                 ir_node*
#define NullValue                 NULL
#define DeletedValue              ((ir_node*)-1)
#ifdef DEBUG_libfirm
#define Hash(this,key)            ((unsigned)((key)->node_nr))
#else
#define Hash(this,key)            HASH_PTR(key)
#endif
#define KeysEqual(this,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))

#define hashset_init            _ir_nodeset_init
#define hashset_init_size       ir_nodeset_init_size
#define hashset_destroy         ir_nodeset_destroy
#define hashset_insert          ir_nodeset_insert
#define hashset_remove          ir_nodeset_remove
#define hashset_find            _ir_nodeset_find
#define hashset_size            ir_nodeset_size
#define hashset_iterator_init   ir_nodeset_iterator_init
#define hashset_iterator_next   ir_nodeset_iterator_next
#define hashset_remove_iterator ir_nodeset_remove_iterator

#include "hashset.c"

void ir_nodeset_init(ir_nodeset_t *nodeset)
{
	ir_nodeset_init_size(nodeset, 16);
}

int ir_nodeset_contains(const ir_nodeset_t *this, const ir_node *node)
{
	return _ir_nodeset_find(this, node);
}
