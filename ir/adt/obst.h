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
 * @brief     Provied obstack_chunk_alloc and obstack_chunk_free for obstack.h
 * @author    Martin Trapp, Christian Schaefer
 * @version   $Id$
 */
#ifndef FIRM_ADT_OBST_H
#define FIRM_ADT_OBST_H

#include <obstack.h>
#include "xmalloc.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

#endif
