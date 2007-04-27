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
 * File name:   ir/tr/type_or_entity.h
 * Purpose:     Provides a datatype to treat types and entities as the same.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 */

/**
 * @file type_or_entity.h
 *
 * Provides a datatype to treat types and entities as the same.
 *
 * @author Goetz Lindenmaier
 */

# ifndef _TYPE_OR_ENTITY_H_
# define _TYPE_OR_ENTITY_H_

#include "firm_types.h"

/** A data type to treat types and entities as the same. */
typedef union {
  ir_type   *typ;   /**< points to a type */
  ir_entity *ent;   /**< points to an entity */
} type_or_ent;


# endif /* _TYPE_OR_ENTITY_H_ */
