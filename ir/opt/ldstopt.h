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
 * @brief   Load/Store optimizations.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_LDSTOPT_H
#define FIRM_OPT_LDSTOPT_H

#include "firm_types.h"

/** Load/Store optimization.
 *
 * Removes redundant non-volatile Loads and Stores.
 * May introduce Bad nodes if exceptional control flow
 * is removed. The following cases are optimized:
 *
 * Load without result: A Load which has only a memory use
 *   is removed.
 *
 * Load after Store: A Load after a Store is removed, if
 *   the Load doesn't have an exception handler OR is in
 *   the same block as the Store.
 *
 * Load after Load: A Load after a Load is removed, if the
 *   Load doesn't have an exception handler OR is in the
 *   same block as the previous Load.
 *
 * Store before Store: A Store immediately before another
 *   Store in the same block is removed, if the Store doesn't
 *   have an exception handler.
 *
 * Store after Load: A Store after a Load is removed, if the
 *   Store doesn't have an exception handler.
 */
void optimize_load_store(ir_graph *irg);

#endif /* FIRM_OPT_LDSTOPT_H */
