/*
 * Project:     libFIRM
 * File name:   ir/opt/ldstopt.h
 * Purpose:     load/store optimizations
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file ldstopt.h
 *
 * Load/Store optimization.
 *
 * @author Michael Beck
 */
#ifndef _LDSTOPT_H_
#define _LDSTOPT_H_

#include "irgraph.h"

/** Load/Store optimization.
 *
 * Removes redundand non-volatile Loads and Stores.
 * May introduce Bad nodes if exceptional control flow
 * is removed. The following cases are optimized:
 *
 * Load without result: A Load which has only a memory use
 *   is removed.
 *
 * Load after Store: A Load after a Store is removed, if
 *   the Load doesn't have an exception handler or is in
 *   the same block as the Store.
 *
 * Load after Load: A Load after a Load is removed, if the
 *   Load doesn't have an exception handler or is in the
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

#endif /* _LDSTOPT_H_ */
