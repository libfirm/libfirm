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
 * is removed.
 */
void optimize_load_store(ir_graph *irg);

#endif /* _LDSTOPT_H_ */
