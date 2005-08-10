/*
 * Project:     libFIRM
 * File name:   ir/opt/scalar_replace.h
 * Purpose:     scalar replacement of compounds
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _SCALAR_REPLACE_H_
#define _SCALAR_REPLACE_H_

#include "irgraph.h"

/**
 * Do the scalar replacement optimization.
 * Replace local compound entities (like structures and arrays)
 * with atomic values if possible. Does not handle classes yet.
 *
 * @param irg  the graph which should be optimized
 */
void scalar_replacement_opt(ir_graph *irg);

#endif /* _SCALAR_REPLACE_H_ */
