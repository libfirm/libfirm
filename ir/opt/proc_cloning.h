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
 * File name:   ir/clone functions./proc_cloning.h
 * Purpose:     procedure cloning
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 */
#ifndef PROC_CLONING_H
#define PROC_CLONING_H

#include "firm_types.h"

/** A default threshold. */
#define DEFAULT_CLONE_THRESHOLD 300

/**
 * Do procedure cloning. Evaluate a heuristic weight for every
 * Call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 *
 * @param threshold   the threshold for cloning
 *
 * The threshold is an estimation of how many instructions are saved
 * when executing a cloned method. If threshold is 0.0, every possible
 * call is cloned.
 */
void proc_cloning(float threshold);

#endif /* PROC_CLONING_H */
