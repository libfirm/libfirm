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
 * @brief      Internal preprocessor directives.
 * @author     Goetz Lindenmaier
 * @version    $Id$
 */
#ifndef FIRM_COMMON_COMMON_T_H
#define FIRM_COMMON_COMMON_T_H

#include "firm_common.h"

/* --- Global flags.  --- */

/** When set Phi node construction uses the values valid when the fragile
 *  operation is executed.  Else it uses the values valid at the end of the
 *  block with the fragile operation. */
#define PRECISE_EXC_CONTEXT 1

/** If this and DEBUG_libfirm are defined irdump uses the nodeid numbers as
 *  labels for the vcg nodes.  This makes the vcg graph better readable.
 *  Sometimes it's useful to see the pointer values, though. */
#define NODEID_AS_LABEL 1

#endif
