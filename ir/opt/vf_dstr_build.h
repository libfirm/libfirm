/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Building Firm graphs from VFirm with arrange information.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_OPT_VF_DSTR_BUILD_H
#define FIRM_OPT_VF_DSTR_BUILD_H

#include "firm_types.h"

/** Build the Firm graph of the given VFirm graph. */
void vb_build(ir_graph *irg);

#endif
