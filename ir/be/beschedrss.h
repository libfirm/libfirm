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
 * @brief       Implementation of a register saturating list scheduler.
 * @author      Christian Wuerdig
 * @date        06.09.2006
 * @version     $Id$
 *
 * Implementation of a register saturating list scheduler
 * as described in: Sid-Ahmed-Ali Touati
 * Register Saturation in Superscalar and VLIW Codes
 */
#ifndef FIRM_BE_BESCHEDRSS_H
#define FIRM_BE_BESCHEDRSS_H

#include "beirg.h"

/**
 * Perform RSS schedule preprocessing for the given irg.
 * @param birg  The backend irg object
 */
void rss_schedule_preparation(const be_irg_t *birg);

#endif /* FIRM_BE_BESCHEDRSS_H */
