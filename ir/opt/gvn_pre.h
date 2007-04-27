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
 * File name:   ir/opt/gvn_pre.h
 * Purpose:     Global Value Numbering Partial Redundancy Elimination
 * Author:      Michael Beck, Rubino Geiss
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 */
#ifndef __FIRM_OPT_GVN_PRE_H__
#define __FIRM_OPT_GVN_PRE_H__

#include "firm_types.h"

/**
 * Does Partial Redundancy Elimination combined with
 * Global Value Numbering.
 * Can be used to replace place_code() completely.
 *
 * Based on VanDrunen and Hosking 2004.
 */
void do_gvn_pre(ir_graph *irg);

#endif /* __FIRM_OPT_GVN_PRE_H__ */
