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
 * @brief   declarations for ppc32 node attributes
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifndef FIRM_BE_PPC32_PPC32_NODES_ATTR_H
#define FIRM_BE_PPC32_PPC32_NODES_ATTR_H

#include "../bearch_t.h"

typedef struct
{
	unsigned shift:5;
	unsigned maskA:5;
	unsigned maskB:5;
} rlwimi_const_t;


typedef enum {
	ppc32_ac_None,
	ppc32_ac_Const,
	ppc32_ac_SymConst,
	ppc32_ac_FrameEntity,
	ppc32_ac_RlwimiConst,
	ppc32_ac_BranchProj,
	ppc32_ac_Offset
} ppc32_attr_content_type;

typedef enum {
	ppc32_ao_None, ppc32_ao_Lo16, ppc32_ao_Hi16, ppc32_ao_Ha16, ppc32_ao_Illegal
} ppc32_attr_offset_mode;

typedef struct _ppc32_attr_t {
	arch_irn_flags_t flags;     /**< indicating if spillable, rematerializeable ... etc. */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */

	ppc32_attr_content_type content_type;
	ppc32_attr_offset_mode offset_mode;
	union {
		tarval *constant_tarval;
		ident *symconst_ident;
		ir_entity *frame_entity;
		rlwimi_const_t rlwimi_const;
		int proj_nr;
		int offset;
		void* empty;
	} data;

	const arch_register_t **slots;       /**< register slots for assigned registers */
} ppc32_attr_t;

#endif
