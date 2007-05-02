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
 * @brief   declarations for the mips backend
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifndef FIRM_BE_MIPS_BEARCH_MIPS_H
#define FIRM_BE_MIPS_BEARCH_MIPS_H

#include "../bearch_t.h"

typedef struct mips_code_gen_t  mips_code_gen_t;

extern const arch_isa_if_t mips_isa_if;

/** return the scheduled block at position pos */
ir_node *mips_get_sched_block(const mips_code_gen_t *cg, int pos);

/** return the number of scheduled blocks */
int mips_get_sched_n_blocks(const mips_code_gen_t *cg);

/** set a block schedule number */
void mips_set_block_sched_nr(ir_node *block, int nr);

/** get a block schedule number */
int mips_get_block_sched_nr(ir_node *block);

#endif
