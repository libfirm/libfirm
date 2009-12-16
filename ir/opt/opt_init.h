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
 * @author  Matthias Braun
 * @brief   Init functions for various optimisations
 * @version $Id$
 */
#ifndef FIRM_OPT_INIT_H
#define FIRM_OPT_INIT_H

void firm_init_inline(void);

void firm_init_funccalls(void);

void firm_init_reassociation(void);

void firm_init_scalar_replace(void);

void firm_init_class_casts_opt(void);

void firm_init_loop_opt(void);

#endif
