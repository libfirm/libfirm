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
 * @file   beifg_impl.h
 * @date   01.12.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Constructors for different implementations of
 * chordal interference graphs.
 */

#ifndef _BEIFG_IMPL_H
#define _BEIFG_IMPL_H

#include "bechordal_t.h"

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_list_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_clique_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_pointer_new(const be_chordal_env_t *env);

#endif /* _BEIFG_IMPL_H */
