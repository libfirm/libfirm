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
 * @brief       PBQP based register allocation.
 * @author      Thomas Bersch
 * @date        27.11.2009
 * @version     $Id: bechordal.c 26750 2009-11-27 09:37:43Z bersch $
 */
#ifndef BEPBQPALLOC_H_
#define BEPBQPALLOC_H_

/**
 * PBQP based register allocation.
 * @param env The chordal environment.
 */
void be_pbqp_coloring(be_chordal_env_t *env);

#endif /* BEPBQPALLOC_H_ */
