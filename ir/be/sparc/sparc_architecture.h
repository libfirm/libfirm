/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief       sparc architecture variants
 * @author      Manuel Mohr
 */
#ifndef FIRM_BE_SPARC_ARCHITECTURE_H
#define FIRM_BE_SPARC_ARCHITECTURE_H

typedef struct {
	/** use register permutation instruction */
	unsigned use_permi:1;
} sparc_code_gen_config_t;

extern sparc_code_gen_config_t  sparc_cg_config;

/** Initialize the sparc architecture module. */
void sparc_init_architecture(void);

/** Setup the sparc_cg_config structure by inspecting current user settings. */
void sparc_setup_cg_config(void);

#endif
