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
#include "config.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "irtools.h"

#include "sparc_architecture.h"

sparc_code_gen_config_t  sparc_cg_config;

/**
 * CPU architectures and features.
 */
enum cpu_arch_features {
	arch_generic        = 0x00000001, /**< no specific architecture */
	/* leave plenty of space :-) */
	arch_mask           = 0x00003FFF,

	arch_feature_permi  = 0x00004000  /**< register permutation instruction */
};

/**
 * CPU's.
 */
typedef enum cpu_support {
	cpu_generic  = arch_generic,

	cpu_icore    = arch_generic | arch_feature_permi
} cpu_support;

static cpu_support arch = cpu_generic;

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "generic", cpu_generic },
	{ "icore",   cpu_icore },

	{ NULL,    0 }
};

static lc_opt_enum_int_var_t arch_var = {
	(int*) &arch, arch_items
};

static const lc_opt_table_entry_t sparc_architecture_options[] = {
	LC_OPT_ENT_ENUM_INT("arch", "select the instruction architecture",
	                    &arch_var),
	LC_OPT_LAST
};

#define FLAGS(x, f) (((x) & (f)) != 0)

void sparc_setup_cg_config(void)
{
	sparc_code_gen_config_t *const c = &sparc_cg_config;
	memset(c, 0, sizeof(*c));

	c->use_permi = FLAGS(arch, arch_feature_permi);
}

void sparc_init_architecture(void)
{
	lc_opt_entry_t *be_grp, *sparc_grp;

	memset(&sparc_cg_config, 0, sizeof(sparc_cg_config));

	be_grp    = lc_opt_get_grp(firm_opt_get_root(), "be");
	sparc_grp = lc_opt_get_grp(be_grp, "sparc");

	lc_opt_add_table(sparc_grp, sparc_architecture_options);
}
