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
 * @brief       ia32 architecture variants
 * @author      Michael Beck, Matthias Braun
 * @version     $Id: bearch_ia32_t.h 16363 2007-10-25 23:27:07Z beck $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#include "irtools.h"

#include "bearch_ia32_t.h"
#include "ia32_architecture.h"

ia32_code_gen_config_t  ia32_cg_config;

/**
 * CPU features.
 */
enum cpu_arch_features {
	arch_feature_intel    = 0x80000000,                      /**< Intel CPU */
	arch_feature_amd      = 0x40000000,                      /**< AMD CPU */
	arch_feature_p6       = 0x20000000,                      /**< P6 instructions */
	arch_feature_mmx      = 0x10000000,                      /**< MMX instructions */
	arch_feature_sse1     = 0x08000000 | arch_feature_mmx,   /**< SSE1 instructions, include MMX */
	arch_feature_sse2     = 0x04000000 | arch_feature_sse1,  /**< SSE2 instructions, include SSE1 */
	arch_feature_sse3     = 0x02000000 | arch_feature_sse2,  /**< SSE3 instructions, include SSE2 */
	arch_feature_ssse3    = 0x01000000 | arch_feature_sse3,  /**< SSSE3 instructions, include SSE3 */
	arch_feature_3DNow    = 0x00800000,                      /**< 3DNow! instructions */
	arch_feature_3DNowE   = 0x00400000 | arch_feature_3DNow, /**< Enhanced 3DNow! instructions */
	arch_feature_netburst = 0x00200000 | arch_feature_intel, /**< Netburst architecture */
	arch_feature_64bit    = 0x00100000 | arch_feature_sse2,  /**< x86_64 support, include SSE2 */
};

/**
 * Architectures.
 */
enum cpu_support {
	/* intel CPU's */
	arch_generic     =  0,

	arch_i386        =  1,
	arch_i486        =  2,
	arch_pentium     =  3 | arch_feature_intel,
	arch_pentium_mmx =  4 | arch_feature_intel | arch_feature_mmx,
	arch_pentium_pro =  5 | arch_feature_intel | arch_feature_p6,
	arch_pentium_2   =  6 | arch_feature_intel | arch_feature_p6 | arch_feature_mmx,
	arch_pentium_3   =  7 | arch_feature_intel | arch_feature_p6 | arch_feature_sse1,
	arch_pentium_4   =  8 | arch_feature_netburst | arch_feature_p6 | arch_feature_sse2,
	arch_pentium_m   =  9 | arch_feature_intel | arch_feature_p6 | arch_feature_sse2,
	arch_core        = 10 | arch_feature_intel | arch_feature_p6 | arch_feature_sse3,
	arch_prescott    = 11 | arch_feature_netburst | arch_feature_p6 | arch_feature_sse3,
	arch_core2       = 12 | arch_feature_intel | arch_feature_p6 | arch_feature_64bit | arch_feature_ssse3,

	/* AMD CPU's */
	arch_k6          = 13 | arch_feature_amd | arch_feature_mmx,
	arch_k6_2        = 14 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNow,
	arch_k6_3        = 15 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNow,
	arch_athlon      = 16 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNowE | arch_feature_p6,
	arch_athlon_xp   = 17 | arch_feature_amd | arch_feature_sse1 | arch_feature_3DNowE | arch_feature_p6,
	arch_opteron     = 18 | arch_feature_amd | arch_feature_64bit | arch_feature_3DNowE | arch_feature_p6,

	/* other */
	arch_winchip_c6  = 19 | arch_feature_mmx,
	arch_winchip2    = 20 | arch_feature_mmx | arch_feature_3DNow,
	arch_c3          = 21 | arch_feature_mmx | arch_feature_3DNow,
	arch_c3_2        = 22 | arch_feature_sse1,  /* really no 3DNow! */
};

/** checks for l <= x <= h */
#define _IN_RANGE(x, l, h)  ((unsigned)((x) - (l)) <= (unsigned)((h) - (l)))

/** returns true if it's Intel architecture */
#define ARCH_INTEL(x)       (((x) & arch_feature_intel) != 0)

/** returns true if it's AMD architecture */
#define ARCH_AMD(x)         (((x) & arch_feature_amd) != 0)

/** return true if it's a Athlon/Opteron */
#define ARCH_ATHLON(x)      _IN_RANGE((x), arch_athlon, arch_opteron)

/** return true if the CPU has MMX support */
#define ARCH_MMX(x)         (((x) & arch_feature_mmx) != 0)

/** return true if the CPU has 3DNow! support */
#define ARCH_3DNow(x)       (((x) & arch_feature_3DNow) != 0)

/** return true if the CPU has P6 features (CMOV) */
#define IS_P6_ARCH(x)       (((x) & arch_feature_p6) != 0)

static cpu_support arch                 = arch_generic;
static cpu_support opt_arch             = arch_pentium_4;
static int         use_sse2             = 0;
static int         opt_cc               = 1;
static int         opt_unsafe_floatconv = 0;

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "386",        arch_i386, },
	{ "486",        arch_i486, },
	{ "pentium",    arch_pentium, },
	{ "586",        arch_pentium, },
	{ "pentiumpro", arch_pentium_pro, },
	{ "686",        arch_pentium_pro, },
	{ "pentiummmx", arch_pentium_mmx, },
	{ "pentium2",   arch_pentium_2, },
	{ "p2",         arch_pentium_2, },
	{ "pentium3",   arch_pentium_3, },
	{ "p3",         arch_pentium_3, },
	{ "pentium4",   arch_pentium_4, },
	{ "p4",         arch_pentium_4, },
	{ "prescott",   arch_pentium_4, },
	{ "pentiumm",   arch_pentium_m, },
	{ "pm",         arch_pentium_m, },
	{ "core",       arch_core, },
	{ "yonah",      arch_core, },
	{ "merom",      arch_core2, },
	{ "core2",      arch_core2, },
	{ "k6",         arch_k6, },
	{ "k6-2",       arch_k6_2, },
	{ "k6-3",       arch_k6_2, },
	{ "athlon",     arch_athlon, },
	{ "athlon-xp",  arch_athlon_xp, },
	{ "athlon-mp",  arch_athlon_xp, },
	{ "athlon-4",   arch_athlon_xp, },
	{ "athlon64",   arch_opteron, },
	{ "k8",         arch_opteron, },
	{ "opteron",    arch_opteron, },
	{ "generic",    arch_generic, },
	{ NULL,         0 }
};

static lc_opt_enum_int_var_t arch_var = {
	(int*) &arch, arch_items
};

static lc_opt_enum_int_var_t opt_arch_var = {
	(int*) &opt_arch, arch_items
};

static const lc_opt_enum_int_items_t fp_unit_items[] = {
	{ "x87" ,    0 },
	{ "sse2",    1 },
	{ NULL,      0 }
};

static lc_opt_enum_int_var_t fp_unit_var = {
	&use_sse2, fp_unit_items
};

static const lc_opt_table_entry_t ia32_architecture_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",        "select the instruction architecture",
	                    &arch_var),
	LC_OPT_ENT_ENUM_INT("opt",         "optimize for instruction architecture",
	                    &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpunit",      "select the floating point unit",
	                    &fp_unit_var),
	LC_OPT_ENT_NEGBIT("nooptcc",       "do not optimize calling convention",
	                  &opt_cc, 1),
	LC_OPT_ENT_BIT("unsafe_floatconv", "do unsage floating point controlword "
	               "optimisations", &opt_unsafe_floatconv, 1),
	LC_OPT_LAST
};

typedef struct insn_const {
	int add_cost;       /**< cost of an add instruction */
	int lea_cost;       /**< cost of a lea instruction */
	int const_shf_cost; /**< cost of a constant shift instruction */
	int cost_mul_start; /**< starting cost of a multiply instruction */
	int cost_mul_bit;   /**< cost of multiply for every set bit */
} insn_const;

/* costs for the i386 */
static const insn_const i386_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	2,   /* cost of a constant shift instruction */
	6,   /* starting cost of a multiply instruction */
	1    /* cost of multiply for every set bit */
};

/* costs for the i486 */
static const insn_const i486_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	2,   /* cost of a constant shift instruction */
	12,  /* starting cost of a multiply instruction */
	1    /* cost of multiply for every set bit */
};

/* costs for the Pentium */
static const insn_const pentium_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	11,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Pentium Pro */
static const insn_const pentiumpro_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the K6 */
static const insn_const k6_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Athlon */
static const insn_const athlon_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	5,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Pentium 4 */
static const insn_const pentium4_cost = {
	1,   /* cost of an add instruction */
	3,   /* cost of a lea instruction */
	4,   /* cost of a constant shift instruction */
	15,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Core */
static const insn_const core_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	10,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the generic */
static const insn_const generic_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

static const insn_const *arch_costs = &generic_cost;

static void set_arch_costs(void)
{
	switch (opt_arch) {
	case arch_i386:
		arch_costs = &i386_cost;
		break;
	case arch_i486:
		arch_costs = &i486_cost;
		break;
	case arch_pentium:
	case arch_pentium_mmx:
		arch_costs = &pentium_cost;
		break;
	case arch_pentium_pro:
	case arch_pentium_2:
	case arch_pentium_3:
		arch_costs = &pentiumpro_cost;
		break;
	case arch_pentium_4:
		arch_costs = &pentium4_cost;
		break;
	case arch_pentium_m:
		arch_costs = &pentiumpro_cost;
		break;
	case arch_core:
		arch_costs = &core_cost;
		break;
	case arch_prescott:
		arch_costs = &pentium4_cost;
		break;
	case arch_core2:
		arch_costs = &core_cost;
		break;
	case arch_k6:
	case arch_k6_2:
		arch_costs = &k6_cost;
		break;
	case arch_athlon:
	case arch_athlon_xp:
	case arch_opteron:
		arch_costs = &athlon_cost;
		break;
	case arch_generic:
	default:
		arch_costs = &generic_cost;
	}
}

/**
 * Evaluate a given simple instruction.
 */
int ia32_evaluate_insn(insn_kind kind, tarval *tv) {
	int cost;

	switch (kind) {
	case MUL:
		cost =  arch_costs->cost_mul_start;
		if (arch_costs->cost_mul_bit > 0) {
			char *bitstr = get_tarval_bitpattern(tv);
			int i;

			for (i = 0; bitstr[i] != '\0'; ++i) {
				if (bitstr[i] == '1') {
					cost += arch_costs->cost_mul_bit;
				}
			}
			free(bitstr);
		}
		return cost;
	case LEA:
		return arch_costs->lea_cost;
	case ADD:
	case SUB:
		return arch_costs->add_cost;
	case SHIFT:
		return arch_costs->const_shf_cost;
	case ZERO:
		return arch_costs->add_cost;
	default:
		return 1;
	}
}



void ia32_setup_cg_config(void)
{
	memset(&ia32_cg_config, 0, sizeof(ia32_cg_config));

	/* on newer intel cpus mov, pop is often faster then leave although it has a
	 * longer opcode */
	ia32_cg_config.use_leave            = !ARCH_INTEL(opt_arch)
	                                       || !IS_P6_ARCH(opt_arch);
	/* P4s don't like inc/decs because they only partially write the flags
	   register which produces false dependencies */
	ia32_cg_config.use_incdec           = (opt_arch != arch_pentium_4);
	ia32_cg_config.use_sse2             = use_sse2;
	ia32_cg_config.use_ffreep           = ARCH_ATHLON(opt_arch);
	ia32_cg_config.use_ftst             = !IS_P6_ARCH(arch);
	ia32_cg_config.use_femms            = ARCH_ATHLON(opt_arch)
	                                      && ARCH_MMX(arch) && ARCH_AMD(arch);
	ia32_cg_config.use_fucomi           = IS_P6_ARCH(arch);
	ia32_cg_config.use_cmov             = IS_P6_ARCH(arch);
	ia32_cg_config.optimize_cc          = opt_cc;
	ia32_cg_config.use_unsafe_floatconv = opt_unsafe_floatconv;

	if(opt_arch == arch_i386) {
		ia32_cg_config.function_alignment = 2;
	} else if(opt_arch == arch_i486) {
		ia32_cg_config.function_alignment = 4;
	} else if(opt_arch == arch_k6) {
		ia32_cg_config.function_alignment = 5;
		ia32_cg_config.label_alignment    = 5;
	} else {
		ia32_cg_config.function_alignment = 4;
		ia32_cg_config.label_alignment    = 4;
	}

	if(opt_arch == arch_i386 || opt_arch == arch_i486) {
		ia32_cg_config.label_alignment_factor = -1;
	} else if(ARCH_AMD(opt_arch)) {
		ia32_cg_config.label_alignment_factor = 3;
	} else {
		ia32_cg_config.label_alignment_factor = 2;
	}

	set_arch_costs();
}

void ia32_init_architecture(void)
{
	lc_opt_entry_t *be_grp, *ia32_grp;

	memset(&ia32_cg_config, 0, sizeof(ia32_cg_config));

	be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_architecture_options);
}
