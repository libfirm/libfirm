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
 * @brief       ia32 architecture variants
 * @author      Michael Beck, Matthias Braun
 * @version     $Id: bearch_ia32_t.h 16363 2007-10-25 23:27:07Z beck $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "irtools.h"

#include "bearch_ia32_t.h"
#include "ia32_architecture.h"

ia32_code_gen_config_t  ia32_cg_config;

/**
 * CPU architectures and features.
 */
enum cpu_arch_features {
	arch_generic32        = 0x00000001, /**< no specific architecture */

	arch_i386             = 0x00000002, /**< i386 architecture */
	arch_i486             = 0x00000004, /**< i486 architecture */
	arch_pentium          = 0x00000008, /**< Pentium architecture */
	arch_ppro             = 0x00000010, /**< PentiumPro architecture */
	arch_netburst         = 0x00000020, /**< Netburst architecture */
	arch_nocona           = 0x00000040, /**< Nocona architecture */
	arch_core2            = 0x00000080, /**< Core2 architecture */
	arch_atom             = 0x00000100, /**< Atom architecture */

	arch_k6               = 0x00000200, /**< k6 architecture */
	arch_geode            = 0x00000400, /**< Geode architecture */
	arch_athlon           = 0x00000800, /**< Athlon architecture */
	arch_k8               = 0x00001000, /**< K8/Opteron architecture */
	arch_k10              = 0x00002000, /**< K10/Barcelona architecture */

	arch_mask             = 0x00003FFF,

	arch_athlon_plus      = arch_athlon | arch_k8 | arch_k10,
	arch_all_amd          = arch_k6 | arch_geode | arch_athlon_plus,

	arch_feature_mmx      = 0x00004000,                      /**< MMX instructions */
	arch_feature_p6_insn  = 0x00008000,                      /**< PentiumPro instructions */
	arch_feature_sse1     = 0x00010000 | arch_feature_mmx,   /**< SSE1 instructions, include MMX */
	arch_feature_sse2     = 0x00020000 | arch_feature_sse1,  /**< SSE2 instructions, include SSE1 */
	arch_feature_sse3     = 0x00040000 | arch_feature_sse2,  /**< SSE3 instructions, include SSE2 */
	arch_feature_ssse3    = 0x00080000 | arch_feature_sse3,  /**< SSSE3 instructions, include SSE3 */
	arch_feature_3DNow    = 0x00100000,                      /**< 3DNow! instructions */
	arch_feature_3DNowE   = 0x00200000 | arch_feature_3DNow, /**< Enhanced 3DNow! instructions */
	arch_feature_64bit    = 0x00400000 | arch_feature_sse2,  /**< x86_64 support, includes SSE2 */
};

#define FLAGS(x, f) (((x) & (f)) != 0)

/**
 * CPU's.
 */
enum cpu_support {
	cpu_generic     = arch_generic32,

	/* intel CPU's */
	cpu_i386        = arch_i386,
	cpu_i486        = arch_i486,
	cpu_pentium     = arch_pentium,
	cpu_pentium_mmx = arch_pentium | arch_feature_mmx,
	cpu_pentium_pro = arch_ppro | arch_feature_p6_insn,
	cpu_pentium_2   = arch_ppro | arch_feature_p6_insn | arch_feature_mmx,
	cpu_pentium_3   = arch_ppro | arch_feature_p6_insn | arch_feature_sse1,
	cpu_pentium_m   = arch_ppro | arch_feature_p6_insn | arch_feature_sse2,
	cpu_pentium_4   = arch_netburst | arch_feature_p6_insn | arch_feature_sse2,
	cpu_prescott    = arch_nocona | arch_feature_p6_insn | arch_feature_sse3,
	cpu_nocona      = arch_nocona | arch_feature_p6_insn | arch_feature_64bit | arch_feature_sse3,
	cpu_core2       = arch_core2 | arch_feature_p6_insn | arch_feature_64bit | arch_feature_ssse3,

	/* AMD CPU's */
	cpu_k6          = arch_k6 | arch_feature_mmx,
	cpu_k6_PLUS     = arch_k6 | arch_feature_mmx | arch_feature_3DNow,
	cpu_geode       = arch_geode | arch_feature_sse1 | arch_feature_3DNowE,
	cpu_athlon      = arch_athlon | arch_feature_sse1 | arch_feature_3DNowE | arch_feature_p6_insn,
	cpu_athlon64    = arch_athlon | arch_feature_sse2 | arch_feature_3DNowE | arch_feature_p6_insn | arch_feature_64bit,
	cpu_k8          = arch_k8 | arch_feature_sse2 | arch_feature_3DNowE | arch_feature_p6_insn | arch_feature_64bit,
	cpu_k8_sse3     = arch_k8 | arch_feature_sse3 | arch_feature_3DNowE | arch_feature_p6_insn | arch_feature_64bit,
	cpu_k10         = arch_k10 | arch_feature_sse3 | arch_feature_3DNowE | arch_feature_p6_insn | arch_feature_64bit,

	/* other CPU's */
	cpu_winchip_c6  = arch_i486 | arch_feature_mmx,
	cpu_winchip2    = arch_i486 | arch_feature_mmx | arch_feature_3DNow,
	cpu_c3          = arch_i486 | arch_feature_mmx | arch_feature_3DNow,
	cpu_c3_2        = arch_ppro | arch_feature_sse1,  /* really no 3DNow! */
};

static int         opt_size             = 0;
static cpu_support arch                 = cpu_generic;
static cpu_support opt_arch             = cpu_core2;
static int         use_sse2             = 0;
static int         opt_cc               = 1;
static int         opt_unsafe_floatconv = 0;

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "i386",         cpu_i386 },
	{ "i486",         cpu_i486 },
	{ "i586",         cpu_pentium },
	{ "pentium",      cpu_pentium },
	{ "pentium-mmx",  cpu_pentium_mmx },
	{ "i686",         cpu_pentium_pro },
	{ "pentiumpro",   cpu_pentium_pro },
	{ "pentium2",     cpu_pentium_2 },
	{ "p2",           cpu_pentium_2 },
	{ "pentium3",     cpu_pentium_3 },
	{ "pentium3m",    cpu_pentium_3 },
	{ "p3",           cpu_pentium_3 },
	{ "pentium-m",    cpu_pentium_m },
	{ "pm",           cpu_pentium_m },
	{ "pentium4",     cpu_pentium_4 },
	{ "pentium4m",    cpu_pentium_4 },
	{ "p4",           cpu_pentium_4 },
	{ "prescott",     cpu_prescott },
	{ "nocona",       cpu_nocona },
	{ "merom",        cpu_core2 },
	{ "core2",        cpu_core2 },

	{ "k6",           cpu_k6 },
	{ "k6-2",         cpu_k6_PLUS },
	{ "k6-3",         cpu_k6_PLUS },
	{ "geode",        cpu_geode },
	{ "athlon",       cpu_athlon },
	{ "athlon-tbird", cpu_athlon },
	{ "athlon-4",     cpu_athlon },
	{ "athlon-xp",    cpu_athlon },
	{ "athlon-mp",    cpu_athlon },
	{ "athlon64",     cpu_athlon64 },
	{ "k8",           cpu_k8 },
	{ "opteron",      cpu_k8 },
	{ "athlon-fx",    cpu_k8 },
	{ "k8-sse3",      cpu_k8_sse3 },
	{ "opteron-sse3", cpu_k8_sse3 },
	{ "k10",          cpu_k10 },
	{ "barcelona",    cpu_k10 },
	{ "amdfam10",     cpu_k10 },

	{ "winchip-c6",   cpu_winchip_c6, },
	{ "winchip2",     cpu_winchip2 },
	{ "c3",           cpu_c3 },
	{ "c3-2",         cpu_c3_2 },

	{ "generic",      cpu_generic },
	{ "generic32",    cpu_generic },
	{ NULL,           0 }
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
	LC_OPT_ENT_BOOL("size",            "optimize for size", &opt_size),
	LC_OPT_ENT_ENUM_INT("arch",        "select the instruction architecture",
	                    &arch_var),
	LC_OPT_ENT_ENUM_INT("opt",         "optimize for instruction architecture",
	                    &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpunit",      "select the floating point unit",
	                    &fp_unit_var),
	LC_OPT_ENT_NEGBIT("nooptcc",       "do not optimize calling convention",
	                  &opt_cc, 1),
	LC_OPT_ENT_BIT("unsafe_floatconv", "do unsafe floating point controlword "
	               "optimisations", &opt_unsafe_floatconv, 1),
	LC_OPT_LAST
};

typedef struct insn_const {
	int      add_cost;                 /**< cost of an add instruction */
	int      lea_cost;                 /**< cost of a lea instruction */
	int      const_shf_cost;           /**< cost of a constant shift instruction */
	int      cost_mul_start;           /**< starting cost of a multiply instruction */
	int      cost_mul_bit;             /**< cost of multiply for every set bit */
	unsigned function_alignment;       /**< logarithm for alignment of function labels */
	unsigned label_alignment;          /**< logarithm for alignment of loops labels */
	unsigned label_alignment_max_skip; /**< maximum skip for alignment of loops labels */
} insn_const;

/* costs for optimizing for size */
static const insn_const size_cost = {
	2,   /* cost of an add instruction */
	3,   /* cost of a lea instruction */
	3,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	0,   /* logarithm for alignment of function labels */
	0,   /* logarithm for alignment of loops labels */
	0,   /* maximum skip for alignment of loops labels */
};

/* costs for the i386 */
static const insn_const i386_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	3,   /* cost of a constant shift instruction */
	9,   /* starting cost of a multiply instruction */
	1,   /* cost of multiply for every set bit */
	2,   /* logarithm for alignment of function labels */
	2,   /* logarithm for alignment of loops labels */
	3,   /* maximum skip for alignment of loops labels */
};

/* costs for the i486 */
static const insn_const i486_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	2,   /* cost of a constant shift instruction */
	12,  /* starting cost of a multiply instruction */
	1,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	15,  /* maximum skip for alignment of loops labels */
};

/* costs for the Pentium */
static const insn_const pentium_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	11,  /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Pentium Pro */
static const insn_const pentiumpro_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	10,  /* maximum skip for alignment of loops labels */
};

/* costs for the K6 */
static const insn_const k6_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	5,   /* logarithm for alignment of function labels */
	5,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Geode */
static const insn_const geode_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	7,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	0,   /* logarithm for alignment of function labels */
	0,   /* logarithm for alignment of loops labels */
	0,   /* maximum skip for alignment of loops labels */
};

/* costs for the Athlon */
static const insn_const athlon_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	5,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Opteron/K8/K10 */
static const insn_const k8_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the K10 */
static const insn_const k10_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	5,   /* logarithm for alignment of function labels */
	5,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Pentium 4 */
static const insn_const netburst_cost = {
	1,   /* cost of an add instruction */
	3,   /* cost of a lea instruction */
	4,   /* cost of a constant shift instruction */
	15,  /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Nocona and Core */
static const insn_const nocona_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	10,  /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

/* costs for the Core2 */
static const insn_const core2_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	10,  /* maximum skip for alignment of loops labels */
};

/* costs for the generic32 */
static const insn_const generic32_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0,   /* cost of multiply for every set bit */
	4,   /* logarithm for alignment of function labels */
	4,   /* logarithm for alignment of loops labels */
	7,   /* maximum skip for alignment of loops labels */
};

static const insn_const *arch_costs = &generic32_cost;

static void set_arch_costs(void)
{
	if (opt_size) {
		arch_costs = &size_cost;
		return;
	}
	switch (opt_arch & arch_mask) {
	case arch_i386:
		arch_costs = &i386_cost;
		break;
	case arch_i486:
		arch_costs = &i486_cost;
		break;
	case arch_pentium:
		arch_costs = &pentium_cost;
		break;
	case arch_ppro:
		arch_costs = &pentiumpro_cost;
		break;
	case arch_netburst:
		arch_costs = &netburst_cost;
		break;
	case arch_nocona:
		arch_costs = &nocona_cost;
		break;
	case arch_core2:
		arch_costs = &core2_cost;
		break;
	case arch_k6:
		arch_costs = &k6_cost;
		break;
	case arch_geode:
		arch_costs = &geode_cost;
		break;
	case arch_athlon:
		arch_costs = &athlon_cost;
		break;
	case arch_k8:
		arch_costs = &k8_cost;
		break;
	case arch_k10:
		arch_costs = &k10_cost;
		break;
	case arch_generic32:
	default:
		arch_costs = &generic32_cost;
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

	set_arch_costs();

	ia32_cg_config.optimize_size        = opt_size != 0;
	/* on newer intel cpus mov, pop is often faster then leave although it has a
	 * longer opcode */
	ia32_cg_config.use_leave            = FLAGS(opt_arch, arch_i386 | arch_all_amd | arch_core2);
	/* P4s don't like inc/decs because they only partially write the flags
	   register which produces false dependencies */
	ia32_cg_config.use_incdec           = !FLAGS(opt_arch, arch_netburst | arch_nocona | arch_geode) || opt_size;
	ia32_cg_config.use_sse2             = use_sse2;
	ia32_cg_config.use_ffreep           = FLAGS(opt_arch, arch_athlon_plus);
	ia32_cg_config.use_ftst             = !FLAGS(arch, arch_feature_p6_insn);
	ia32_cg_config.use_femms            = FLAGS(opt_arch, arch_athlon_plus) &&
	                                      FLAGS(arch, arch_feature_mmx | arch_all_amd);
	ia32_cg_config.use_fucomi           = FLAGS(arch, arch_feature_p6_insn);
	ia32_cg_config.use_cmov             = FLAGS(arch, arch_feature_p6_insn);
	ia32_cg_config.use_modeD_moves      = FLAGS(opt_arch, arch_athlon_plus | arch_geode | arch_ppro |
	                                            arch_netburst | arch_nocona | arch_core2 | arch_generic32);
	ia32_cg_config.use_add_esp_4        = FLAGS(opt_arch, arch_geode | arch_athlon_plus |
	                                            arch_netburst | arch_nocona | arch_core2 | arch_generic32) &&
	                                      !opt_size;
	ia32_cg_config.use_add_esp_8        = FLAGS(opt_arch, arch_geode | arch_athlon_plus |
	                                            arch_i386 | arch_i486 | arch_ppro | arch_netburst |
	                                            arch_nocona | arch_core2 | arch_generic32) &&
	                                      !opt_size;
	ia32_cg_config.use_sub_esp_4        = FLAGS(opt_arch, arch_athlon_plus | arch_ppro |
	                                            arch_netburst | arch_nocona | arch_core2 | arch_generic32) &&
	                                      !opt_size;
	ia32_cg_config.use_sub_esp_8        = FLAGS(opt_arch, arch_athlon_plus | arch_i386 | arch_i486 |
	                                            arch_ppro | arch_netburst | arch_nocona | arch_core2 | arch_generic32) &&
	                                      !opt_size;
	ia32_cg_config.use_imul_mem_imm32   = !FLAGS(opt_arch, arch_k8 | arch_k10) || opt_size;
	ia32_cg_config.use_pxor             = FLAGS(opt_arch, arch_netburst);
	ia32_cg_config.use_mov_0            = FLAGS(opt_arch, arch_k6) && !opt_size;
	ia32_cg_config.use_pad_return       = FLAGS(opt_arch, arch_athlon_plus | arch_core2 | arch_generic32) && !opt_size;
	ia32_cg_config.use_bt               = FLAGS(opt_arch, arch_core2) || opt_size;
	ia32_cg_config.optimize_cc          = opt_cc;
	ia32_cg_config.use_unsafe_floatconv = opt_unsafe_floatconv;

	ia32_cg_config.function_alignment       = arch_costs->function_alignment;
	ia32_cg_config.label_alignment          = arch_costs->label_alignment;
	ia32_cg_config.label_alignment_max_skip = arch_costs->label_alignment_max_skip;

	if (opt_arch & (arch_i386 | arch_i486)) {
		ia32_cg_config.label_alignment_factor = 0;
	} else if (opt_arch & arch_all_amd) {
		ia32_cg_config.label_alignment_factor = 3;
	} else {
		ia32_cg_config.label_alignment_factor = 2;
	}
}

void ia32_init_architecture(void)
{
	lc_opt_entry_t *be_grp, *ia32_grp;

	memset(&ia32_cg_config, 0, sizeof(ia32_cg_config));

	be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_architecture_options);
}
