/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       ia32 architecture variants
 * @author      Michael Beck, Matthias Braun
 */
#include "ia32_architecture.h"

#include <stdbool.h>
#include <string.h>

#include "irmode_t.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "irtools.h"
#include "tv.h"
#include "x86_architecture.h"

ia32_code_gen_config_t ia32_cg_config;

static bool              opt_size             = false;
static bool              emit_machcode        = false;
static bool              use_softfloat        = false;
static bool              use_cmov             = false;
static bool              use_sse              = false;
static bool              use_sse2             = false;
static bool              use_sse3             = false;
static bool              use_sse4             = false;
static bool              use_sse4_1           = false;
static bool              use_sse4_2           = false;
static bool              use_sse4a            = false;
static bool              use_sse5             = false;
static bool              use_ssse3            = false;
static x86_cpu           arch_val             = cpu_generic;
static x86_cpu           opt_arch_val         = cpu_autodetect;
static cpu_arch_features arch;
static cpu_arch_features opt_arch;
static int               fpu_arch             = 0;
static bool              opt_cc               = true;
static bool              opt_unsafe_floatconv = false;

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "i386",           cpu_i386 },
	{ "i486",           cpu_i486 },
	{ "i586",           cpu_pentium },
	{ "pentium",        cpu_pentium },
	{ "pentium-mmx",    cpu_pentium_mmx },
	{ "i686",           cpu_pentium_pro },
	{ "pentiumpro",     cpu_pentium_pro },
	{ "pentium2",       cpu_pentium_2 },
	{ "p2",             cpu_pentium_2 },
	{ "pentium3",       cpu_pentium_3 },
	{ "pentium3m",      cpu_pentium_3 },
	{ "p3",             cpu_pentium_3 },
	{ "pentium-m",      cpu_pentium_m },
	{ "pm",             cpu_pentium_m },
	{ "pentium4",       cpu_pentium_4 },
	{ "pentium4m",      cpu_pentium_4 },
	{ "p4",             cpu_pentium_4 },
	{ "prescott",       cpu_prescott },
	{ "nocona",         cpu_nocona },
	{ "merom",          cpu_core2 },
	{ "core2",          cpu_core2 },
	{ "penryn",         cpu_penryn },
	{ "atom",           cpu_atom },
	{ "bonnell",        cpu_atom },
	{ "silvermont",     cpu_silvermont },
	{ "slm",            cpu_silvermont },
	{ "goldmont",       cpu_goldmont },
	{ "goldmont-plus",  cpu_goldmont_plus },
	{ "tremont",        cpu_tremont },
	{ "knl",            cpu_knl },
	{ "knm",            cpu_knm },
	{ "nehalem",        cpu_nehalem },
	{ "corei7",         cpu_nehalem },
	{ "westmere",       cpu_westmere },
	{ "sandybridge",    cpu_sandybridge },
	{ "corei7-avx",     cpu_sandybridge },
	{ "ivybridge",      cpu_ivybridge },
	{ "core-avx-i",     cpu_ivybridge },
	{ "haswell",        cpu_haswell },
	{ "core-avx2",      cpu_haswell },
	{ "broadwell",      cpu_broadwell },
	{ "skylake",        cpu_skylake },
	{ "skylake-avx512", cpu_skylake_avx512 },
	{ "skx",            cpu_skylake_avx512 },
	{ "cascadelake",    cpu_cascade_lake },
	{ "cooperlake",     cpu_cooperlake },
	{ "cannonlake",     cpu_cannonlake },
	{ "icelake-client", cpu_icelake_client },
	{ "icelake-server", cpu_icelake_server },
	{ "tigerlake",      cpu_tigerlake },
	{ "sapphirerapids", cpu_sapphirerapids },
	{ "alderlake",      cpu_alderlake },

	{ "k6",             cpu_k6 },
	{ "k6-2",           cpu_k6_PLUS },
	{ "k6-3",           cpu_k6_PLUS },
	{ "geode",          cpu_geode },
	{ "athlon",         cpu_athlon_old },
	{ "athlon-tbird",   cpu_athlon },
	{ "athlon-4",       cpu_athlon },
	{ "athlon-xp",      cpu_athlon },
	{ "athlon-mp",      cpu_athlon },
	{ "athlon64",       cpu_athlon64 },
	{ "k8",             cpu_k8 },
	{ "opteron",        cpu_k8 },
	{ "athlon-fx",      cpu_k8 },
	{ "k8-sse3",        cpu_k8_sse3 },
	{ "opteron-sse3",   cpu_k8_sse3 },
	{ "athlon64-sse3",  cpu_k8_sse3 },
	{ "k10",            cpu_k10 },
	{ "barcelona",      cpu_k10 },
	{ "amdfam10",       cpu_k10 },
	{ "btver1",         cpu_btver1 },
	{ "btver2",         cpu_btver2 },
	{ "bdver1",         cpu_bdver1 },
	{ "bdver2",         cpu_bdver2 },
	{ "bdver3",         cpu_bdver3 },
	{ "bdver4",         cpu_bdver4 },
	{ "znver1",         cpu_znver1 },
	{ "znver2",         cpu_znver2 },
	{ "znver3",         cpu_znver3 },

	{ "winchip-c6",     cpu_winchip_c6, },
	{ "winchip2",       cpu_winchip2 },
	{ "c3",             cpu_c3 },
	{ "c3-2",           cpu_c3_2 },

	{ "generic",        cpu_generic },
	{ "generic32",      cpu_generic },

#ifdef NATIVE_X86
	{ "native",       cpu_autodetect },
#endif

	{ NULL,           0 }
};

static lc_opt_enum_int_var_t arch_var = {
	(int*) &arch_val, arch_items
};

static lc_opt_enum_int_var_t opt_arch_var = {
	(int*) &opt_arch_val, arch_items
};

typedef enum ia32_fpu_mode_t {
	IA32_FPU_X87,
	IA32_FPU_SSE2,
	IA32_FPU_SOFTFLOAT,
} ia32_fpu_mode_t;

static const lc_opt_enum_int_items_t fp_unit_items[] = {
	{ "387" ,      IA32_FPU_X87 },
	{ "sse",       IA32_FPU_SSE2 },
	{ "softfloat", IA32_FPU_SOFTFLOAT },
	{ NULL,        IA32_FPU_X87 }
};

static lc_opt_enum_int_var_t fp_unit_var = {
	&fpu_arch, fp_unit_items
};

static const lc_opt_table_entry_t ia32_architecture_options[] = {
	LC_OPT_ENT_BOOL    ("size",             "optimize for size",                                  &opt_size),
	LC_OPT_ENT_ENUM_INT("arch",             "select the instruction architecture",                &arch_var),
	LC_OPT_ENT_ENUM_INT("tune",             "optimize for instruction architecture",              &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpmath",           "select the floating point unit",                     &fp_unit_var),
	LC_OPT_ENT_BOOL    ("optcc",            "optimize calling convention",                        &opt_cc),
	LC_OPT_ENT_BOOL    ("unsafe_floatconv", "do unsafe floating point controlword optimizations", &opt_unsafe_floatconv),
	LC_OPT_ENT_BOOL    ("machcode",         "output machine code instead of assembler",           &emit_machcode),
	LC_OPT_ENT_BOOL    ("soft-float",       "equivalent to fpmath=softfloat",                     &use_softfloat),
	LC_OPT_ENT_BOOL    ("cmov",             "use conditional move",                               &use_cmov),
	LC_OPT_ENT_BOOL    ("sse",              "gcc compatibility",                                  &use_sse),
	LC_OPT_ENT_BOOL    ("sse2",             "gcc compatibility",                                  &use_sse2),
	LC_OPT_ENT_BOOL    ("sse3",             "gcc compatibility",                                  &use_sse3),
	LC_OPT_ENT_BOOL    ("sse4",             "gcc compatibility",                                  &use_sse4),
	LC_OPT_ENT_BOOL    ("sse4.1",           "gcc compatibility",                                  &use_sse4_1),
	LC_OPT_ENT_BOOL    ("sse4.2",           "gcc compatibility",                                  &use_sse4_2),
	LC_OPT_ENT_BOOL    ("sse4a",            "gcc compatibility",                                  &use_sse4a),
	LC_OPT_ENT_BOOL    ("sse5",             "gcc compatibility",                                  &use_sse5),
	LC_OPT_ENT_BOOL    ("ssse3",            "gcc compatibility",                                  &use_ssse3),
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
	4,   /* starting cost of a multiply instruction */
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

/* costs for the Opteron/K8 */
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
	switch (opt_arch.arch) {
	case arch_i386:      arch_costs = &i386_cost;       break;
	case arch_i486:      arch_costs = &i486_cost;       break;
	case arch_pentium:   arch_costs = &pentium_cost;    break;
	case arch_ppro:      arch_costs = &pentiumpro_cost; break;
	case arch_netburst:  arch_costs = &netburst_cost;   break;
	case arch_nocona:    arch_costs = &nocona_cost;     break;
	case arch_core2:     arch_costs = &core2_cost;      break;
	case arch_k6:        arch_costs = &k6_cost;         break;
	case arch_geode:     arch_costs = &geode_cost;      break;
	case arch_athlon:    arch_costs = &athlon_cost;     break;
	case arch_k8:        arch_costs = &k8_cost;         break;
	case arch_k10:       arch_costs = &k10_cost;        break;
	default:
	case arch_generic:   arch_costs = &generic32_cost;  break;
	}
}

/* Evaluate the costs of an instruction. */
int ia32_evaluate_insn(insn_kind kind, const ir_mode *mode, ir_tarval *tv)
{
	int cost;

	switch (kind) {
	case MUL:
		cost = arch_costs->cost_mul_start;
		if (arch_costs->cost_mul_bit > 0)
			cost += get_tarval_popcount(tv) * arch_costs->cost_mul_bit;
		if (get_mode_size_bits(mode) <= 32)
			return cost;
		/* 64bit mul supported, approx 4times of a 32bit mul*/
		return 4 * cost;
	case LEA:
		/* lea is only supported for 32 bit */
		if (get_mode_size_bits(mode) <= 32)
			return arch_costs->lea_cost;
		/* in 64bit mode, the Lea cost are at worst 2 shifts and one add */
		return 2 * arch_costs->add_cost + 2 * (2 * arch_costs->const_shf_cost);
	case ADD:
	case SUB:
		if (get_mode_size_bits(mode) <= 32)
			return arch_costs->add_cost;
		/* 64bit add/sub supported, double the cost */
		return 2 * arch_costs->add_cost;
	case SHIFT:
		if (get_mode_size_bits(mode) <= 32)
			return arch_costs->const_shf_cost;
		/* 64bit shift supported, double the cost */
		return 2 * arch_costs->const_shf_cost;
	case ZERO:
		return arch_costs->add_cost;
	default:
		return 1;
	}
}

void ia32_setup_cg_config(void)
{
	arch = cpu_arch_feature_defs[arch_val];
	opt_arch = cpu_arch_feature_defs[opt_arch_val];

	if (use_softfloat)
		fpu_arch = IA32_FPU_SOFTFLOAT;

#ifdef NATIVE_X86
	if (arch_val == cpu_autodetect) {
		arch = autodetect_arch();
		opt_arch = arch;
	}
#endif
	if (opt_arch_val == cpu_autodetect)
		opt_arch = arch;

	set_arch_costs();

	ia32_code_gen_config_t *const c = &ia32_cg_config;
	memset(c, 0, sizeof(*c));
	c->optimize_size        = opt_size != 0;
	/* on newer intel cpus mov, pop is often faster than leave although it has a
	 * longer opcode */
	c->use_leave            = arch_flags(opt_arch, arch_i386 | arch_all_amd | arch_core2) || opt_size;
	/* P4s don't like inc/decs because they only partially write the flags
	 * register which produces false dependencies */
	c->use_incdec           = !arch_flags(opt_arch, arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_geode) || opt_size;
	c->use_softfloat        = (fpu_arch & IA32_FPU_SOFTFLOAT) != 0;
	c->use_sse2             = (fpu_arch & IA32_FPU_SSE2) != 0 && feature_flags(arch, arch_feature_sse2);
	c->use_ffreep           = arch_flags(opt_arch, arch_athlon_plus);
	c->use_femms            = arch_flags(opt_arch, arch_athlon_plus) && feature_flags(arch, arch_feature_3DNow);
	c->use_fucomi           = feature_flags(arch, arch_feature_fcmov);
	c->use_cmov             = feature_flags(arch, arch_feature_cmov) && use_cmov;
	c->use_modeD_moves      = arch_flags(opt_arch, arch_generic | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_ppro | arch_geode);
	c->use_add_esp_4        = arch_flags(opt_arch, arch_generic | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_geode) && !opt_size;
	c->use_add_esp_8        = arch_flags(opt_arch, arch_generic | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_ppro | arch_geode | arch_i386 | arch_i486) && !opt_size;
	c->use_sub_esp_4        = arch_flags(opt_arch, arch_generic | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_ppro) && !opt_size;
	c->use_sub_esp_8        = arch_flags(opt_arch, arch_generic | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2_plus | arch_atom_plus | arch_ppro | arch_i386 | arch_i486) && !opt_size;
	c->use_imul_mem_imm32   = !arch_flags(opt_arch, arch_k8 | arch_k10) || opt_size;
	c->use_pxor             = arch_flags(opt_arch, arch_netburst);
	c->use_mov_0            = arch_flags(opt_arch, arch_k6) && !opt_size;
	c->use_short_sex_eax    = !arch_flags(opt_arch, arch_k6) || opt_size;
	c->use_pad_return       = arch_flags(opt_arch, arch_athlon | arch_k8 | arch_k10) && !opt_size;
	c->use_bt               = arch_flags(opt_arch, arch_core2_plus | arch_atom_plus | arch_athlon_plus) || opt_size;
	c->use_fisttp           = feature_flags(opt_arch, arch_feature_sse3) && feature_flags(arch, arch_feature_sse3);
	c->use_sse_prefetch     = feature_flags(arch, (arch_feature_3DNowE | arch_feature_sse1));
	c->use_3dnow_prefetch   = feature_flags(arch, arch_feature_3DNow);
	c->use_popcnt           = feature_flags(arch, arch_feature_popcnt);
	c->use_bswap            = (arch.arch) >= arch_i486;
	c->use_cmpxchg          = (arch.arch) != arch_i386;
	c->optimize_cc          = opt_cc;
	c->use_unsafe_floatconv = opt_unsafe_floatconv;
	c->emit_machcode        = emit_machcode;

	c->function_alignment       = arch_costs->function_alignment;
	c->label_alignment          = arch_costs->label_alignment;
	c->label_alignment_max_skip = arch_costs->label_alignment_max_skip;

	c->label_alignment_factor =
		arch_flags(opt_arch, arch_i386 | arch_i486) || opt_size ? 0 :
		arch_flags(opt_arch, arch_all_amd) ? 3 :
		2;
}

void ia32_init_architecture(void)
{
	memset(&ia32_cg_config, 0, sizeof(ia32_cg_config));

	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");
	lc_opt_add_table(ia32_grp, ia32_architecture_options);
}
