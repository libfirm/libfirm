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

#undef NATIVE_X86

#ifdef _MSC_VER
#if defined(_M_IX86) || defined(_M_X64)
#include <intrin.h>
#define NATIVE_X86
#endif
#else
#if defined(__i386__) || defined(__x86_64__)
#define NATIVE_X86
#endif
#endif

ia32_code_gen_config_t ia32_cg_config;

/**
 * CPU architectures and features.
 */
typedef enum cpu_arch_features {
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

	arch_feature_mmx      = 0x00004000, /**< MMX instructions */
	arch_feature_cmov     = 0x00008000, /**< cmov instructions */
	arch_feature_p6_insn  = 0x00010000, /**< PentiumPro instructions */
	arch_feature_sse1     = 0x00020000, /**< SSE1 instructions */
	arch_feature_sse2     = 0x00040000, /**< SSE2 instructions */
	arch_feature_sse3     = 0x00080000, /**< SSE3 instructions */
	arch_feature_ssse3    = 0x00100000, /**< SSSE3 instructions */
	arch_feature_3DNow    = 0x00200000, /**< 3DNow! instructions */
	arch_feature_3DNowE   = 0x00400000, /**< Enhanced 3DNow! instructions */
	arch_feature_64bit    = 0x00800000, /**< x86_64 support */
	arch_feature_sse4_1   = 0x01000000, /**< SSE4.1 instructions */
	arch_feature_sse4_2   = 0x02000000, /**< SSE4.2 instructions */
	arch_feature_sse4a    = 0x04000000, /**< SSE4a instructions */
	arch_feature_popcnt   = 0x08000000, /**< popcnt instruction */

	arch_mmx_insn     = arch_feature_mmx,                         /**< MMX instructions */
	arch_sse1_insn    = arch_feature_sse1   | arch_mmx_insn,      /**< SSE1 instructions, include MMX */
	arch_sse2_insn    = arch_feature_sse2   | arch_sse1_insn,     /**< SSE2 instructions, include SSE1 */
	arch_sse3_insn    = arch_feature_sse3   | arch_sse2_insn,     /**< SSE3 instructions, include SSE2 */
	arch_ssse3_insn   = arch_feature_ssse3  | arch_sse3_insn,     /**< SSSE3 instructions, include SSE3 */
	arch_sse4_1_insn  = arch_feature_sse4_1 | arch_ssse3_insn,    /**< SSE4.1 instructions, include SSSE3 */
	arch_sse4_2_insn  = arch_feature_sse4_2 | arch_sse4_1_insn,   /**< SSE4.2 instructions, include SSE4.1 */
	arch_sse4a_insn   = arch_feature_sse4a  | arch_ssse3_insn,    /**< SSE4a instructions, include SSSE3 */

	arch_3DNow_insn   = arch_feature_3DNow  | arch_feature_mmx,   /**< 3DNow! instructions, including MMX */
	arch_3DNowE_insn  = arch_feature_3DNowE | arch_3DNow_insn,    /**< Enhanced 3DNow! instructions */
	arch_64bit_insn   = arch_feature_64bit  | arch_sse2_insn,     /**< x86_64 support, includes SSE2 */

	cpu_generic             = arch_generic32,

	/* intel CPUs */
	cpu_i386                = arch_i386,
	cpu_i486                = arch_i486,
	cpu_pentium             = arch_pentium,
	cpu_pentium_mmx         = arch_pentium | arch_mmx_insn,
	cpu_pentium_pro_generic = arch_ppro | arch_feature_p6_insn,
	cpu_pentium_pro         = arch_ppro | arch_feature_cmov | arch_feature_p6_insn,
	cpu_pentium_2           = arch_ppro | arch_feature_cmov | arch_feature_p6_insn | arch_mmx_insn,
	cpu_pentium_3           = arch_ppro | arch_feature_cmov | arch_feature_p6_insn | arch_sse1_insn,
	cpu_pentium_m           = arch_ppro | arch_feature_cmov | arch_feature_p6_insn | arch_sse2_insn,
	cpu_netburst_generic    = arch_netburst | arch_feature_p6_insn,
	cpu_pentium_4           = arch_netburst | arch_feature_cmov | arch_feature_p6_insn | arch_sse2_insn,
	cpu_prescott            = arch_nocona | arch_feature_cmov | arch_feature_p6_insn | arch_sse3_insn,
	cpu_nocona              = arch_nocona | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn | arch_sse3_insn,
	cpu_core2_generic       = arch_core2 | arch_feature_p6_insn,
	cpu_core2               = arch_core2 | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn | arch_ssse3_insn,
	cpu_penryn              = arch_core2 | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn | arch_sse4_1_insn,
	cpu_atom_generic        = arch_atom | arch_feature_p6_insn,
	cpu_atom                = arch_atom | arch_feature_cmov | arch_feature_p6_insn | arch_ssse3_insn,

	/* AMD CPUs */
	cpu_k6_generic     = arch_k6,
	cpu_k6             = arch_k6 | arch_mmx_insn,
	cpu_k6_PLUS        = arch_k6 | arch_3DNow_insn,
	cpu_geode_generic  = arch_geode,
	cpu_geode          = arch_geode  | arch_sse1_insn | arch_3DNowE_insn,
	cpu_athlon_generic = arch_athlon | arch_feature_p6_insn,
	cpu_athlon_old     = arch_athlon | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn,
	cpu_athlon         = arch_athlon | arch_sse1_insn | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn,
	cpu_athlon64       = arch_athlon | arch_sse2_insn | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn,
	cpu_k8_generic     = arch_k8  | arch_feature_p6_insn,
	cpu_k8             = arch_k8  | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn,
	cpu_k8_sse3        = arch_k8  | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn | arch_64bit_insn | arch_sse3_insn,
	cpu_k10_generic    = arch_k10 | arch_feature_p6_insn,
	cpu_k10            = arch_k10 | arch_3DNowE_insn | arch_feature_cmov | arch_feature_p6_insn | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn,

	/* other CPUs */
	cpu_winchip_c6  = arch_i486 | arch_feature_mmx,
	cpu_winchip2    = arch_i486 | arch_feature_mmx | arch_feature_3DNow,
	cpu_c3          = arch_i486 | arch_feature_mmx | arch_feature_3DNow,
	cpu_c3_2        = arch_ppro | arch_feature_cmov | arch_feature_p6_insn | arch_sse1_insn, /* really no 3DNow! */

	cpu_autodetect  = 0,
} cpu_arch_features;
ENUM_BITSET(cpu_arch_features)

static bool              opt_size             = false;
static bool              emit_machcode        = false;
static bool              use_softfloat        = false;
static bool              use_sse              = false;
static bool              use_sse2             = false;
static bool              use_sse3             = false;
static bool              use_sse4             = false;
static bool              use_sse4_1           = false;
static bool              use_sse4_2           = false;
static bool              use_sse4a            = false;
static bool              use_sse5             = false;
static bool              use_ssse3            = false;
static cpu_arch_features arch                 = cpu_generic;
static cpu_arch_features opt_arch             = 0;
static int               fpu_arch             = 0;
static bool              opt_cc               = true;
static bool              opt_unsafe_floatconv = false;
static int               po2_stack_alignment  = 2;

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
	{ "penryn",       cpu_penryn },
	{ "atom",         cpu_atom },

	{ "k6",           cpu_k6 },
	{ "k6-2",         cpu_k6_PLUS },
	{ "k6-3",         cpu_k6_PLUS },
	{ "geode",        cpu_geode },
	{ "athlon",       cpu_athlon_old },
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

#ifdef NATIVE_X86
	{ "native",       cpu_autodetect },
#endif

	{ NULL,           0 }
};

static lc_opt_enum_int_var_t arch_var = {
	(int*) &arch, arch_items
};

static lc_opt_enum_int_var_t opt_arch_var = {
	(int*) &opt_arch, arch_items
};

static const lc_opt_enum_int_items_t fp_unit_items[] = {
	{ "387" ,      IA32_FPU_ARCH_X87 },
	{ "sse",       IA32_FPU_ARCH_SSE2 },
	{ "softfloat", IA32_FPU_ARCH_SOFTFLOAT },
	{ NULL,        IA32_FPU_ARCH_NONE }
};

static lc_opt_enum_int_var_t fp_unit_var = {
	&fpu_arch, fp_unit_items
};

static const lc_opt_table_entry_t ia32_architecture_options[] = {
	LC_OPT_ENT_INT     ("stackalign",       "set power of two stack alignment for calls",         &po2_stack_alignment),
	LC_OPT_ENT_BOOL    ("size",             "optimize for size",                                  &opt_size),
	LC_OPT_ENT_ENUM_INT("arch",             "select the instruction architecture",                &arch_var),
	LC_OPT_ENT_ENUM_INT("tune",             "optimize for instruction architecture",              &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpmath",           "select the floating point unit",                     &fp_unit_var),
	LC_OPT_ENT_BOOL    ("optcc",            "optimize calling convention",                        &opt_cc),
	LC_OPT_ENT_BOOL    ("unsafe_floatconv", "do unsafe floating point controlword optimizations", &opt_unsafe_floatconv),
	LC_OPT_ENT_BOOL    ("machcode",         "output machine code instead of assembler",           &emit_machcode),
	LC_OPT_ENT_BOOL    ("soft-float",       "equivalent to fpmath=softfloat",                     &use_softfloat),
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
	switch (opt_arch & arch_mask) {
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
	case arch_generic32: arch_costs = &generic32_cost;  break;
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

/* auto detection code only works if we're on an x86 cpu obviously */
#ifdef NATIVE_X86
typedef struct x86_cpu_info_t {
	unsigned char cpu_stepping;
	unsigned char cpu_model;
	unsigned char cpu_family;
	unsigned char cpu_type;
	unsigned char cpu_ext_model;
	unsigned char cpu_ext_family;
	unsigned      edx_features;
	unsigned      ecx_features;
	unsigned      add_features;
} x86_cpu_info_t;

enum {
	CPUID_FEAT_ECX_SSE3      = 1 << 0,
	CPUID_FEAT_ECX_PCLMUL    = 1 << 1,
	CPUID_FEAT_ECX_DTES64    = 1 << 2,
	CPUID_FEAT_ECX_MONITOR   = 1 << 3,
	CPUID_FEAT_ECX_DS_CPL    = 1 << 4,
	CPUID_FEAT_ECX_VMX       = 1 << 5,
	CPUID_FEAT_ECX_SMX       = 1 << 6,
	CPUID_FEAT_ECX_EST       = 1 << 7,
	CPUID_FEAT_ECX_TM2       = 1 << 8,
	CPUID_FEAT_ECX_SSSE3     = 1 << 9,
	CPUID_FEAT_ECX_CID       = 1 << 10,
	CPUID_FEAT_ECX_FMA       = 1 << 12,
	CPUID_FEAT_ECX_CX16      = 1 << 13,
	CPUID_FEAT_ECX_ETPRD     = 1 << 14,
	CPUID_FEAT_ECX_PDCM      = 1 << 15,
	CPUID_FEAT_ECX_DCA       = 1 << 18,
	CPUID_FEAT_ECX_SSE4_1    = 1 << 19,
	CPUID_FEAT_ECX_SSE4_2    = 1 << 20,
	CPUID_FEAT_ECX_x2APIC    = 1 << 21,
	CPUID_FEAT_ECX_MOVBE     = 1 << 22,
	CPUID_FEAT_ECX_POPCNT    = 1 << 23,
	CPUID_FEAT_ECX_AES       = 1 << 25,
	CPUID_FEAT_ECX_XSAVE     = 1 << 26,
	CPUID_FEAT_ECX_OSXSAVE   = 1 << 27,
	CPUID_FEAT_ECX_AVX       = 1 << 28,

	CPUID_FEAT_EDX_FPU       = 1 << 0,
	CPUID_FEAT_EDX_VME       = 1 << 1,
	CPUID_FEAT_EDX_DE        = 1 << 2,
	CPUID_FEAT_EDX_PSE       = 1 << 3,
	CPUID_FEAT_EDX_TSC       = 1 << 4,
	CPUID_FEAT_EDX_MSR       = 1 << 5,
	CPUID_FEAT_EDX_PAE       = 1 << 6,
	CPUID_FEAT_EDX_MCE       = 1 << 7,
	CPUID_FEAT_EDX_CX8       = 1 << 8,
	CPUID_FEAT_EDX_APIC      = 1 << 9,
	CPUID_FEAT_EDX_SEP       = 1 << 11,
	CPUID_FEAT_EDX_MTRR      = 1 << 12,
	CPUID_FEAT_EDX_PGE       = 1 << 13,
	CPUID_FEAT_EDX_MCA       = 1 << 14,
	CPUID_FEAT_EDX_CMOV      = 1 << 15,
	CPUID_FEAT_EDX_PAT       = 1 << 16,
	CPUID_FEAT_EDX_PSE36     = 1 << 17,
	CPUID_FEAT_EDX_PSN       = 1 << 18,
	CPUID_FEAT_EDX_CLF       = 1 << 19,
	CPUID_FEAT_EDX_DTES      = 1 << 21,
	CPUID_FEAT_EDX_ACPI      = 1 << 22,
	CPUID_FEAT_EDX_MMX       = 1 << 23,
	CPUID_FEAT_EDX_FXSR      = 1 << 24,
	CPUID_FEAT_EDX_SSE       = 1 << 25,
	CPUID_FEAT_EDX_SSE2      = 1 << 26,
	CPUID_FEAT_EDX_SS        = 1 << 27,
	CPUID_FEAT_EDX_HTT       = 1 << 28,
	CPUID_FEAT_EDX_TM1       = 1 << 29,
	CPUID_FEAT_EDX_IA64      = 1 << 30,
	CPUID_FEAT_EDX_PBE       = 1 << 31
};

static cpu_arch_features auto_detect_Intel(x86_cpu_info_t const *info)
{
	cpu_arch_features auto_arch = cpu_generic;

	unsigned family = info->cpu_ext_family + info->cpu_family;
	unsigned model  = (info->cpu_ext_model << 4) | info->cpu_model;

	switch (family) {
	case 4:
		auto_arch = cpu_i486;
		break;
	case 5:
		auto_arch = cpu_pentium;
		break;
	case 6:
		switch (model) {
		case 0x01: /* PentiumPro */
		case 0x03: /* Pentium II Model 3 */
		case 0x05: /* Pentium II Model 5 */
		case 0x06: /* Celeron Model 6 */
		case 0x07: /* Pentium III Model 7 */
		case 0x08: /* Pentium III Model 8 */
		case 0x09: /* Pentium M Model 9 */
		case 0x0A: /* Pentium III Model 0A */
		case 0x0B: /* Pentium III Model 0B */
		case 0x0D: /* Pentium M Model 0D */
		case 0x0E: /* Core Model 0E */
			auto_arch = cpu_pentium_pro_generic;
			break;
		case 0x0F: /* Core2 Model 0F */
		case 0x15: /* Intel EP80579 */
		case 0x16: /* Celeron Model 16 */
		case 0x17: /* Core2 Model 17 */
			auto_arch = cpu_core2_generic;
			break;
		default:
			/* unknown */
			break;
		}
		break;
	case 15:
		switch (model) {
		case 0x00: /* Pentium 4 Model 00 */
		case 0x01: /* Pentium 4 Model 01 */
		case 0x02: /* Pentium 4 Model 02 */
		case 0x03: /* Pentium 4 Model 03 */
		case 0x04: /* Pentium 4 Model 04 */
		case 0x06: /* Pentium 4 Model 06 */
			auto_arch = cpu_netburst_generic;
			break;
		case 0x1A: /* Core i7 */
			auto_arch = cpu_core2_generic;
			break;
		case 0x1C: /* Atom */
			auto_arch = cpu_atom_generic;
			break;
		case 0x1D: /* Xeon MP */
			auto_arch = cpu_core2_generic;
			break;
		default:
			/* unknown */
			break;
		}
		break;
	default:
		/* unknown */
		break;
	}

	return auto_arch;
}

static cpu_arch_features auto_detect_AMD(x86_cpu_info_t const *info)
{
	cpu_arch_features auto_arch = cpu_generic;

	unsigned family, model;

	if (info->cpu_family == 0x0F) {
		family = info->cpu_ext_family + info->cpu_family;
		model  = (info->cpu_ext_model << 4) | info->cpu_model;
	} else {
		family = info->cpu_family;
		model  = info->cpu_model;
	}

	switch (family) {
	case 0x04:
		auto_arch = cpu_i486;
		break;
	case 0x05:
		switch (model) {
		case 0x00: /* K5 Model 0 */
		case 0x01: /* K5 Model 1 */
		case 0x02: /* K5 Model 2 */
		case 0x03: /* K5 Model 3 */
			auto_arch = cpu_pentium;
			break;
		case 0x06: /* K6 Model 6 */
		case 0x07: /* K6 Model 7 */
		case 0x08: /* K6-2 Model 8 */
		case 0x09: /* K6-III Model 9 */
		case 0x0D: /* K6-2+ or K6-III+ */
			auto_arch = cpu_k6_generic;
			break;
		case 0x0A: /* Geode LX */
			auto_arch = cpu_geode_generic;
			break;
		default:
			/* unknown K6 */
			auto_arch = cpu_k6_generic;
			break;
		}
		break;
	case 0x06:
		switch (model) {
		case 0x01: /* Athlon Model 1 */
		case 0x02: /* Athlon Model 2 */
		case 0x03: /* Duron Model 3 */
		case 0x04: /* Athlon Model 4 */
		case 0x06: /* Athlon MP/Mobile Athlon Model 6 */
		case 0x07: /* Mobile Duron Model 7 */
		case 0x08: /* Athlon (TH/AP core) including Geode NX */
		case 0x0A: /* Athlon (BT core) */
		default:   /* unknown K7 */
			auto_arch = cpu_athlon_generic;
			break;
		}
		break;
	case 0x0F:
		auto_arch = cpu_k8_generic;
		break;
	case 0x10:
	case 0x11: /* AMD Family 11h */
	case 0x12: /* AMD Family 12h */
	case 0x14: /* AMD Family 14h */
	case 0x15: /* AMD Family 15h */
		auto_arch = cpu_k10_generic;
		break;
	default:
		/* unknown */
		break;
	}

	return auto_arch;
}

typedef union {
	struct {
        unsigned eax;
        unsigned ebx;
        unsigned ecx;
        unsigned edx;
	} r;
	int bulk[4];
} cpuid_registers;

static void x86_cpuid(cpuid_registers *regs, unsigned level)
{
#if defined(__GNUC__)
#	if defined(__PIC__) && !defined(__amd64) // GCC cannot handle EBX in PIC
	__asm (
		"movl %%ebx, %1\n\t"
		"cpuid\n\t"
		"xchgl %%ebx, %1"
	: "=a" (regs->r.eax), "=r" (regs->r.ebx), "=c" (regs->r.ecx), "=d" (regs->r.edx)
	: "a" (level)
	);
#	else
	__asm ("cpuid\n\t"
	: "=a" (regs->r.eax), "=b" (regs->r.ebx), "=c" (regs->r.ecx), "=d" (regs->r.edx)
	: "a" (level)
	);
#	endif
#elif defined(_MSC_VER)
	__cpuid(regs->bulk, level);
#else
#	error CPUID is missing
#endif
}

static bool x86_toogle_cpuid(void)
{
	unsigned eflags_before = 0;
	unsigned eflags_after = 0;

#if defined(__GNUC__)
#ifdef __i386__
	/* If bit 21 of the EFLAGS register can be changed, the cpuid instruction is available */
	__asm__(
		"pushf\n\t"
		"popl %0\n\t"
		"movl %0, %1\n\t"
		"xorl $0x00200000, %1\n\t"
		"pushl %1\n\t"
		"popf\n\t"
		"pushf\n\t"
		"popl %1"
		: "=r" (eflags_before), "=r" (eflags_after) :: "cc"
		);
#else
	eflags_after = 0x00200000;
#endif
#elif defined(_MSC_VER)
#if defined(_M_IX86)
	__asm {
		pushfd
		pop eax
		mov eflags_before, eax
		xor eax, 0x00200000
		push eax
		popfd
		pushfd
		pop eax
		mov eflags_after, eax
	}
#else
	eflags_after = 0x00200000;
#endif
#endif
	return (eflags_before ^ eflags_after) & 0x00200000;
}

static void autodetect_arch(void)
{
	cpu_arch_features auto_arch = cpu_generic;

	/* We use the cpuid instruction to detect the CPU features */
	if (x86_toogle_cpuid()) {

		/* get vendor ID */
		cpuid_registers regs;
		x86_cpuid(&regs, 0);
		char vendorid[13];
		memcpy(&vendorid[0], &regs.r.ebx, 4);
		memcpy(&vendorid[4], &regs.r.edx, 4);
		memcpy(&vendorid[8], &regs.r.ecx, 4);
		vendorid[12] = '\0';

		/* get processor info and feature bits */
		x86_cpuid(&regs, 1);

		x86_cpu_info_t cpu_info;
		cpu_info.cpu_stepping   = (regs.r.eax >>  0) & 0x0F;
		cpu_info.cpu_model      = (regs.r.eax >>  4) & 0x0F;
		cpu_info.cpu_family     = (regs.r.eax >>  8) & 0x0F;
		cpu_info.cpu_type       = (regs.r.eax >> 12) & 0x03;
		cpu_info.cpu_ext_model  = (regs.r.eax >> 16) & 0x0F;
		cpu_info.cpu_ext_family = (regs.r.eax >> 20) & 0xFF;
		cpu_info.edx_features   = regs.r.edx;
		cpu_info.ecx_features   = regs.r.ecx;
		cpu_info.add_features   = regs.r.ebx;

		if        (0 == strcmp(vendorid, "GenuineIntel")) {
			auto_arch = auto_detect_Intel(&cpu_info);
		} else if (0 == strcmp(vendorid, "AuthenticAMD")) {
			auto_arch = auto_detect_AMD(&cpu_info);
		} else if (0 == strcmp(vendorid, "Geode by NSC")) {
			auto_arch = cpu_geode_generic;
		}

		if (cpu_info.edx_features & CPUID_FEAT_EDX_CMOV)
			auto_arch |= arch_feature_cmov;
		if (cpu_info.edx_features & CPUID_FEAT_EDX_MMX)
			auto_arch |= arch_feature_mmx;
		if (cpu_info.edx_features & CPUID_FEAT_EDX_SSE)
			auto_arch |= arch_feature_sse1;
		if (cpu_info.edx_features & CPUID_FEAT_EDX_SSE2)
			auto_arch |= arch_feature_sse2;

		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE3)
			auto_arch |= arch_feature_sse3;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSSE3)
			auto_arch |= arch_feature_ssse3;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE4_1)
			auto_arch |= arch_feature_sse4_1;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE4_2)
			auto_arch |= arch_feature_sse4_2;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_POPCNT)
			auto_arch |= arch_feature_popcnt;
	}

	arch     = auto_arch;
	opt_arch = auto_arch;
}
#endif  /* NATIVE_X86 */

static bool flags(cpu_arch_features features, cpu_arch_features flags)
{
	return (features & flags) != 0;
}

void ia32_setup_cg_config(void)
{
	if (use_softfloat)
		fpu_arch = IA32_FPU_ARCH_SOFTFLOAT;

#ifdef NATIVE_X86
	if (arch == cpu_autodetect)
		autodetect_arch();
#endif
	if (opt_arch == 0)
		opt_arch = arch;

	set_arch_costs();

	ia32_code_gen_config_t *const c = &ia32_cg_config;
	memset(c, 0, sizeof(*c));
	c->optimize_size        = opt_size != 0;
	/* on newer intel cpus mov, pop is often faster than leave although it has a
	 * longer opcode */
	c->use_leave            = flags(opt_arch, arch_i386 | arch_all_amd | arch_core2) || opt_size;
	/* P4s don't like inc/decs because they only partially write the flags
	 * register which produces false dependencies */
	c->use_incdec           = !flags(opt_arch, arch_netburst | arch_nocona | arch_core2 | arch_geode) || opt_size;
	c->use_softfloat        = (fpu_arch & IA32_FPU_ARCH_SOFTFLOAT) != 0;
	c->use_sse2             = (fpu_arch & IA32_FPU_ARCH_SSE2) != 0 && flags(arch, arch_feature_sse2);
	c->use_ffreep           = flags(opt_arch, arch_athlon_plus);
	c->use_femms            = flags(opt_arch, arch_athlon_plus) && flags(arch, arch_feature_3DNow);
	c->use_fucomi           = flags(arch, arch_feature_p6_insn);
	c->use_cmov             = flags(arch, arch_feature_cmov);
	c->use_modeD_moves      = flags(opt_arch, arch_generic32 | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2 | arch_ppro | arch_geode);
	c->use_add_esp_4        = flags(opt_arch, arch_generic32 | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2 |             arch_geode)                         && !opt_size;
	c->use_add_esp_8        = flags(opt_arch, arch_generic32 | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2 | arch_ppro | arch_geode | arch_i386 | arch_i486) && !opt_size;
	c->use_sub_esp_4        = flags(opt_arch, arch_generic32 | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2 | arch_ppro)                                      && !opt_size;
	c->use_sub_esp_8        = flags(opt_arch, arch_generic32 | arch_athlon_plus | arch_netburst | arch_nocona | arch_core2 | arch_ppro |              arch_i386 | arch_i486) && !opt_size;
	c->use_imul_mem_imm32   = !flags(opt_arch, arch_k8 | arch_k10) || opt_size;
	c->use_pxor             = flags(opt_arch, arch_netburst);
	c->use_mov_0            = flags(opt_arch, arch_k6) && !opt_size;
	c->use_short_sex_eax    = !flags(opt_arch, arch_k6) || opt_size;
	c->use_pad_return       = flags(opt_arch, arch_athlon_plus) && !opt_size;
	c->use_bt               = flags(opt_arch, arch_core2 | arch_athlon_plus) || opt_size;
	c->use_fisttp           = flags(opt_arch & arch, arch_feature_sse3);
	c->use_sse_prefetch     = flags(arch, (arch_feature_3DNowE | arch_feature_sse1));
	c->use_3dnow_prefetch   = flags(arch, arch_feature_3DNow);
	c->use_popcnt           = flags(arch, arch_feature_popcnt);
	c->use_bswap            = (arch & arch_mask) >= arch_i486;
	c->use_cmpxchg          = (arch & arch_mask) != arch_i386;
	c->optimize_cc          = opt_cc;
	c->use_unsafe_floatconv = opt_unsafe_floatconv;
	c->emit_machcode        = emit_machcode;

	c->function_alignment       = arch_costs->function_alignment;
	c->label_alignment          = arch_costs->label_alignment;
	c->label_alignment_max_skip = arch_costs->label_alignment_max_skip;

	c->label_alignment_factor =
		flags(opt_arch, arch_i386 | arch_i486) || opt_size ? 0 :
		opt_arch & arch_all_amd                            ? 3 :
		2;
	c->po2_stack_alignment = po2_stack_alignment;
}

void ia32_init_architecture(void)
{
	memset(&ia32_cg_config, 0, sizeof(ia32_cg_config));

	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");
	lc_opt_add_table(ia32_grp, ia32_architecture_options);
}
