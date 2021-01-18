#ifndef FIRM_BE_X86_ARCHITECTURE_H
#define FIRM_BE_X86_ARCHITECTURE_H

#include "firm_types.h"
#include <stdbool.h>

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
	arch_feature_fma      = 0x10000000, /**< FMA instructions */

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

	arch_generic64    = arch_generic32 | arch_64bit_insn,

	cpu_generic             = arch_generic32,
	cpu_generic64           = arch_generic64,

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

cpu_arch_features autodetect_arch(void);

bool flags(cpu_arch_features features, cpu_arch_features flags);

#endif
