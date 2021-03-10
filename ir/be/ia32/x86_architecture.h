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



typedef enum x86_cpu_architectures {
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

	arch_athlon_plus      = arch_athlon | arch_k8 | arch_k10,
	arch_all_amd          = arch_k6 | arch_geode | arch_athlon_plus,

} x86_cpu_architectures;
ENUM_BITSET(x86_cpu_architectures)

/**
 * CPU architectures and features.
 */
typedef enum x86_cpu_features {

	arch_feature_none     = 0,

	arch_feature_mmx      = 0x00000001, /**< MMX instructions */
	arch_feature_cmov     = 0x00000002, /**< cmov instructions */
	arch_feature_p6_insn  = 0x00000004, /**< PentiumPro instructions */
	arch_feature_sse1     = 0x00000008, /**< SSE1 instructions */
	arch_feature_sse2     = 0x00000010, /**< SSE2 instructions */
	arch_feature_sse3     = 0x00000020, /**< SSE3 instructions */
	arch_feature_ssse3    = 0x00000040, /**< SSSE3 instructions */
	arch_feature_3DNow    = 0x00000080, /**< 3DNow! instructions */
	arch_feature_3DNowE   = 0x00000100, /**< Enhanced 3DNow! instructions */
	arch_feature_64bit    = 0x00000200, /**< x86_64 support */
	arch_feature_sse4_1   = 0x00000400, /**< SSE4.1 instructions */
	arch_feature_sse4_2   = 0x00000800, /**< SSE4.2 instructions */
	arch_feature_sse4a    = 0x00001000, /**< SSE4a instructions */
	arch_feature_popcnt   = 0x00002000, /**< popcnt instruction */
	arch_feature_fma      = 0x00004000, /**< FMA instructions */

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

} x86_cpu_features;
ENUM_BITSET(x86_cpu_features)

typedef struct cpu_arch_features {
	x86_cpu_architectures arch;
	x86_cpu_features features;
} cpu_arch_features;


typedef enum x86_cpu {
	cpu_autodetect = 0,
	cpu_generic = 1,
	cpu_generic64,

	/* intel CPUs */
	cpu_i386,
	cpu_i486,
	cpu_pentium,
	cpu_pentium_mmx,
	cpu_pentium_pro_generic,
	cpu_pentium_pro,
	cpu_pentium_2,
	cpu_pentium_3,
	cpu_pentium_m,
	cpu_netburst_generic,
	cpu_pentium_4,
	cpu_prescott,
	cpu_nocona,
	cpu_core2_generic,
	cpu_core2,
	cpu_penryn,
	cpu_atom_generic,
	cpu_atom,

	/* AMD CPUs */
	cpu_k6_generic,
	cpu_k6,
	cpu_k6_PLUS,
	cpu_geode_generic,
	cpu_geode,
	cpu_athlon_generic,
	cpu_athlon_old,
	cpu_athlon,
	cpu_athlon64,
	cpu_k8_generic,
	cpu_k8,
	cpu_k8_sse3,
	cpu_k10_generic,
	cpu_k10,

	/* other CPUs */
	cpu_winchip_c6,
	cpu_winchip2,
	cpu_c3,
	cpu_c3_2,

	cpu_max
} x86_cpu;

extern cpu_arch_features cpu_arch_feature_defs[];

cpu_arch_features autodetect_arch(void);

bool arch_flags(cpu_arch_features arch_features, x86_cpu_architectures flags);

bool feature_flags(cpu_arch_features arch_features, x86_cpu_features flags);

#endif
