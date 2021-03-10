/**
 * @file
 * @brief   x86 architecture and CPU feature detection
 * @author  Michael Beck, Matthias Braun, Johannes Bucher
 */
#include "x86_architecture.h"

#include <stdbool.h>
#include <string.h>
#include "util.h"

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
	unsigned      ext_edx_features;
	unsigned      ext_ecx_features;
	unsigned      ext_ebx_features;
	unsigned      ext_level_edx_features;
	unsigned      ext_level_ecx_features;
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
	CPUID_FEAT_EDX_PBE       = 1 << 31,

	CPUID_EXT_FEAT_EBX_AVX2  = 1 << 5,

	CPUID_EXT_LEVEL_FEAT_EDX_LM     = 1 << 29,
	CPUID_EXT_LEVEL_FEAT_EDX_3DNOWE = 1 << 30,
	CPUID_EXT_LEVEL_FEAT_EDX_3DNOW  = 1 << 31,
	CPUID_EXT_LEVEL_FEAT_ECX_SSE4A  = 1 << 6
};

/* feature definitions for cpu variants (selectable through -march=.../-mtune=... options) */
cpu_arch_features cpu_arch_feature_defs[cpu_max] = {
	[cpu_generic]        = {arch_generic, arch_feature_none},
	[cpu_generic64]      = {arch_generic, arch_64bit_insn},

	/* Intel CPUs */
	[cpu_i386]           = {arch_i386, arch_feature_none},
	[cpu_i486]           = {arch_i486, arch_feature_none},
	[cpu_pentium]        = {arch_pentium, arch_feature_none},
	[cpu_pentium_mmx]    = {arch_pentium, arch_mmx_insn},
	[cpu_pentium_pro]    = {arch_ppro, arch_feature_cmov | arch_feature_fcmov},
	[cpu_pentium_2]      = {arch_ppro, arch_feature_cmov | arch_feature_fcmov | arch_mmx_insn},
	[cpu_pentium_3]      = {arch_ppro, arch_feature_cmov | arch_feature_fcmov | arch_sse1_insn},
	[cpu_pentium_m]      = {arch_ppro, arch_feature_cmov | arch_feature_fcmov | arch_sse2_insn},
	[cpu_pentium_4]      = {arch_netburst, arch_feature_cmov | arch_feature_fcmov | arch_sse2_insn},
	[cpu_prescott]       = {arch_nocona, arch_feature_cmov | arch_feature_fcmov | arch_sse3_insn},
	[cpu_nocona]         = {arch_nocona, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_sse3_insn},
	[cpu_core2]          = {arch_core2, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_ssse3_insn},
	[cpu_penryn]         = {arch_core2, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_sse4_1_insn},
	[cpu_nehalem]        = {arch_nehalem, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_westmere]       = {arch_nehalem, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_sandybridge]    = {arch_sandybridge, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx_insn | arch_feature_popcnt},
	[cpu_ivybridge]      = {arch_sandybridge, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx_insn | arch_feature_popcnt},
	[cpu_haswell]        = {arch_haswell, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_broadwell]      = {arch_haswell, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_skylake]        = {arch_skylake, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_skylake_avx512] = {arch_skylake, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_cascade_lake]   = {arch_skylake, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_cooperlake]     = {arch_skylake, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_cannonlake]     = {arch_skylake, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_icelake_client] = {arch_sunnycove, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_icelake_server] = {arch_sunnycove, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_tigerlake]      = {arch_sunnycove, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_sapphirerapids] = {arch_sunnycove, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},
	[cpu_alderlake]      = {arch_sunnycove, arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_avx2_insn | arch_feature_popcnt | arch_feature_fma},

	[cpu_atom]           = {arch_atom, arch_feature_cmov | arch_feature_fcmov | arch_ssse3_insn},
	[cpu_silvermont]     = {arch_silvermont, arch_feature_cmov | arch_feature_fcmov | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_goldmont]       = {arch_goldmont, arch_feature_cmov | arch_feature_fcmov | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_goldmont_plus]  = {arch_goldmont_plus, arch_feature_cmov | arch_feature_fcmov | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_tremont]        = {arch_tremont, arch_feature_cmov | arch_feature_fcmov | arch_sse4_2_insn | arch_feature_popcnt},
	[cpu_knl]            = {arch_silvermont, arch_feature_cmov | arch_feature_fcmov | arch_feature_avx2 | arch_feature_popcnt},
	[cpu_knm]            = {arch_silvermont, arch_feature_cmov | arch_feature_fcmov | arch_feature_avx2 | arch_feature_popcnt},


	/* AMD CPUs */
	[cpu_k6]             = {arch_k6, arch_mmx_insn},
	[cpu_k6_PLUS]        = {arch_k6, arch_3DNow_insn},
	[cpu_geode]          = {arch_geode, arch_sse1_insn | arch_3DNowE_insn},
	[cpu_athlon_old]     = {arch_athlon, arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov},
	[cpu_athlon]         = {arch_athlon, arch_sse1_insn | arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov},
	[cpu_athlon64]       = {arch_athlon, arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn},
	[cpu_k8]             = {arch_k8, arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn},
	[cpu_k8_sse3]        = {arch_k8, arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov | arch_64bit_insn | arch_sse3_insn},
	[cpu_k10]            = {arch_k10, arch_3DNowE_insn | arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn},
	[cpu_btver1]         = {arch_amdfam14h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn},
	[cpu_btver2]         = {arch_amdfam16h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx_insn},
	[cpu_bdver1]         = {arch_amdfam15h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx_insn},
	[cpu_bdver2]         = {arch_amdfam15h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx_insn | arch_feature_fma},
	[cpu_bdver3]         = {arch_amdfam15h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx_insn | arch_feature_fma},
	[cpu_bdver4]         = {arch_amdfam15h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx2_insn | arch_feature_fma},
	[cpu_znver1]         = {arch_amdfam17h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx2_insn | arch_feature_fma},
	[cpu_znver2]         = {arch_amdfam17h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx2_insn | arch_feature_fma},
	[cpu_znver3]         = {arch_amdfam19h, arch_feature_cmov | arch_feature_fcmov | arch_feature_popcnt | arch_64bit_insn | arch_sse4a_insn | arch_avx2_insn | arch_feature_fma},

	/* other CPUs */
	[cpu_winchip_c6] = {arch_i486, arch_feature_mmx},
	[cpu_winchip2]   = {arch_i486, arch_feature_mmx | arch_feature_3DNow},
	[cpu_c3]         = {arch_i486, arch_feature_mmx | arch_feature_3DNow},
	[cpu_c3_2]       = {arch_ppro, arch_feature_cmov | arch_feature_fcmov | arch_sse1_insn}, /* really no 3DNow! */

};

static cpu_arch_features auto_detect_Intel(x86_cpu_info_t const *info)
{
	x86_cpu_architectures auto_arch = arch_generic;

	unsigned family = info->cpu_ext_family + info->cpu_family;
	unsigned model  = (info->cpu_ext_model << 4) | info->cpu_model;

	switch (family) {
	case 4:
		auto_arch = arch_i486;
		break;
	case 5:
		auto_arch = arch_pentium;
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
			auto_arch = arch_ppro;
			break;
		case 0x0F: /* Merom / Core2 Model 0F */
		case 0x15: /* Intel EP80579 */
		case 0x16: /* Celeron Model 16 */
		case 0x17: /* Core2 Penryn/Wolfdale/Yorkfield */
		case 0x1D: /* Xeon MP Dunnington */
			auto_arch = arch_core2;
			break;
		case 0x1c:
		case 0x26: /* Atom Bonnell */
		case 0x27: /* Atom Medfield */
	    case 0x35:
	    case 0x36: /* Atom Midview */
			auto_arch = arch_atom;
			break;
		case 0x37:
		case 0x4a:
		case 0x4d:
		case 0x5d: /* Atom Silvermont */
		case 0x4c:
		case 0x5a:
		case 0x75: /* Atom Airmont */
		case 0x57: /* Xeon Phi Knights Landing */
		case 0x85: /* Xeon Phi Knights Mill */
			auto_arch = arch_silvermont;
			break;
		case 0x5c:
		case 0x5f: /* Atom Goldmont */
			auto_arch = arch_goldmont;
			break;
		case 0x7a: /* Atom Goldmont Plus */
			auto_arch = arch_goldmont_plus;
			break;
		case 0x86:
		case 0x96:
		case 0x9c: /* Atom Tremont */
			auto_arch = arch_tremont;
			break;
		case 0x1a:
		case 0x1e:
		case 0x1f:
		case 0x2e: /* Nehalem */
		case 0x25:
		case 0x2c:
		case 0x2f: /* Westmere */
			auto_arch = arch_nehalem;
			break;
		case 0x2a:
		case 0x2d: /* Sandy Bridge */
		case 0x3a:
		case 0x3e: /* Ivy Bridge */
			auto_arch = arch_sandybridge;
			break;
		case 0x3c:
		case 0x3f:
		case 0x45:
		case 0x46: /* Haswell */
		case 0x3d:
		case 0x47:
		case 0x4f:
		case 0x56: /* Broadwell */
			auto_arch = arch_haswell;
			break;
		case 0x4e:
	    case 0x5e: /* Skylake */
	    case 0x8e:
	    case 0x9e: /* Kaby Lake */
	    case 0xa5:
	    case 0xa6: /* Comet Lake */
	    case 0xa7: /* Rocket Lake */
	    case 0x55: /* Skylake Xeon, Cooper Lake / Cascade Lake / Skylake-AVX512 */
	    case 0x66: /* Cannon Lake */
		    auto_arch = arch_skylake;
		    break;
	    case 0x6a:
	    case 0x6c: /* Ice Lake server */
	    case 0x7e:
	    case 0x7d:
	    case 0x9d: /* Ice Lake client */
	    case 0x8c:
	    case 0x8d: /* Tiger Lake */
	    case 0x97: /* Alder Lake */
	    case 0x8f: /* Sapphire Rapids */
		    auto_arch = arch_sunnycove;
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
			auto_arch = arch_netburst;
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

	return (cpu_arch_features) {auto_arch, arch_feature_none};
}

static cpu_arch_features auto_detect_AMD(x86_cpu_info_t const *info)
{
	x86_cpu_architectures auto_arch = arch_generic;

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
		auto_arch = arch_i486;
		break;
	case 0x05:
		switch (model) {
		case 0x00: /* K5 Model 0 */
		case 0x01: /* K5 Model 1 */
		case 0x02: /* K5 Model 2 */
		case 0x03: /* K5 Model 3 */
			auto_arch = arch_pentium;
			break;
		case 0x06: /* K6 Model 6 */
		case 0x07: /* K6 Model 7 */
		case 0x08: /* K6-2 Model 8 */
		case 0x09: /* K6-III Model 9 */
		case 0x0D: /* K6-2+ or K6-III+ */
			auto_arch = arch_k6;
			break;
		case 0x0A: /* Geode LX */
			auto_arch = arch_geode;
			break;
		default:
			/* unknown K6 */
			auto_arch = arch_k6;
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
			auto_arch = arch_athlon;
			break;
		}
		break;
	case 0x0F: /* AMD K8 Family */
	case 0x11: /* AMD Family 11h (Turion X2 Ultra / Puma mobile platform) */
		auto_arch = arch_k8;
		break;
	case 0x10: /* AMD Family 10h */
	case 0x12: /* AMD Family 12h: Fusion/Llano APU */
		auto_arch = arch_k10;
		break;
	case 0x14: /* AMD Family 14h Bobcat */
		auto_arch = arch_amdfam14h;
		break;
	case 0x15: /* AMD Family 15h Bulldozer/Piledriver/Steamroller/Excavator */
		auto_arch = arch_amdfam15h;
		break;
	case 0x16: /* AMD Family 16h Jaguar/Puma */
		auto_arch = arch_amdfam16h;
		break;
	case 0x17: /* AMD Family 17h Zen/Zen2 */
		auto_arch = arch_amdfam17h;
		break;
	case 0x19: /* AMD Family 19h Zen3 */
		auto_arch = arch_amdfam19h;
		break;
	default:
		/* unknown */
		break;
	}

	return (cpu_arch_features) {auto_arch, arch_feature_none};
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

static bool x86_toggle_cpuid(void)
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

cpu_arch_features autodetect_arch(void)
{
	cpu_arch_features auto_arch = cpu_arch_feature_defs[cpu_generic];

	/* We use the cpuid instruction to detect the CPU features */
	if (x86_toggle_cpuid()) {

		/* get vendor ID */
		cpuid_registers regs;
		x86_cpuid(&regs, 0);
		char vendorid[13];
		memcpy(&vendorid[0], &regs.r.ebx, 4);
		memcpy(&vendorid[4], &regs.r.edx, 4);
		memcpy(&vendorid[8], &regs.r.ecx, 4);
		vendorid[12] = '\0';
		int max_cpuid_level = regs.r.eax;

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

		if        (streq(vendorid, "GenuineIntel")) {
			auto_arch = auto_detect_Intel(&cpu_info);
		} else if (streq(vendorid, "AuthenticAMD")) {
			auto_arch = auto_detect_AMD(&cpu_info);
		} else if (streq(vendorid, "Geode by NSC")) {
			auto_arch.arch = arch_geode;
		}

		if (cpu_info.edx_features & CPUID_FEAT_EDX_CMOV) {
			auto_arch.features |= arch_feature_cmov;
			if (cpu_info.edx_features & CPUID_FEAT_EDX_FPU) {
				auto_arch.features |= arch_feature_fcmov;
			}
		}

		if (cpu_info.edx_features & CPUID_FEAT_EDX_MMX)
			auto_arch.features |= arch_feature_mmx;
		if (cpu_info.edx_features & CPUID_FEAT_EDX_SSE)
			auto_arch.features |= arch_feature_sse1;
		if (cpu_info.edx_features & CPUID_FEAT_EDX_SSE2)
			auto_arch.features |= arch_feature_sse2;

		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE3)
			auto_arch.features |= arch_feature_sse3;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSSE3)
			auto_arch.features |= arch_feature_ssse3;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE4_1)
			auto_arch.features |= arch_feature_sse4_1;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_SSE4_2)
			auto_arch.features |= arch_feature_sse4_2;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_POPCNT)
			auto_arch.features |= arch_feature_popcnt;
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_FMA)
			auto_arch.features |= arch_feature_fma;

		if ((cpu_info.ecx_features & (CPUID_FEAT_ECX_OSXSAVE | CPUID_FEAT_ECX_AVX)) ==
		    (CPUID_FEAT_ECX_OSXSAVE | CPUID_FEAT_ECX_AVX)) {
			//TODO: full AVX support detection
			// (see for example https://github.com/llvm/llvm-project/blob/main/compiler-rt/lib/builtins/cpu_model.c)
			auto_arch.features |= arch_feature_avx;
		}

		if (max_cpuid_level >= 7) {
			/* get extended cpu features */
			x86_cpuid(&regs, 7);
			cpu_info.ext_edx_features = regs.r.edx;
			cpu_info.ext_ecx_features = regs.r.ecx;
			cpu_info.ext_ebx_features = regs.r.ebx;

			if (feature_flags(auto_arch, arch_feature_avx) && cpu_info.ext_ebx_features & CPUID_EXT_FEAT_EBX_AVX2) {
				auto_arch.features |= arch_feature_avx2;
			}
		}

		/* get max extension level */
		x86_cpuid(&regs, 0x80000000);
		bool has_ext_level1 = regs.r.eax >= 0x80000001;

		if (has_ext_level1) {
			/* get extended level cpu features */
			x86_cpuid(&regs, 0x80000001);
			cpu_info.ext_level_ecx_features = regs.r.ecx;
			cpu_info.ext_level_edx_features = regs.r.edx;

			if (cpu_info.ext_level_edx_features & CPUID_EXT_LEVEL_FEAT_EDX_LM)
				auto_arch.features |= arch_feature_64bit;
			if (cpu_info.ext_level_edx_features & CPUID_EXT_LEVEL_FEAT_EDX_3DNOW)
				auto_arch.features |= arch_feature_3DNow;
			if (cpu_info.ext_level_edx_features & CPUID_EXT_LEVEL_FEAT_EDX_3DNOWE)
				auto_arch.features |= arch_feature_3DNowE;

			if (cpu_info.ext_level_ecx_features & CPUID_EXT_LEVEL_FEAT_ECX_SSE4A)
				auto_arch.features |= arch_feature_sse4a;
		}
	}

	return auto_arch;
}

bool arch_flags(cpu_arch_features features, x86_cpu_architectures flags)
{
	return (features.arch & flags) != 0;
}

bool feature_flags(cpu_arch_features features, x86_cpu_features flags)
{
	return (features.features & flags) != 0;
}
