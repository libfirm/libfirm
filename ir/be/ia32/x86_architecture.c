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
	cpu_arch_features auto_arch = cpu_generic;

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
		if (cpu_info.ecx_features & CPUID_FEAT_ECX_FMA)
			auto_arch |= arch_feature_fma;
	}

	return auto_arch;
}

bool flags(cpu_arch_features features, cpu_arch_features flags)
{
	return (features & flags) != 0;
}
