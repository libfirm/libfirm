/**
 * @file
 * @brief   AMD64 architecture specific options
 * @author  Johannes Bucher
 */
#include "amd64_architecture.h"

#include <stdbool.h>
#include <string.h>

#include "lc_opts_enum.h"
#include "irtools.h"
#include "x86_architecture.h"

amd64_code_gen_config_t amd64_cg_config;

static x86_cpu           arch_val             = cpu_generic64;
static x86_cpu           opt_arch_val         = cpu_autodetect;
static cpu_arch_features arch;
static cpu_arch_features opt_arch;
static bool              use_red_zone         = false;
static bool              use_scalar_fma3      = false;

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "generic",        cpu_generic64 },
	{ "x86-64",         cpu_generic64 },

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

#ifdef NATIVE_X86
	{ "native",         cpu_autodetect },
#endif

	{ NULL,           0 }
};

static lc_opt_enum_int_var_t arch_var = {
	(int*) &arch_val, arch_items
};

static lc_opt_enum_int_var_t opt_arch_var = {
	(int*) &opt_arch_val, arch_items
};

static const lc_opt_table_entry_t amd64_architecture_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",             "select the instruction architecture",                 &arch_var),
	LC_OPT_ENT_ENUM_INT("tune",             "optimize for instruction architecture",               &opt_arch_var),
	LC_OPT_ENT_BOOL    ("no-red-zone",      "gcc compatibility",                                  &use_red_zone),
	LC_OPT_ENT_BOOL    ("fma",              "support FMA3 code generation",                       &use_scalar_fma3),
	LC_OPT_LAST
};

void amd64_setup_cg_config(void)
{
	arch = cpu_arch_feature_defs[arch_val];
	opt_arch = cpu_arch_feature_defs[opt_arch_val];

	/* auto detection code only works if we're on an x86 cpu obviously */
#ifdef NATIVE_X86
	if (arch_val == cpu_autodetect) {
		arch = autodetect_arch();
		opt_arch = arch;
	}
#endif
	if (opt_arch_val == cpu_autodetect)
		opt_arch = arch;

	amd64_code_gen_config_t *const c = &amd64_cg_config;
	memset(c, 0, sizeof(*c));
	c->use_scalar_fma3      = feature_flags(arch, arch_feature_fma) && use_scalar_fma3;
}

void amd64_init_architecture(void)
{
	memset(&amd64_cg_config, 0, sizeof(amd64_cg_config));

	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *amd64_grp = lc_opt_get_grp(be_grp, "amd64");
	lc_opt_add_table(amd64_grp, amd64_architecture_options);
}
