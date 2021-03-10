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
	{ "generic",      cpu_generic64 },

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

static const lc_opt_table_entry_t amd64_architecture_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",             "select the instruction architecture",                &arch_var),
	LC_OPT_ENT_ENUM_INT("tune",             "optimize for instruction architecture",              &opt_arch_var),
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
