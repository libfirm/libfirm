/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend module interface.
 * @author      Matthias Braun
 * @date        29.09.2005
 */
#include <stdlib.h>
#include <stdbool.h>

#include "bemodule.h"
#include "xmalloc.h"

/**
 * A module list entry.
 */
struct be_module_list_entry_t {
	const char *name;                    /**< The name of the entry. */
	void *data;                          /**< Some data associated with this entry. */
	struct be_module_list_entry_t *next; /**< Points to the next entry. */
};

void be_init_arch_TEMPLATE(void);
void be_init_arch_amd64(void);
void be_init_arch_arm(void);
void be_init_arch_ia32(void);
void be_init_arch_sparc(void);
void be_init_blocksched(void);
void be_init_chordal(void);
void be_init_chordal_common(void);
void be_init_chordal_main(void);
void be_init_copyheur4(void);
void be_init_copyilp(void);
void be_init_copyilp2(void);
void be_init_copynone(void);
void be_init_copyopt(void);
void be_init_copystat(void);
void be_init_daemelspill(void);
void be_init_dwarf(void);
void be_init_gas(void);
void be_init_listsched(void);
void be_init_live(void);
void be_init_loopana(void);
void be_init_pbqp(void);
void be_init_pbqp_coloring(void);
void be_init_peephole(void);
void be_init_pref_alloc(void);
void be_init_ra(void);
void be_init_sched(void);
void be_init_sched_normal(void);
void be_init_sched_rand(void);
void be_init_sched_trivial(void);
void be_init_spill(void);
void be_init_spillbelady(void);
void be_init_spilloptions(void);
void be_init_spillslots(void);
void be_init_ssaconstr(void);
void be_init_state(void);

void be_quit_copystat(void);
void be_quit_pbqp(void);

/**
 * Driver for module initialization.
 * Call your module initialization function here.
 */
void be_init_modules(void)
{
	static bool run_once = false;
	if (run_once)
		return;
	run_once = true;

	be_init_blocksched();
	be_init_chordal_common();
	be_init_copyopt();
	be_init_copystat();
	be_init_dwarf();
	be_init_gas();
	be_init_live();
	be_init_loopana();
	be_init_peephole();
	be_init_ra();
	be_init_sched();
	be_init_spill();
	be_init_spilloptions();
	be_init_spillslots();
	be_init_ssaconstr();
	be_init_state();

	/* in the following groups the first one is the default */
	be_init_arch_ia32();
	be_init_arch_arm();
	be_init_arch_sparc();
	be_init_arch_amd64();
	be_init_arch_TEMPLATE();

	be_init_listsched();
	be_init_sched_normal();
	be_init_sched_rand();
	be_init_sched_trivial();

	be_init_chordal_main();
	be_init_pref_alloc();

	be_init_chordal();
	be_init_pbqp_coloring();

	be_init_spillbelady();
	be_init_daemelspill();

	be_init_copyheur4();
	be_init_copyilp2();
	be_init_copynone();
	be_init_copyilp();

#ifdef FIRM_GRGEN_BE
	be_init_pbqp();
#endif
}

void be_quit_modules(void)
{
	be_quit_copystat();
#ifdef FIRM_GRGEN_BE
	be_quit_pbqp();
#endif
}

//---------------------------------------------------------------------------

typedef struct module_opt_data_t {
	void **var;
	be_module_list_entry_t * const *list_head;
} module_opt_data_t;

/**
 * Searches in list for module option. If found, set option to given value and
 * return true.
 * Beware: return value of 0 means error.
 */
static bool set_opt_module(const char *name, lc_opt_type_t type, void *data,
                           size_t length, ...)
{
	(void)length;
	(void)type;
	(void)name;

	va_list args;
	va_start(args, length);
	const char *opt = va_arg(args, const char*);

	const module_opt_data_t *moddata = (module_opt_data_t*)data;
	bool                     res     = false;
	for (const be_module_list_entry_t *module = *(moddata->list_head);
	     module != NULL; module = module->next) {
		if (strcmp(module->name, opt) == 0) {
			*(moddata->var) = module->data;
			res = true;
			break;
		}
	}
	va_end(args);

	return res;
}

/**
 * Dump the names of all registered module options.
 */
static int dump_opt_module(char *buf, size_t buflen, lc_opt_type_t type, void *data)
{
	(void)type;

	const module_opt_data_t *moddata = (module_opt_data_t*)data;
	for (const be_module_list_entry_t *module = *(moddata->list_head);
	     module != NULL; module = module->next) {
		if (module->data == *(moddata->var)) {
			snprintf(buf, buflen, "%s", module->name);
			return strlen(buf);
		}
	}

	snprintf(buf, buflen, "none");
	return strlen(buf);
}

/**
 * Dump the values of all register module options.
 */
static int dump_opt_module_vals(char *buf, size_t buflen, const char *name, lc_opt_type_t type, void *data)
{
	(void)name;
	(void)type;

	const module_opt_data_t *moddata = (module_opt_data_t*)data;
	char                    *p       = buf;
	for (const be_module_list_entry_t *module = *(moddata->list_head);
	     module != NULL; module = module->next) {
		size_t name_len = strlen(module->name);

		if (module != *(moddata->list_head)) {
			p       = strncat(p, ", ", buflen - 1);
			buflen -= 2;
		}

		p = strncat(p, module->name, buflen - 1);
		if (name_len >= buflen)
			break;

		buflen -= name_len;
	}

	return strlen(buf);
}

/**
 * Add a new module to list.
 */
void be_add_module_to_list(be_module_list_entry_t **list_head, const char *name,
                           void *module)
{
	be_module_list_entry_t *entry = XMALLOC(be_module_list_entry_t);
	entry->name = name;
	entry->data = module;
	entry->next = *list_head;
	*list_head  = entry;
}

/**
 * Add an option for a module.
 */
void be_add_module_list_opt(lc_opt_entry_t *grp, const char *name,
                            const char *description,
                            be_module_list_entry_t * const * list_head,
                            void **var)
{
	module_opt_data_t *moddata = XMALLOC(module_opt_data_t);
	moddata->var       = var;
	moddata->list_head = list_head;

	lc_opt_add_opt(grp, name, description, lc_opt_type_enum,
	               moddata, sizeof(moddata[0]), set_opt_module,
	               dump_opt_module, dump_opt_module_vals);
}
