/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Backend module interface.
 * @author      Matthias Braun
 * @date        29.09.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>

#include "bemodule_t.h"
#include "xmalloc.h"

void be_init_sched(void);
void be_init_blocksched(void);
void be_init_spill(void);
void be_init_spilloptions(void);
void be_init_listsched(void);
void be_init_schedrss(void);
void be_init_chordal(void);
void be_init_chordal_main(void);
void be_init_copycoal(void);
void be_init_copyheur2(void);
void be_init_copyheur3(void);
void be_init_copyheur4(void);
void be_init_copystat(void);
void be_init_daemelspill(void);
void be_init_arch_ia32(void);
void be_init_arch_ppc32(void);
void be_init_arch_mips(void);
void be_init_arch_arm(void);
void be_init_arch_sta(void);
void be_init_arch_TEMPLATE(void);
void be_init_ilpsched(void);
void be_init_copyilp(void);
void be_init_javacoal(void);
void be_init_ra(void);
void be_init_spillbelady(void);
void be_init_spillbelady2(void);
void be_init_spillmorgan(void);
void be_init_spillremat(void);
void be_init_ssaconstr(void);
void be_init_ifg(void);
void be_init_irgmod(void);
void be_init_loopana(void);
void be_init_spillslots(void);
void be_init_live(void);
void be_init_state(void);

void be_quit_copystat(void);

/**
 * Driver for module initialization.
 * Call your module initialization function here.
 */
void be_init_modules(void)
{
	static int run_once = 0;

	if (run_once)
		return;
	run_once = 1;

	be_init_irgmod();
	be_init_loopana();
	be_init_live();
	be_init_spillslots();
	be_init_sched();
	be_init_blocksched();
	be_init_spill();
	be_init_spilloptions();
	be_init_listsched();
	be_init_schedrss();
	be_init_chordal_main();
	be_init_chordal();
	be_init_copycoal();
	be_init_copyheur2();
	be_init_copyheur4();
	be_init_copystat();
	be_init_ra();
	be_init_spillbelady();
	be_init_spillbelady2();
	be_init_spillmorgan();
	be_init_daemelspill();
	be_init_ssaconstr();
	be_init_state();
	be_init_ifg();

	be_init_arch_ia32();
	be_init_arch_ppc32();
	be_init_arch_mips();
	be_init_arch_arm();
	/* do NOT call be_init_arch_TEMPLATE() here, this is NOT a backend :-) */

#ifdef WITH_ILP
	be_init_ilpsched();
	be_init_copyilp();
	be_init_spillremat();
#endif /* WITH_ILP */

#ifdef WITH_JVM
	be_init_copyheur3();
	be_init_javacoal();
#endif /* WITH_JVM */

#if PLUGIN_IR_BE_STA
	be_init_arch_sta();
#endif /* PLUGIN_IR_BE_STA */
}

void be_quit_modules(void)
{
	be_quit_copystat();
}

//---------------------------------------------------------------------------

typedef struct module_opt_data_t {
	void **var;
	be_module_list_entry_t * const *list_head;
} module_opt_data_t;

/**
 * Searches in list for module option. If found, set option to given value and return true.
 * Beware: return value of 0 means error.
 */
static int set_opt_module(const char *name, lc_opt_type_t type, void *data,
                          size_t length, ...)
{
	module_opt_data_t            *moddata = data;
	int                          res      = 0;
	va_list                      args;
	const char                   *opt;
	const be_module_list_entry_t *module;
	(void) type;
	(void) name;

	va_start(args, length);
	opt = va_arg(args, const char*);

	for (module = *(moddata->list_head); module != NULL; module = module->next) {
		if (strcmp(module->name, opt) == 0) {
			*(moddata->var) = module->data;
			res = 1;
			break;
		}
	}
	va_end(args);

	return res;
}

/**
 * Dump the names of all registered module options.
 */
int dump_opt_module(char *buf, size_t buflen, const char *name,
                    lc_opt_type_t type, void *data, size_t length)
{
	module_opt_data_t            *moddata = data;
	const be_module_list_entry_t *module;
	(void) name;
	(void) type;
	(void) length;

	for (module = *(moddata->list_head); module != NULL; module = module->next) {
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
int dump_opt_module_vals(char *buf, size_t buflen, const char *name,
                         lc_opt_type_t type, void *data, size_t len)
{
	module_opt_data_t            *moddata = data;
	char                         *p       = buf;
	const be_module_list_entry_t *module;
	(void) name;
	(void) type;
	(void) len;

	for (module = *(moddata->list_head); module != NULL; module = module->next) {
		size_t len = strlen(module->name);

		if (module != *(moddata->list_head)) {
			p       = strncat(p, ", ", buflen - 1);
			buflen -= 2;
		}

		p = strncat(p, module->name, buflen - 1);

		if (len >= buflen)
			break;

		buflen -= len;
	}

	return strlen(buf);
}

/**
 * Add a new module to list.
 */
void be_add_module_to_list(be_module_list_entry_t **list_head, const char *name,
                           void *module)
{
	be_module_list_entry_t *entry;

    entry       = xmalloc(sizeof(entry[0]));
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
	module_opt_data_t *moddata;

	moddata            = xmalloc(sizeof(moddata[0]));
	moddata->var       = var;
	moddata->list_head = list_head;

	lc_opt_add_opt(grp, name, description, lc_opt_type_enum,
	               moddata, sizeof(moddata[0]),
	               set_opt_module, dump_opt_module, dump_opt_module_vals,
				   NULL);
}
