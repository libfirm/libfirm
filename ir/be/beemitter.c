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
 * @brief       Interface for assembler output.
 * @author      Matthias Braun
 * @date        12.03.2007
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "beemitter.h"
#include "irprintf.h"
#include "ident.h"
#include "tv.h"
#include "dbginfo.h"

FILE           *emit_file;
struct obstack  emit_obst;
int             emit_linelength;

void be_emit_init(FILE *file)
{
	emit_file       = file;
	emit_linelength = 0;
	obstack_init(&emit_obst);
}

void be_emit_exit(void)
{
	obstack_free(&emit_obst, NULL);
}

void be_emit_ident(ident *id)
{
	size_t      len = get_id_strlen(id);
	const char *str = get_id_str(id);

	be_emit_string_len(str, len);
}

void be_emit_tarval(tarval *tv)
{
	char buf[64];

	tarval_snprintf(buf, sizeof(buf), tv);
	be_emit_string(buf);
}

void be_emit_irvprintf(const char *fmt, va_list args)
{
	char buf[256];

	ir_vsnprintf(buf, sizeof(buf), fmt, args);
	be_emit_string(buf);
}

void be_emit_irprintf(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	be_emit_irvprintf(fmt, ap);
	va_end(ap);
}

void be_emit_write_line(void)
{
	char *finished_line = obstack_finish(&emit_obst);

	fwrite(finished_line, emit_linelength, 1, emit_file);
	emit_linelength = 0;
	obstack_free(&emit_obst, finished_line);
}

void be_emit_pad_comment(void)
{
	while(emit_linelength <= 30) {
		be_emit_char(' ');
	}
	be_emit_cstring("    ");
}

void be_emit_finish_line_gas(const ir_node *node)
{
	dbg_info   *dbg;
	const char *sourcefile;
	unsigned    lineno;

	if(node == NULL) {
		be_emit_char('\n');
		be_emit_write_line();
		return;
	}

	be_emit_pad_comment();
	be_emit_cstring("/* ");
	be_emit_irprintf("%+F ", node);

	dbg        = get_irn_dbg_info(node);
	sourcefile = ir_retrieve_dbg_info(dbg, &lineno);
	if(sourcefile != NULL) {
		be_emit_string(sourcefile);
		be_emit_irprintf(":%u", lineno);
	}
	be_emit_cstring(" */\n");
	be_emit_write_line();
}
