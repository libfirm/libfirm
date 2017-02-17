/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */
#include "bediagnostic.h"

#include "dbginfo.h"
#include "irnode_t.h"
#include "irprintf.h"
#include <stdarg.h>

static void be_vdiagnosticf(ir_node const *const node, char const *const kind, char const *const fmt, va_list ap)
{
	FILE *const out = stderr;

	if (node) {
		ir_fprintf(out, "%+F", node);

		dbg_info *const dbgi = get_irn_dbg_info(node);
		src_loc_t const loc  = ir_retrieve_dbg_info(dbgi);
		if (loc.file) {
			ir_fprintf(out, " (%s", loc.file);
			if (loc.line != 0) {
				ir_fprintf(out, ":%u", loc.line);
				if (loc.column != 0)
					ir_fprintf(out, ":%u", loc.column);
			}
			fputc(')', out);
		}

		fputs(": ", out);
	}

	ir_fprintf(out, "%s: ", kind);
	ir_vfprintf(out, fmt, ap);
	fputc('\n', out);
}

void be_errorf(ir_node const *const node, char const *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	be_vdiagnosticf(node, "error", fmt, ap);
	va_end(ap);
}

void be_warningf(ir_node const *const node, char const *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	be_vdiagnosticf(node, "warning", fmt, ap);
	va_end(ap);
}
