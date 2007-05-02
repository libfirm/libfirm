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

void be_emit_init_env(be_emit_env_t *env, FILE *F)
{
	memset(env, 0, sizeof(env[0]));

	env->F = F;
	obstack_init(&env->obst);
	env->linelength = 0;
}

void be_emit_destroy_env(be_emit_env_t *env)
{
	obstack_free(&env->obst, NULL);
}

void be_emit_ident(be_emit_env_t *env, ident *id)
{
	size_t len = get_id_strlen(id);
	const char *str = get_id_str(id);

	be_emit_string_len(env, str, len);
}

void be_emit_tarval(be_emit_env_t *env, tarval *tv)
{
	char buf[64];

	tarval_snprintf(buf, sizeof(buf), tv);
	be_emit_string(env, buf);
}

void be_emit_irvprintf(be_emit_env_t *env, const char *fmt, va_list args)
{
	char buf[256];

	ir_vsnprintf(buf, sizeof(buf), fmt, args);
	be_emit_string(env, buf);
}

void be_emit_irprintf(be_emit_env_t *env, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	be_emit_irvprintf(env, fmt, ap);
	va_end(ap);
}

void be_emit_write_line(be_emit_env_t *env)
{
	char *finished_line = obstack_finish(&env->obst);

	fwrite(finished_line, env->linelength, 1, env->F);
	env->linelength = 0;
	obstack_free(&env->obst, finished_line);
}

void be_emit_pad_comment(be_emit_env_t *env)
{
	while(env->linelength <= 30) {
		be_emit_char(env, ' ');
	}
	be_emit_cstring(env, "    ");
}

void be_emit_finish_line_gas(be_emit_env_t *env, const ir_node *node)
{
	dbg_info *dbg;
	const char *sourcefile;
	unsigned lineno;

	if(node == NULL) {
		be_emit_char(env, '\n');
		be_emit_write_line(env);
		return;
	}

	be_emit_pad_comment(env);
	be_emit_cstring(env, "/* ");
	be_emit_irprintf(env, "%+F ", node);

	dbg = get_irn_dbg_info(node);
	sourcefile = be_retrieve_dbg_info(dbg, &lineno);
	if(sourcefile != NULL) {
		be_emit_string(env, sourcefile);
		be_emit_irprintf(env, ":%u", lineno);
	}
	be_emit_cstring(env, " */\n");
	be_emit_write_line(env);
}
