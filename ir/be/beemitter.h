/*
 * Author:      Matthias Braun
 * Date:		12.03.2007
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILLMORGAN_H_
#define BESPILLMORGAN_H_

#include <stdio.h>
#include <stdarg.h>
#include "obst.h"
#include "ident.h"
#include "irnode.h"
#include "be.h"

/* framework for emitting data (usually the final assembly code) */

typedef struct be_emit_env_t {
	FILE *F;
	struct obstack obst;
	int            linelength;
} be_emit_env_t;

static INLINE void be_emit_char(be_emit_env_t *env, char c)
{
	obstack_1grow(&env->obst, c);
	env->linelength++;
}

static INLINE void be_emit_string_len(be_emit_env_t *env, const char *str,
                                      size_t l)
{
	obstack_grow(&env->obst, str, l);
	env->linelength += l;
}

static INLINE void be_emit_string(be_emit_env_t *env, const char *str)
{
	size_t len = strlen(str);
	be_emit_string_len(env, str, len);
}

#define be_emit_cstring(env,x) { be_emit_string_len(env, x, sizeof(x)-1); }

void be_emit_init_env(be_emit_env_t *env, FILE *F);
void be_emit_destroy_env(be_emit_env_t *env);

void be_emit_ident(be_emit_env_t *env, ident *id);
void be_emit_irprintf(be_emit_env_t *env, const char *fmt, ...);
void be_emit_irvprintf(be_emit_env_t *env, const char *fmt, va_list args);
void be_emit_write_line(be_emit_env_t *env);

/* appends a gas-style comment with the node number and writes the line */
void be_emit_finish_line_gas(be_emit_env_t *env, const ir_node *node);
void be_emit_pad_comment(be_emit_env_t *env);

#endif
