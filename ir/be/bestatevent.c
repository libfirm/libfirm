/**
 * Statistic events
 * @date    3.9.2006
 * @author  Sebastian Hack
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <string.h>

#include "bestatevent.h"

#define STACK_SIZE 16

typedef struct {
	FILE *f;
	char tag[512];
} ev_env_t;

static ev_env_t envs[STACK_SIZE];
static unsigned sp = 0;

void be_stat_ev_push(const char **tags, int n_tags, FILE *f)
{
	int i;
	ev_env_t *env;

	assert(sp < STACK_SIZE && "stat event stack full");
	env = &envs[sp++];

	env->tag[0] = '\0';
	for(i = 0; i < n_tags; ++i) {
		strncat(env->tag, tags[i], sizeof(env->tag));
		strncat(env->tag, ";", sizeof(env->tag));
	}
	env->tag[sizeof(env->tag) - 1] = '\0';
	env->f = f;
}

void be_stat_ev_pop(void)
{
	if (sp == 0)
		return;

	envs[--sp].f = NULL;
}

void be_stat_ev(const char *ev, int value)
{
	ev_env_t *env;

	if (sp == 0)
		return;

	env = &envs[sp - 1];
	if (env->f == NULL)
		return;

	fprintf(env->f, "%s%s;%d\n", env->tag, ev, value);
}

void be_stat_ev_l(const char *ev, long value)
{
	ev_env_t *env;

	if (sp == 0)
		return;

	env = &envs[sp - 1];
	if (env->f == NULL)
		return;

	fprintf(env->f, "%s%s;%ld\n", env->tag, ev, value);
}

void be_stat_ev_dbl(const char *ev, double value)
{
	ev_env_t *env;

	if (sp == 0)
		return;

	env = &envs[sp - 1];
	if (env->f == NULL)
		return;

	fprintf(env->f, "%s%s;%f\n", env->tag, ev, value);
}

void be_stat_ev_ull(const char *ev, ulong64 value)
{
	ev_env_t *env;

	if (sp == 0)
		return;

	env = &envs[sp - 1];
	if (env->f == NULL)
		return;

	fprintf(env->f, "%s%s;%" ULL_FMT "\n", env->tag, ev, value);
}

int be_stat_ev_is_active(void)
{
	return sp > 0 && envs[sp - 1].f != NULL;
}
