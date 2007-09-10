
#include <stdio.h>
#include "timing.h"

/* we can only use the scheduling stuff, if that macro is defined in unistd.h */
#ifdef _POSIX_PRIORITY_SCHEDULING

timing_sched_env_t *timing_sched_get(timing_sched_env_t *env)
{
	int res;

#ifdef __linux__
	res = sched_getaffinity(0, sizeof(env->affinity), &env->affinity);
	if (res < 0)
		return NULL;
#endif

	env->scheduler = sched_getscheduler(0);
	if (env->scheduler < 0)
		return NULL;

	res = sched_getparam(0, &env->params);
	if (res < 0)
		return NULL;

	return env;
}

int timing_sched_set(const timing_sched_env_t *env)
{
	int res;

#ifdef __linux__
	res = sched_setaffinity(0, sizeof(env->affinity), &env->affinity);
	if (res < 0)
		return 0;
#endif

	res = sched_setscheduler(0, env->scheduler, &env->params);
	if (res < 0)
		return 0;

	return 1;
}

timing_sched_env_t *timing_sched_prepare_max_prio(timing_sched_env_t *env)
{
	int policy = SCHED_FIFO;
#ifdef __linux__
	CPU_ZERO(&env->affinity);
	CPU_SET(0, &env->affinity);
#endif
	env->scheduler             = policy;
	env->params.sched_priority = sched_get_priority_max(policy);
	return env;
}

#else /* _POSIX_PRIORITY_SCHEDULING */

timing_sched_env_t *timing_sched_get(timing_sched_env_t *env)
{
	return NULL;
}

int timing_sched_set(const timing_sched_env_t *env)
{
	return 0;
}

timing_sched_env_t *timing_sched_prepare_max_prio(timing_sched_env_t *env)
{
	return env;
}

#endif /* _POSIX_PRIORITY_SCHEDULING */
