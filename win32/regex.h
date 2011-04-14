#ifndef REGEX_H
#define REGEX_H

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

/* naive and wrong regex stubs */
typedef struct regex_t {
	char *pattern;
} regex_t;

#define REG_EXTENDED 1
#define REG_ICASE    2
#define REG_NOSUB    4
#define REG_NEWLINE  8

int regcomp(regex_t *regex, const char *pattern, int cflags)
{
	size_t len = strlen(pattern)+1;
	(void) cflags;
	regex->pattern = malloc(len);
	memcpy(regex->pattern, pattern, len);
}

int regexec(const regex_t *regex, const char *haystack, int flags)
{
	size_t i = 0;
	const char *pattern = regex->pattern;
	for (i = 0; pattern[i] != '\0'; ++i) {
		if (pattern[i] != haystack[i])
			return 0;
	}
	return 1;
}

void regfree(regex_t *regex)
{
	free(regex->pattern);
}

#endif
