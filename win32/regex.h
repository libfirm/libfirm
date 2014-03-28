#ifndef REGEX_H
#define REGEX_H

#include <stdlib.h>

/* naive and wrong regex stubs */
typedef struct regex_t {
	char *pattern;
} regex_t;

typedef size_t regoff_t;

typedef struct regmatch_t {
	regoff_t rm_so;
	regoff_t rm_eo;
} regmatch_t;

#define REG_EXTENDED 1
#define REG_ICASE    2
#define REG_NOSUB    4
#define REG_NEWLINE  8
#define REG_NOTBOL   16
#define REG_NOTEOL   32

#define REG_NOMATCH 1

int regcomp(regex_t *regex, const char *pattern, int cflags)
{
	(void) cflags;
	regex->pattern = _strdup(pattern);
	return 0;
}

int regexec(const regex_t *regex, const char *haystack, size_t nmatch, regmatch_t pmatch[], int flags)
{
	size_t i = 0;
	const char *pattern = regex->pattern;
	for (i = 0; pattern[i] != '\0'; ++i) {
		if (pattern[i] != haystack[i])
			return REG_NOMATCH;
	}
	return 0;
}

void regfree(regex_t *regex)
{
	free(regex->pattern);
}

#endif
