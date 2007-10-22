#include <stdio.h>

static char *block;

int processblock(void)
{
	int i;
	block[0] = 10;
	for(i = 0; i < 100; ++i) {
		block[i] += 5;
	}

	return 0;
}

/* just here so the block variable doesn't get optimized away... */
void initblock(void)
{
	block = malloc(100);
}

char* getme(void)
{
	return block;
}
