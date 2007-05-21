#include <stdio.h>

int main()
{
	const char *s = "no compiler is perfect";
	const char *t = s;
	char c = *t;

	for( ; !((*t == ' ') && (*t != '\"')); t++) {
		c = *t;
	}

	printf("Res: %s\n", t);

	return 0;
}
