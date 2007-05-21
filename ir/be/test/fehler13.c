#include <stdio.h>

int main()
{
	const char *s = "This is cool";
	const char *t;

	for(t=s; !((isspace(*t) || (*t == '\"')) || *t=='\0'); t++)
		;

	printf("Res: %s\n", t);

	return 0;
}
