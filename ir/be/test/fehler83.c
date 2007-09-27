#include <stdio.h>

/* produces a graph with wrong modes */
static char parens[] = "=!<,>";
static char *p = & parens[2];

int f(void)
{
	return (p - parens) % 2 ? 42 : 13;
}

int main(void)
{
	printf("Res: %d (should be 13)\n", f());
	return 0;
}
