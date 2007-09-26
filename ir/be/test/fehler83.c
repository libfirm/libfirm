#include <stdio.h>

/* produces a graph with wrong modes */

static char parens[] = "=!<,>";
static char *p = & parens[2];

int main(void)
{
	int n = ((p - parens) % 2) ? 42 : 13;
	printf("Res: %d (should be 13)\n", n);
	return 0;
}
