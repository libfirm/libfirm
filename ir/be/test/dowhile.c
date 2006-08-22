#include <stdio.h>

int main()
{
	char *p = "Hallo Welt\n";
	int i = 0;

	do {
		putchar(*p);
		++i;
		if(i < 5)
			continue;
		putchar('.');
		i = 0;
	} while(*++p != 0);

	return 0;
}
