#include <stdio.h>

static char string[2048] = "";
static char string2[2048] = "It's indeed okay";

int main()
{
	sprintf(string, "This is a very long sentence to test, whether the compiler crashs because of obscure bugs... If you can read it all until the exclamation mark, then your compiler is probably okay!");

	puts(string);
	puts(string2);
	return 0;
}
