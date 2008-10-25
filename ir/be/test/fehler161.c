/*$ -O4 -march=core2 $*/
#include <alloca.h>

extern void puts(const char *text);

void test (void)
{
	char *temp = (char *) alloca (16);
	puts(temp);
}

int main(int argc, char *argv[]) {
	return 0;
}
