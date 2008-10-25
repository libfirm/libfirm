/*$ -O4 -march=core2 $*/
#include <alloca.h>

extern void print_spelling(char *text);

void test (void)
{
	char *temp = (char *) alloca (16);
	print_spelling (temp);
}

int main(int argc, char *argv[]) {
	return 0;
}
