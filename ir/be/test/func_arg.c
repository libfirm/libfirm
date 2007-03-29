#include <stdio.h>

void mymain(int argc, const char * const*argv) {
	printf("A0: %s AC: %d\n", argv[0], argc);
}

int main(argc, argv)
  int argc;
  char **argv;
{
	const char *args[] = { "blup", "bla" };

	mymain(2, args);
	return 0;
}
