#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

int x = 2;
int test(void) {

	x = 3;

	for(;;);
}

void handler(int sig)
{
	printf(x == 3 ? "ok\n" : "fail");
	fflush(stdout);
	exit(x == 3);
}

int main(int argc, char *argv[]) {
	alarm(1);
	signal(SIGALRM, handler);
	test();
	printf("FAIL ENDLESS LOOP\n");
	return 1;
}
