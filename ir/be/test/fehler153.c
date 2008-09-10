#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

volatile int x = 2;
volatile int y;

int test(void) {

	x = 3;
	y = 4;

	for(;;) {
		x = y;
	}
}

void handler(int sig)
{
	printf(x == 4 ? "ok\n" : "fail");
	fflush(stdout);
	exit(x != 4);
}

int main(int argc, char *argv[]) {
	alarm(1);
	signal(SIGALRM, handler);
	test();
	printf("FAIL ENDLESS LOOP\n");
	return 1;
}
