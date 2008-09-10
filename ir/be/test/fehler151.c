#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

int test(void) {
	abort();

	for(;;);
}

void handler(int sig)
{
	printf(sig == SIGABRT ? "ok\n" : "fail");
	fflush(stdout);
	exit(sig != SIGABRT);
}

int main(int argc, char *argv[]) {
	alarm(1);
	signal(SIGABRT, handler);
	signal(SIGALRM, handler);
	test();
	return 1;
}
