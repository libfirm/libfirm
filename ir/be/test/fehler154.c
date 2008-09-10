#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

int foo(int x)
{
	while(x == 0)
		;

	return x;
}

int k;

void handler(int sig)
{
	printf("ok\n");
	_exit(0);
}

int main(void) {
	alarm(1);
	signal(SIGALRM, handler);

	foo(0);

	printf("endless loop returned!\n");
	return 1;
}
