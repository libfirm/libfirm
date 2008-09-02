#include <assert.h>
#include <signal.h>
#include <stdlib.h>

void f(int x)
{
	assert(x < 0);
}

void handler(int sig)
{
	(void)sig;
	exit(0);
}

int main(void)
{
	signal(SIGABRT, handler);
	f(1);
	return 1;
}
