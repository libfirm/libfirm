#include <assert.h>
#include <signal.h>
#include <unistd.h>

void f(int x)
{
	assert(x < 0);
}

void handler(int sig)
{
	(void)sig;
	_exit(0);
}

int main(void)
{
	signal(SIGABRT, handler);
	f(1);
	return 1;
}
