#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include <stdio.h>

struct x {
	int a, b;
};

static void t(struct x *p)
{
	printf("%d\n", p->a + p->b);
}

static void t2()
{
	char *mem;

	printf("hallo\n");
	mem = alloca(23);

	printf("%.0s", mem);
}

int main(int argc, char *argv[])
{
	struct x *p = alloca(sizeof(*p));

	p->a = argc;
	p->b = 3;

	t(p);
	t2();

	return 0;
}
