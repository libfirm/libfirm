#include <stdio.h>

static inline unsigned char inb(const unsigned short port)
{
    unsigned char val;

    __asm__ __volatile__ ("inb  %w1, %0" : "=a"(val) : "dN"(port));

    return val;
}

static inline void outb(const unsigned short port, const unsigned char val)
{
	int k = val; /* just here to test the b modifier in %b0 */
    __asm__ __volatile__ ("outb %b0, %1" : : "a"(k), "dN"(port));
}

static void sincostest(double arg)
{
	double cos, sin;

	__asm__ ("fsincos" : "=t"(cos), "=u"(sin) : "0" (arg));
	printf("Arg: %f Sin: %f Cos: %f\n", arg, sin, cos);
}

static inline int mov(int val)
{
	int res;

	__asm__ ("movl %0, %1" : "=r"(res) : "ri" (val));

	return res;
}


int main()
{
	//sincostest(0.5);
	outb(123, 42);
	outb(12345, 42);

	return mov(0) + inb(12345) + inb(123);
}
