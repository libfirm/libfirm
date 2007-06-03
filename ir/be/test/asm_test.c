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

	__asm__ ("movl %1, %0" : "=r"(res) : "ri" (val));

	return res;
}

static inline unsigned short swap16(unsigned short x)
{
	__asm__("xchgb %b0, %h0 /* in: %1 out: %0 */" : "=q" (x) : "0" (x));
	return x;
}

static inline unsigned int swap32(unsigned int x)
{
	__asm__("bswap %0 /* %1 */" : "=r" (x) : "0" (x));
	return x;
}

int main()
{
	//sincostest(0.5);
	/*outb(123, 42);
	outb(12345, 42);*/

	printf("Swap16: %d Swap32: %d\n", swap16(12), swap32(123551235));

	return mov(0) /*+ inb(12345) + inb(123)*/;
}
