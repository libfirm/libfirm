#include <stdio.h>
#include <assert.h>

#ifdef __i386__
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

static inline int mov_noeax(int val)
{
	int res;

	__asm__ ("movl %1, %0" : "=r"(res) : "ri" (val) : "eax");

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

void inc(int *v)
{
	__asm__("incl %0" : "+rm" (*v) : : "cc");
}

void inc2(int *v)
{
	__asm__("incl %0" : "+m" (*v) : : "cc");
}

#if 1
typedef struct kernel_fd_set {
	int bla;
	int blup;
} kernel_fd_set;
#else
typedef int kernel_fd_set;
#endif

void fd_set(int fd, kernel_fd_set* set) {
	__asm__("btsl %1,%0" : "=m" (*(set)) : "r" (fd) : "cc");
}

int fd_isset(int fd, kernel_fd_set *set) {
	unsigned char result;

	__asm__ __volatile__("btl %1,%2\n"
                         "\tsetb %0"
			: "=q" (result)
			: "r" (fd),  "m" (*set)
			: "cc");
	return result;
}

int justcompile(void)
{
	outb(123, 42);
	outb(12345, 42);
	return inb(20) + inb(5);
}

int main()
{
	kernel_fd_set s;
	int k;

	fd_set(20, &s);
	assert(fd_isset(20, &s));

	printf("Swap16(0xAABB): %X Swap32(0xAABBCCDD): %X\n",
	       swap16(0xAABB), swap32(0xAABBCCDD));
	k = 41;
	inc(&k);
	printf("mov(inc(41)): %d\n", mov_noeax(k));

	return mov_noeax(0);
}

#else

int main()
{
	printf("Warning: asmtest only work on x86\n");
	return 0;
}

#endif
