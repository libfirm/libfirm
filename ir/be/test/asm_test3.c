/* THIS IS WRONG, but gcc compiles it: (unsigned int)x is NO lvalue */
#define udiv_qrnnd(q, r, n1, n0, dv) \
	 __asm__ ("divl %4"                                                    \
	          : "=a" ((unsigned int) (q)),                                      \
	            "=d" ((unsigned int) (r))                                       \
	          : "0" ((unsigned int) (n0)),                                      \
	            "1" ((unsigned int) (n1)),                                      \
	            "rm" ((unsigned int) (dv)))

unsigned int X;
unsigned test(void) {
	unsigned int d0, n0, n1, q0;

	n1 = n0 = d0 = X;

	udiv_qrnnd (q0, n0, n1, n0, d0);

	return q0;
}

int main(void) {
	return 0;
}
