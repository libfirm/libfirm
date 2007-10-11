typedef struct K K;
struct K {
	int dummy;
	int high;
	int low;
};

K v = { 0, 0, -1 };
K c = { 0, 0, 4 };

#define H(A) (A)->high
#define L(A) (A)->low

#define LTU(A, B) \
	(((unsigned) H(A) < (unsigned) H(B)) || (((unsigned) H(A) == (unsigned) (H(B))) && ((unsigned) L(A) < (unsigned) L(B))))

K *p_v = &v;
K *p_c = &c;

int main(void) {
	K* pv = p_v;
	K* pc = p_c;
	int res;

	res = LTU(pv,pc);

	printf("Res: %d (should be 0)\n", res);
	return 0;
}
