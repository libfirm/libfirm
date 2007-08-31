/*$ -fno-inline $*/

int val;

#define T(name,OP) \
	int test_##name(void) { return val OP 7; }   \
	int test2_##name(int v) { return val OP v; }  \
	int testp_##name(void) { return 7 OP val; }  \
	int testp2_##name(int v) { return v OP val; }

int test_cmp_testset(int v, int v2) { return (v & 14) > 0; }

T(add,+)
T(sub,-)
T(or,|)
T(and,&)
T(xor,^)
T(cmp,<)
T(shl,<<)
T(shr,>>)

#undef T

int main(void) {
	int res1, res2, res3, res4;
	val = 11;

#define T(name,OP)          \
	res1 = test_##name();   \
	res2 = test2_##name(20); \
	res3 = testp_##name();   \
	res4 = testp2_##name(20); \
	printf("Test %s: %d (should be %d)\n", #name, res1, 11 OP 7);   \
	printf("Test2 %s: %d (should be %d)\n", #name, res2, 11 OP 20); \
	printf("Testp %s: %d (should be %d)\n", #name, res3, 7 OP 11);   \
	printf("Testp2 %s: %d (should be %d)\n", #name, res4, 20 OP 11);

	T(add,+)
	T(sub,-)
	T(or,|)
	T(and,&)
	T(xor,^)
	T(cmp,<)
	T(shl,<<)
	T(shr,>>)
}
