/*$ -fno-inline $*/

#include <stdio.h>

int val;

#define TTYPE(name,type,OP) \
	type test_##name##type(void) { return val OP 7; }   \
	type test2_##name##type(type v) { return val OP v; }  \
	type testp_##name##type(void) { return 7 OP val; }  \
	type testp2_##name##type(type v) { return v OP val; }

int test_cmp_testset(int v, int v2) { return (v & 14) > 0; }

#define T(name,OP) \
	TTYPE(name,int,OP) \
	TTYPE(name,short,OP) \
	TTYPE(name,char,OP)

T(add,+)
T(sub,-)
T(or,|)
T(and,&)
T(xor,^)
T(cmp,<)
T(shl,<<)
T(shr,>>)

#undef T
#undef TTYPE

int main(void) {
	int res1, res2, res3, res4;
	val = 11;

#define TTYPE(name,type,OP)          \
	res1 = test_##name##type();   \
	res2 = test2_##name##type(20); \
	res3 = testp_##name##type();   \
	res4 = testp2_##name##type(20); \
	printf("Test %s: %d (should be %d)\n", #name, res1, (type) 11 OP (type) 7);   \
	printf("Test2 %s: %d (should be %d)\n", #name, res2, (type) 11 OP (type)20); \
	printf("Testp %s: %d (should be %d)\n", #name, res3, (type) 7 OP (type)11);   \
	printf("Testp2 %s: %d (should be %d)\n", #name, res4, (type) 20 OP (type)11);

#define T(name,OP) \
	TTYPE(name,int,OP) \
	TTYPE(name,short,OP) \
	TTYPE(name,char,OP)

	T(add,+)
	T(sub,-)
	T(or,|)
	T(and,&)
	T(xor,^)
	T(cmp,<)
	T(shl,<<)
	T(shr,>>)

	return 0;
}
