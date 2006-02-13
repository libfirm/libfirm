#include <stdio.h>

typedef int myint;

#define TESTANZ 	16
#define IMM		23
#define	test16_1	42
#define	test16_2	11
#define	test32_1	0x001200AB
#define	test32_2	0x00341500



myint test_add(myint a, myint b) {
	return a+b;
}

myint test_addi(myint a) {
	return a+IMM;
}

myint test_sub(myint a, myint b) {
	return a-b;
}

myint test_subi(myint a) {
	return a-IMM;
}

myint test_subfi(myint a) {
	return IMM-a;
}

myint test_mul(myint a, myint b) {
	return a*b;
}

myint test_muli(myint a) {
	return a*IMM;
}

myint test_div(myint a, myint b) {
	return a/b;
}

myint test_divi(myint a) {
	return a/IMM;
}

myint test_shl(myint a, myint b) {
	return a<<b;
}

myint test_shli(myint a) {
	return a<<IMM;
}

myint test_shr(myint a, myint b) {
	return a>>b;
}

myint test_shri(myint a) {
	return a>>IMM;
}

myint test_cmp(myint a, myint b) {
	return (a>b) ? 1 : 0;
}

myint test_cmpi(myint a) {
	return (a>IMM) ? 1 : 0;
}

int main(int argc, char *argv[]) {
  myint res16[TESTANZ];
  myint res32[TESTANZ];
  int i;

  res16[ 0] = test_add  (test16_1, test16_2);
  res16[ 1] = test_sub  (test16_1, test16_2);
  res16[ 2] = test_mul  (test16_1, test16_2);
  res16[ 3] = test_div  (test16_1, test16_2);
  res16[ 4] = test_shl  (test16_1, test16_2);
  res16[ 5] = test_shr  (test16_1, test16_2);
  res16[ 6] = test_div  (test16_1, test16_2);
  res16[ 7] = test_cmp  (test16_1, test16_2);
  res16[ 8] = test_addi (test16_1);
  res16[ 9] = test_subi (test16_1);
  res16[10] = test_subfi(test16_1);
  res16[11] = test_muli (test16_1);
  res16[12] = test_divi (test16_1);
  res16[13] = test_shli (test16_1);
  res16[14] = test_shri (test16_1);
  res16[15] = test_cmpi (test16_1);

  res32[ 0] = test_add  (test32_1, test32_2);
  res32[ 1] = test_sub  (test32_1, test32_2);
  res32[ 2] = test_mul  (test32_1, test32_2);
  res32[ 3] = test_div  (test32_1, test32_2);
  res32[ 4] = test_shl  (test32_1, test32_2);
  res32[ 5] = test_shr  (test32_1, test32_2);
  res32[ 6] = test_div  (test32_1, test32_2);
  res32[ 7] = test_cmp  (test32_1, test32_2);
  res32[ 8] = test_addi (test32_1);
  res32[ 9] = test_subi (test32_1);
  res32[10] = test_subfi(test32_1);
  res32[11] = test_muli (test32_1);
  res32[12] = test_divi (test32_1);
  res32[13] = test_shli (test32_1);
  res32[14] = test_shri (test32_1);
  res32[15] = test_cmpi (test32_1);

  for (i=0; i<TESTANZ; i++) {
    printf("res16[%d] = %d\n", i, res16[i]);
    printf("res32[%d] = %d\n", i, res32[i]);
  }
  return 0;
}
