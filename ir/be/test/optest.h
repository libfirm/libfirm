#ifndef TESTANZ
#define TESTANZ 21
#define IMM         23
#define	test16_1	42
#define	test16_2	11
#define	test32_1	0x001200AB
#define	test32_2	0x00341501
#define test32_s    7
#endif

T tname(test_add_) (T a, T b) {
	return a+b;
}

T tname(test_addi_) (T a) {
	return a+IMM;
}

T tname(test_sub_) (T a, T b) {
	return a-b;
}

T tname(test_subi_) (T a) {
	return a-IMM;
}

T tname(test_subfi_) (T a) {
	return IMM-a;
}

T tname(test_mul_) (T a, T b) {
	return a*b;
}

T tname(test_muli_) (T a) {
	return a*IMM;
}

T tname(test_div_) (T a, T b) {
	return a/b;
}

T tname(test_divi_) (T a) {
	return a/IMM;
}

#ifndef TEST_UNSIGNED
T tname(test_abs_) (T a) {
	return a < 0 ? -a : a;
}

T tname(test_neg_) (T a) {
	return -a;
}
#endif

#ifndef TEST_FLOAT
T tname(test_shl_) (T a, T b) {
	return a<<b;
}

T tname(test_shli_) (T a) {
	return a<<IMM;
}

T tname(test_shr_) (T a, T b) {
	return a>>b;
}

T tname(test_shri_) (T a) {
	return a>>IMM;
}

T tname(test_mod_) (T a, T b) {
	return a%b;
}
#endif

T tname(test_cmp_) (T a, T b) {
	return (a>b) ? 1 : 0;
}

T tname(test_cmpi_) (T a) {
	return (a>IMM) ? 1 : 0;
}

T tname(res16_) [TESTANZ];
T tname(res32_) [TESTANZ];

void tname(test_) () {
	int i;
	T *res16 = tname(res16_);
	memset(res16, 0, TESTANZ * sizeof(res16[0]));
	T *res32 = tname(res32_);
	memset(res32, 0, TESTANZ * sizeof(res32[0]));

	res16[ 0] = tname(test_add_)  (test16_1, test16_2);
	res16[ 1] = tname(test_sub_)  (test16_1, test16_2);
	res16[ 2] = tname(test_mul_)  (test16_1, test16_2);
	res16[ 3] = tname(test_div_)  (test16_1, test16_2);
#ifndef TEST_FLOAT
	res16[ 4] = tname(test_shl_)  (test16_1, test16_2);
	res16[ 5] = tname(test_shr_)  (test16_1, test16_2);
	res16[ 6] = tname(test_mod_)  (test16_1, test16_2);
#endif
	res16[ 7] = tname(test_div_)  (test16_1, test16_2);
	res16[ 8] = tname(test_cmp_)  (test16_1, test16_2);
	res16[ 9] = tname(test_addi_) (test16_1);
	res16[10] = tname(test_subi_) (test16_1);
	res16[11] = tname(test_subfi_)(test16_1);
	res16[12] = tname(test_muli_) (test16_1);
	res16[13] = tname(test_divi_) (test16_1);
#ifndef TEST_FLOAT
	res16[14] = tname(test_shli_) (test16_1);
	res16[15] = tname(test_shri_) (test16_1);
#endif
	res16[16] = tname(test_cmpi_) (test16_1);
#ifndef TEST_UNSIGNED
	res16[17] = tname(test_neg_)  (test16_1);
	res16[18] = tname(test_neg_)  (-test16_1);
	res16[19] = tname(test_abs_)  (test16_1);
	res16[20] = tname(test_abs_)  (-test16_1);
#endif

	res32[ 0] = tname(test_add_)  (test32_1, test32_2);
	res32[ 1] = tname(test_sub_)  (test32_1, test32_2);
	res32[ 2] = tname(test_mul_)  (test32_1, test32_2);
	res32[ 3] = tname(test_div_)  (test32_1, test32_2);
#ifndef TEST_FLOAT
	res32[ 4] = tname(test_shl_)  (test32_1, test32_2);
	res32[ 5] = tname(test_shr_)  (test32_1, test32_2);
	res32[ 6] = tname(test_mod_)  (test32_1, test32_2);
#endif
	res32[ 7] = tname(test_div_)  (test32_1, test32_2);
	res32[ 8] = tname(test_cmp_)  (test32_1, test32_2);
	res32[ 9] = tname(test_addi_) (test32_1);
	res32[10] = tname(test_subi_) (test32_1);
	res32[11] = tname(test_subfi_)(test32_1);
	res32[12] = tname(test_muli_) (test32_1);
	res32[13] = tname(test_divi_) (test32_1);
#ifndef TEST_FLOAT
	res32[14] = tname(test_shli_) (test32_1);
	res32[15] = tname(test_shri_) (test32_1);
#endif
	res32[16] = tname(test_cmpi_) (test32_1);
#ifndef TEST_UNSIGNED
	res32[17] = tname(test_neg_)  (test32_1);
	res32[18] = tname(test_neg_)  (-test32_1);
	res32[19] = tname(test_abs_)  (test32_1);
	res32[20] = tname(test_abs_)  (-test32_1);
#endif

	printf("Result for %s\n", __PRETTY_FUNCTION__);
	for (i=0; i<TESTANZ; i++) {
#ifndef TEST_FLOAT
		printf("res16[%d] = %d\n", i, res16[i]);
		printf("res32[%d] = %d\n", i, res32[i]);
#else
		printf("res16[%d] = %f\n", i, res16[i]);
		printf("res32[%d] = %f\n", i, res32[i]);
#endif
	}
}
