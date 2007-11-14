#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* transform into Abs node, gcc(+), icc(+), cl(+) */
int test_abs(int a) {
	return abs(a);
}

/* transform into Abs node, gcc(+), icc(+), cl(?) */
long test_labs(long a) {
	return labs(a);
}

/* transform into Abs node, gcc(+), icc(+), cl(+) */
double test_fabs(double a) {
	return fabs(a);
}

/* transform info memcpy(test + strlen(test), "ab", 2), gcc(+), icc(-), cl(-) */
char *test_strcat(void) {
	static char test[10] = "ab";
	return strcat(test, "bc");
}

/* evaluate, gcc(+), icc(-), cl(-) */
char *test_strchr(void) {
	return strchr("abc", 'b');
}

/* evaluate, gcc(+), icc(-), cl(-) */
char *test_strrchr(void) {
	return strrchr("abc", 'b');
}

/* evaluate into 0, gcc(+), icc(-), cl(-) */
int test_strcmp1(const char *s) {
	return strcmp(s, s);
}

/* transform info -(*s), gcc(+), icc(-), cl(-) */
int test_strcmp2(const char *s) {
	return strcmp("", s);
}

/* transform info *s, gcc(+), icc(-), cl(-) */
int test_strcmp3(const char *s) {
	return strcmp(s, "");
}

/* evaluate, gcc(+), icc(-), cl(-) */
int test_strcmp4(void) {
	return strcmp("ab", "cd");
}

/* evaluate into 0, gcc(+), icc(-), cl(-) */
int test_strncmp1(const char *s, int len) {
	return strncmp(s, s, len);
}

/* transform info -(*s), gcc(+), icc(-), cl(-) */
int test_strncmp2(const char *s) {
	return strncmp("", s, 1);
}

/* transform info *s, gcc(+), icc(-), cl(-) */
int test_strncmp3(const char *s) {
	return strncmp(s, "", 1);
}

/* evaluate, gcc(+), cl(-) */
int test_strncmp4(void) {
	return strncmp("ab", "cd", 2);
}

/* evaluate, gcc(+), cl(-) */
int test_strncmp5(char *a, char *b) {
	return strncmp(a, b, 0);
}

/* transform into *s = '\0', s, gcc(+), icc(-), cl(+) */
char *test_strcpy1(char *s) {
	return strcpy(s, "");
}

/* transform into memcpy(s, c, len(c)+1), gcc(+), icc(-), cl(+) */
char *test_strcpy2(char *s) {
	return strcpy(s, "ab");
}

/* evaluate, gcc(+), icc(+), cl(+) */
int test_strlen(void) {
	return strlen("ab");
}

/* transform into d, gcc(+), icc(+), cl(-) */
void *test_memcpy1(void *d, void *s) {
	return memcpy(d, s, 0);
}

/* transform into *(char *)d = *(char *)s, d, gcc(+), icc(+), cl(+) */
void *test_memcpy2(void *d, void *s) {
	return memcpy(d, s, 1);
}

/* transform into *(short *)d = *(short *)s, d, gcc(+), icc(+), cl(+) */
void *test_memcpy3(short *d, short *s) {
	return memcpy(d, s, 2);
}

/* transform into *(int *)d = *(int *)s, d, gcc(+), icc(+), cl(+) */
void *test_memcpy4(int *d, int *s) {
	return memcpy(d, s, 4);
}

/* transform into *(long long *)d = *(long long *)s, d, gcc(+), icc(+), cl(+) */
void *test_memcpy5(long long *d, long long *s) {
	return memcpy(d, s, 8);
}

/* transform into d, gcc(+), icc(+), cl(-) */
void *test_memset1(void *d, int C) {
	return memset(d, C, 0);
}

/* transform into *(char *)d = (char)C, d, gcc(+), icc(+), cl(+) */
void *test_memset2(void *d, int C) {
	return memset(d, C, 1);
}

/* transform into *(short *)d = (short)((char)C * 0x0101), d, gcc(+), icc(+), cl(+) */
void *test_memset3(short *d, int C) {
	return memset(d, C, 2);
}

/* transform into *(int *)d = (int)((char)C * 0x01010101), d, gcc(+), icc(+), cl(+) */
void *test_memset4(int *d, int C) {
	return memset(d, C, 4);
}

/* transform into *(long long *)d = (long long)((char)C) * 0x0101010101010101, d, gcc(+), icc(+), cl(+) */
void *test_memset5(long long *d, int C) {
	return memset(d, C, 8);
}

/* evaluate into 1.0, gcc(+), icc(-), cl(-) */
double test_pow1(double a) {
	return pow(1.0, a);
}

/* evaluate into 1.0, gcc(+), icc(+), cl(-) */
double test_pow2(double a) {
	return pow(a, 0.0);
}

/* transform into sqrt(a), gcc(-), icc(+), cl(-) */
double test_pow3(double a) {
	return pow(a, 0.5);
}

/* evaluate into a, gcc(+), icc(+), cl(-) */
double test_pow4(double a) {
	return pow(a, 1.0);
}

/* evaluate into 1.0/a. gcc(+), icc(+), cl(-) */
double test_pow5(double a) {
	return pow(a, -1.0);
}

/* evaluate into 1.0 gcc(+), icc(+), cl(-) */
double test_exp1(void) {
	return exp(0.0);
}

/* evaluate into M_E gcc(+), icc(-), cl(?) */
double test_exp2(void) {
	return exp(1.0);
}

/* evaluate gcc(-), icc(-), cl(?) */
double test_exp3(void) {
	return exp(7.3434);
}

/* evaluate into 0.0 gcc(+), icc(-), cl(?) */
double test_log1(void) {
	return log(1.0);
}

/* evaluate into 0.0 gcc(+), icc(-), cl(?) */
double test_log2(void) {
	return log2(1.0);
}

/* evaluate into 0.0 gcc(+), icc(-), cl(?) */
double test_log3(void) {
	return log10(1.0);
}

/* evaluate into 0.0 gcc(+), icc(-), cl(?) */
double test_trunc1(void) {
	return trunc(0.1);
}

/* evaluate into 0.0 gcc(+), icc(-), cl(?) */
double test_trunc2(void) {
	return trunc(-8.9);
}

/* transform into putchar, gcc(+), icc(-), cl(-) */
void test_printf1() {
	printf("\n");
}

/* transform into putchar(c), gcc(+), icc(+), cl(-) */
void test_printf2(char c) {
	printf("%c", c);
}

/* transform into puts(s), gcc(+), icc(-), cl(-) */
void test_printf3(char *s) {
	printf("%s\n", s);
}

/* transform into fwrite(s,strlen(s),f), gcc(+) OR fputs(s, f), icc(+), cl(-) */
void test_fprintf1(FILE *f) {
	fprintf(f, "ab");
}

/* transform into fputc(c,f), gcc(+), icc(+), cl(-) */
void test_fprintf2(FILE *f, char c) {
	fprintf(f, "%c", c);
}

/* transform into fputs(s,file), gcc(+), icc(+), cl(-) */
void test_fprintf3(FILE *f, char *s) {
	fprintf(f, "%s", s);
}

/* transform into memcpy(d,s,strlen(s)+1,1), gcc(+), icc(-), cl(-) */
void test_sprintf1(char *d) {
	sprintf(d, "ab");
}

/* transform into d[0] = c; d[1] = '\0';, gcc(-), icc(-), cl(-) */
void test_sprintf2(char *d, char c) {
	sprintf(d, "%c", c);
}

/* transform into memcpy(d, s, strlen(s)+1, 1)), gcc(-), icc(-), cl(-) */
void test_sprintf3(char *d, char *s) {
	sprintf(d, "%s", s);
}

/* transform fwrite(s,1,strlen(s),F), gcc(+), icc(-), cl(-) */
void test_fputs(FILE *f) {
	fputs("abs", f);
}

/* evaluate to 0, gcc(-), icc(-), cl(-) */
int test_fwrite1(FILE *f, char *s, int l) {
	return fwrite(s, 0, l, f);
}

/* evaluate to 0, gcc(-), icc(-), cl(-) */
int test_fwrite2(FILE *f, char *s, int l) {
	return fwrite(s, l, 0, f);
}

/* transform into fputc(s[0],F) if this is usefull ..., gcc(-), icc(-), cl(-) */
int test_fwrite3(FILE *f, char *s, int l) {
	return fwrite(s,1,1,f);
}

/* evaluate, gcc(+), cl(-) */
double test_sin1(void) {
	return sin(0.0);
}

/* transform into fsin, gcc(+), cl(-) */
double test_sin2(double x) {
	return sin(x);
}

/* transform into cos(x), gcc(+), icc(-), cl(-) */
double test_cos1(double x) {
	return cos(-x);
}

/* evaluate, gcc(+), cl(-) */
double test_cos2(void) {
	return cos(0.0);
}

/* transform into fcos, gcc(+), cl(-) */
double test_cos3(double x) {
	return cos(x);
}

/* transform into cosf(x), gcc(+), icc(-), cl(-) */
float test_cosf1(float x) {
	return cosf(-x);
}

#if 0
/* transform into cosl(x), gcc(+), icc(-), cl(-) */
long double test_cosl(long double x) {
	return cosl(-x);
}
#endif

/* evaluate, gcc(+), cl(-) */
double test_tan1(void) {
	return tan(0);
}

/* evaluate, gcc(-), cl(-) */
double test_asin1(void) {
	return asin(0.0);
}

/* evaluate, gcc(-), cl(-) */
double test_acos1(void) {
	return acos(1.0);
}

/* evaluate, gcc(+), cl(-) */
double test_atans1(void) {
	return atan(0.0);
}

/* evaluate into 0.0, gcc(+), icc(+), cl(-) */
double test_sqrt1(void) {
	return sqrt(0.0);
}

/* evaluate into 1.0, gcc(+), icc(+), cl(-) */
double test_sqrt2(void) {
	return sqrt(1.0);
}

/* evaluate, gcc(+), icc(+), cl(-) */
double test_sqrt3(void) {
	return sqrt(7.345);
}

/* evaluate into 0.0, gcc(+), icc(-), cl(?) */
double test_cbrt1(void) {
	return cbrt(0.0);
}

/* evaluate into 1.0, gcc(+), icc(-), cl(?) */
double test_cbrt2(void) {
	return cbrt(1.0);
}

/* evaluate into -1.0, gcc(+), icc(-), cl(?) */
double test_cbrt3(void) {
	return cbrt(-1.0);
}

/* transform exit(3) into a return 3, gcc(-), icc(-), cl(-) */
int main() {
	printf("%f\n", test_asin1());
	printf("%f\n", test_acos1());
	exit(0);
	return 42;
}
