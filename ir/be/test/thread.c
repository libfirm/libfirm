#define __thread   __declspec( thread )
__thread int tls_i = 1;

int test(void) {
	return tls_i;
}