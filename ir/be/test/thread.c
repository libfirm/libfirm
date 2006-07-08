#ifdef _WIN32
#define __thread   __declspec( thread )
__thread int tls_i = 1;

int test(void) {
	return tls_i;
}
#else
int test() {
    return 1;
}
#endif

int main()
{
    printf("Result: %d\n", test());
    return 0;
}
