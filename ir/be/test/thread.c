#ifdef _WIN32
#define __thread   __declspec( thread )
#endif

__thread int tls_i[10];
__thread int tls_j = 5;

int test(int i) {
        tls_j = i;
	return tls_i[i];
}

int main()
{
    printf("tls_i: %d\n", test(3));
    printf("tls_j: %d\n", tls_j);
    return 0;
}
