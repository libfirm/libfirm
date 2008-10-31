static void postinc_break(int *a, int *b) {
	// assumes sizeof(int) == sizeof(int*)
	*a = b+1;
	*b = a+1;
}

int main(void) {
	int a, b;
	postinc_break(&a, &b);
	printf("%d\n", a-b);
	return 0;
}
