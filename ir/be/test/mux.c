
int f(int a, int b) {
	return a && b ? 11 : 42;
}

int x = 2, y = 3;

int main(void) {
	int ret = 23 < f(x,y);
	printf("%d\n", ret);
	return ret;
}
