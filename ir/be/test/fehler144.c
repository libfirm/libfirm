int x=42,y=42;

int main(void) {
	int ret = x & y ? 0:1;
	printf("%d\n", ret);
	return ret;
}
