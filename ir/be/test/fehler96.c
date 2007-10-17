int shrs1(unsigned x) {
	return -(x >> 31);
}

int shr1(int x) {
	return -(x >> 31);
}

int main(void)
{
	printf("%d (should be -1)\n", shrs1(-3));
	printf("%d (should be 1)\n", shr1(-3));
	printf("%d (should be 0)\n", shrs1(3));
	printf("%d (should be 0)\n", shr1(3));
	return 0;
}
