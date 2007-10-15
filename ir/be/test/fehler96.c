int shrs1(unsigned x) {
	return -(x >> 31);
}

int main(void)
{
	printf("%d (should be -1)\n", shrs1(-3));
}
