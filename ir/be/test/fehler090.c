unsigned int x = 12345;

int main(int argc, char **argv)
{
	unsigned short k = ~x;
	char b1 = k & 0xff;
	char b2 = k >> 8;

	printf("%d %d\n", b1, b2);
	return 0;
}
