/* Frontend assert while building initialisers */

union {
	int i;
	char a[4];
} blub = {
	.a[2] = 9,
	.i = 23
};

int main()
{
	return 0;
}
