/* Emitter dies while emitting initialisers */

union {
	int i;
	char a[4];
} blub = {
	.i = 23,
	.a[2] = 9
};

int main()
{
	return 0;
}
