typedef union blub {
	int i;
	char a[4];
} blub;

blub a = {
	.i = 23,
	.a[2] = 23, 19
};

blub b = {
	.a[2] = 23, 19,
	.i = 23
};

#if 0
blub c = {
	.a[2] = 23, 19, 17,
	.i = 23
};
#endif

blub d = {
	.a[2] = 23,
	.i = 23,
	.a[3] = 19
};

blub e = {
	.a[2] = 23,
	.i = 23,
	.a[2] = 19, 23
};

blub f = {
	.i = 23,
	.a[2] = 23,
	.a[1] = 19
};

int main(void)
{
	printf("%d %d %d %d\n", a.a[0], a.a[1], a.a[2], a.a[3]);
	printf("%d\n", b.i);
	printf("%d %d %d %d\n", d.a[0], d.a[1], d.a[2], d.a[3]);
	printf("%d %d %d %d\n", e.a[0], e.a[1], e.a[2], e.a[3]);
	printf("%d %d %d %d\n", f.a[0], f.a[1], f.a[2], f.a[3]);
	return 0;
}
