typedef struct rtx_def
{
	unsigned short code;
	int mode : 8;
	int rtint;
} *rtx;

struct rtx_def bla;

void t(int num_eliminable, int n_reloads) {
	register rtx insn = &bla;
	int did_elimination = 0;

	if(num_eliminable)
		did_elimination = rand();

	insn->mode = did_elimination ? 42 : insn->mode == 6 ? 6 : 0;
}

int main()
{
	bla.rtint = 8;
	bla.mode  = 6;
	printf("Before: %d %d\n", bla.mode, bla.rtint);
	t(0, 1);
	printf("After: %d %d\n", bla.mode, bla.rtint);
	return 0;
}
