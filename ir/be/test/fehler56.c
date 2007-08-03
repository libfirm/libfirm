typedef union rtunion_def
{
	int rtwint;
	int rtint;
	char *str;
	struct rtx_def *rtx;
} rtunion;

typedef struct rtx_def
{
	unsigned short code;
	int mode : 8;
	unsigned int jump : 1;
	unsigned int call : 1;
	unsigned int unchanging : 1;
	unsigned int volatil : 1;
	unsigned int in_struct : 1;
	unsigned int used : 1;
	unsigned int integrated : 1;
	rtunion fld[1];
} *rtx;

enum machine_mode {
	DImode = 6,
	HImode = 2,
	QImode = 1,
	VOIDmode = 0
};

#define GET_MODE(RTX)       ((RTX)->mode)
#define PUT_MODE(RTX, MODE) ((RTX)->mode = (MODE))

struct rtx_def bla;
int b1, b2, b3, b4;

void t(int num_eliminable, int n_reloads) {
	register rtx insn = &bla;
	int did_elimination = 0;

	if(num_eliminable)
		did_elimination = rand();

#if 0
	b1 = did_elimination;
	b2 = n_reloads;
	b3 = GET_MODE(insn);
#endif

	PUT_MODE(insn, (did_elimination ? QImode
				: n_reloads ? HImode
				: GET_MODE (insn) == DImode ? DImode
				: VOIDmode));
}

int main()
{
	bla.fld[0].rtint = 8;
	bla.mode = DImode;
	printf("Before: %d %d\n", bla.mode, bla.fld[0].rtint);
	t(0, 1);
	printf("After: %d %d\n", bla.mode, bla.fld[0].rtint);
	return 0;
}
