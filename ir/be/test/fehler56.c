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
	PSImode,
	SImode,
	PDImode,
	DImode,
	TImode,
	OImode,
	QImode,
	HImode,
	VOIDmode
};

#define GET_MODE(RTX)       ((RTX)->mode)
#define PUT_MODE(RTX, MODE) ((RTX)->mode = (MODE))

struct rtx_def bla;

void t(int did_elimination, int n_reloads) {
	rtx insn = &bla;

	printf("Before: mode %d, fldint: %d\n", insn->fld[0].rtint);
	PUT_MODE(insn, (did_elimination ? QImode
				: n_reloads ? HImode
				: GET_MODE (insn) == DImode ? DImode
				: VOIDmode));
	printf("After: mode %d, fldint: %d\n", insn->fld[0].rtint);
}

int main()
{
	t(1, 1);
	return 0;
}
