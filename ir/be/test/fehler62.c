#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef union rtunion_def {
	int rtint;
	struct rtx_def *rtx;
} rtunion;

typedef struct rtx_def
{
	unsigned short code;
	int mode : 8;
	unsigned int flag1 : 1;
	unsigned int flag2 : 2;
	rtunion fld[1];
} *rtx;

enum rtx_code {
	CODE1,
	CODE2,
	CODE3,
	AND,
	THIS_IS_NOT_AND
};

enum machine_mode {
	some_mode,
	VOIDmode,
};

char rtx_class[] = {
	'a', 'e', 'i', '1', '1'
};

#define GET_CODE(rtx)	((enum rtx_code) (rtx)->code)
#define XEXP(RTX,N)    ((RTX)->fld[N].rtx)
#define GET_RTX_CLASS(CODE)     (rtx_class[(int)(CODE)])

static rtx* t(rtx *loc, rtx insn) {
	register rtx x = *loc;
	rtx *split;
	enum rtx_code code = GET_CODE(x);

	printf("start\n");

	switch(GET_RTX_CLASS(code)) {
	case 'b':
	case '3':
		split = t(&XEXP(x, 2), insn);
		if(split)
			return split;
		/* fall through */

	case '2':
	case 'c':
	case '<':
		split = t(&XEXP(x, 1), insn);
		if(split)
			return split;
		/* fall through */

	case '1':
		if(GET_CODE(x) != AND && GET_CODE(XEXP(x, 0)) == AND) {
			return &XEXP(x, 0);
		}
		split = t(&XEXP(x, 0), insn);
		if(split) {
			/* in r15484 (and probably earlier) tail-recursion produces a jump
			   to the beginning here */
			return split;
		}
		return loc;
	}

	printf("42\n");
	return (rtx) 42;
}

struct rtx_def test_rtx_inner = {
	/*AND */ CODE2,
	VOIDmode,
	0,
	1,
	{
		{ .rtx = NULL }
	}
};

struct rtx_def test_rtx = {
	THIS_IS_NOT_AND,
	VOIDmode,
	0,
	1,
	{
		{ .rtx = &test_rtx_inner }
	}
};

int main()
{
	rtx blo = &test_rtx;
	//printf("%p %p %p\n", &test_rtx, &test_rtx_inner, &blo);
	rtx *res = t(&blo, NULL);
	//assert(res == &XEXP(blo, 0));
	printf("Res: %p\n", res);
	return 0;
}
