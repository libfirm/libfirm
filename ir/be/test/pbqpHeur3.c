/**
 * This test case shows that it's not possible to find a PBQP solution for
 * arbitrary pattern sets.
 *
 * You also have to use the following compiler flags:
 * -O3 -fno-reassociation
 *
 * Because of the disabled reassociation you have to add the following rule:
 * - Add(Add(Shl(ShiftConst(), index=IR_node()), imm=Const()), base=IR_node())
 *
 * To get infinity costs you have to remove the following Lea rules:
 * - Add(Shl(ShiftConst(), index=IR_node()), imm=Const())
 * - Add(Shl(ShiftConst(), index=IR_node()), base=IR_node())
 *
 * For more details take a look at the diamond function.
 */
unsigned   a;
unsigned   b;
unsigned  *gi1, gi2, gi3, gi4, gi5;
unsigned  *gi101, gi102, gi103, gi104, gi105;
unsigned  *gi201, gi202, gi203, gi204, gi205;
unsigned  *gi211, gi212, gi213, gi214, gi215;
unsigned  *gi301, gi302, gi303, gi304, gi305;
unsigned  *gi311, gi312, gi313, gi314, gi315;
unsigned  *gi401, gi402, gi403, gi404, gi405;
unsigned  *gi411, gi412, gi413, gi414, gi415;
unsigned  *gi501, gi502, gi503, gi504, gi505;
unsigned  *gi511, gi512, gi513, gi514, gi515;
unsigned  *gi601, gi602, gi603, gi604, gi605;
unsigned  *gi611, gi612, gi613, gi614, gi615;
unsigned  *gi701, gi702, gi703, gi704, gi705;
unsigned  *gi711, gi712, gi713, gi714, gi715;
unsigned  *gi801, gi802, gi803, gi804, gi805;
unsigned  *gi811, gi812, gi813, gi814, gi815;
unsigned **gp;
unsigned   use;

int main(int argc, char **argv)
{
	return 0;
}

unsigned add_1_shift_users(unsigned i1, unsigned i2, unsigned i3,
		char *k1, char *k2,	char *k3,
		unsigned **gp1, unsigned **gp2, unsigned **gp3, unsigned **gp4,
		unsigned **gp5, unsigned **gp6, unsigned **gp7, unsigned **gp8,
		unsigned **gp9,
		const int c1, const int c2, const int c3)
{
	unsigned tmp1 = i1 + c1;
	unsigned tmp2 = (i2 << 3) + c2;
	unsigned tmp3 = (i3 << 3) + c3;

	*gp1 = tmp1 + k1;
	*gp2 = tmp2 + k1;
	*gp3 = tmp3 + k1;

	*gp4 = tmp1 + k2;
	*gp5 = tmp2 + k2;
	*gp6 = tmp3 + k2;

	*gp7 = tmp1 + k3;
	*gp8 = tmp2 + k3;
	*gp9 = tmp3 + k3;

	return 0;
}

unsigned add_3_add_const_shift_users(unsigned i1, unsigned i2, unsigned i3,
		char *k1, char *k2,	char *k3,
		unsigned **gp1, unsigned **gp2, unsigned **gp3, unsigned **gp4,
		unsigned **gp5, unsigned **gp6, unsigned **gp7, unsigned **gp8,
		unsigned **gp9,
		const int c1, const int c2)
{
	unsigned tmp2 = (i2 << 3) + c1;
	unsigned tmp3 = (i3 << 3) + c2;

	*gp1 = i1 + k1;
	*gp2 = tmp2 + k1;
	*gp3 = tmp3 + k1;

	*gp4 = i1 + k2;
	*gp5 = tmp2 + k2;
	*gp6 = tmp3 + k2;

	*gp7 = i1 + k3;
	*gp8 = tmp2 + k3;
	*gp9 = tmp3 + k3;

	return 0;
}

void diamond(void)
{
	/*
	 * This is the basic structure.
	 *
	 *             as
	 *            /  \
	 *          asb  asc
	 *            \  /
	 *             sum
	 *
	 * The basic idea is to make a heuristical "consume me" decision for "as".
	 * If "asb" and "asc" have no terminal rules which consumes "as", they also
	 * have to be consumed. Therefore "sum" has to consume both paths up to
	 * "as", which isn't possible.
	 */
	unsigned as  = a << 3;
	unsigned asb = as + 123235;
	unsigned asc = as + 235346;
	unsigned sum = asb + asc;

	b = sum;

	/* Add 3 users for asb. */
	use = add_3_add_const_shift_users(asb, gi1, gi2,
			&gi3, &gi4, &gi5,
			&gp[0], &gp[1], &gp[2], &gp[3], &gp[4], &gp[5], &gp[6], &gp[7], &gp[8],
			7, 8);

	/* Add 3 users for asc. */
	use = add_3_add_const_shift_users(asc, gi101, gi102,
			&gi103, &gi104, &gi105,
			&gp[100], &gp[101], &gp[102], &gp[103], &gp[104], &gp[105], &gp[106], &gp[107], &gp[108],
			107, 108);

	/* Add 4 users for as. */
	use = add_1_shift_users(as, gi201, gi202,
			&gi203, &gi204, &gi205,
			&gp[200], &gp[201], &gp[202], &gp[203], &gp[204], &gp[205], &gp[206], &gp[207], &gp[208],
			200, 201, 202);
	use = add_1_shift_users(as, gi301, gi302,
			&gi303, &gi304, &gi305,
			&gp[300], &gp[301], &gp[302], &gp[303], &gp[304], &gp[305], &gp[306], &gp[307], &gp[308],
			300, 301, 302);
	use = add_1_shift_users(as, gi401, gi402,
			&gi403, &gi404, &gi405,
			&gp[400], &gp[401], &gp[402], &gp[403], &gp[404], &gp[405], &gp[406], &gp[407], &gp[408],
			400, 401, 402);
	use = add_1_shift_users(as, gi501, gi502,
			&gi503, &gi504, &gi505,
			&gp[500], &gp[501], &gp[502], &gp[503], &gp[504], &gp[505], &gp[506], &gp[507], &gp[508],
			500, 501, 502);
}
