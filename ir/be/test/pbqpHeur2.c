/**
 * This test case should provoke incompatible heuristical decision of the
 * PBQP solver, which lead to infinity cost, i.e. no solution of the PBQP could
 * found.
 *
 * It is necessary to manipulation some cost vectors to let the PBQP solver
 * "crash". More details are shown below.
 *
 * This also seems more a missing feature of the PBQP solver than a counter
 * example against the PBQP approach for instruction selection.
 */

unsigned *block;
unsigned *block1, *block2, *block3, *block4, *block5, *block6;
unsigned *block7, *block8,*block9,*block10,*block11;
volatile unsigned arr[100];
unsigned b = 3008;
unsigned g1,g2,g3;
unsigned g4,g5,g6;
unsigned g7,g8;
unsigned h1,h2,h3;
unsigned h4,h5,h6;
unsigned h7,h8,h9;
unsigned k1,k2,k3;
unsigned k4,k5,k6;
unsigned kb1,kb2,kb3;
unsigned kc1,kc2,kc3;
unsigned kd1,kd2,kd3;
unsigned ke1,ke2,ke3;

unsigned k3_3(char* base, unsigned i1, unsigned i2, unsigned i3, unsigned k1, unsigned k2, unsigned k3)
{
	char a1, a2, a3;
	char b1, b2, b3;
	char c1, c2, c3;

	a1 = i1 + k1;
	a2 = base[i2 + k1];
	a3 = base[i3 + k1];

	b1 = i1 + k2;
	b2 = base[i2 + k2];
	b3 = base[i3 + k2];

	c1 = i1 + k3;
	c2 = base[i2 + k3];
	c3 = base[i3 + k3];

	if (a1 != a2)
		return a3;
	if (b1 != b2)
		return b3;
	if (c1 != c2)
		return c3;

	return 0;
}

unsigned k3_3_2(char* base, int i1, int i2, int i3, int k1, int k2, int k3)
{
	char a1, a2, a3;
	char b1, b2, b3;
	char c1, c2, c3;

	a1 = base[i1 + k1];
	a2 = base[i2 + k1];
	a3 = base[i3 + k1];

	b1 = base[i1 + k2];
	b2 = base[i2 + k2];
	b3 = base[i3 + k2];

	c1 = base[i1 + k3];
	c2 = base[i2 + k3];
	c3 = base[i3 + k3];

	if (a1 != a2)
		return a3;
	if (b1 != b2)
		return b3;
	if (c1 != c2)
		return c3;

	return 0;
}

unsigned k3_3_am(char* base, unsigned i1, unsigned i2, unsigned i3, unsigned k1, unsigned k2, unsigned k3)
{
	char a1, a2, a3;
	char b1, b2, b3;
	char c1, c2, c3;

	a1 = base[i1 + k1];
	a2 = base[i2 + k1];
	a3 = base[i3 + k1];

	b1 = base[i1 + k2];
	b2 = base[i2 + k2];
	b3 = base[i3 + k2];

	c1 = base[i1 + k3];
	c2 = base[i2 + k3];
	c3 = base[i3 + k3];

	if (a1 != a2)
		return a3;
	if (b1 != b2)
		return b3;
	if (c1 != c2)
		return c3;

	return 0;
}

void full_am(unsigned base, int index, unsigned base2, int index2, unsigned base3, int index3, unsigned base4, int index4, unsigned base5, int index5)
{
	/*
	 * This is the core of this example.
	 * The following line can be done with one instruction
	 * (add with address mode) on x86.
	 * We provoke four heuristical decision on nodes of this address mode
	 * pattern. The mean idea is that the root (the add node) choose this
	 * pattern, but the decision on the shift const forbid these.
	 * To achieve this you have to manipulate the cost of the shift const by
	 * hand!
	 * The other two heuristical decision ensure, that the two heuristical
	 * decision above are separated, i.e. there are at least two irreducible
	 * nodes between them.
	 */
	unsigned ca = arr[index] + base;

	/*
	 * The following function ensure irreducible users of given nodes.
	 * All of these function have to be inlined.
	 */

	/* users for shift const */
	b = k3_3_am(block, h1, h2, h3, 2, 3, 4);
	b = k3_3_am(block, h4, h5, h6, 2, 5, 6);
	b = k3_3_am(block, h7, h8, h9, 2, 7, 8);

	/* users for symconst */
	unsigned cb = arr[index2] + base2;
	b = k3_3(block1, cb, kb1, kb2, 101, 102, 103);
	unsigned cc = arr[index3] + base3;
	b = k3_3(block2, cc, kc1, kc2, 111, 112, 113);
	unsigned cd = arr[index4] + base4;
	b = k3_3(block3, cd, kd1, kd2, 121, 122, 123);
	unsigned ce = arr[index5] + base5;
	b = k3_3(block4, ce, ke1, ke2, 131, 132, 133);

	/* users for offset */
	b = k3_3_2(block5, 4 * index, g1, g2, 31, 32, 33);
	b = k3_3_2(block6, 4 * index, g3, g4, 34, 35, 36);
	b = k3_3_2(block7, 4 * index, g5, g6, 37, 38, 39);
	b = k3_3_2(block8, 4 * index, g7, g8, 40, 41, 42);

	/* users for computed value */
	b = k3_3(block9, ca, k1, k2, 13, 14, 15);
	b = k3_3(block10, ca, k3, k4, 16, 17, 18);
	b = k3_3(block11, ca, k5, k6, 19, 20, 21);
}

int main(int argc, char **argv) {
	return 0;
}
