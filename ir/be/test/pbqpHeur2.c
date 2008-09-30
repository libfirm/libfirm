unsigned *block;
unsigned *block1, *block2, *block3, *block4, *block5, *block6, *block7;
unsigned *block8,*block9,*block10,*block11,*block12,*block13,*block14,*block15,*block16,*block17,*block18,*block19;
unsigned *block20,*block21,*block22,*block23;
volatile unsigned arr[100];
unsigned ca,cb,cc,cd,ce;
unsigned b = 3008;

// TODO what if we use unsigned* instead of char*?
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

unsigned g1,g2,g3;
unsigned g4,g5,g6;
unsigned g7,g8,g9;
unsigned g10,g11,g12;
unsigned h1,h2,h3;
unsigned h4,h5,h6;
unsigned h7,h8,h9;
unsigned k1,k2,k3;
unsigned k4,k5,k6;
unsigned k7,k8,k9;
unsigned k10,k11,k12;
unsigned kb1,kb2,kb3;
unsigned kb4,kb5,kb6;
unsigned kc1,kc2,kc3;
unsigned kc4,kc5,kc6;
unsigned kd1,kd2,kd3;
unsigned kd4,kd5,kd6;
unsigned ke1,ke2,ke3;
unsigned ke4,ke5,ke6;
unsigned s1,s2,s3;
unsigned s4,s5,s6;
unsigned s7,s8,s9;

void full_am(unsigned base, int index, unsigned base2, int index2, unsigned base3, int index3, unsigned base4, int index4, unsigned base5, int index5)
{
	unsigned ca = arr[index] + base;

	/* users for shift const */
	b = k3_3_am(block, h1, h2, h3, 2, 3, 4);
	b = k3_3_am(block, h4, h5, h6, 2, 5, 6);
	b = k3_3_am(block, h7, h8, h9, 2, 7, 8);

	/* users for symconst */
	unsigned cb = arr[index2] + base2;
	b = k3_3(block12, cb, kb1, kb2, 101, 102, 103);
	unsigned cc = arr[index3] + base3;
	b = k3_3(block15, cc, kc1, kc2, 111, 112, 113);
	unsigned cd = arr[index4] + base4;
	b = k3_3(block18, cd, kd1, kd2, 121, 122, 123);
	unsigned ce = arr[index5] + base5;
	b = k3_3(block21, ce, ke1, ke2, 131, 132, 133);

	/* users for offset */
	b = k3_3_2(block1, 4 * index, g2, g3, 31, 32, 33);
	b = k3_3_2(block2, 4 * index, g5, g6, 34, 35, 36);
	b = k3_3_2(block6, 4 * index, g7, g8, 37, 38, 39);
	b = k3_3_2(block7, 4 * index, g9, g10, 40, 41, 42);

	/* users for computed value */
	b = k3_3(block3, ca, k8, k9, 13, 14, 15);
	b = k3_3(block4, ca, k11, k12, 16, 17, 18);
	b = k3_3(block5, ca, k7, k10, 19, 20, 21);
}

int main(int argc, char **argv) {
	return 0;
}
