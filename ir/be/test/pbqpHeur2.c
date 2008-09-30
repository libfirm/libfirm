unsigned *block;
unsigned *block1, *block2, *block3, *block4, *block5;
volatile char arr[100];
unsigned ca,cb,cc;
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
unsigned h1,h2,h3;
unsigned h4,h5,h6;
unsigned h7,h8,h9;
unsigned k1,k2,k3;
unsigned k4,k5,k6;
unsigned k7,k8,k9;
unsigned k10,k11,k12;

void full_am(unsigned base, unsigned index)
{
	unsigned ca = arr[base + 4 * index] + b;

	/* user for shift const */
	b = k3_3_am(block, h1, h2, h3, 2, 3, 4);
	b = k3_3_am(block, h4, h5, h6, 2, 5, 6);
	b = k3_3_am(block, h7, h8, h9, 2, 7, 8);

	b = k3_3(block1, base + 4 * index, g2, g3, 31, 32, 33);
	b = k3_3(block2, base + 4 * index, g5, g6, 34, 35, 36);
	//b = k3_3(base + 4 * index, base + 4 * index, g8, g9, 37, 38, 39);

	/* user for computed value */
	//b = k3_3(block1, ca, k2, k3, 7, 8, 9);
	//b = k3_3(block2, ca, k5, k6, 10, 11, 12);
	b = k3_3(block3, ca, k8, k9, 13, 14, 15);
	b = k3_3(block4, ca, k11, k12, 16, 17, 18);
	b = k3_3(block5, ca, k7, k10, 19, 20, 21);
}

int main(int argc, char **argv) {
	return 0;
}
