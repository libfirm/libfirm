int *block1, *block2, *block3, *block4, *block5;
volatile int arr[100];
int ca,cb,cc;
int b = 3008;

// TODO what if we use int* instead of char*?
int k3_3(char* base, int i1, int i2, int i3, int k1, int k2, int k3)
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

int g1,g2,g3;
int h1,h2,h3;
int k1,k2,k3;
int k4,k5,k6;
int k7,k8,k9;
int k10,k11,k12;

void full_am(int base, int index)
{
	int ca = arr[index] + b;

	//b = k3_3(base + 4 * index, g1, g2, g3, 1, 2, 3);
	//b = k3_3(block, h1, h2, h3, 42, 5, 6);

	b = k3_3(block1, ca, k2, k3, 7, 8, 9);
	b = k3_3(block2, ca, k5, k6, 10, 11, 12);
	b = k3_3(block3, ca, k8, k9, 13, 14, 15);
	b = k3_3(block4, ca, k11, k12, 16, 17, 18);
	b = k3_3(block5, ca, k7, k10, 19, 20, 21);
}

int main(int argc, char **argv) {
	return 0;
}
