char *block;

int k3_3(int i1, int i2, int i3) {
	char a1, a2, a3;
	char b1, b2, b3;
	char c1, c2, c3;

	a1 = block[++i1];
	a2 = block[++i2];
	a3 = block[++i3];

	b1 = block[++i1];
	b2 = block[++i2];
	b3 = block[++i3];

	c1 = block[++i1];
	c2 = block[++i2];
	c3 = block[++i3];

	if (a1 != a2)
		return a3;
	if (b1 != b2)
		return b3;
	if (c1 != c2)
		return c3;

	return 0;
}

int main(int argc, char **argv) {
	return 0;
}
