char *block;

int k3_3(int i1, int i2, int i3) {
	char c1, c2, c3;

	for (;;) {
		c1 = block[++i1];
		c2 = block[++i2];
		c3 = block[++i3];
		if (c1 != c2)
			return c3;

		c1 = block[++i1];
		c2 = block[++i2];
		c3 = block[++i3];
		if (c1 != c2)
			return c3;

		c1 = block[++i1];
		c2 = block[++i2];
		c3 = block[++i3];
		if (c1 != c2)
			return c3;
	}
}

int main(int argc, char **argv) {
	return 0;
}
