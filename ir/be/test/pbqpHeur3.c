unsigned a;
unsigned b;

unsigned  *gi1, gi2, gi3, gi4, gi5, gi6, gi7, gi8, gi9, gi10;
unsigned **gp;
unsigned use;

int main(int argc, char **argv)
{
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
	unsigned as  = a << 3;
	unsigned asb = as + 123235;
	unsigned asc = as + 235346;
	unsigned sum = asb + asc;

	b = sum;

	use = add_3_add_const_shift_users(asb, gi1, gi2,
			&gi3, &gi4, &gi5,
			&gp[0], &gp[1], &gp[2], &gp[3], &gp[4], &gp[5], &gp[6], &gp[7], &gp[8],
			7, 8);
}
