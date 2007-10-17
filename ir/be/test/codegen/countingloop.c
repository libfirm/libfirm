char *pt;

int gf, gm;

int f(int max) {
	int i;
	int f = 0;
	int m = 0;
	char *p = pt;

	for(i = 0; i < max; ++i) {
		if(pt[i] == 0) {
			f++;
		} else {
			m++;
		}
	}
	gf = f;
	gm = m;
	return 0;
}
