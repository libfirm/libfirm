volatile int G1, G2, G3, G4, G5;

volatile int VG;

static void __attribute__((noinline)) f(int k)
{
	(void) k;
}

static void __attribute__((noinline)) test()
{
	int foo = rand();

        // spill foo.
	int t1 = G1;
	int t2 = G2;
	int t3 = G3;
	int t4 = G4;
	int t5 = G5;

	int bar = rand();
	int i;

	G1 = t1; G2 = t2; G3 = t3; G4 = t4; G5 = t5;
	G1 = t1; G2 = t2; G3 = t3; G4 = t4; G5 = t5;
	G1 = t1; G2 = t2; G3 = t3; G4 = t4; G5 = t5;

	i = 10000;
	while (i--) {
		VG = foo;  // really keep foo in a reg.
		f(bar);   // bar live
		G1 = t1;
		VG = foo;
		G2 = t2;
		VG = foo;
		G3 = t3;
		VG = foo;
		G4 = t4;
		VG = foo;
		G5 = t5;

	}
}

int main(void)
{
	int i;

	for(i = 0; i < 50000; ++i) {
		test();
	}
}
