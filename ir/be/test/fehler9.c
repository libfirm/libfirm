struct x {
	int a;
	int b;
} ;

typedef struct x X;

X test(void);


int main(void) {

	X y;
	y = test();
	printf("%d %d\n", y.a, y.b);

	return 0;
}

X test(void) {
	X a;
	a.a = 2;
	a.b = 3;
	return(a);
}
