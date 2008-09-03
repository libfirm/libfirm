float g = 0.098;

#define myftol(x) ((int)(x))

void foo(unsigned char *colors) {
	int v;
	float glow = g;

	v = myftol(255*glow);
	colors[0] = colors[1] = colors[2] = v;
}

int main(void) {
	unsigned char colors[3];
	foo(colors);
	printf("%d %d %d\n", colors[0], colors[1], colors[2]);
	return 0;
}
