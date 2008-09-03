float g = 0.098;

#define myftol(x) ((int)(x))

void foo(unsigned char *colors) {
	int v;
	float glow = g;

	if (glow < 0) {
		glow = 0;
	} else if(glow > 1) {
		glow = 1;
	}

	v = myftol(255*glow);
	colors[0] = colors[1] = colors[2] = v;
}

int main(void) {
	unsigned char colors[3];
	foo(colors);
	printf("%d %d %d\n", colors[0], colors[1], colors[2]);
	return 0;
}
