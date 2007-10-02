#include <stdio.h>

typedef struct sv {
}SV ;

typedef struct hek HEK;
struct hek {
	char    hek_key[4];
};

HEK hekimek;

int main() {
	(*(SV**) hekimek.hek_key) = (SV*) -2;
	printf("Result: %d (expected -2)\n", (int) (*(SV**) hekimek.hek_key));
	return 0;
}
