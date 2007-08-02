typedef struct sv SV;

struct sv {
};

typedef struct hek HEK;
struct hek {
	char    hek_key[3];
};

HEK hekimek;

int main() {
	(*(SV**) hekimek.hek_key) = (SV*) -2;
	printf("Result: %d (expected -2)\n", (int) (*(SV**) hekimek.hek_key));
	return 0;
}
