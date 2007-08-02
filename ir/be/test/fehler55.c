typedef struct sv SV;

struct sv {
};

typedef struct hek HEK;
struct hek {
	char    hek_key[1];
};

int main() {
	HEK hekimek;
	(*(SV**) hekimek.hek_key) = (SV*) -2;
	return 0;
}
