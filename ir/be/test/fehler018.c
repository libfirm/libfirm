#define MIN_MATCH  3
#define HASH_BITS 15
#define HASH_SIZE (unsigned)(1<<HASH_BITS)
#define HASH_MASK (HASH_SIZE-1)
#define H_SHIFT  ((HASH_BITS+MIN_MATCH-1)/MIN_MATCH)

static __inline
void UPDATE_HASH(unsigned *h, unsigned c) {
	    *h = (((*h) << H_SHIFT) ^ (c)) & HASH_MASK;
}

static unsigned ins_h = 123;
static const char *window = "Hello Test";

int main() {
	int j;

	ins_h = 0;
	for(j = 0; j < MIN_MATCH-1; j++)
		UPDATE_HASH(&ins_h, window[j]);

	rand();

	printf("Result: %d\n", ins_h);

	return 0;
}
