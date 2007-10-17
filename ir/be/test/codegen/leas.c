int k, g, h;

char *arr;

enum e_block_types {CLB, OUTPAD, INPAD, IO, ILLEGAL};
struct s_block {
	char *name;
	enum e_block_types type;
	int *nets;
	int x;
	int y;
};

extern struct s_block *block;
extern int num_blocks;

int my_rand(int max);

int f(int x, int y, int z)
{
	k = x;
	g = y;
	h = z;
	int b_from = my_rand(num_blocks - 1);

	while(block[b_from].type != CLB) {
		b_from = my_rand(num_blocks-1);
	}
	return 0;
}
