
#define SIZE 10000
int arr1[SIZE];
int arr2[SIZE];

int main(int argc, char **argv)
{
	int i;
	int iter;
	int iterations = 100;

	if(argc > 1)
		iterations = atoi(argv[1]);

	for(iter = 0; iter < iterations; ++iter) {
		for(i = 0; i < SIZE; ++i) {
			arr1[i] = arr2[i] + 23.5;
		}
	}

	return 0;
}
