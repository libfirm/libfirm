#define GAMMEL

double M;

int main(void)
{
  int j;
#ifdef GAMMEL
  int Ke[1], Me[42], Ce[42];
#else
	int Me[42], Ce[42];
#endif

	while(!j)
    for (j = 0; j < 1; j++)
      M = Me[j] + Me[j + 1] + Me[j + 2] + Ce[j] + Ce[j + 1] + Ce[j + 2];

	printf("done.\n");

    return 0;
}
