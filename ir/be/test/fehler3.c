

int main(void) {


  int i = 100;
  int j = 99;

  int **zeiger;
  int *p[2];

  p[0] = &i;
  p[1] = &j;
  zeiger = &p[0];

  printf("%d %d \n",**zeiger, **(zeiger+1));

	return 0;
}
