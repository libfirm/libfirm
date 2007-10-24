#include <stdio.h>

int main()  /* prints all subsets of bit vector v in numerical order */
{
  int u=0, v = 5;

  //printf( "Enter bit vector v: ");
  //scanf( "%d", &v );

  do
	printf("%d\n",u);
  while(u=(u-v)&v);

	return 0;
}
