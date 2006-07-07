#include <stdio.h>
#include <stdlib.h>

typedef int arraya[8][8];

int main()
{
 int i = 0,j = 0;
 arraya *p;
 p = (arraya*)calloc(1,sizeof(arraya));
 (*p)[i][j] = 1;

 return 0;
}
