#include<stdio.h>

int main()
{
  int i = 0;
  int j = 0;
  do
    {
      j++;
      i++;
      printf("%d\n",i);
    }
  while(j < 10);
  printf("Cyklusvariable1: %d\n",j);

  i = 0;
  do
    {
      i++;
    }
  while(i < 10);
  printf("Cyklusvariable2: %d\n",i);

  return 0;
};
