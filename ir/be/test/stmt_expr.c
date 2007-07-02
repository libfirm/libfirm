#include <stdio.h>

#define maxint(a,b) \
       ({int _a = (a), _b = (b); _a > _b ? _a : _b; })

int main(int argc, char *argv[])
{
  int i = argc + 3;
  int j = argc * argc;

  printf("max %d, %d = %d\n", i, j, maxint(i,j));

  return 0;
}
