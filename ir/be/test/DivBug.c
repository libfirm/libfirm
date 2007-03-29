#include <string.h>

int x[(1024 / sizeof (int))];

int main(int argc, char *argv[]) {
  int y[(1024 / sizeof (int))];

  memset(y, 20, sizeof(y));
  memset(x, 22, (1024 / sizeof (int)));

  printf("DivBug. ok!\n");
  printf("Result: %d (should be 336860180)", y[(1024 / sizeof (int))-1]+x[(1024 / sizeof (int))-1]);

  return 0;
}
