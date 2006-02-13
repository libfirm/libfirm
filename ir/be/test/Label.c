#include <stdio.h>

static void for_test(void)
{
  int i;

  for (i = 0; i < 10; ++i) {
    goto end;
    printf(" %d\n", i);
  }
end:
  printf(" Leaving with i = %d\n", i);
  return;
}

int main(int argc, char *argv[])
{
  printf("Label.c\n");

  goto weg;
  printf(" GOTO Failed\n");
  return 0;
weg:
  printf(" GOTO Successful\n");

  for_test();

  return 0;
}
