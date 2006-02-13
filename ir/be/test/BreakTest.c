#include <stdio.h>


static void test_for_break(int *array, int size, int end) {
  int i;

  printf("for:");
  for(i = 0 ; i < size; i++) {
    if(end == i)
      break;
    printf("%d", array[i]);
  }
}

static void test_while_break(int *array, int size, int end) {
  int i;

  i = 0;
  printf("while:");
  while(i < size) {
    if(end == i)
      break;
    printf("%d", array[i]);
    ++i;
  }
}

static void test_do_break(int *array, int size, int end) {
  int i;

  i = 0;
  printf("do:");
  do {
    if(end == i)
      break;
    printf("%d", array[i]);
    ++i;
  } while(i < size);
}

int main(int argc, char *argv[]) {
  int i, j;
  int array[20];

  i = 0;
  while(i < 20) {
      array[i] = i;
    i++;
  }

  printf("must print for:0123456789\n");
  printf("must print while:01234567891011\n");
  printf("must print do:012345678910\n\n");

  test_for_break(array, 20, 10);
  printf("\n");

  test_while_break(array, 20, 12);
  printf("\n");

  test_do_break(array, 20, 11);
  printf("\n");

  return 0;
}
