#include <stdio.h>

#define SIZE 100

static void test_do_continue(int *a, int size) {
  int i = -1;

  printf("  test_do_continue:\n");
  do {
    ++i;
    if(10 == i)
      continue; // dont print
    printf("%d\n", a[i]);
  }
  while(i < size);
}

static void test_for_continue(int *a, int size) {
  int i;

  printf("  test_for_continue:\n");
  for(i = 0; i <= size; i++) {
    if(11 == i)
      continue;
    printf("%d\n", a[i]);
  }
}

static void test_while_continue(int *a, int size) {
  int i;

  i = -1;

  printf("  test_while_continue:\n");
  while(i < size) {
    i++;
    if(12 == i)
      continue;
    printf("%d\n", a[i]);
  }
}

int main(int argc, char *argv[]) {
  int i, j;
  int array[SIZE];

  printf("ContinueTest.c:\n");
  i = 0;
  while(i < SIZE) {
    array[i] = i;
    i++;
  }

  test_do_continue(array, 20);
  printf("\n");
  test_for_continue(array, 20);
  printf("\n");
  test_while_continue(array, 20);
  printf("\n");

	return 0;
}
