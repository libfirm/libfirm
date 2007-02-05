#include <stdio.h>

int test(int data[], int len) {
  int arr[len];
  int i;

  for (i = 0; i < len; ++i) {
    arr[i] = data[i];
  }

  for(i = 0; i < len; ++i) {
    printf("%d\n", arr[i]);
  }
}

int _data[] = {3, 2, 1};

int main()
{
  test(_data, 3);
  return 0;
}
