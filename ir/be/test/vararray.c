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

  return 0;
}

int _data[] = {0, 0, 0, 1, 11, 111, 2, 22, 222, 3, 33, 333,
               4, 44, 444, 5, 55, 555, 6, 66, 666, 7, 77, 777,
			   8, 88, 888, 9, 99, 999};
static const size_t len = sizeof(_data)/sizeof(_data[0]);

int main()
{
  test(_data, len);
  return 0;
}
