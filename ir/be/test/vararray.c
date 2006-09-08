int test(int data[], int len) {
  int arr[len];
  int i;

  for (i = 0; i < len; ++i) {
    arr[i] = data[i];
  }
}

int _data[] = {3, 2, 1};

int main()
{
  test(_data, 3);
  return 0;
}
