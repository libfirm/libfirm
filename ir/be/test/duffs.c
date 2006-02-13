const char *str = "12345678901234567890";

char str1[20], str2[20], str3[20];

char *duff_copy(char *dst, const char *from, int count)
{
  int n = (count+7)/8;
  char *to = dst;

  switch (count % 8){
  case 0: do{ *to++ = *from++;
  case 7:     *to++ = *from++;
  case 6:     *to++ = *from++;
  case 5:     *to++ = *from++;
  case 4:     *to++ = *from++;
  case 3:     *to++ = *from++;
  case 2:     *to++ = *from++;
  case 1:     *to++ = *from++;
          }while(--n > 0);
  }
  return dst;
}

int main(int argc, char *argv[])
{
  printf("duff's Device 15 : %s\n", duff_copy(str1, str, 15));
  printf("duff's Device  3 : %s\n", duff_copy(str2, str, 3));
  printf("duff's Device  8 : %s\n", duff_copy(str2, str, 8));

  return 0;
}
