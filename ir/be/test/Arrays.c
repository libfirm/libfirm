#include <stdio.h>
#include <string.h>

struct Arr2Object {
    int a[10];
    int inrom[10];
    int b[10];
} arr_init = {
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
  { 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 },
  { 1000, 1001 , 1002, 1003, 1004 , 1005, 1006, 1007, 1008, 1009}
};

struct ArrObject {
  int num;
};

void ctorArrObject(struct ArrObject *pThis, int i) {
  pThis->num = i;
}

void f(struct ArrObject *pThis) {
  printf("num is %d\n", pThis->num);
}

static void pass_array_test(struct ArrObject *a, int a_len)
{
  int i;

  for(i = 0; i < a_len; i++) {
    f(&a[i]);
  }
}

#define length(a)	(sizeof(a)/sizeof((a)[0]))

int main(int argc, char *argv[]) {
  int i, j, bi;
  struct ArrObject ao[10];
  struct Arr2Object a2o[2];
  struct Arr2Object a2;

  for(i = 0; i < length(ao); i++)
    ctorArrObject(&ao[i], i);

  for(i = 0; i < 10; i++) {
    f(&ao[i]);
  }

  pass_array_test(ao, length(ao));

  for(i = 0; i < length(a2o); i++)
    memcpy(&a2o[i], &arr_init, sizeof(arr_init));

  for(i = 0; i < length(a2o[0].a); i++) {
    memcpy(&a2, &a2o[0], sizeof(a2));
    j = a2.a[i];
    printf("%d\n", j);
  }

  a2o[0].a[5] = 4711;

  printf("length(a2o[0].a) = %d\n", length(a2o[0].a));
  for(i = 0; i < length(a2o[0].a); i++) {
    printf("%d\n", a2o[0].a[i]);
  }

  printf("length(a2o[1].a) = %d\n", length(a2o[1].a));
  for(i = 0; i < length(a2o[1].a); i++) {
    printf("%d\n", a2o[1].a[i]);
  }

  printf("length(a2o[0].b) = %d\n", length(a2o[0].b));
  for(i = 0; i < length(a2o[0].b); i++) {
    memcpy(&a2, &a2o[0], sizeof(a2));
    bi = a2.b[i];
    printf("%d\n", bi);
  }

  printf("inrom 0 .. 9:\n");
  for(i = 0; i < 10; i++) {
    printf("%d\n", arr_init.inrom[i]);
  }
  return 0;
}
