typedef struct TypeToken
{
  int Handle;
} tokentype;

typedef struct CppObjTypeDesc
{
  int Handle;
} cppobjtype;


int test(cppobjtype *CppObject, tokentype *Anchor)
{
  CppObject->Handle = Anchor->Handle;
  return 1;
}

int main()
{
  cppobjtype CppObject;
  tokentype Anchor;

  printf("%d\n", test(&CppObject, &Anchor));

  return 0;
}
