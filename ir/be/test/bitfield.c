struct a {
	unsigned int i:1;
};

struct b {
  int x:20;
  int y:8;
  int z:10;
};

struct b B = { 1, 2, 3 };
struct b C = { 1, 2, 3 };

int main()
{
  printf("sizeof(struct a) = %zu\n", sizeof(struct a));
  printf("sizeof(B) = %d\n", sizeof(B));

  printf("x = %d\n", B.x);
  printf("y = %d\n", B.y);
  printf("z = %d\n", B.z);

  B.y = C.z;

  if (C.z)
    return 0;

  return 42;
}
