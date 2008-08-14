/*$ -fcombo $*/
int *zptr;

#define swap(lv1, lv2) \
   { int tmp = lv1; lv1 = lv2; lv2 = tmp; }

void vswap ( int p1, int p2, int n )
{
   while (n > 0) {
      swap(zptr[p1], zptr[p2]);
      p1++; p2++; n--;
   }
}

int main() {
	return 0;
}
