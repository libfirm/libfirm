int incs[14];

int bla(int b);

void simpleSort ( int lo, int hi, int d )
{
   int i, j, h, bigN, hp;
   int v;

   for (; hp >= 0; hp--) {
      h = incs[hp];
      if (hp >= 5)
         bla(lo);

      i = lo + h;
      while (1) {
         j = i;
         while (bla(d)) {
            j = j - h;
         }
       }
   }
}
