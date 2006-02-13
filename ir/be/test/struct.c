//#include <stdio.h>

struct A {
  int x;
};

struct B {
  int y;
  struct A a1;
  struct A a2;
} b = { 1, 2, 3 };

struct C {
  char s;
  int d[3];
} c = { 'q', { 4, 5, 6 } };

int e[3] = { 7, 8, 9 };

struct X {
  char c;
} x = { 'w' };

struct N {
  struct C foo[3];
  char c;
  struct B b;
} n = { '1', { 10, 11, 12 },
        '2', { 13, 14, 15 },
        '3', { 16, 17, 18 },
        'c',
        { 19, 20, 21 }
      };

int md[5][4][3] = { { { 22, 23, 24 }, { 25, 26, 27 }, { 28, 29, 30 }, { 31, 32, 33 } },
                    { { 34, 35, 36 }, { 37, 38, 39 }, { 40, 41, 42 }, { 43, 44, 45 } },
                    { { 46, 47, 48 }, { 49, 50, 51 }, { 52, 53, 54 }, { 55, 56, 57 } },
                    { { 58, 59, 60 }, { 61, 62, 63 }, { 64, 65, 66 }, { 67, 68, 69 } },
                    { { 70, 71, 72 }, { 73, 74, 75 }, { 76, 77, 78 }, { 79, 80, 81 } },
                  };

struct G {
  char z1;
  int k[2][2][2];
  char z2;
} g = {
        'u',
        { { { 82, 83 }, { 84, 85 } }, { { 86, 87 }, { 88, 89 } } },
        'v'
      };

struct X xarr[] = { { 'H' }, { 'e' }, { 'l' }, { 'l' }, { 'o' }, { ' ' }, { 'W' }, { 'o' }, { 'r' }, { 'l' }, { 'd' }, { '!' }, { '\n' } } ;

struct C carr[] = {
                    { 'k', { 90, 91, 92 } },
                    { 'm', { 93, 94, 95 } },
                    { 'n', { 96, 97, 98 } }
                  };

int main(void) {
  int i = 0;

  printf("expected 1 2 3\nfound    %d %d %d\n\n", b.y, b.a1.x, b.a2.x);

  printf("expected q 4 5 6\nfound    %c %d %d %d\n\n", c.s, c.d[0], c.d[1], c.d[2]);

  printf("expected 7 8 9\nfound    %d %d %d\n\n", e[0], e[1], e[2]);

  printf("expected w\nfound    %c\n\n", x.c);

  printf("expected 0\nfound    %d\n\n", i);

  printf("expected 1 10 11 12 | 2 13 14 15 | 3 16 17 18 | c | 19 20 21\nfound    %c %d %d %d | %c %d %d %d | %c %d %d %d | %c | %d %d %d\n\n",
      n.foo[0].s, n.foo[0].d[0], n.foo[0].d[1], n.foo[0].d[2],
      n.foo[1].s, n.foo[1].d[0], n.foo[1].d[1], n.foo[1].d[2],
      n.foo[2].s, n.foo[2].d[0], n.foo[2].d[1], n.foo[2].d[2],
      n.c, n.b.y, n.b.a1.x, n.b.a2.x);

  printf("expected numbers 22-81: %d %d %d | %d %d %d | %d %d %d | %d %d %d\n", md[0][0][0], md[0][0][1], md[0][0][2],
                                                                                md[0][1][0], md[0][1][1], md[0][1][2],
                                                                                md[0][2][0], md[0][2][1], md[0][2][2],
                                                                                md[0][3][0], md[0][3][1], md[0][3][2]);
  printf("                        %d %d %d | %d %d %d | %d %d %d | %d %d %d\n", md[1][0][0], md[1][0][1], md[1][0][2],
                                                                                md[1][1][0], md[1][1][1], md[1][1][2],
                                                                                md[1][2][0], md[1][2][1], md[1][2][2],
                                                                                md[1][3][0], md[1][3][1], md[1][3][2]);
  printf("                        %d %d %d | %d %d %d | %d %d %d | %d %d %d\n", md[2][0][0], md[2][0][1], md[2][0][2],
                                                                                md[2][1][0], md[2][1][1], md[2][1][2],
                                                                                md[2][2][0], md[2][2][1], md[2][2][2],
                                                                                md[2][3][0], md[2][3][1], md[2][3][2]);
  printf("                        %d %d %d | %d %d %d | %d %d %d | %d %d %d\n", md[3][0][0], md[3][0][1], md[3][0][2],
                                                                                md[3][1][0], md[3][1][1], md[3][1][2],
                                                                                md[3][2][0], md[3][2][1], md[3][2][2],
                                                                                md[3][3][0], md[3][3][1], md[3][3][2]);
  printf("                        %d %d %d | %d %d %d | %d %d %d | %d %d %d\n", md[4][0][0], md[4][0][1], md[4][0][2],
                                                                                md[4][1][0], md[4][1][1], md[4][1][2],
                                                                                md[4][2][0], md[4][2][1], md[4][2][2],
                                                                                md[4][3][0], md[4][3][1], md[4][3][2]);

  printf("\nexpected u | 82 83 | 84 85 | 86 87 | 88 89 | v\nfound    %c | %d %d | %d %d | %d %d | %d %d | %c\n\n", g.z1, g.k[0][0][0],
      g.k[0][0][1], g.k[0][1][0], g.k[0][1][1], g.k[1][0][0], g.k[1][0][1], g.k[1][1][0], g.k[1][1][1], g.z2);

  printf("expected Hello World!\nfound    ");
  for (i = 0; i < sizeof(xarr); i++) printf("%c", xarr[i].c);

  printf("\nexpected k 90 91 92 | m 93 94 95 | n 96 97 98\nfound    %c %d %d %d | %c %d %d %d | %c %d %d %d\n",
      carr[0].s, carr[0].d[0], carr[0].d[1], carr[0].d[2], carr[1].s, carr[1].d[0], carr[1].d[1], carr[1].d[2],
      carr[2].s, carr[2].d[0], carr[2].d[1], carr[2].d[2]);

  return 0;
}
