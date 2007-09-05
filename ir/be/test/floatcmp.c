/*$ -std=c99 $*/

#include <stdio.h>
#include <math.h>

#define test(type, name, op)                    \
static void test_##type##_##name(type a, type b) {       \
  if (a op b)                                   \
    printf("%f " #op " %f = true\n", a, b);    \
  else                                          \
    printf("%f " #op " %f = false\n", a, b);   \
}

#define testu(type, op)                    \
static void test_##type##_##op(type a, type b) {       \
  if (op(a,b))                                   \
    printf(#op "(%f, %f) = true\n", a, b);    \
  else                                          \
    printf(#op "(%f, %f) = false\n", a, b);   \
}

#define testnu(type, op)                    \
static void test_##type##_not##op(type a, type b) {       \
  if (!op(a,b))                                 \
    printf("!" #op "(%f, %f) = true\n", a, b);  \
  else                                          \
    printf("!" #op "(%f, %f) = false\n", a, b); \
}

test(float, l,  <)
test(float, le, <=)
test(float, eq, ==)
test(float, ge, >=)
test(float, g,  >)
test(float, lg, !=)

testu(float, isgreater)
testu(float, isgreaterequal)
testu(float, isless)
testu(float, islessequal)
testu(float, islessgreater)
testu(float, isunordered)

testnu(float, isgreater)
testnu(float, isgreaterequal)
testnu(float, isless)
testnu(float, islessequal)
testnu(float, islessgreater)
testnu(float, isunordered)

/*-------------------------- */
test(double, l,  <)
test(double, le, <=)
test(double, eq, ==)
test(double, ge, >=)
test(double, g,  >)
test(double, lg, !=)

testu(double, isgreater)
testu(double, isgreaterequal)
testu(double, isless)
testu(double, islessequal)
testu(double, islessgreater)
testu(double, isunordered)

testnu(double, isgreater)
testnu(double, isgreaterequal)
testnu(double, isless)
testnu(double, islessequal)
testnu(double, islessgreater)
testnu(double, isunordered)

#undef test

double dA = 3.0, dB = 4.0, dNan = NAN;
float fA = 3.0, fB = 4.0, fNan = NAN;

int main() {
#define test(type, name, a, b) test_##type##_##name(a,b)

  test(float, l,  fA, fB);
  test(float, le, fA, fB);
  test(float, eq, fA, fB);
  test(float, ge, fA, fB);
  test(float, g,  fA, fB);
  test(float, lg, fA, fB);
  test(float, l,  fA, fNan);
  test(float, le, fA, fNan);
  test(float, eq, fA, fNan);
  test(float, ge, fA, fNan);
  test(float, g,  fA, fNan);
  test(float, lg, fA, fNan);
  test(float, lg, fNan, fNan);

  test(float, isgreater, fA, fB);
  test(float, isgreaterequal, fA, fB);
  test(float, isless, fA, fB);
  test(float, islessequal, fA, fB);
  test(float, islessgreater, fA, fB);
  test(float, isunordered, fA, fB);
  test(float, isgreater, fA, fNan);
  test(float, isgreaterequal, fA, fNan);
  test(float, isless, fA, fNan);
  test(float, islessequal, fA, fNan);
  test(float, islessgreater, fA, fNan);
  test(float, isunordered, fA, fNan);
  test(double, islessgreater, fA, fB);
  test(float, islessgreater, fNan, fNan);

  test(float, notisgreater, fA, fNan);
  test(float, notisgreaterequal, fA, fNan);
  test(float, notisless, fA, fNan);
  test(float, notislessequal, fA, fNan);
  test(float, notislessgreater, fA, fNan);
  test(float, notisunordered, fA, fNan);

  test(double, l,  dA, dB);
  test(double, le, dA, dB);
  test(double, eq, dA, dB);
  test(double, ge, dA, dB);
  test(double, g,  dA, dB);
  test(double, lg, dA, dB);
  test(double, l,  dA, dNan);
  test(double, le, dA, dNan);
  test(double, eq, dA, dNan);
  test(double, ge, dA, dNan);
  test(double, g,  dA, dNan);
  test(double, lg, dA, dNan);
  test(double, lg, dNan, dNan);

  test(double, isgreater, dA, dB);
  test(double, isgreaterequal, dA, dB);
  test(double, isless, dA, dB);
  test(double, islessequal, dA, dB);
  test(double, islessgreater, dA, dB);
  test(double, isunordered, dA, dB);
  test(double, isgreater, dA, dNan);
  test(double, isgreaterequal, dA, dNan);
  test(double, isless, dA, dNan);
  test(double, islessequal, dA, dNan);
  test(double, islessgreater, dA, dNan);
  test(double, isunordered, dA, dNan);
  test(double, islessgreater, dA, dB);
  test(double, islessgreater, dNan, dNan);

  test(double, notisgreater, dA, dNan);
  test(double, notisgreaterequal, dA, dNan);
  test(double, notisless, dA, dNan);
  test(double, notislessequal, dA, dNan);
  test(double, notislessgreater, dA, dNan);
  test(double, notisunordered, dA, dNan);
}
