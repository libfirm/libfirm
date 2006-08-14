/* Verify that flexible arrays can be initialized from STRING_CST
   constructors. */

/* The tests.  */
struct S3 {
  char a3c;
  char a3p[];
} a3 = {
  'o',
  "wx"
};

main()
{
  if (a3.a3c != 'o')
    abort();
  if (a3.a3p[0] != 'w')
    abort();
  if (a3.a3p[1] != 'x')
    abort();

  return 0;
}
