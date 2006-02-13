long long fac(long long n)
{
  if (n==1) return n;
  return (n*fac(n-1));
}


int main(void) {
  /*
  int i = 0;
  int j = 0;
  int k = 0;
  int l = 0;
  int summe = 0;
  	printf("ello");
	for( k = 0; k<100;k++)
        for( l = 0; l<100;l++)
	for( j = 0; j<100;j++)
	for (i= 0; i<100; i++)
	  summe++;
  */
	printf("\n%lld\n",fac(20));

}
