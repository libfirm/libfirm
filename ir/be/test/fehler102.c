/*$ -fdeconv -fif-conv $*/

char abs_Bs(char x)
{
 if (x < 0) x *= -1;
 return x;
}

char c = -23;

int main(void)
{
	printf("%d\n", abs_Bs(c));
}
