#include <stdio.h>
#define MAXPRINTABLE 128
#include "rantext.h"
#if ULTRIX || _AIX || _WIN32
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif

double ran();
double ran2();
char   getranchar();
#define BUFFERSIZE	14500000
long int seedi;
#define COMPRESS 0
#define UNCOMPRESS 1

double prob_tab[MAXPRINTABLE][MAXPRINTABLE];

int
add_line(char* buf, int count, int num_letters, char letter)
{
int i;
  for (i = count; i < (count + num_letters); i ++)
  {
	buf[i]=letter;
  }
	count=i;
	buf[count]='\n';
	count=count+1;
	return(count);
}

fill_text_buffer(int count, char start_char, char* text_buffer)
{
	long int total;		/* normalization */
	int i,j;		/* array indexers */
	char c1,c2;		/* character holders for level-three search */
	int	bufindex;

	/*
	 * For each ith, jth element in the frequency table, set the
	 * ith, jth, element of the probability table to the frequency
	 * table entry divided by the total.
	 */

	for (i=0;i<128;i++)
	{
		total = 0.0;
		for (j=0;j<128;j++)
		{
			total += freq_tab[i][j];
		}
		if (total<1)
			total=1;
		for (j=0;j<128;j++)
		{
			prob_tab[i][j] = (double)freq_tab[i][j]/(double)total;
		}
	}
	/*
	 * For each ith element in the probability table, make the
	 * jth elements cumulative in order to simplify 'getranchar'.
	*/

	for (i=0;i<128;i++)
	{
		for (j=1;j<128;j++)
		{
			prob_tab[i][j]+=prob_tab[i][j-1];
		}
	}

#if SDEBUG
	fprintf(stderr,
		"Probability table built, about to open file \n");
	fflush(stderr) ;
#endif /* SDEBUG */

	/*
	 * Start off the simulation with seed letter.
	 */

	c1=start_char;

	/*
	 * Get "count" characters and spit 'em out.
	 */

	count-- ; /* pre-decrement for the final new line */

	bufindex = 0 ;
	while (count>0)
	{
		c2=getranchar(c1,ran2());
		text_buffer[bufindex++]=c2 ;
#if SDEBUG>2
		fprintf(stderr,"Character number %d is %c\n",
			bufindex,c2) ;
		fflush(stderr) ;
#endif /* SDEBUG */
		c1=c2;
		count--;
	}
	/*
	 * Complete the last line
	 */

	c2 = '\n' ;
	text_buffer[bufindex++]=c2 ;
}

/* Routine For dumping contents of a buffer to the output
	Jeff Reilly, 1/15/95				*/
print_buffer(int count, char* text_buffer)

{
int i;

for (i=0;i<count;i++)
	printf("%c", text_buffer[i]);


}

char getranchar(c,rnno)
char c;
double rnno;

{
	int mid, k;
	int low=0;
	int high=127;

	/*
 * Ascend the jth column (given by c1).
 * if the cumulative probability exceeds the random number, then
 * the current (char) i is the character to return.
 */
	if (rnno > prob_tab[c][127])
		return ('e');

	for (k=0;k<7;k++)
	{
		mid = (low+high)>>1;
		if (rnno < prob_tab[c][mid])
			high = mid;
		else if (rnno > prob_tab[c][mid])
			low = mid + 1;
		else	/* exact match found -unlikely */
			return ((char)mid);
	}
	return ((char)low);
}

double ran2()
{
  seedi=((314157*seedi)+19)&0xffffff;
  return ( (double) seedi/(double)0xffffff);
}

double ran()
/* See "Random Number Generators: Good Ones Are Hard To Find", */
/*     Park & Miller, CACM 31#10 October 1988 pages 1192-1201. */
/***********************************************************/
/* THIS IMPLEMENTATION REQUIRES AT LEAST 32 BIT INTEGERS ! */
/***********************************************************/
#define _A_MULTIPLIER  16807L
#define _M_MODULUS     2147483647L /* (2**31)-1 */
#define _Q_QUOTIENT    127773L     /* 2147483647 / 16807 */
#define _R_REMAINDER   2836L       /* 2147483647 % 16807 */
{
	long lo;
	long hi;
	long test;

	hi = seedi / _Q_QUOTIENT;
	lo = seedi % _Q_QUOTIENT;
	test = _A_MULTIPLIER * lo - _R_REMAINDER * hi;
	if (test > 0) {
		seedi = test;
	} else {
		seedi = test + _M_MODULUS;
	}
	return ( (float) seedi / _M_MODULUS);
}

compare_buffer(char* buf1, int count1, char* buf2, int count2)
{
if (count1 == count2)
{
  printf("Files both have length %d\n", count1);
  if (count1 > 0)
  {
    if ( (buf1[0] == buf2[0]) && (buf1[count1-1] == buf2[count2-1]) )
    {
      printf("First character (%c) and Last Character (%c) match. \n", buf1[0], buf1[count1-1]);
    }
    else
    {
      printf("First and last characters do not match.\n");
      printf("%c does not match %c\n", buf1[0], buf2[0]);
      printf("or %c does not match %c\n", buf1[count1-1], buf2[count2-1]);
    }
  }
}
else
{
  printf("Warning: Files of differing lengths: %d and %d\n", count1, count2);
}


}

char	orig_text_buffer[BUFFERSIZE], comp_text_buffer[BUFFERSIZE], new_text_buffer[BUFFERSIZE];


int main(int argc, char *argv[])

{
int count, i;
int new_count;
char	start_char;
int comp_count = 0;

	printf("SPEC 129.compress harness\n");

	//scanf("%i	%c	%li", &count, &start_char, &seedi);
	count = 10;
	start_char=10;
	seedi = 12345;
	printf("Initial File Size:%i	Start character:%c\n", count, start_char, seedi);
	fill_text_buffer(count, start_char, orig_text_buffer);
	for (i = 1; i <= 25; i++)
	{
	  new_count=add_line(orig_text_buffer, count, i, start_char);
	  count=new_count;
	  //oper=COMPRESS;
	  printf("The starting size is: %d\n", count);
	  //comp_count=spec_select_action(orig_text_buffer, count, oper, comp_text_buffer);
	  printf("The compressed size is: %d\n", comp_count);
	  //oper=UNCOMPRESS;
	  //new_count=spec_select_action(comp_text_buffer, comp_count, oper, new_text_buffer);
	  printf("The compressed/uncompressed size is: %d\n", new_count);
	  //compare_buffer(orig_text_buffer, count, new_text_buffer, new_count);
	}
/* Remove comments for Debugging */
/*
	printf("Original Text File:\n");
	print_buffer(count, orig_text_buffer);
	printf("New Text File:\n");
	print_buffer(count, new_text_buffer); */

	return 0;
}
