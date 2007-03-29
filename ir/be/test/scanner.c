/*$Header$*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>

#ifdef PLATFORM_NT
#include <io.h>
#endif
#ifdef PLATFORM_LINUX
#endif

#define TRUE 1
#define FALSE 0

#define MODE_RECOGNIZE 0
#define MODE_TRAIN1 1
#define MODE_TRAIN2 2

void alloc_td_bu();


unsigned char **cimage;
double **tds;
double **bus;
int lwidth,lheight;
int width, height,numinputs;
long i,j;

int pass_flag;
int highx[2], highy[2];
double highest_confidence[2];
int set_high[2];

#define DB1 1
#define DB2 0
#define DB3 1

FILE *fp;

int winner,numf1s,numf2s, resonant,cp,numpatterns;

double a, b, c, d, theta, delta_t;
double rho;

typedef struct {
	double *I;
	double W;
	double X;
	double V;
	double U;
	double P;
	double Q;
	double R;
		} f1_neuron;

f1_neuron *f1_layer;

typedef struct {
      double y;
      int   reset;
      } xyz;

xyz *Y;


double g(i)
int i;
{  double result;
    if (i != winner)
        result =0;
    else
       if ( Y[i].y > 0)
           result = d;
       else
           result =0;
    return(result);
}

void find_match()
{ int i;
   winner = 0;
   for (i=0;i<numf2s;i++)
       if (Y[i].y > Y[winner].y)
              winner =i;
}

double simtest()
{   int j;
   double sum,norm;
   double temp_sum;

   sum = norm =0;
   for (j=0;j<numf1s;j++)
   {
      norm += f1_layer[j].P * f1_layer[j].P;
   }
   norm = sqrt((double) norm);
   norm *= c;
   sum += norm;
   norm =0;
   for (j=0;j<numf1s;j++)
   {
      temp_sum = f1_layer[j].U * f1_layer[j].U;
      norm += temp_sum;
   }
   norm = sqrt((double) norm);
   sum += norm;
   for (j=0;j<numf1s;j++)
      f1_layer[j].R =  (f1_layer[j].U+ c * f1_layer[j].P)/sum;
   norm = 0;
   for (j=0;j<numf1s;j++)
      norm += f1_layer[j].R * f1_layer[j].R;
   norm = sqrt((double) norm);
#ifdef DEBUG
   if (DB2 && (winner==0)) printf("%5.3f",norm);
#endif
return(norm);
}


double simtest2()
{   int j;
   double Su,Sp,numerator,denom;
   double su,sp;
   double su2,sp2;
   double sup;
   double r;
   double e = 0.0000000001; /* 10e-10 */

   su = sp = sup = su2 = sp2=numerator= 0.0;
   for (j=0;j<numf1s;j++)
   {
      su += f1_layer[j].U;
      sp += f1_layer[j].P;
      su2 += f1_layer[j].U * f1_layer[j].U;
      sp2 += f1_layer[j].P * f1_layer[j].P;
      sup += f1_layer[j].U * f1_layer[j].P;
   }
   Su = ((double)numf1s*su2-su*su)/((double)numf1s*((double)numf1s-1.0));
   Su = sqrt(Su);
   Sp = ((double)numf1s*sp2-sp*sp)/((double)numf1s*((double)numf1s-1.0));
   Sp = sqrt(Sp);
   numerator = (double) numf1s * sup - su * sp;
   denom = sqrt((double) numf1s*su2 - su*su) * sqrt((double) numf1s*sp2 - sp*sp);
   r = (numerator+e)/(denom+e);

   if ((numerator == 0) || (denom==0))
   {
       fprintf(stderr,"potential div by zero");
       r=1;
   }
   if ((numerator != 0) && (denom==0))
   {
       fprintf(stderr,"div by zero");
       r=1;
   }
   r *= r;     /*  trying to match inverse images */
#ifdef DEBUG
   if (DB1) printf( "  simtest2(r) = %8.6f\n",r);
   if (DB2 && (winner==0)) printf("%8.4f",r);
#endif
   return (r);
}

void weightadj()
{ int i,j,k;
  double temp;
  double er = 0.000000001;
#ifdef DEBUG
  int bad_count;
  int good_count;
#endif

    i = winner;
#ifdef DEBUG
    fprintf(stdout,"winner %d\n",i);
#endif
    for (k=0;k<1;k++)
    {   resonant=0;
	for (j=0;j<numf1s;j++)
	{    temp = tds[j][i];
             tds[j][i] += g(i)*(f1_layer[j].P - tds[j][i])*delta_t;
	     if (fabs(temp - tds[j][i]) <= er)
		resonant = 1;
	}
#ifdef DEBUG
        bad_count=0;
        good_count = 0;
#endif
	for (j=0;j<numf1s;j++)
	{   temp = bus[j][i];
            bus[j][i] += g(i) * (f1_layer[j].P - bus[j][i]) * delta_t;
	    if ((fabs(temp - bus[j][i]) <= er) && resonant)
            {
#ifdef DEBUG
                 good_count++;
#endif
		 resonant =1;
            }
	    else
            {
#ifdef DEBUG
                 bad_count++;
#endif
		 resonant =0;
            }
	}
    }
#ifdef DEBUG
    printf("bad %d good %d\n",bad_count,good_count);
#endif
}

/* init_globs - initialize ART 2 parameters based
 *   on whether we are training a network or loading
 *   the weights of a pretrained network.
 */
void init_globs(int mode)
{
  if (mode==MODE_RECOGNIZE)
  {
     a       = 255;
     b       = 0.0;
     c       = 0.11;
     d       = 0.9;
     theta   = 1/sqrt((double) numf1s);
     delta_t = 0.1;
     rho     = 0.70;
  }
  else
  {
     a       = 255;
     b       = 10.0;
     c       = 0.11;
     d       = 0.9;
     theta   = 1/sqrt((double) numf1s);
     delta_t = 0.7;
     rho     = 0.95;
  }
} /* end of init_globs */

void init_net()
{
   int i;

   f1_layer = (f1_neuron *)malloc(numf1s * sizeof (f1_neuron));
   if (f1_layer==NULL)
   {
     fprintf(stderr,"malloc error in init_net\n");
     exit(1);
   }
   for (i=0;i<numf1s;i++)
   {

     f1_layer[i].I = (double *)malloc(2*sizeof(double));
     if (f1_layer[i].I==NULL)
     {
       fprintf(stderr,"malloc error in init_net\n");
       exit(1);
     }
     f1_layer[i].W = 0;
     f1_layer[i].X = 0;
     f1_layer[i].V = 0;
     f1_layer[i].U = 0;
     f1_layer[i].P = 0;
     f1_layer[i].Q = 0;
     f1_layer[i].R = 0;
   }

   Y = (xyz *)malloc(numf2s*sizeof(xyz));
   if (Y==NULL)
   {
     fprintf(stdout,"Malloc error for Y\n");
     exit(1);
   }

}

void analog_conv()
{ int j,lines,k;
  int x1,x2;
  double y1,y2;

   fscanf(fp,"%i",&lines);
   for (j=0;j<lines;j++)
   {   fscanf(fp,"%i %f %i %f",&x1,&y1,&x2,&y2);
       for (k=x1;k<=x2;k++)
          f1_layer[k].I[cp] = ((y2-y1)/(x2-x1) * (k-x2)) + y2;
          /*I[cp][k] = ((y2-y1)/(x2-x1) * (k-x2)) + y2;*/
   }

}

void get_pat()
{ int i;

  for(i=0;i<numf1s;i++)
     fscanf(fp,"%f",&f1_layer[i].I[cp]);
}

void show_pat()
{ int i;

   for(i=0;i<numf1s;i++)
   {  if ( (i%5) == 0)
          printf("\n");
      printf(" %8.5f ",f1_layer[i].I[cp]);
   }
   printf("\n\n");
/*   getchar();*/
}

void reset_nodes()
{ int i;

   for (i=0;i<numf1s;i++)
   {
     f1_layer[i].W = 0.0;
     f1_layer[i].X = 0.0;
     f1_layer[i].V = 0.0;
     f1_layer[i].U = 0.0;
     f1_layer[i].P = 0.0;
     f1_layer[i].Q = 0.0;
     f1_layer[i].R = 0.0;
   }
   for (i=0;i<numf2s;i++)
    {   Y[i].y = 0.0;
        Y[i].reset =0;
    }
    winner = 0;
    resonant = 0;
}


void reset_nodes2()
{ int i;

   for (i=0;i<numf1s;i++)
   {
     f1_layer[i].W = 0.0;
     f1_layer[i].X = 0.0;
     f1_layer[i].V = 0.0;
     f1_layer[i].U = 0.0;
     f1_layer[i].P = 0.0;
     f1_layer[i].Q = 0.0;
     f1_layer[i].R = 0.0;
   }
   for (i=0;i<numf2s;i++)
       Y[i].y = 0.0;
    winner = 0;
    resonant = 0;
}



void print_weights()
{ int i,j;

  /* print td's */
  printf("============  TOP down WEIGHTS ==============\n");
  for (i=0;i<numf1s;i++)
      for (j=0;j<numf2s;j++)
         if (j==(numf2s-1))
            printf(" %8.16f\n",tds[i][j]);
         else
            printf(" %8.16f ",tds[i][j]);
  /* print bu's */
  printf("============  BOTTOM up WEIGHTS ==============\n");
  for (i=0;i<numf1s;i++)
      for (j=0;j<numf2s;j++)
         if (j==(numf2s-1))
            printf(" %8.16f\n",bus[i][j]);
         else
            printf(" %8.16f ",bus[i][j]);
}


void print_f12()
{ int j;
     printf("\n\n");
     for(j=0;j<numf2s;j++)
         printf(" j = %i  Y= %9.7f\n",j,Y[j].y);
}

void train_match(int spot)
{ int j,matched,f1res,mt;
  int ti,tj,tresult;
  double tnorm;
  double xr,qr,tsum,ttemp;
  char matchtest;
  double match_confidence;
  f1res=0;
  reset_nodes();
  cp =spot;
  matched = 0;
  while (!matched)
  {
    f1res = 0;
    for (j=0;j<9 && !f1res ;j++)
    {

      /* Compute F1 layer - W values */
      tnorm = 0;
      for (ti=0;ti<numf1s;ti++)
      {
          f1_layer[ti].W = f1_layer[ti].I[cp] + a*(f1_layer[ti].U);
          tnorm += f1_layer[ti].W * f1_layer[ti].W;
      }
      tnorm =  sqrt((double)tnorm);
      /* Compute F1 layer - X values */

      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].X = f1_layer[tj].W/tnorm;

      /* Compute F1 layer - V values */

      tnorm =0;
      for (ti=0;ti<numf1s;ti++)
      {
           if (f1_layer[ti].X < theta)
                  xr = 0;
           else
                  xr = f1_layer[ti].X;
           if (f1_layer[ti].Q < theta)
                  qr = 0;
           else
                  qr = f1_layer[ti].Q;
           f1_layer[ti].V = xr + b*qr;
           tnorm += f1_layer[ti].V * f1_layer[ti].V;
      }

      /* Compute F1 layer - U values */
      tnorm = sqrt((double) tnorm);
      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].U = f1_layer[tj].V/tnorm;



      /* Compute F1 layer - P values */
      tnorm =0;
      tsum=0;
      tresult = 1;
      for (ti=0;ti<numf1s;ti++)
      {
        tsum = 0;
        ttemp = f1_layer[ti].P;

        for (tj=spot;tj<numf2s;tj++)
        {
           if ((tj == winner)&&(Y[tj].y > 0))
                 tsum += tds[ti][tj] * d;
        }

        f1_layer[ti].P = f1_layer[ti].U + tsum;

        tnorm += f1_layer[ti].P * f1_layer[ti].P;

        if (ttemp != f1_layer[ti].P)
           tresult=0;
      }
      f1res = tresult;

      /* Compute F1 - Q values */

      tnorm = sqrt((double) tnorm);
      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].Q = f1_layer[tj].P;

      /* Compute F2 - y values */
      for (tj=spot;tj<numf2s;tj++)
      {
        Y[tj].y = 0;
        if ( !Y[tj].reset )
        for (ti=0;ti<numf1s;ti++)
           Y[tj].y += f1_layer[ti].P * bus[ti][tj];
      }

      /* Find match */
      winner = 0;
      for (ti=spot;ti<numf2s;ti++)
      {
       if (Y[ti].y > Y[winner].y)
          winner =ti;
      }


    }
#ifdef DEBUG
    if (DB1) print_f12();
    if (DB1) printf("\n num iterations for p to stabalize = %i \n",j);
#endif
    match_confidence=simtest();
#ifdef DEBUG
fprintf(stdout,"rho %e\n",match_confidence);
#endif
    if ((match_confidence) > rho)
    {
#ifdef DEBUG
       if (DB2 && (winner==0)) printf("#%i",winner);
#endif
       weightadj();
       matched = 1;
    }
    else
    {    Y[winner].y = 0;
         Y[winner].reset = 1;
#ifdef DEBUG
         if (DB1) printf("#%iN",winner);
#endif
         matchtest=0;
         for (mt=spot;mt<numf2s;mt++)
              if (Y[mt].reset==0)
                   matchtest =1;
         if (matchtest)
            find_match();
         else
            matched = 1;
    }
  } /* end while */
} /* end of train_match() */

void match()
{ int j,matched,f1res,mt;
  int ti,tj,tresult;
  double tnorm;
  double xr,qr,tsum,ttemp;
  char matchtest;
  double match_confidence;
  f1res=0;

  cp =0;
  reset_nodes();

  matched = 0;
  while (!matched)
  {
    reset_nodes2();
    f1res = 0;
    for (j=0;j<9 && !f1res ;j++)
    {

      /* Compute F1 layer - W values */
      tnorm = 0;
      for (ti=0;ti<numf1s;ti++)
      {
          f1_layer[ti].W = f1_layer[ti].I[cp] + a*(f1_layer[ti].U);
          tnorm += f1_layer[ti].W * f1_layer[ti].W;
      }
      tnorm =  sqrt((double)tnorm);
      /* Compute F1 layer - X values */

      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].X = f1_layer[tj].W/tnorm;

      /* Compute F1 layer - V values */

      tnorm =0;
      for (ti=0;ti<numf1s;ti++)
      {
           if (f1_layer[ti].X < theta)
                  xr = 0;
           else
                  xr = f1_layer[ti].X;
           if (f1_layer[ti].Q < theta)
                  qr = 0;
           else
                  qr = f1_layer[ti].Q;
           f1_layer[ti].V = xr + b*qr;
           tnorm += f1_layer[ti].V * f1_layer[ti].V;
      }

      /* Compute F1 layer - U values */
      tnorm = sqrt((double) tnorm);
      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].U = f1_layer[tj].V/tnorm;



      /* Compute F1 layer - P values */
      tnorm =0;
      tsum=0;
      tresult = 1;
      for (ti=0;ti<numf1s;ti++)
      {
        tsum = 0;
        ttemp = f1_layer[ti].P;

        for (tj=0;tj<numf2s;tj++)
        {
           if ((tj == winner)&&(Y[tj].y > 0))
                 tsum += tds[ti][tj] * d;
        }

        f1_layer[ti].P = f1_layer[ti].U + tsum;

        tnorm += f1_layer[ti].P * f1_layer[ti].P;

        if (ttemp != f1_layer[ti].P)
           tresult=0;
      }
      f1res = tresult;

      /* Compute F1 - Q values */

      tnorm = sqrt((double) tnorm);
      for (tj=0;tj<numf1s;tj++)
            f1_layer[tj].Q = f1_layer[tj].P;

      /* Compute F2 - y values */
      for (tj=0;tj<numf2s;tj++)
      {
        Y[tj].y = 0;
        if ( !Y[tj].reset )
        for (ti=0;ti<numf1s;ti++)
           Y[tj].y += f1_layer[ti].P * bus[ti][tj];
      }

      /* Find match */
      winner = 0;
      for (ti=0;ti<numf2s;ti++)
      {
       if (Y[ti].y > Y[winner].y)
          winner =ti;
      }


    }
#ifdef DEBUG
    if (DB1) print_f12();
    if (DB1) printf("\n num iterations for p to stabilize = %i \n",j);
#endif
    match_confidence=simtest2();
    if ((match_confidence) > rho)
    {
       /* If the winner is not the default F2 neuron (the highest one)
        * we have a match.
        */
       if (winner!=numf2s-1)
       {
           pass_flag=1;
           fprintf(stdout,"F2 neuron %d passes vigilance with a value of %0.4f\n",winner,match_confidence);
           print_f12();
           if (match_confidence > highest_confidence[winner])
           {
             highest_confidence[winner] = match_confidence;
             set_high[winner] = TRUE;
           }
       }
       matched = 1;
    }
    else
    {    Y[winner].y = 0;
         Y[winner].reset = 1;
#ifdef DEBUG
         if (DB1) printf("#%i No",winner);
#endif
         matchtest=0;
         for (mt=0;mt<numf2s;mt++)
              if (Y[mt].reset==0)
                   matchtest =1;
         if (matchtest)
            find_match();
         else
            matched = 1;
    }
  } /* end while */
}


/*
 *  loadimage - load image to scan
 *  This was rewritten because Windows NT seems to have
 *  problems with sequential calls to getc, scanf, fread,
 *  and fread.  The bug is flaky.  It appears that I'd
 *  only get one big read and all of the rest of the reads
 *  would contain bogus data, yet no error condition is
 *  generated by the read.  Solution: one big read of the
 *  whole image and then a copy to where I really want it,
 *  cimage.
 */
void loadimage(char *input_file)
{
   int i,j;
   int fd;
   char buffer[64];
   char *superbuffer;
   if ((fd=open(input_file,O_RDONLY))==-1)
   {
     fprintf(stderr,"Error opening %s\n",input_file);
     exit(1);
   }
#ifdef DEBUG
   printf("made it to loadimage\n");
#endif

   /* Strip Format descriptor */
   read(fd,buffer,8);
   /* Read width */
   read(fd,buffer,4);
   for (i=0;i<4;i++)
     if (buffer[i] != ' ')
       width = width * 10 + buffer[i] - '0';

   /* Read height */
   read(fd,buffer,4);
   for (i=0;i<4;i++)
     if (buffer[i] != ' ')
       height = height * 10 + buffer[i] - '0';

#ifdef DEBUG
   fprintf(stderr,"width %d, height %d\n",width,height);
#endif

   superbuffer = (char *)malloc(width * height * sizeof(char ));
   if (superbuffer == NULL)
   {
      fprintf(stderr,"Problems with malloc in loadimage()\n");
      exit(1);
   }
   cimage = (unsigned char **) malloc(sizeof(unsigned char *) * height);
   if (cimage == NULL)
   {
      fprintf(stderr,"Problems with malloc in loadimage()\n");
      exit(1);
   }

   for (i=0;i<height;i++)
   {
      cimage[i] = (unsigned char *) malloc(width* sizeof(unsigned char));
      if (cimage[i]==NULL)
      {
        fprintf(stderr,"Problems with malloc in loadimage()\n");
        exit(1);
      }
   }

   read(fd,superbuffer,width*height);
   for (i=0;i<height;i++)
   {
     for (j=0;j<width;j++)
     {
       cimage[i][j] = superbuffer[i*width + j];
     }
   }


#ifdef DEBUG
   printf(" upper left 10X10 corner of image\n");
   for (i=0;i<10;i++)
   {
      for (j=0;j<10;j++)
          printf("%4d",cimage[i][j]);
      printf("\n");
   }
#endif

}

/* load_weights - load neural net weights
 * This seems to function properly which is odd because
 * loadimage does not.  The only difference really is that
 * loadimage is trying to load bytes of any value, whereas
 * load_weights is looking at a file that only contains
 * ascii characters and reading them as ints or doubles.
 */
void load_weights(char *weightfile)
{  double a;
   long i,j;

   FILE *inp;

   if ((inp=fopen(weightfile,"r"))==NULL)
   {
     fprintf(stderr,"Unable to open %s\n",weightfile);
     exit(1);
   }

   printf("made it to load_weights\n");
   fscanf (inp,"%d %d",&lwidth,&lheight);
   numf1s = numinputs = lwidth * lheight;
   numf2s = numpatterns+1;

   alloc_td_bu();

   j = 0;
   for (i=0;i<numf1s;i++)
   {   fscanf(inp,"%le",&a);
       bus[i][j] = tds[i][j] =a;
   }
} /* end of load_weights */

/* alloc_td_bu - memory alloc of top down and bottom up
 *   connections
 */
void alloc_td_bu()
{
   bus = (double **)malloc(numf1s*sizeof(double *));
   tds = (double **)malloc(numf1s*sizeof(double *));
   if ((bus==NULL)||(tds==NULL))
   {
     fprintf(stderr,"Malloc problem in load_weights\n");
     exit(1);
   }
   for (i=0;i<numf1s;i++)
   {
     bus[i] = (double *)malloc(numf2s*sizeof(double));
     tds[i] = (double *)malloc(numf2s*sizeof(double));
   }
} /* end of alloc_td_bu */

/* init_td - initialize top down weights
 *   start signifies which F2 neuron to initialize for.  Enables
 *   training on more than one image.
 */
void init_td(int start)
{
  int i,j;
  for (i=0;i<numf1s;i++)
    for (j=start;j<numf2s;j++)
      tds[i][j] = 0.0;
} /* end of init_td */

/* init_bu - initialize bottom up weights
 */
void init_bu(int start )
{
  int i,j;
  for (i=0;i<numf1s;i++)
    for (j=start;j<numf2s;j++)
      bus[i][j] = 1/(1.0 - d)/sqrt((double)numf1s);
} /* end of init_bu */

/* load_train - load a training file into
 *   location f1_layer[].I[spot]
 */
void load_train(char *trainfile,int mode, int objects)
{
   int i;
   int fd;
   char buffer[64];
   char *superbuffer;
   unsigned char t;
   int spot;

   if (mode==MODE_TRAIN1)
   {
     spot=0;
   }
   else
   {
     spot=1;
   }

   if ((fd=open(trainfile,O_RDONLY))==-1)
   {
     fprintf(stderr,"Error opening %s\n",trainfile);
     exit(1);
   }
#ifdef DEBUG
   printf("made it to load_train. opening %s\n",trainfile);
#endif

   lwidth = 0;
   lheight = 0;

   /* Strip Format descriptor */
   read(fd,buffer,8);
   /* Read width */
   read(fd,buffer,4);
   for (i=0;i<4;i++)
     if (buffer[i] != ' ')
       lwidth = lwidth * 10 + buffer[i] - '0';
   /* Read height */
   read(fd,buffer,4);
   for (i=0;i<4;i++)
     if (buffer[i] != ' ')
       lheight = lheight * 10 + buffer[i] - '0';

#ifdef DEBUG
   fprintf(stderr,"width %d, height %d\n",lwidth,lheight);
#endif

   /* The first time through we set up the network
    * based on what is read from the file.
    * The second time through (if we have more than
    * one training file, we make sure the parameters
    * match what was read the first time, e.g. the
    * f1 layer is the same size.
    */
   if (mode==MODE_TRAIN1)
   {
     numf1s = numinputs = lwidth * lheight;
     numf2s = objects+1;
     init_globs(MODE_TRAIN1);
     init_net();
   }
   else
   {
     if ((lwidth * lheight)!= numf1s)
     {
       fprintf(stderr,"Dimensions of first image do not match");
       fprintf(stderr," dimensions of second.\n");
       exit(1);
     }
   }

   superbuffer = (char *)malloc(lwidth * lheight * sizeof(char ));
   if (superbuffer == NULL)
   {
      fprintf(stderr,"Problems with malloc in loadimage()\n");
      exit(1);
   }

   read(fd,superbuffer,lwidth*lheight);
   for (i=0;i<lheight*lwidth;i++)
   {
     t = superbuffer[i];
     f1_layer[i].I[spot] = (double) t;
   }

   free (superbuffer);

} /* end of load_train */

/* This routine is used to simulate training of other objects.
 * Training on multiple objects would consume the entire execution
 * time of the benchmark.  Instead we train on a few then we simulate
 * others by copying the interconnections for the objects we are trained
 * on and then adding noise to the connections.  This simulates training on
 * other objects.  Need to blur the objects enough to overcome ART's
 * noise filtering.
 */
void sim_other_objects(int low, int high, int stop)
{
  int i,j;
  int noise1;
  double noise2;
#ifdef DEBUG
  printf("sim other low %d high %d stop %d\n",low,high,stop);
  printf("sim other numf2s %d numpat %d\n",numf2s,numpatterns);
#endif
  if (high<=low) {
    return;
  }
  srand(10);
  for (i=low;i<high;i++) {
    for (j=0;j<numf1s;j++) {
      if (i%low) {
        tds[j][i] = tds[j][0];
        tds[j][i] = bus[j][0];
      } else {
        tds[j][i] = tds[j][1];
        tds[j][i] = bus[j][1];
      }
    }
  }
  for (i=low;i<high;i++) {
    for (j=0;j<numf1s;j++) {
        noise1 = rand()&0xffff;
        noise2 = (double)noise1/(double)0xffff;
        tds[j][i] += noise2;
        bus[j][i] += noise2;
    }
  }
#if 0
  for (i=low;i<high;i++) {
    for (j=0;j<numf1s;j++) {
        fprintf(stderr,"%d %d %f\n",i,j,tds[j][i]);
    }
  }
#endif
} /* sim_other_objects */

void setup_base_pattern(int spot)
{ int i,j;

  for (i=0;i<numf1s;i++)
  {
    for (j=spot;j<numf2s;j++)
    {
      tds[i][j] = bus[i][j] = 1.0 /sqrt((double)numf1s) /(1-d);
    }
  }
}

void scan_recognize(int startx, int starty, int endx, int endy, int stride)
{
    int i,j,m,n;
    long k;


    if ((starty>(height-lheight+1))||(startx>(width-lwidth+1)))
    {
      fprintf(stderr,"Startx %d or Starty %d is out of range\n", startx, starty);
      exit(1);
    }
    if ((endy>(height-lheight+1))||(endx>(width-lwidth+1)))
    {
      fprintf(stderr,"endx %d or endy %d is out of range\n", endx, endy);
      exit(1);
    }
#ifdef DEBUG
    if (DB3)
    {
      fprintf(stdout,"made it to scan_recognize\n");
      fprintf(stdout,"w= %d h = %d lw = %d lh = %d\n",width,height,lwidth,lheight);
    }
#endif
    for (j=starty;j<endy;j=j+stride )
       for (i=startx;i<endx;i=i+stride)
       {
         k=0;
         for (m=j;m<(lheight+j);m++)
           for (n=i;n<(lwidth+i);n++)
               f1_layer[k++].I[0] = cimage[m][n];
         pass_flag =0;
         match();
         if (pass_flag==1)
         {
#ifdef DEBUG
            printf(" at X= %d Y = %d\n",i,j);
#endif
            if (set_high[0]==TRUE)
            {
              highx[0] = i;
              highy[0] = j;
              set_high[0] = FALSE;
            }
            if (set_high[1]==TRUE)
            {
              highx[1] = i;
              highy[1] = j;
              set_high[1] = FALSE;
            }
         }
#ifdef DEBUG
         else if (DB3)
            printf("0.00#%dx%da%2.1fb%2.1f\n",i,j,a,b);
#endif
       }
}



int main(argc,argv)
int argc;
char *argv[];
{  int k;
   int startx, starty;
   int endx, endy;
   int stride;
   int objects;
   int arg_index;
   char *scanfile=NULL;
   char *weightfile=NULL;
   char *trainfile1=NULL;
   char *trainfile2=NULL;

   if (argc<2)
   {
     goto Usage;
   }
   if (argc == 2)
   {
     if (strcmp(argv[1],"-v")==0)
       goto Version;
     else if (strcmp(argv[1],"-h")==0)
       goto Usage;
   }

   stride = 0;
   startx = 0;
   starty = 0;
   endy = 0;
   endx = 0;
   objects = 0;

   /* Read command line options */
   arg_index = 1;
   while (arg_index < argc-1)
   {
     if (strcmp(argv[arg_index],"-scanfile")==0)
     {
       scanfile= argv[arg_index+1];
     }
     else if (strcmp(argv[arg_index],"-weightfile")==0)
     {
       weightfile= argv[arg_index+1];
     }
     else if (strcmp(argv[arg_index],"-trainfile1")==0)
     {
       trainfile1= argv[arg_index+1];
     }
     else if (strcmp(argv[arg_index],"-trainfile2")==0)
     {
       trainfile2= argv[arg_index+1];
     }
     else if (strcmp(argv[arg_index],"-startx")==0)
     {
       startx = atoi(argv[arg_index+1]);
     }
     else if (strcmp(argv[arg_index],"-starty")==0)
     {
       starty = atoi(argv[arg_index+1]);
     }
     else if (strcmp(argv[arg_index],"-endx")==0)
     {
       endx = atoi(argv[arg_index+1]);
     }
     else if (strcmp(argv[arg_index],"-endy")==0)
     {
       endy = atoi(argv[arg_index+1]);
     }
     else if (strcmp(argv[arg_index],"-stride")==0)
     {
       stride = atoi(argv[arg_index+1]);
     }
     else if (strcmp(argv[arg_index],"-objects")==0)
     {
       objects = atoi(argv[arg_index+1]);
     }
     else
     {
       fprintf(stderr,"ERROR: Unknown option -> %s\n",argv[arg_index]);
       goto Usage;
     }
     arg_index+=2; /* this works as long as options are duals!!! */
   }

   /* Some basic error checking. */

   if (scanfile==NULL)
   {
     fprintf(stderr,"ERROR: Must specify input files\n");
     goto Usage;
   }
   if ((weightfile==NULL)&&(trainfile1==NULL))
   {
     fprintf(stderr,"ERROR: Must specify weightfile or trainfile1\n");
     goto Usage;
   }
   if ((weightfile!=NULL)&&(trainfile1!=NULL))
   {
     fprintf(stderr,"ERROR: Cannot specify weightfile and trainfile1\n");
     goto Usage;
   }

#ifdef DEBUG
   fprintf(stdout,"scanfile = %s\n weightfile = %s\n startx = %d\n starty = %d\n stride = %d\n",scanfile,weightfile,startx,starty,stride);
#endif

   loadimage(scanfile);

   /* Differentiate between loading pretrained network (load_weights)
    * and training.  Currently, loading a pretrained network
    * supports only 1 object.  If we train, we can learn to
    * recognize two objects.
    */
   if (weightfile!=NULL)
   {
     numpatterns = 1;
     if (objects==0)
	 {
        objects = numpatterns;
	 }
     load_weights(weightfile);
     init_globs(MODE_RECOGNIZE);
     init_net();
   }
   else
   {
     if (trainfile2!=NULL)
     {
       numpatterns = 2;
       if (objects<numpatterns)
	   {
          objects = numpatterns;
	   }
       load_train(trainfile1,MODE_TRAIN1,objects);
       alloc_td_bu();
       init_td(0);
       init_bu(0);
       resonant=k=0;
       while (!resonant)
       {
#ifdef DEBUG
         fprintf(stdout,"k=%d\n",k);
#endif
         train_match(0);
         k++;
       }
       load_train(trainfile2,MODE_TRAIN2,objects);
       init_globs(MODE_TRAIN2);
       init_td(1);
       init_bu(1);
       resonant=k=0;
       while (!resonant)
       {
#ifdef DEBUG
         fprintf(stdout,"k=%d\n",k);
#endif
         train_match(1);
         k++;
       }
       init_globs(MODE_RECOGNIZE);
       init_td(objects);
       init_bu(objects);
       sim_other_objects(numpatterns,objects,numf2s);
       setup_base_pattern(objects);
     }
     else
     {
       numpatterns = 1;
       if (objects<numpatterns)
	   {
          objects = numpatterns;
	   }
       load_train(trainfile1,MODE_TRAIN1,objects);
       alloc_td_bu();
       init_td(0);
       init_bu(0);
       resonant=k=0;
       while (!resonant)
       {
#ifdef DEBUG
         fprintf(stdout,"k=%d\n",k);
#endif
         train_match(0);
         k++;
       }
       init_globs(MODE_RECOGNIZE);
       init_td(1);
       init_bu(1);
       setup_base_pattern(1);
     }
   }
   /* Set endx and endy if user never specified */
   if (endy==0)
   {
     endy=height-lheight;
   }
   if (endx==0)
   {
     endx=width-lwidth;
   }
   highest_confidence[0] = 0.0;
   highest_confidence[1] = 0.0;
   highx[0] = 0;
   highx[1] = 0;
   highy[0] = 0;
   highy[1] = 0;
   set_high[0] = FALSE;
   set_high[1] = FALSE;

   scan_recognize(startx, starty, endx, endy, stride);

   fprintf(stdout,"Highest vigilance for 1 = %0.4f for object at X = %d, Y = %d\n", highest_confidence[0], highx[0], highy[0]);
   if (numpatterns==2) {
   fprintf(stdout,"Highest vigilance for 2 = %0.4f for object at X = %d, Y = %d\n", highest_confidence[1], highx[1], highy[1]);
   }
   return 0;
Usage:
   fprintf(stderr,"Usage: scanner [-startx <num>] [-starty <num>] [-endx <num>] [-endy <num>] [-stride <num>] -scanfile <filename> -trainfile1 <filename> [-trainfile2 <filename>]\n");
   exit(0);
Version:
   fprintf(stderr,"Version 1.00 \n");
   exit(1);
}
