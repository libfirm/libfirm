/*
##########################################################################
#                                                                        #
# Program: IeeeCC754                                                     #
#                                                                        #
# Description:                                                           #
#   IeeeCC754 or IEEE 754 Compliance Checker is a precision and range    #
#   independent tool to test whether an implementation of                #
#   floating-point arithmetic (in hardware or software) is compliant     #
#   with the principles of the IEEE 754-854 floating-point standards.    #
#   You can find out more about the testing tool IeeeCC754 at            #
#                                                                        #
#         http://win-www.uia.ac.be/u/cant/ieeecc754.html                 #
#                                                                        #
#   This tool is in parts based on and greatly benefited from the        #
#   the program FPTEST developed by Jerome Coonen. For a full            #
#   description of the extensions to FPTEST and a reference to           #
#   the original Coonen program, please refer to the URL given above.    #
#   For the options available with the program IeeeCC754 and its         #
#   compatibility with David Hough's hexadecimal UCB format, we          #
#   also refer to the file readme.usage.                                 #
#                                                                        #
#  Usage: see readme.usage                                               #
#                                                                        #
#  Responsible authors:                                                  #
#         Brigitte Verdonk                                               #
#         Annie Cuyt                                                     #
#                                                                        #
#  Contributors:                                                         #
#         Tarun Agarwal (05-07/2002)                                     #
#         Johan Bogo (1998-1999)                                         #
#         Tim Gevers (10-12/2000)                                        #
#         Debby Ooms (1996-1997)                                         #
#         Geert Vermuyten (1996-1997)                                    #
#         Dennis Verschaeren (09/1996-06/2000)                           #
#                                                                        #
#  Copyright (C) 2000  University of Antwerp                             #
#                                                                        #
#  This program can be obtained from the authors, free, but WITHOUT ANY  #
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or     #
#  FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                        #
#  Contact:                                                              #
#       Brigitte.Verdonk@uia.ua.ac.be                                    #
#       Department of Mathematics and Computer Science                   #
#       University of Antwerp (UIA)                                      #
#       Universiteitsplein 1                                             #
#       B2610 Antwerp, BELGIUM                                           #
#                                                                        #
##########################################################################

Filename:
       $RCSfile$

Last updated:
       $Date$

*/

#ifndef _UCB_H
#define _UCB_H

// ----
// Includes
// ----
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <fstream.h>
#include <Fp.h>

// ----
// Defines
// ----
#define BUF_LEN    1024
#define FP_STR      256

#define NO_FLAGS_INEXACT      16
#define NO_FLAGS_OVERFLOW     8
#define NO_FLAGS_UNDERFLOW    4
#define NO_FLAGS_INVALID      2
#define NO_FLAGS_DIV_BY_ZERO  1

extern int ieee;

template<class T> class UCB
{

protected:

  fstream infile, outfile,logfile;
  char dest[ BUF_LEN ];
  strstream logstream;

  int line,sizeE,sizeM,hidden,dsizeE,dsizeM,dhidden,noFlags,signedZero,ieeeVector;
  char operation[ 10 ],prec,rounding,compare[ 5 ],exceptions[ 10 ];
  T operand1,operand2,result;
  int pre, post; // to accomodate '?'
  unsigned int errors,warnings,skipped;

  void PrintError( T &res );

public:

  int signalu,notsignalu,signalv,signalw,notsignalv,notsignalw,nou,nov,now,allops;

  UCB( );
  ~UCB( );

  void SetFPRound ( );

  int OpenInput( char* filename );
  int OpenOutput( char* filename );
  int OpenLogFile( char* filename,int argc,char **argv );
  int ReadLine( char* str=NULL,int sZero=1, int nflags=0,int *lines=NULL );

  char* GetOperation( );
  char  GetPrecision( );
  char  GetRounding( );
  char* GetCompare( );
  char* GetExceptions( );
  T &  GetOperand1( );
  T &  GetOperand2( );
  T &  GetResult( );

  char* DoLine( int tiny,int inf, int nan);
  void Compare ( T &reslt );
  void Close( int tiny );
};


template <class T>
UCB<T>::UCB( )
{
  line = 0;
  //  result.SetEnvironment(); change BV
  signalu = 0;
  signalv = 0;
  signalw = 0;
  notsignalu = 0;
  notsignalv = 0;
  notsignalw = 0;
  nou= 1;
  nov = 1;
  now = 1;
  pre = post = 0;
  errors = 0;
  warnings = 0;
  skipped = 0;
  allops = 0;
}

template <class T>
UCB<T>::~UCB( )
{}


template <class T>
int UCB<T>::OpenInput( char* filename )
{
  infile.open( filename,ios::in );
  if ( infile.good( ) )
    return 1;

  return 0;
}

template <class T>
void UCB<T>::Close( int tiny )
{
  logfile << "Summary:  " << endl;
  logfile << "--------  " << endl;

  if ( tiny )   { // conclusion underflow
    if ( ucb.nou )  {
      if ( ucb.nov )  {
        if ( !( ucb.now ) )
          logfile << "Warning: only 'w' underflow cases in the testset" << endl;
      } else
        if ( ucb.now )
          logfile << "Warning: only 'v' underflow cases in the testset" << endl;
        else
          logfile << "Warning: only 'v' and 'w' underflow cases in the testset" << endl;
    } // if
    else {
      if ( ucb.nov )  {
        if ( ucb.now )  {
          logfile << "Warning: no special 'v' or 'w' underflow cases in the testset" << endl;
          if ( ucb.signalu && !ucb.notsignalu )
            logfile << "Implementation signals underflow in case the result" << endl << "(1) is tiny after rounding and" << endl << "(2) suffers denormalization loss" << endl << "('u' -  underflow)" << endl;
        } else
          logfile << "Warning: no special 'v' underflow cases in the testset" << endl;
      } else {
        if ( ucb.now )  {
          logfile << "Warning: no special 'w' underflow cases in the testset" << endl;
          if ( ( ucb.signalu && !ucb.notsignalu )  && ( ucb.signalv && !ucb.notsignalv ) )
            logfile << "Implementation signals underflow in case the result" << endl << "(1) is tiny after rounding and" << endl << "(2) raises the inexact exception" << endl << "('v' - underflow)" << endl;
        } else if ( ucb.signalu && !ucb.notsignalu )  {
          if ( ucb.signalw && !ucb.notsignalw )
            logfile << "Implementation signals underflow in case the result" << endl << "(1) is tiny before rounding and" << endl << "(2) raises the inexact exception" << endl << "('w' - underflow)" << endl;
          else if ( ucb.signalv && !ucb.notsignalv )
            logfile << "Implementation signals underflow in case the result" << endl << "(1) is tiny after rounding and" << endl << "(2) raises the inexact exception"<< endl << "('v' - underflow)" << endl;
          else
            logfile << "Implementation signals underflow in case the result" << endl << "(1) is tiny after rounding and" << endl << "(2) suffers denormalization loss" << endl << "('u' -  underflow)" << endl;
        } // else
      } // else
    } // else
  } // if tiny

  logfile << "Errors:   " << errors << "/" << allops << endl;
  logfile << "Warnings: " << warnings << "/" << allops << endl;
  logfile << "Skipped:  " << skipped << "/" << allops << endl;
}

template <class T>
int UCB<T>::OpenOutput( char* filename )
{
  outfile.open( filename,ios::out );
  if ( outfile.good( ) )
    return 1;

  return 0;
}

template <class T>
int UCB<T>::OpenLogFile( char* filename,int argc, char **argv )
{
  logfile.open( filename,ios::out );
  if ( logfile.good( ) )  {
    logfile << "Testrun: ";
    for ( int i = 0; i < argc;i++ )
      logfile << argv[ i ]  << " ";
    logfile << endl << endl << flush;
    return 1;
  }

  return 0;
}

template <class T>
int UCB<T>::ReadLine( char* str,int sZero,int nflags,int *lines )
{
  int i,j,k,gotInput=0,count;
  Hex tmphex,tmphex2;
  Bitstring tmpbitstring,tmpbitstring2;
  char buf[ BUF_LEN ],tmp[ FP_STR ];
  line++;
  signedZero=sZero;

  pre = post = 0;
  if( !str ) {
    if ( !infile.eof( ) ) {
      infile.getline( buf,BUF_LEN );
      if( strlen( buf )  !=0 )
        gotInput=1;
    }

    if( nflags )
      noFlags = nflags;
  } else {
    strncpy( buf,str,strlen( str ) );
    gotInput=1;
    line=*lines;
    noFlags=nflags;
  }


  if( gotInput ) {
    i=0;
    while ( buf[ i ] != ' ' ) {
      operation[ i ] =buf[ i ];
      i++;
    }

    operation[ i ] ='\0';

    if ( !isdigit( operation[ strlen( operation ) -1 ] ) ) {
      prec= operation[ strlen( operation ) -1 ];
      operation[ strlen( operation ) -1 ] ='\0';
      switch ( prec ) {
        case 's':
          sizeE=8;
          sizeM=23+1;
          hidden=1;
          ieeeVector=0;
          break;
        case 'S':
          sizeE=8;
          sizeM=23+1;
          hidden=1;
          ieeeVector=1;
          break;
        case 'd':
          sizeE=11;
          sizeM=52+1;
          hidden=1;
          ieeeVector=0;
          break;
        case 'D':
          sizeE=11;
          sizeM=52+1;
          hidden=1;
          ieeeVector=1;
          break;
        case 'l':
          sizeE=15;
          sizeM=64 + 1;
          hidden=0;
          ieeeVector=0;
          break;
        case 'L':
          sizeE=15;
          sizeM=64 + 1;
          hidden=0;
          ieeeVector=1;
          break;
        case 'q':
          sizeE=15;
          sizeM=112+1;
          hidden=1;
          ieeeVector=0;
          break;
        case 'Q':
          sizeE=15;
          sizeM=112+1;
          hidden=1;
          ieeeVector=1;
          break;
        case 'm':
          sizeE=15;
          sizeM=240 + 1;
          hidden = 0;
          ieeeVector=0;
          break;
        case 'M':
          sizeE=15;
          sizeM=240 + 1;
          hidden = 0;
          ieeeVector=1;
          break;
      }
    } else {

      ieeeVector=0; // Cannot be ieeeVector!

      i--;

      while ( isdigit( buf[ i ] ) )  i--; // rewind

      i++;
      operation[ i ]  = '\0'; // ignore digit after operation
      j = 0;

      while ( buf[ i ] != ' ' )
        tmp[ j++ ] =buf[ i++ ];

      tmp[ j ] ='\0';
      sizeE = atoi( tmp );

      while ( buf[ ++i ] == ' ' );

      j = 0;

      while ( buf[ i ] != ' ' )
        tmp[ j++ ] =buf[ i++ ];

      tmp[ j ] ='\0';
      hidden = atoi( tmp );

      while ( buf[ ++i ] == ' ' );

      j = 0;

      while ( buf[ i ]  != ' ' )
      tmp[ j++ ] =buf[ i++ ];

      tmp[ j ] ='\0';
      sizeM = atoi( tmp );
      sizeM++ ;  // +1 for the sign
    }

    while ( buf[ i ]  == ' ' )  i++;

    // read destination format
    if ( ( strncmp( operation,"rt",2 )  == 0 )  || ( strncmp( operation,"ct",2 )  == 0 ) )  {
      if ( !isdigit( buf[ i ] ) )  {
        prec = buf[ i ];
        switch ( prec )  {
          case 's':
            dsizeE=8;
            dsizeM=23+1;
            dhidden=1;
            break;
          case 'd':
            dsizeE=11;
            dsizeM=52+1;
            dhidden=1;
            break;
          case 'l':
            dsizeE=15;
            dsizeM=64 + 1;
            dhidden=0;
            // cout << 'l' << endl;
            break;
          case 'q':
            dsizeE=15;
            dsizeM=112+1;
            dhidden=1;
            break;
          case 'm':
            dsizeE=15;
            dsizeM=240 + 1;
            dhidden = 0;
            break;
        } // switch

        i++;
        while ( buf[ i ]  == ' ' )  i++;
      } // if
      else {
        j = 0;

        while ( buf[ i ]  != ' ' )
          tmp[ j++ ] =buf[ i++ ];
        tmp[ j ] ='\0';
        dsizeE = atoi( tmp );

        while ( buf[ ++i ] == ' ' );

        j = 0;
        while ( buf[ i ]  != ' ' )
         tmp[ j++ ]  = buf[ i++ ];
        tmp[ j ] ='\0';
        dhidden = atoi( tmp );

        while ( buf[ ++i ] == ' ' );
        j = 0;

        while ( buf[ i ]  != ' ' )
          tmp[ j++ ] =buf[ i++ ];
        tmp[ j ] ='\0';
        dsizeM = atoi( tmp );
        dsizeM++;

      } // else

      while ( buf[ i ]  == ' ' )  i++;
    } // if
    else {
      dsizeE = sizeE;
      dhidden = hidden;
      dsizeM = sizeM;
    } // else

    rounding=buf[ i ];

    while ( buf[ ++i ] == ' ' );

    j=0;

    while ( buf[ i ]  != ' ' )
       compare[ j++ ] =buf[ i++ ];

    compare[ j ] ='\0';

    while ( buf[ ++i ] == ' ' );

    j=0;
    while ( buf[ i ]  != ' ' )  {
      exceptions[ j++ ] =buf[ i++ ];
      exceptions[ j++ ] =' ';
    }
    exceptions[ j ] ='\0';

    while ( buf[ ++i ] == ' ' );

    if ( ( strncmp( operation,"ci",2 ) ==0 )  ||
         ( strncmp( operation,"cu",2 ) ==0 ) )  {
      count = 32; // 32 bit integer
      i += 2; // avoid 0x

      for ( j=0; j<count;j++ )  {
      if ( isdigit( buf[ i ] )  || ( ( buf[ i ]  >= 'a' )  && ( buf[ i ]  <= 'f' ) ) )
          tmp[ j ]  = buf[ i++ ];
        else {
          tmp[ j ]  = 0;
          break;
        }
      } // for

    for ( ; j<count;j++ )
        tmp[ j ]  = 0;

    } else if ( ( strncmp( operation,"cI",2 ) ==0 )  ||
                ( strncmp( operation,"cU",2 ) ==0 ) )  {
      count = 64; // 64 bit integer
      i += 2; // avoid 0x
      for ( j=0; j<count;j++ )  {
      if ( isdigit( buf[ i ] )  || ( ( buf[ i ]  >= 'a' )  && ( buf[ i ]  <= 'f' ) ) )
          tmp[ j ]  = buf[ i++ ];
        else {
          tmp[ j ]  = 0;
          break;
        }
      }
      for ( ; j<count;j++ )
        tmp[ j ]  = 0;

    } else if ( strncmp( operation,"d2b",3 )  == 0 )  {
      j = 0;
      while ( buf[ i ]  != ' ' )  {
        tmp[ j++ ]  = buf[ i++ ];
      }

    } else {
      count =( int )  ceil( ( double ) ( sizeM+sizeE ) /32.0 ) *8;
      for ( j=0; j<count ;j++ ) {
        if ( buf[ i ] ==' ' )
          i++;
        tmp[ j ]  = buf[ i++ ];
      }
    }
    tmp[ j ] ='\0';

    if ( strncmp( operation,"d2b",3 )  == 0 )  {
      operand1 = T( sizeM-1, sizeE, hidden ); // sets Mantissa and exp right
      operand1.decimal = new char[ maxstr ];
      for ( k = 0; k <= j; k++ )
        operand1.decimal[ k ]  = tmp[ k ];
      operand1.decimal[ k ]  = '\n';
    } else if ( strncmp( operation,"ci",2 ) ==0 )  {
      tmphex.StringToBitstr( tmp );
      operand1 = T( tmphex,32,0,0 );
    } else if ( strncmp( operation,"cu",2 ) ==0 )  {
      tmphex.StringToBitstr( tmp );
      operand1 = T( tmphex,32,0,0 );
    } else if ( strncmp( operation,"cI",2 ) ==0 )  {
      tmphex.StringToBitstr( tmp );
      operand1 = T( tmphex,64,0,0 );
    } else if ( strncmp( operation,"cU",2 ) ==0 )  {
      tmphex.StringToBitstr( tmp );
      operand1 = T( tmphex,64,0,0 );
    } else {
      tmphex.StringToBitstr( tmp );
      operand1 = T( tmphex,sizeM-1,sizeE,hidden );
    }

    i++;
    count =( int )  ceil( ( double ) ( sizeM+sizeE ) /32.0 ) *8; // reset count!

    for ( j=0; j<count ;j++ ) {
      if ( buf[ i ] ==' ' )
        i++;
      tmp[ j ]  = buf[ i++ ];
    }

    tmp[ j ] ='\0';
    tmphex.StringToBitstr( tmp );
    operand2 = T( tmphex,sizeM-1,sizeE,hidden );

    if ( buf[ i+1 ]  == '?' )  {
      pre = 1;
    } else {
      if ( ( strncmp( operation,"ri",2 ) ==0 )  ||
           ( strncmp( operation,"ru",2 ) ==0 ) )  {
        count = 32; // 32 bit integer
        i += 3; // avoid 0x
        for ( j=0; j<count;j++ )  {
          if ( isdigit( buf[ i ] )  || ( ( buf[ i ]  >= 'a' )  && ( buf[ i ]  <= 'f' ) ) )
            tmp[ j ]  = buf[ i++ ];
          else {
            tmp[ j ]  = 0;
            break;
          }
        } // for
        for ( ; j<count;j++ )
          tmp[ j ]  = 0;
      }
      else if ( ( strncmp( operation,"rI",2 ) ==0 )  ||
                ( strncmp( operation,"rU",2 ) ==0 ) )  {
        count = 64; // 64 bit integer
        i += 3; // avoid 0x
        for ( j=0; j<count;j++ )  {
          if ( isdigit( buf[ i ] )  || ( ( buf[ i ]  >= 'a' )  && ( buf[ i ]  <= 'f' ) ) )
            tmp[ j ]  = buf[ i++ ];
          else {
            tmp[ j ]  = 0;
            break;
          }
        } // for
        for ( ; j<count;j++ )
          tmp[ j ]  = 0;
      } else if ( strncmp( operation,"b2d",3 )  == 0 )  {
        i++;
        j = 0;
        while ( buf[ i ]  != '\n' )  {
          tmp[ j++ ]  = buf[ i++ ];
        }
      } else {
        count =( int )  ceil( ( double ) ( dsizeM+dsizeE ) /32.0 ) *8;
        i++;
        for ( j=0; j<count ;j++ ) {
          if ( buf[ i ] ==' ' )
            i++;
          tmp[ j ]  = buf[ i++ ];
        }
      }

      tmp[ j ] ='\0';

      if ( strncmp( operation,"b2d",3 )  == 0 )  {
        result.decimal = new char[ maxstr ];
        for ( k = 0; k <= j; k++ )
          result.decimal[ k ]  = tmp[ k ];
        result.decimal[ k ]  = '\0';
      }

      else if ( strncmp( operation,"ri",2 ) ==0 )  {
        tmphex.StringToBitstr( tmp );
        result = T( tmphex,32,0,0 );
      } else if ( strncmp( operation,"ru",2 ) ==0 )  {
        tmphex.StringToBitstr( tmp );
        result = T( tmphex,32,0,0 );
      } else if ( strncmp( operation,"rI",2 ) ==0 )  {
        tmphex.StringToBitstr( tmp );
        result = T( tmphex,64,0,0 );
      } else if ( strncmp( operation,"rU",2 ) ==0 )  {
        tmphex.StringToBitstr( tmp );
        result = T( tmphex,64,0,0 );
      } else {
        tmphex.StringToBitstr( tmp );
        result = T( tmphex,dsizeM-1,dsizeE,dhidden );
      }

      return 1;
    }
  }
  return 0;
}

template <class T>
char* UCB<T>::GetOperation( )
{
  return operation;
}

template <class T>
char  UCB<T>::GetPrecision( )
{
  return prec;
}

template <class T>
char  UCB<T>::GetRounding( )
{
  return rounding;
}

template <class T>
char* UCB<T>::GetCompare( )
{
  return compare;
}

template <class T>
char* UCB<T>::GetExceptions( )
{
  return exceptions;
}

template <class T>
T &  UCB<T>::GetOperand1( )
{
  return operand1;
}

template <class T>
T &  UCB<T>::GetOperand2( )
{
  return operand2;
}

template <class T>
T &  UCB<T>::GetResult( )
{
  return result;
}

template <class T>
char* UCB<T>::DoLine( int tiny,int inf, int nan)
{
  T res;
  int i = 0;

  if ( !( tiny )  && ( operand1.istiny( )  || operand2.istiny( )  || result.istiny( ) ) )
    return NULL; // do not test tiny denormalized numbers
  else if ( !( inf )  && ( operand1.isInf( )  || operand2.isInf( )  || result.isInf( ) ) )
    return NULL; // do not test infinities
  else if ( !( nan )  && ( operand1.isNan( )  || operand2.isNan( )  || result.isNan( ) ) )
    return NULL; // do not test NaNs
  // logstream.seekp(0,ios::beg);

  allops++;
  SetFPRound( );
  result.ClearFPEnvironment( );

#ifdef BasicOperations
  if ( strncmp( operation,"add",3 ) ==0 )
    res = operand1 + operand2;
  else if( strncmp( operation,"sub",3 ) ==0 )
    res = operand1 - operand2;
  else if( strncmp( operation,"mul",3 ) ==0 )
    res = operand1 * operand2;
  else if( strncmp( operation,"div",3 ) ==0 )
    res = operand1 / operand2; // debug
  else if( strncmp( operation,"rem",3 ) ==0 )
    res = operand1 % operand2;
  else if( strncmp( operation,"sqrt",4 ) ==0 )
    res = operand1.sqrt( );
#endif

#ifdef Conversions
  if ( strncmp( operation,"rt",2 ) ==0 )
    res = operand1.roundto( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"ct",2 ) ==0 )
    res = operand1.copyto( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"i",1 ) ==0 )
    res = operand1.rint( );
  else if ( strncmp( operation,"ri",2 ) ==0 )
    res = operand1.ri( );
  else if ( strncmp( operation,"ru",2 ) ==0 )
    res = operand1.ru( );
  else if ( strncmp( operation,"rI",2 ) ==0 )
    res = operand1.rI( );
  else if ( strncmp( operation,"rU",2 ) ==0 )
    res = operand1.rU( );
  else if ( strncmp( operation,"ci",2 ) ==0 )
    res = operand1.ci( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"cu",2 ) ==0 )
    res = operand1.cu( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"cI",2 ) ==0 )
    res = operand1.cI( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"cU",2 ) ==0 )
    res = operand1.cU( dsizeE,dsizeM-1,dhidden );
  else if ( strncmp( operation,"b2d",3 ) ==0 )  {
    i = 0;
    while ( result.decimal[ i ]  != 'E' )  {
      i++;
    }
    if ( result.decimal[ 0 ]  == '+' || result.decimal[ 0 ]  == '-' )
      i--;

    res = operand1.b2d( i );
  }
  else if ( strncmp( operation,"d2b",3 ) ==0 )  {
    res = operand1.d2b( );
  }
#if defined(IntelPentium) && defined(NONE_TEST) && defined(Conversions)
  else if( strncmp( operation,"rem",3 ) ==0 )
    res = operand1 % operand2;
  else if( strncmp( operation,"sqrt",4 ) ==0 )
    res = operand1.sqrt( );
#endif

#endif

  Compare( res );
  return dest;
}

template <class T>
void UCB<T>::PrintError( T &res )
{
  unsigned int i;
  if ( (!ieeeVector) || (ieee) )
    errors++; // total number of errors encountered
  else
    warnings++;
  logfile<<"Operation: " << operation << endl;
  switch ( rounding )  {
    case 'n':
      logfile<<"Round to nearest" << endl;
      break;
    case 'z':
      logfile<<"Round to zero" << endl;
      break;
    case 'p':
      logfile<<"Round up" << endl;
      break;
    case 'm':
      logfile<<"Round down" << endl;
      break;
  } // switch
  logfile<<"Operand 1: " << operand1 << endl;
  logfile<<"Operand 2: " << operand2 << endl;
  logfile<< "Flags expected: ";
  for( i=0 ; i < strlen( exceptions );i++ )
    switch ( exceptions[ i ] ) {
      case 'x':
        logfile<<"x ";
        break;
      case 'o':
        logfile<<"o ";
        break;
      case 'u':
        logfile<<"u ";
        break;
      case 'a':
        logfile<<"v ";
        break;
      case 'b':
        logfile<<"w ";
        break;
      case 'v':
        logfile<<"i ";
        break;
      case 'd':
        logfile<<"z ";
        break;
    } // switch
  logfile << endl;
  logfile <<"Flags returned: ";
  if ( res.GetFPDivByZero( ) )
    logfile << "z " ;
  if ( res.GetFPInvalid( ) )
    logfile << "i " ;
  if ( res.GetFPInexact( ) )
    logfile << "x " ;
  if ( res.GetFPOverflow( ) )
    logfile << "o " ;
  if ( res.GetFPUnderflow( ) )  {
    logfile << "u " ;
  }
  logfile << endl;
  logfile << "Correct result:  " << result << endl;
  logfile << "Returned result: " << res << endl<< endl;
}


template <class T>
void UCB<T>::Compare ( T &reslt )
{
  unsigned int i,check=1;
  if ( ( noFlags &  NO_FLAGS_DIV_BY_ZERO )  == 0 ) {
    if( reslt.GetFPDivByZero( ) )
      if( !strchr( exceptions,'d' ) ) {
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") << "Line "<<line<< ": divide by zero not expected"<<endl;
        check = 0;
      }
  }

  if ( ( noFlags &  NO_FLAGS_INVALID )  == 0 ) {
    if( reslt.GetFPInvalid( ) )
      if( !strchr( exceptions,'v' ) ) {
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": invalid not expected "<<endl;
        check = 0;
      }
  }

  if ( ( noFlags &  NO_FLAGS_INEXACT )  == 0 ) {
    if( reslt.GetFPInexact( ) )
      if( !strchr( exceptions,'x' ) ) {
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": inexact not expected"<<endl;
        check = 0;
      }
  }

  if ( ( noFlags &  NO_FLAGS_OVERFLOW )  == 0 ) {
    if( reslt.GetFPOverflow( ) )
      if( !strchr( exceptions,'o' ) ) {
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": overflow not expected"<<endl;
        check = 0;
      }
  }

  if ( ( noFlags &  NO_FLAGS_UNDERFLOW )  == 0 ) {
    if( reslt.GetFPUnderflow( ) )
      if( !strchr( exceptions,'u' ) ) {
        if( strchr( exceptions,'a' ) )  {
          signalv = 1;
          if ( notsignalv )  {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": underflow without denormalization loss previously not detected"<< endl;
            check = 0;
          } // end if
        }
        else if ( strchr( exceptions,'b' ) )  {
          signalw = 1;
          if ( notsignalv )  {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": underflow without denormalization loss previously not detected" << endl;
          }
          if ( notsignalw )  {
            logfile <<((ieeeVector) && !(ieee) ? "Warning " : "Error ")<<"Line "<<line<< ": underflow before rounding previously not detected"<< endl;
            check = 0;
          } // end if
        }
        else {
          logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": underflow not expected"<<endl;
          check = 0;
        } // end if
      }
      else
        signalu = 1;
  } // end if

  for( i=0 ; i < strlen( exceptions );i++ ) {
    switch ( exceptions[ i ] ) {
      case 'x':
        if ( ( noFlags &  NO_FLAGS_INEXACT )  == 0 ) {
          if( !reslt.GetFPInexact( ) ) {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": inexact flag not returned"<<endl;
            check = 0;
          }
        }
        break;
      case 'o':
        if ( ( noFlags &  NO_FLAGS_OVERFLOW )  == 0 ) {
          if( !reslt.GetFPOverflow( ) ) {
            logfile <<((ieeeVector) && !(ieee) ? "Warning " : "Error ")<<"Line "<<line<< ": overflow flag not returned"<<endl;
            check = 0;
          }
        }
        break;
      case 'u':
        nou = 0;
        if ( ( noFlags &  NO_FLAGS_UNDERFLOW )  == 0 ) {
          if( !reslt.GetFPUnderflow( ) ) {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": underflow not returned"<<endl;
            notsignalu = 1;
            check = 0;
          }
        }
        break;
      case 'a':
        nov = 0;
        if ( ( noFlags &  NO_FLAGS_UNDERFLOW )  == 0 ) {
          if( !reslt.GetFPUnderflow( ) )  {
            notsignalv = 1;
            //PrintError(reslt);
            if ( signalv )  {
              check = 0;
            } // end if
          } // end if
        } // end if

        break;
      case 'b':
        now = 0;
        if ( ( noFlags &  NO_FLAGS_UNDERFLOW )  == 0 ) {
          if( !reslt.GetFPUnderflow( ) ) {
            notsignalw = 1;
            if ( signalw )  {
              logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": underflow before rounding previously detected"<< endl;
              check = 0;
            } // end if
          } // end if
        } // end if

        break;
      case 'v':
        if ( ( noFlags &  NO_FLAGS_INVALID )  == 0 ) {
          if( !reslt.GetFPInvalid( ) ) {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": invalid flag not returned"<<endl;
            check = 0;
          }
        }
        break;
      case 'd':
        if ( ( noFlags &  NO_FLAGS_DIV_BY_ZERO )  == 0 ) {
          if( !reslt.GetFPDivByZero( ) ) {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ")  <<"Line "<<line<< ": divide flag not returned"<<endl;
            check = 0;
          }
        }
        break;
    }
  }
  if ( !pre )  { // no '?'
    if ( result.decimal != NULL )  {
      if ( strcmp( result.decimal,reslt.decimal )  != 0 )  {
              int rd=0, rdd=0, epos=-100;
              char *resultdummy=new char[maxstr];
              while (result.decimal[rd] != '\0')
              {
                      if ((rd==epos+1) && result.decimal[rd]!='-')
                         resultdummy[rdd++]='+';

                 resultdummy[rdd] = result.decimal[rd];

                 if (result.decimal[rd] == 'E')
                    epos=rd;
                 rd++;
                 rdd++;
              }
              resultdummy[rdd] = '\0';

              if (strcmp(resultdummy, reslt.decimal) != 0)
                 {
		   logfile <<((ieeeVector) && !(ieee) ? "Warning " : "Error ") << "Line "<<line<< ": different decimal representation"<< endl;
		   check =0;
		 }
	      delete[] resultdummy;
      }
    } else if ( result.IsNaN( ) ) {
      if( !reslt.IsNaN( ) ) {
        check=0;
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line : "<< line << ": result is not a NaN"<<endl;
      }
    } else {
      if ( reslt.sizeExp > 0 )  {
        if  ( result.Sign( )  != reslt.Sign( ) ) {
          if (!result.IsZero( )  ) {
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": Different sign"<< endl;
            check =0;
          }
          else if ( signedZero ) {
            // In this case result is a zero and there is signedzero
            logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") <<"Line "<<line<< ": Different sign"<< endl;
            check =0;
          }
        }
        if ( result.GetExponent( )  != reslt.GetExponent( ) ) {
          logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") << "Line "<<line<< ": exponent different" << endl;
          check =0;
        }
      }
      if( result.GetMantissa( )  != reslt.GetMantissa( ) ) {
        logfile<<((ieeeVector) && !(ieee) ? "Warning " : "Error ") << "Line "<<line<< ": mantissa different" << endl;
        check =0;
      }
    }
  }
  if ( !check )
    PrintError( reslt );
}

template <class T>
void UCB<T>::SetFPRound( )
{
  switch ( rounding ) {
    case 'n':
      result.SetFPRound( RM_NEAR );
      break;
    case 'z':
      result.SetFPRound( RM_ZERO );
      break;
    case 'p':
      result.SetFPRound( RM_UP );
      break;
    case 'm':
      result.SetFPRound( RM_DOWN );
      break;
  }
}

#endif
