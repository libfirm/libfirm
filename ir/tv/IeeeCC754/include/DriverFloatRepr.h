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


// MORE INFORMATION ON THE FUNCTIONS IN  THIS HEADER FILE CAN BE FOUND
// ON THE WEBPAGE
//          http://win-www.uia.ac.be/u/cant/ieeecc754.html
//   ``Adapt IeeeCC754 to test your floating-point implementation''

// ----
// Includes
// ----

#include <Fp.h>
#include <Bitstring.h>

// ----
// Includes and Defines specificly for  testing MpIeee
// ----
#ifdef MPIEEE_TEST
#include <MpIeee.hh>
#endif

// ----
// Includes and Defines specificly for testing FMLib
// ----
#ifdef FMLIB_TEST
#include <Sdefs.h>
#endif

// ----
// CLASS DEFINITION : DriverFloatRepr
// ----


class DriverFloatRepr: public FP
{
protected:

void SetLibRound( );
// call the appropriate functions to set the rounding mode
// on the target platform - see item (a) on webpage

void SetLibEnvironment( );
// call the appropriate functions to clear all floating-point
// exceptions on the target platform - see item (b) on webpage

void GetLibExceptions( );
// call the appropriate functions to read out exceptions generated
// by the target platform - see item (b) on webpage

public:

#ifndef FMLIB_TEST
// For testing FMLib we use other definitions
// --
DriverFloatRepr::DriverFloatRepr( ) :FP( )
  {}

DriverFloatRepr::DriverFloatRepr( int m,int e,int h ) :FP( m,e,h )
  {}

DriverFloatRepr::DriverFloatRepr( Bitstring  &fp,int m,int e,int h ) :FP( fp,m,e,h )
  {}
  ;
#endif

// If your target implementation is implemented in hardware,
// provide an implementation of the functions listed below for
// conversion between your hardware data types and DriverFloatRepr
// see item (c) on webpage and
//      ftp://win-ftp.uia.ac.be/pub/cant/IeeeCC754/converting.pdf

DriverFloatRepr( float f );
DriverFloatRepr( double d );
DriverFloatRepr( long double l );
float tofloat( );
double todouble( );
long double tolongdouble( );


// If your target implementation is implemented in software,
// provide an implementation of the functions listed below for
// conversion between your floating-point datatype and
// DriverFloatRepr (don't forget to replace MyDatatype by the
// appropriate identifier in this declaration) - see item (c) on
// webpage and
//      ftp://win-ftp.uia.ac.be/pub/cant/IeeeCC754/converting.pdf

DriverFloatRepr (void *val);
void* to(void *val);



//
// Here is an example to test the multiprecision floating-point
// implementation MpIeee:

#ifdef MPIEEE_TEST
// Functions that Convert MpIeee to DriverFloatRepr and vice-versa
//--

DriverFloatRepr ( const MpIeee &M );
MpIeee to( );

#endif

#ifdef FMLIB_TEST
// Functions specifically for Testing FMLib
// --

DriverFloatRepr::DriverFloatRepr( );
DriverFloatRepr::DriverFloatRepr( int m,int e,int h );
DriverFloatRepr::DriverFloatRepr( Bitstring  &fp,int m,int e,int h );
DriverFloatRepr::DriverFloatRepr( DriverFloatRepr& );
void DriverFloatRepr::fromDriverFloatRepr( Bitstring& m,Bitstring& e,int h );
void DriverFloatRepr::toDriverFloatRepr( Bitstring m,Bitstring e,int h, int signf );
FmNumber value;
long Length;
char* byteToStr( long );
void Reverse( Bitstring& );
void SetOperations( );

#endif

// provide implementaton of the functions listed below in the file
//      BasicOperations/$PLATFORM/BasicOperationstest.cc
// see item (d) on webpage
// --

DriverFloatRepr operator + ( DriverFloatRepr &T );
DriverFloatRepr operator - ( DriverFloatRepr &T );
DriverFloatRepr operator * ( DriverFloatRepr &T );
DriverFloatRepr operator / ( DriverFloatRepr &T );
DriverFloatRepr operator % ( DriverFloatRepr &T );
DriverFloatRepr sqrt( );

// provide implementation of the functions listed below in the file
//      Conversions/$PLATFORM/Conversionstest.cc
// see item (d) on webpage
// --

DriverFloatRepr roundto ( int, int, int );
DriverFloatRepr copyto ( int, int, int );
DriverFloatRepr rint ( );
DriverFloatRepr ri ( ) ;
DriverFloatRepr ru ( ) ;
DriverFloatRepr rI ( ) ;
DriverFloatRepr rU ( ) ;
DriverFloatRepr ci ( int, int, int );
DriverFloatRepr cu ( int, int, int );
DriverFloatRepr cI ( int, int, int );
DriverFloatRepr cU ( int, int, int );
DriverFloatRepr b2d ( int );
DriverFloatRepr d2b ( );

// conversions between DriverFloatRepr and hardware integer
// data types; implementation is provided in the files
//    BasicOperations/$PLATFORM/fpenv_$PLATFORM.cc
// and
//    Conversions/$PLATFORM/fpenv_$PLATFORM.cc
// and should not be modified
// --

DriverFloatRepr( long i );
DriverFloatRepr( unsigned long i );
DriverFloatRepr( long long int i );
DriverFloatRepr( unsigned long long int i );
int toint( );
unsigned int touint( );
long long int tolonglong( );
unsigned long long int toulonglong( );

};
