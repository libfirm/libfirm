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

#include <math.h>
#include <DriverFloatRepr.h>
extern "C" {
#include <fltcalc.h>
}

#ifndef DOUBLE
#define DOUBLE
#endif
/*
By default, all functions in this file are declared in the header file

  DriverFloatRepr.h

except for two functions

DriverFloatRepr::DriverFloatRepr(<MyDatatype &D>)
<MyDatatype> DriverFloatRepr::to()

These two functions, for conversion between the floating-point
data type of your target implementation and DriverFloatRepr, the
floating-point data type of the driver program, should be declared
explicitly in the header file DriverFloatRepr.h, with MyDatatype
replaced by the appropriate identifier.  */

void DriverFloatRepr::SetLibRound()
{
  /* Make your library calls for changing the rounding mode */

  switch (GetFPRound())
    {
      case RM_NEAR:
      // change rounding mode to round to nearest break;
        fc_set_rounding_mode(FC_TONEAREST);
        break;
      case RM_ZERO:
      // change rounding mode to round to zero break;
        fc_set_rounding_mode(FC_TOZERO);
        break;
      case RM_UP:
      // change rounding mode to round up break;
        fc_set_rounding_mode(FC_TOPOSITIVE);
        break;
      case RM_DOWN:
      // change rounding mode to round down break;
        fc_set_rounding_mode(FC_TONEGATIVE);
        break;
    }
}

void DriverFloatRepr::SetLibEnvironment()
{
  // clear all floating-point exceptions
}

void DriverFloatRepr::GetLibExceptions()
{
  // do nothing, exceptions are unsupported
}

DriverFloatRepr::DriverFloatRepr(void* val) {
  // convert your floating-point data type to the floating-point
  // representation of the driver program
  // replace MyDatatype by the appropriate identifier
  int exponent;
  unsigned int mantissa0;
  unsigned int mantissa1;

#ifdef DOUBLE
  sizeExp = 11;
  sizeMant = 52;
#else
  sizeExp = 8;
  sizeMant = 23;
#endif
  mant = Bitstring(sizeMant);
  exp = Bitstring(sizeExp);

  if (fc_is_negative(val))
    sign = 1;
  else
    sign = 0;

#ifdef DOUBLE
  // convert exponent
  exponent = ((int)(fc_sub_bits(val, 64, 7) & 0x7F)) << 4;
  exponent |= ((fc_sub_bits(NULL, 64, 6) & 0xF0) >> 4);
  for (int i = sizeExp - 1;i >= 0; i--) {
    exp.PutBit(i,exponent%2);
    exponent /= 2;
  }
  // convert mantissa
  mantissa0 = ((int)(fc_sub_bits(NULL, 64, 6) & 0x0F)) << 16;
  mantissa0 |= ((int)fc_sub_bits(NULL, 64, 5)) << 8;
  mantissa0 |= (int)fc_sub_bits(NULL, 64, 4);

  mantissa1 = ((int)fc_sub_bits(NULL, 64, 3)) << 24;
  mantissa1 |= ((int)fc_sub_bits(NULL, 64, 2)) << 16;
  mantissa1 |= ((int)fc_sub_bits(NULL, 64, 1)) << 8;
  mantissa1 |= ((int)fc_sub_bits(NULL, 64, 0));

  for (int i = 19;i >= 0; i--) {
    mant.PutBit(i,mantissa0%2);
    mantissa0 /= 2;
  }
  for (int i = 31;i >= 0; i--) {
    mant.PutBit(20+i,mantissa1%2);
    mantissa1 /= 2;
  }
#else
  // convert exponent
  exponent = ((int)(fc_sub_bits(val, 32, 3) & 0x7F)) << 1;
  exponent |= ((fc_sub_bits(NULL, 32, 2) & 0x80) >> 7);
  for (int i = sizeExp - 1;i >= 0; i--) {
    exp.PutBit(i,exponent%2);
    exponent /= 2;
  }
  // convert mantissa
  mantissa0 = ((int)(fc_sub_bits(NULL, 32, 2) & 0x7F)) << 16;
  mantissa0 |= ((int)fc_sub_bits(NULL, 32, 1)) << 8;
  mantissa0 |= (int)fc_sub_bits(NULL, 32, 0);

  for (int i = 22;i >= 0; i--) {
    mant.PutBit(i,mantissa0%2);
    mantissa0 /= 2;
  }
#endif
}

void *DriverFloatRepr::to(void *buf)
  // convert the the floating-point representation of the driver program
  // to the floating-point data type of your implementation
{
  unsigned int upper = 0;
  unsigned int lower = 0;
#ifdef DOUBLE
  double im_val;
#else
  float im_val;
#endif

  if (sign != 0) {
    upper |= 0x80000000;
  }
  for (int i = 0;i < sizeExp; i++) {
    if (exp.GetBit(i) == 1)
      upper |= 1<<(30-i);
  }
#ifdef DOUBLE
  for (int i = 0;i < 20; i++) {
    if (mant.GetBit(i) == 1)
      upper |= 1<<(19-i);
  }
  for (int i = 20;i < sizeMant; i++) {
    if (mant.GetBit(i) == 1)
      lower |= 1<<(51-i);
  }

  if (Endian == MYLITTLE_ENDIAN)
  {
    ((unsigned int*)&im_val)[0] = lower;
    ((unsigned int*)&im_val)[1] = upper;
  }
  else
  {
    ((unsigned int*)&im_val)[1] = lower;
    ((unsigned int*)&im_val)[0] = upper;
  }
  fc_val_from_float(im_val, 11, 52, (char*)buf);
#else
  for (int i = 0;i < sizeMant; i++) {
    if (mant.GetBit(i) == 1)
      upper |= 1<<(22-i);
  }

  *((unsigned int*)&im_val) = upper;
  fc_val_from_float(im_val, 8, 23, (char*)buf);
#endif
  return buf;
}

// conversions between DriverFloatRepr and hardware integer
// data types; implementation should not be modified !

DriverFloatRepr::DriverFloatRepr(long i) {
  Bitstring temp(32);
  hidden = 0;
  sizeMant = 32;
  sizeExp = 0;
  temp[0] = (unsigned long) i;
  temp.SubBitstring(0,mant);
}

DriverFloatRepr::DriverFloatRepr(unsigned long i) {
  Bitstring temp(32);
  hidden = 0;
  sizeMant = 32;
  sizeExp = 0;
  temp[0] = (unsigned long) i;
  temp.SubBitstring(0,mant);
}

DriverFloatRepr::DriverFloatRepr(long long i) {
  unsigned long *tmparray=new unsigned long[2];
  tmparray = (unsigned long *)&i;
  Bitstring temp(64);
  if (Endian == MYLITTLE_ENDIAN)
    {
      temp[0] = tmparray[0];
      temp[1] = tmparray[1];
    }
  else
    {
      temp[0] = tmparray[1];
      temp[1] = tmparray[0];
    }
  hidden = 0;
  sizeMant = 64;
  sizeExp = 0;
  temp.SubBitstring(0,mant);
}

DriverFloatRepr::DriverFloatRepr(unsigned long long i) {
  unsigned long *tmparray=new unsigned long[2];
  tmparray = (unsigned long *)&i;
  Bitstring temp(64);
  if (Endian == MYLITTLE_ENDIAN)
    {
      temp[0] = tmparray[0];
      temp[1] = tmparray[1];
    }
  else
    {
      temp[0] = tmparray[1];
      temp[1] = tmparray[0];
    }
  hidden = 0;
  sizeMant = 64;
  sizeExp = 0;
  temp.SubBitstring(0,mant);
}

int DriverFloatRepr::toint() {
  return (int&)mant[0];
}

unsigned int DriverFloatRepr::touint() {
  return (unsigned int&)mant[0];
}

long long int DriverFloatRepr::tolonglong() {
  unsigned long tmparray[2];

  if (Endian == MYLITTLE_ENDIAN)
    {
      tmparray[0] = mant[0];
      tmparray[1] = mant[1];
    }
  else
    {
      tmparray[0] = mant[1];
      tmparray[1] = mant[0];
    }
  return (long long int&)*tmparray;
}

unsigned long long int DriverFloatRepr::toulonglong() {
  unsigned long tmparray[2];
  if (Endian == MYLITTLE_ENDIAN)
    {
      tmparray[0] = mant[0];
      tmparray[1] = mant[1];
    }
  else
    {
      tmparray[0] = mant[1];
      tmparray[1] = mant[0];
    }
  return (unsigned long long int&)*tmparray;
}
