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
#include "Fp.h"

#ifdef IntelPentium
#include <string.h>
#endif


int FP::Endian = NOT_KNOWN;
Bitstring FP::fpEnv(32);

int FP::GetFPRound()
{
  Bitstring temp(2);
  //cout << "fpEnv = " << fpEnv << endl;
  fpEnv.SubBitstring(21,temp);
  //cout << "temp = " << temp << endl;
  return (temp[0]>>30);
}

void FP::ClearFPEnvironment()
{
  fpEnv.Clear(0);
  fpEnv.Clear(1);
  fpEnv.Clear(2);
  fpEnv.Clear(3);
  fpEnv.Clear(4);
}

/***********************************************************************
* Member:  CheckEndian()
* Purpose:
* Return:  void
***********************************************************************/
void FP::CheckEndian()
{
  //  char *tmp = new char[4];
  char *tmp;
  unsigned long tmplong =1;

  if (FP::Endian == NOT_KNOWN)
    {
      tmp= (char*)&tmplong;
      if (tmp[0]== 1)
  Endian= MYLITTLE_ENDIAN;
      else
  Endian= MYBIG_ENDIAN;
    }

  //  delete tmp;
}


/***********************************************************************
* Member:  FP () Constructor
* Purpose: Create a floating point number
* Return:  Nothing
***********************************************************************/
FP::FP()
{
  CheckEndian();
  sign= -1;
  hidden =-1;
  sizeMant=0;
  sizeExp=0;
  decimal = NULL;
  //  fpEnv.Resize(32);
}

/***********************************************************************
* Member:  FP (int sizeM, int sizeE, int hiddenbit=0) Constructor
* Purpose: Create a floating point number with the size of the exponent
*          and the mantissa defined and if the there is a hidden bit
*          0 -> false
*          1 -> true
* Return:  Nothing
***********************************************************************/
FP::FP(int sizeM, int sizeE,int hiddenbit)
{
  CheckEndian();
  sign= -1;
  hidden=hiddenbit;
  sizeMant=sizeM;
  sizeExp=sizeE;
  decimal = NULL;
  mant.Resize(sizeM);
  exp.Resize(sizeE);
  //  fpEnv.Resize(32);
}

/***********************************************************************
* Member:  FP (Bitstring &fp,int sizeM,int sizeE,int hiddenbit=0) Constructor
* Purpose: Create a floating point number with the size of the exponent
*          and the mantissa defined and if the there is a hidden bit
*          0 -> false, 1 -> true and initialize it with "fp" as a
*          bit representation of a IEEE 754 floating point number
* Return:  Nothing
***********************************************************************/

FP::FP(Bitstring &fp,int sizeM, int sizeE,int hiddenbit)
{
  CheckEndian();
  //  fpEnv.Resize(32);
  sign= -1;
  hidden =hiddenbit;
  sizeMant=sizeM;
  sizeExp=sizeE;
  decimal = NULL;

  mant = Bitstring(sizeMant);
  if (sizeExp > 0)
    exp = Bitstring(sizeExp);
  else
    exp = Bitstring();

  sign = fp.GetBit(0);
  // cout << "exp (voor) = " << exp << endl;
  if (sizeExp > 0) {
    fp.SubBitstring(1,exp);
    fp.SubBitstring(sizeExp+1,mant);
  }
  else // integer
    fp.SubBitstring(0,mant);
}

/***********************************************************************
* Member:  FP(FP & copy)  Copy constructor
* Purpose: Create a floating point number with initiale value the
*          floating point "copy"
* Return:  Nothing
***********************************************************************/
FP::FP(FP & copy)
{
  CheckEndian();
  //  fpEnv.Resize(32);
  sign = copy.sign;
  hidden = copy.hidden;
  sizeExp = copy.sizeExp;
  sizeMant = copy.sizeMant;
  mant = copy.mant;
  exp = copy.exp;
  if (copy.decimal != NULL) {
    decimal = new char[256];
    strcpy(decimal,copy.decimal);
  }
  else
    decimal = NULL;
  // cout << "copy decimal" << decimal << endl << flush;
}



FP& FP::operator = ( const FP &copy)
{
  sign = copy.sign;
  hidden = copy.hidden;
  sizeExp = copy.sizeExp;
  sizeMant = copy.sizeMant;
  mant = copy.mant;
  exp = copy.exp;
  if (copy.decimal != NULL) {
    decimal = new char[maxstr];
    strcpy(decimal,copy.decimal);
  }
  else
    decimal = NULL;
  // cout << "copy decimal" << decimal << endl << flush;
  return *this;
}


int FP::Sign(int sgn)
{
  int temp = sign;
  if (sgn != -1)
    {
      if ((sgn == 0)||(sgn==1))
  sign = sgn;
    }
  return temp;
}

Bitstring & FP::GetMantissa()
{
  // cout << "sizeMant = " << sizeMant << endl;
  return mant;
}

Bitstring & FP::GetExponent()
{
    return exp;
}

Bitstring  FP::PutMantissa(Bitstring &mantissa)
{
  Bitstring temp(mant);

  mant=mantissa;
  // mant.Resize(sizeMant+hidden);       //in case the mantisa is too big
  return temp;
}

Bitstring  FP::PutExponent(Bitstring &exponent)
{
  Bitstring temp(exp);

  exp=exponent;
  exp.Resize(sizeExp);       //in case the mantisa is too big
  return temp;
}

int FP::IsNaN()
{
  int i;
  Bitstring fullExp(sizeExp);
  Bitstring fullMant(sizeMant+hidden);

  if (sizeExp > 0) {
    for (i=0; i < sizeExp ; i++)
      fullExp.PutBit(i,1);
    if (fullExp != exp)
      return 0;
    for (i=0; i < sizeMant ; i++)
      fullMant.PutBit(i,0);
    if (!hidden)
      fullMant.PutBit(0,1);
    return(fullMant != mant);
  }
  else
    return 0;
}

int FP::istiny()
{
  int i;
  Bitstring fullExp(sizeExp);
  Bitstring fullMant(sizeMant);
  if ((sizeMant > 0) && (sizeExp > 0)) {
    for (i=0; i < sizeExp; i++)
      fullExp.PutBit(i,0);
    for (i=0; i < sizeMant ; i++)
      fullMant.PutBit(i,0);
    return ((fullExp == exp) && (fullMant != mant));
  }
  else
    return 0;
}

int FP::isInf()
{
  int i;
  Bitstring fullExp(sizeExp);
  Bitstring fullMant(sizeMant);
  if ((sizeMant > 0) && (sizeExp > 0)) {
    for (i=0; i < sizeExp ; i++)
      fullExp.PutBit(i,1);
    if (fullExp == exp) {
      i = 0;
      if (!hidden)
  i++;
      for (;i < sizeMant;i++)
  if (mant.GetBit(i) != 0)
    return 0;
      return 1;
    }
    else
      return 0;
  }
  else
    return 0;
}

int FP::isNan()
{
  int i;
  Bitstring fullExp(sizeExp);
  Bitstring fullMant(sizeMant);
  if ((sizeMant > 0) && (sizeExp > 0)) {
    for (i=0; i < sizeExp ; i++)
      fullExp.PutBit(i,1);
    if (fullExp == exp) {
      i = 0;
      if (!hidden)
  i++;
      for (;i < sizeMant;i++)
  if (mant.GetBit(i) != 0)
    return 1;
      return 0;
    }
    else
      return 0;
  }
  else
    return 0;
}


void  FP::CreateQFPNan()
{

  int i;
  for (i=0; i < sizeExp ; i++)
    exp.PutBit(i,1);

  mant.PutBit(sizeMant-1,1); //PLAATS
}


FP& FP::operator = (const Bitstring &copy)
{
  copy.SubBitstring(0,mant);
  copy.SubBitstring(sizeMant,exp);
  sign = copy.GetBit(sizeMant+sizeExp);
/*
  if (hidden)
    {
      mant.Resize(sizeMant+1);

      Bitstring emptyExp(sizeExp);
      for (int i=0; i< sizeExp; i++)
  emptyExp.PutBit(i,0);

      if (emptyExp != exp)                 //denormalized
  mant.PutBit(sizeMant,1);
    }
*/
  return *this;
}


void FP::SetFPRound (int rm)
{
  fpEnv.Clear(21);
  fpEnv.Clear(22);
  // cout << "fpEnv (SetFPR) = " << fpEnv << endl;
  switch (rm)
    {
    case RM_UP:
      fpEnv.Set(21);
      break;
    case RM_DOWN:
      fpEnv.Set(22);
      break;
    case RM_ZERO:
      fpEnv.Set(21);
      fpEnv.Set(22);
      break;
    }
  // cout << "fpEnv = " << fpEnv << endl;
}

void FP::GetFPExceptions(Bitstring E)
{
  fpEnv.SubBitstring(0,E);
}

ostream& operator << (ostream& outs, FP &strout)
{int i;

  if (strout.decimal != NULL) // decimal representation
    outs << strout.decimal;
  else {
/*
    if (strout.sizeExp > 0) {
      if (strout.sign)
         outs << '1';
      else
         outs << '0';
      outs << " " << hex << strout.exp << " " << strout.mant;
    }
    else
      outs << hex << strout.mant;
*/
    int size;
    if (strout.sizeExp > 0)
      size = 1+strout.sizeExp+strout.sizeMant;
    else
      size = strout.sizeMant;
    Bitstring merge(size);
    if (strout.sizeExp > 0) {
      if (strout.sign)
  merge.PutBit(0,1);
      else
  merge.PutBit(0,0);
      for (i = 0; i < strout.sizeExp;i++)
  merge.PutBit(i+1,strout.exp.GetBit(i));
      for (i = 0; i < strout.sizeMant;i++)
  merge.PutBit(1+strout.sizeExp+i,strout.mant.GetBit(i));
    }
    else { // integer
      for (i = 0; i < strout.sizeMant;i++)
  merge.PutBit(i,strout.mant.GetBit(i));
    }
    outs << merge;
  }
  return outs;
}

/*
istream& operator >> (istream& ins, FP &instr)
{
  char str[255];

  cin>> str;
  instr.StringToBitstr(str);

  return ins;
}*/

int FP::IsZero()
{
  int i;
  if ((sizeMant > 0) && (sizeExp > 0)) {
    for (i=0; i < sizeExp ; i++)
      if ( exp.GetBit(i) != 0 ){
        return 0;
      }
  i=0;
  if (!hidden)
    i++;
  for (;i < sizeMant;i++)
    if (mant.GetBit(i) != 0)
      return 0;
  }
  return 1;
}

int FP::IsNegZero()
{
  return 0;
  if ( sign == 0 )
    return 0;
  int i;
  if ((sizeMant > 0) && (sizeExp > 0)) {
    for (i=0; i < sizeExp ; i++)
      if ( exp.GetBit(i) != 0 ){
        return 0;
      }
  i=0;
  if (!hidden)
    i++;
  for (;i < sizeMant;i++)
    if (mant.GetBit(i) != 0)
      return 0;
  }
  return 1;
}
