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
#         http:/* win-www.uia.ac.be/u/cant/ieeecc754.html                 # */
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

/***************************************************************************
* This is a class definition for floating point numbers (IEEE 754).
* It uses the bitstring class for storing the exponent and the mantissa.
************************************************************************** */

#ifndef _FP_H
#define _FP_H
#include <stdio.h>

#include <Bitstring.h>
#include <Hex.h>

#define RM_NEAR   0
#define RM_UP     2
#define RM_DOWN   1
#define RM_ZERO   3


#define NOT_KNOWN        100

#ifndef MYBIG_ENDIAN
  #define MYBIG_ENDIAN     101
#endif

#ifndef MYLITTLE_ENDIAN
  #define MYLITTLE_ENDIAN  102
#endif

#define maxstr 5000
/*  maximum chars for binary to decimal conversion */

/**This is an abstract class defining floating-points as described in the IEEE 754 standard. It uses the Bitstring class to represent the mantissa and exponent
separately. The hidden bit is always present in the mantissa and needs to be defined for input and output in a hexadecimal form.\\
\\
There are three functions that need to be overloaded: SetRound(), GetExceptions(), SetEnvironment(). The first one must set the rounding mode of the library,
according to the rounding mode stored in the environment \textit{fpEnv}. This
can be done by calling the function GetFPRound() which returns the value actual rounding mode. GetExceptions() needs to convert the exceptions stored in the
testing library to the exceptions located in the environment \textit{fpEnv}.
To set the exceptions in the environment the functions: SetFPDivByZero(), SetFPInvalid(), SetFPUnderflow(), SetFPOverflow(), SetFPInexact() can be used.
The last function SetEnvironment() must set the library default environment.


@author Bogo Johan*/

class FP
{

protected:
  /* * Little or Big Endian */ */
  static int Endian;

  /**The representation of a floating-point environment. \\
  Bit positions and values:  \begin{itemize}
      \item 0: divide by zero
      \item 1: invalid
      \item 2: underflow
      \item 3: overflow
      \item 4: inexact
      \item 21 - 22 : rounding mode
       \begin{itemize}
        \item 00: round to nearest
        \item 01: round up
        \item 10: round down
        \item 11: round to zero
      \end{itemize}
    \end{itemize}  */
  static Bitstring fpEnv;

  /* /The sign of the floating-point number */
  int sign;
  /* /Hidden bit (yes/no) */
  int hidden;
  /* * The size of the mantissa */ */
  int sizeMant;
  /* * The mantissa */ */
  Bitstring mant;
  /* * The exponent */ */
  Bitstring exp;


  /**Checks if the hardware is big or little endian. This function is only
    needed to correctly convert a FP to a hardware floating-point*/
  static void CheckEndian();

  /**Set the library rounding mode. This member function needs to be
    overloaded when testing a floating-point implementation. The tester should
    call his library function(s) to set the rounding mode corresponding the
    one saved in fpEnv.*/
  void SetLibRound(){};
  /**Get library exceptions. This member functions needs to be overloaded
    when testing a floating-point implementation. The tester should call his
    function(s) to get the occured exceptions in his library and set them in
    the fpEnv with SetFPDivByZero(), SetFPInvalid(),...*/
  void GetLibExceptions(){};
  /**Get FP rounding mode stored in fpEnv
   @return the rounding mode \\ \begin{tabular}{ccl}  0 &:& round to nearest $(RM\_NEAR)$  \\
                                  1 &:& round up (RM\_UP) \\
                2 &:& round down $(RM\_DOWN)$ \\
          3 &:& round to zero $(RM\_ZERO)$
                              \end{tabular}
   */
  int GetFPRound();

  /* * Sets the divide by zero exception in fpEnv */ */
  void SetFPDivByZero(){fpEnv.Set(0);}
  /* * Sets the invalid exception in fpEnv */ */
  void SetFPInvalid()  {fpEnv.Set(1);}
  /* * Sets the underflow exception in fpEnv */ */
  void SetFPUnderflow(){fpEnv.Set(2);}
  /* * Sets the overflow exception in fpEnv */ */
  void SetFPOverflow() {fpEnv.Set(3);}
  /* * Sets the inexact exception in fpEnv */ */
  void SetFPInexact()  {fpEnv.Set(4);}



public:

  /* * The size of the exponent */ */
  int sizeExp;
  /* * decimal */ */
  char *decimal; /*  binary to decimal conversion */

  /* * Constructor */ */
  FP();
  /**Constructor setting the size of the exponent, the mantissa and the
    hidden bit
    @param sizeM  the size of the mantissa
    @param sizeE  the size of the exponent
    @param hiddenbit if there is a hidden bit (0: false / 1: true)*/
  FP(int sizeM, int sizeE,int hiddenbit=0);
  /**Constructor setting the size of the exponent,the mantissa and the
    hidden bit. It initializes the floating-point number with a bitstring.
    The bitstring must have the following form (sign exp (h) mant)
    @param fp  a bitstring containing the hardware reprecentation
               of a floating-point
    @param sizeM  the size of the mantissa
    @param sizeE  the size of the exponent
    @param hiddenbit if there is a hidden bit (0: false / 1: true) */
  FP(Bitstring &fp,int sizeM, int sizeE,int hiddenbit=0);
  /**Copy constructor
   @param copy  an other FP object*/
  FP(FP & copy);

  /* Destructor */
  virtual~FP(){};

  /* * returns true (1) if there is a hidden bit */ */
  int Hidden(){return hidden;};
  /**returns or sets the sign
    @param  sgn  the new sign \\ \begin{tabular}{rcl} 1 &:& negative \\
                                                   0 &:& positive \\
               -1 &:& no change
                              \end{tabular}
   @return the current sign or the previous depending \it{sgn}
               */
  int Sign(int sgn=-1);

  /**Returns the mantissa
   @return the mantissa hidden bit included (if defined)*/
  Bitstring & GetMantissa();
  /**Returns the exponent
   @return the exponent*/
  Bitstring & GetExponent();
  /**Sets the mantissa
    @param mantissa the new mantissa including the hidden bit (if defined)
    @return the previous mantissa*/
  Bitstring   PutMantissa(Bitstring &mantissa);
  /**Sets the exponent
    @param exponent the new exponent.
    @return the previous exponent.*/
  Bitstring   PutExponent(Bitstring &exponent);

  /**Checks if the floating-point is a Zero
    @return  \begin{tabular}{rcl} 1 &:& is a Zero\\
                                   0 &:& is not a Zero
    \end{tabular}*/
  int IsZero();

  /**Checks if the floating-point is a negative Zero
    @return  \begin{tabular}{rcl} 1 &:& is a neg Zero\\
                                   0 &:& is not a neg Zero
    \end{tabular}*/
  int IsNegZero();

  /**Checks if the floating-point is a NaN
    @return  \begin{tabular}{rcl} 1 &:& is a NaN \\
                                   0 &:& is not a NaN
    \end{tabular}*/
  int IsNaN();
  /**Creates a quiet NaN   */
  void CreateQFPNan();

  /**Assignment operator
   @param copy an other FP object to be copied
   @return *this*/
  FP& operator = (const FP &copy);
  /**Assignment operator
    @param copy an Bitstring object to be converted to a FP object
    @return *this*/
  FP& operator = (const Bitstring &copy);

  /**Set the default library environment rounding mode. This member function
    needs to be overloaded when testing a floating-point implementation. The
    tester should call his library function(s) to set the default library
    environment. Normaly this means to clear the exception flags and restoring
    the rounding mode to round to nearest.*/
  void SetLibEnvironment(){};

  /** Sets the rounding mode in the fpEnv to rm
    @param rm next values a defined \\ \begin{tabular}{rcl} 0 &:& round to nearest $(RM\_NEAR)$  \\
                                  1 &:& round up (RM\_UP) \\
                2 &:& round down $(RM\_DOWN)$ \\
          3 &:& round to zero $(RM\_ZERO)$
                              \end{tabular}
    */
  void SetFPRound (int rm);
  /* * Clears the environment (fpEnv) */ */
  void ClearFPEnvironment();
  /**Returns the exceptions stored in fpEnv as a Bitstring
   @return a Bitstring with next bit positions set, depending the exception \\\begin{tabular}{rcl}
       0 &:& divide by zero\\
       1 &:& invalid\\
       2 &:& underflow\\
             3 &:& overflow\\
       4 &:& inexact
    \end{tabular} */
  void GetFPExceptions(Bitstring);


  /**Chekcs if there has occured a divide by zero exception
    @return\begin{tabular}{rcl} 1 &:& divide by zero exception\\
                                0 &:& no divide by zero exception
    \end{tabular}*/
  int GetFPDivByZero(){return fpEnv.GetBit(0);}
  /**chekcs if there has occured a invalid exception
    @return\begin{tabular}{rcl} 1 &:& invalid exception  \\
                                 0 &:& no invalid exception
    \end{tabular}*/
  int GetFPInvalid()  {/* cout << "fpEnv.GetBit(1)" << fpEnv.GetBit(1); */
   return fpEnv.GetBit(1);}
  /**chekcs if there has occured a underflow exception
    @return\begin{tabular}{rcl} 1 &:& underflow exception \\
                                0 &:& no underflow exception
    \end{tabular}*/
  int GetFPUnderflow(){return fpEnv.GetBit(2);}
  /**chekcs if there has occured a overflow exception
   @return\begin{tabular}{rcl} 1 &:& overflow exception \\
                               0 &:& no overflow exception
    \end{tabular}*/
  int GetFPOverflow() {return fpEnv.GetBit(3);}
  /**chekcs if there has occured a inexact exception
   @return\begin{tabular}{rcl} 1 &:& inexact exception \\
                               0 &:& no inexact exception
    \end{tabular}*/
  int GetFPInexact()  {return fpEnv.GetBit(4);}
  int istiny();
  int isInf();
  int isNan();

  /* * Overloaded output operator */ */
  friend ostream& operator << (ostream& outs, FP &outstr);

  friend istream& operator >> (istream& ins, FP &instr);

};

/* @Include: MyFloat.h MyDouble.h MyQuad.h FpSim.h Bitstring.h UCB.h dlist.h stack.h ../Calculator/FPcalculator.h */

#endif
