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
* This is a class definition for bitstrings. The bitstring is a array of type
* (class T). The use of templates is to specify the block size. Only use
* unsigned types to define the class T, because else the shift operations
* (especially right shift) won't work correct.
************************************************************************** */

#ifndef _BITSTRING_H
#define _BITSTRING_H


#include <math.h>
#include <iostream.h>


#define NO_ERR               500
#define ERR_OUT_OF_BOUND    501



/*class definition*/
/**This class has been created to simplify the manipulation of bits over some array of integers (unsigned longs).
The internal structure of the bitstring resembles a little endian structure,
because this concept simplifies the resizing of the bitstring.
\TEX{\begin{figure}[h]
\begin{center}
\includegraphics[totalheight=3cm]{InterneBitstr.eps}
\caption{Internal structure of Bitstring}
\end{center}
\end{figure}}
@author Bogo Johan*/



class Bitstring
{
protected:
  /* * the bitstring */ */
  unsigned long * bitstr;
  /* * length of bitstring,          */ */
  long length;
  /* *  number of array elments */ */
  long lengthBlock;
  /* * number of bits in class T */ */
  long lengthT;


  /* *  returns a unsigned long  with the first res bits set. */ */
  unsigned long GetPattern(unsigned long rest);

  /** returns the first block of bitstr. It's only defined
    to be used in PutByte so the xor-operator would work
    properly.*/
  unsigned long Convert();

public:
  /* *  Constructor, creates an empty bitstring */ */
  Bitstring();
  /**Constructor,  creates a bitstring of size size.
    @param size the default size*/
  Bitstring(unsigned long size);
  /**Constructor,initiate the bitstring with the str as input
    @param str the default value for the bitstring  */
  Bitstring(char * str);
  /* *  Copy constructor. */ */
  Bitstring(const Bitstring& copy);
  /* * Destructor */ */
  ~Bitstring();

  /**Get the length of the bitstring.
    @return length of the bitstring*/
  virtual unsigned long Length() const;
  /**Change the length of the bitstring to "len". If "len" is
      larger then the length, the bitstring is appended with 0, else
      the bitstring is truncated to the new length "len"
    @param len the new length of the bitstring
    @return  previous length
      */
  virtual unsigned long Resize(unsigned long len);

  /**Get the bit value at position "bit"
    @param bit      position of the bit
   @return the bit value \\ \begin{tabular}{rcl}  0 <= "bit" < length &->& The bit value\\
            else  &         -> &0\end{tabular} */
  int GetBit(long bit) const;

  /**Replace the bit value at position "bit" with the new "bitvalue"
    @param  bit         posisition of the bit
    @param  bitvalue    new value
    @return  Previous bit value*/
  int PutBit(unsigned long bit,unsigned int bitvalue);

  /* Replace the bit value at position "bit" with the 1
     @param   bit          position of the bit
     @return  Previous bit value*/
  int Set(long bit);

  /** Replace the bit value at position "bit" with the 0
    @param   bit     position of the bit
    @return  Previous bit value   */
  int Clear(long bit);

  /** Clears the bitstring
    @return  Cleared bitstring  */
  Bitstring ClearBitString();

  /**Get the byte value at position "byte"
    @param  byte    position of the byte
    @return  The byte value */
    void GetByte(unsigned long byte,Bitstring sub) const;

  /** Replace the byte value at position "byte" with the new "bytevalue"
    @param  byte    position of the byte
    @param  bytevalue   new value
    @return:  void /*  Previous byte value */ */
  void PutByte(unsigned long byte,Bitstring bytevalue);


  /** Increase the bitstring value by 1
    @return  Carry after most significant bit*/
  int Inc();

  /** Decrease the bitstring value by 1
    @return  \begin{tabular}{rcl}  1& -> &wrap around (negative)\\
              0& -> &OK \end{tabular} */
  int Dec();

  /**Converts a bitstring to a C-string
     @return the converted bitstring\\  \begin{tabular}{ccl}  length >0 &  -> & The C-string\\
      else   &     -> & "Bitstring is empty" \end{tabular}*/
  virtual void BitstrToString(char str[256]) const;

  /**  Converts a C-string to a bitstring
    @param  str    the C-string to be converted
    @return  converted bitstring  */
  virtual void StringToBitstr(char *str);

  /** Puts bitstring *this in front of b2 -> so the first bit of b2
          becomes the first bit of *thisb2
    @param   b2   second bitstring
    @return  *this -> *thisb2 */
  void Concat(const Bitstring &b2);

  /* Returns the substring defined by position "begin" to "count"
     @param  begin  the begining of the substring
     @param  count  the length of the substring
     @return  substring from bitstring */
  /*  Bitstring SubBitstring (unsigned long begin, unsigned long count) const; */
     void SubBitstring(unsigned long begin, Bitstring &sub) const;

  /** Overloads the array operator. Returns/change block "n"
    @param n   block number
    @return  block "n" */
  unsigned long& operator [](unsigned long n) const;

  /**Overloads the assign operator. Makes the bistring equal to "copy"
    @param  copy        the new bitstring
    @return  The changed bitstring */
  Bitstring& operator = (const Bitstring &copy);

  /** Overloads the equal operator.
    @param  b     second bitstring
     @return  \begin{tabular}{lcl}  equal &    -> &1\\
                                   not equal &->& 0 \end{tabular}*/
  int operator == (const Bitstring & b) const;

  /** Overloads the not equal operator.
    @return  \begin{tabular}{lcl}  not equal& ->& 1\\
       equal    & ->& 0 \end{tabular}*/
  int operator != (const Bitstring &b) const;

  /** Overloads the greater than operator.
    @return  \begin{tabular}{lcl}  greater  & ->& 1\\
            not greater &  ->& 0\end{tabular} */
  int operator > (const Bitstring &b) const;

  /** Overloads the smaller than operator.
    @return  \begin{tabular}{lcl}  smaller&      ->& 1\\
         not smaller & ->& 0\end{tabular} */
  int operator < (const Bitstring &b) const;


  /**Overloads the shift left operator. Shifts the bitstring "count"
    bits to the left
    @param count      number of shifts
    @return  shifted bitstring  */
  void operator <<(long count);

  /**Overloads the shift right operator. Shifts the bitstring "count"
    bits to the righ
    @param  count   number of shifts
    @return  shifted bitstring */
  void operator >>(unsigned long count );


  /**Overloads the bitwise and operator. Does a bitwise and with
       "bitstr" and "bitst". Makes the size of bitst equal to the
          bitstring.
    @param bitst  second bitstring
    @return  bitwised and -> *this \& bitst */
  Bitstring operator &( const Bitstring &bitst);

  /** Overloads the bitwise or operator. Does a bitwise or with
          "bitstr" and "bitst". Makes the size of bitst equal to the
          bitstring.
    @param  bitst   second bitstring
    @return  bitwised or -> *this $|$ bitst */
  Bitstring operator |( const Bitstring &bitst);

  /** Overloads the bitwise xor operator. Does a bitwise xor with
          "bitstr" and "bitst". Makes the size of bitst equal to the
          bitstring.
    @param   bitst    second bitstring
    @return  bitwised xor -> *this \^\ bitst  */
  Bitstring operator ^( const Bitstring &bitst);

  /** Overloads the bitwise not operator. Does a bitwise not with
          "bitstr".
    @return  bitwised not -> \cxxtilde(*this) */
  Bitstring operator ~();

  /** Overloads the stream output operator. Converts the bitstring to a
          C-string and returns it to "ostream".
    @param  outs   the ouput stream
    @param  outstr   the bitstring
    @return  outs -> converted bitstring*/
  friend ostream& operator << (ostream& outs,const Bitstring &outstr);

  /** Overloads the stream input operator. Converts the C-string to a
    bitstring.
    @param  ins   the input stream
    @param  instr  the bitstring
    @return  ins  */
  friend istream& operator >> (istream& ins, Bitstring &instr);
};

/* @Include: Hex.h */

#endif
