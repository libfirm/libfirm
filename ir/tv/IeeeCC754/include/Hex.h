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

/***************************************************************************
* This is a class definition for hexadecimal input of the bitstrings class.
* Hex is inherited from bitstring, so there are no data members necessery.
* The only thing that has to be done is to define the functions that change
* in behaviour.
************************************************************************** */
#ifndef _HEXSTR_H
#define _HEXSTR_H


#include <iostream.h>
#include <Bitstring.h>

/*class definition*/

/**
  This class manipulates the Bitstring class as a hexadecimal string of bits.
  The only difference in the
  hexadecimal representation is the length of the string, as it
  is four times smaller than the binary representation.

   All the members are defined in the class Bitstring, therefore there are no
  extra data members in this class.

  @author Bogo Johan
 */

class Hex : public Bitstring
{

protected:
  /** returns the binairy value of character v
    @param v: character between 0-9 and A-F or a-F
    @return: the binary value of v*/
  void GetBin(char v);

  /** returns the character value of the Bitstring b
     @param b: binary value, from 0000-1111
     @return: the character value of b*/
  char GetHex(Bitstring& b) const;


public:
  /// Constructor, creates empty Bitstring
  Hex();

  /** Constructor
    @param size: specifies the initial size of the bitstring.
                 The size is multiplied by 4 for a bistring.*/
  Hex(unsigned long size);

  /**Constructor, initiates the object with str
   @param hstr: the hexadecimal number in a string-format */
  Hex(char *hstr);

  /**Copy constructor
   @param copy a Bitstring object */
  Hex(const Bitstring &copy);

  ///Deconstructor
  ~Hex(){};


  /** Get the length of the bitstring
    @return: the length of the bitstring divided by 4*/
  unsigned long Length() const {return length/4;};

  /** changes the length of the bitstring to \textit{len} *4.
    If the new length is larger then before, the bitstring
    is appended with 0, else the bitstring is truncated to
    the new length.
    @param len the new length of the Bitstring
    @return the previous length.
    */
  unsigned long Resize(unsigned long len);

  /**converts the Bitstring to a C-string.
   @return the converted Bitstring*/
  void BitstrToString(char* out) const;

  /**converts a C-string to a bitstring.
   @param hstr a C-string to be converted
   @return the HexString*/
  void StringToBitstr(char *hstr);

};

#endif
