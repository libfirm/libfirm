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
#include <Hex.h>

#ifdef IntelPentium
#include <string.h>
#endif

/*class implementation*/

/***********************************************************************
* Member:  GetBin(char v)
* Purpose: returns a Bitstring with the corresponding binary value of "v"
* Return:  Bitstring with binary value of "v"
***********************************************************************/
void Hex::GetBin(char v)
{
  // cout << "char = " << v << endl;
  switch (v)
    {
    case '0':
      bitstr[0] = 0;
      // Bitstring::StringToBitstr("0000");
      break;
    case '1':
      bitstr[0] = 1;
      // StringToBitstr("0001");
      break;
    case '2':
      bitstr[0] = 2;
      // StringToBitstr("0010");
      break;
    case '3':
      bitstr[0] = 3;
      // StringToBitstr("0011");
      break;
    case '4':
      bitstr[0] = 4;
      // StringToBitstr("0100");
      break;
    case '5':
      bitstr[0] = 5;
      // StringToBitstr("0101");
      break;
    case '6':
      bitstr[0] = 6;
      // StringToBitstr("0110");
      break;
    case '7':
      bitstr[0] = 7;
      // StringToBitstr("0111");
      break;
    case '8':
      bitstr[0] = 8;
      // StringToBitstr("1000");
      break;
    case '9':
      bitstr[0] = 9;
      // StringToBitstr("1001");
      break;
    case 'A':
    case 'a':
      bitstr[0] = 10;
      // StringToBitstr("1010");
      break;
    case 'B':
    case 'b':
      bitstr[0] = 11;
      // StringToBitstr("1011");
      break;
    case 'C':
    case 'c':
      bitstr[0] = 12;
      // StringToBitstr("1100");
      break;
    case 'D':
    case 'd':
      bitstr[0] = 13;
      // StringToBitstr("1101");
      break;
    case 'E':
    case 'e':
      bitstr[0] = 14;
      // StringToBitstr("1110");
      break;
    case 'F':
    case 'f':
      bitstr[0] = 15;
      // StringToBitstr("1111");
      break;
    }
}


/***********************************************************************
* Member:  GetHex(Bitstring &b)
* Purpose: It gives the character that is represented in "b"
* Return:  returns b as a character
***********************************************************************/
char Hex::GetHex(Bitstring &b) const
{

  Bitstring temp(4);


  temp.StringToBitstr("0000");
  if(temp == b)
    return '0';

  temp.StringToBitstr("0001");
  if(temp == b)
    return '1';

  temp.StringToBitstr("0010");
  if(temp== b)
    return '2';

  temp.StringToBitstr("0011");
  if(temp == b)
    return '3';

  temp.StringToBitstr("0100");
  if(temp == b)
    return '4';

  temp.StringToBitstr("0101");
  if(temp == b)
    return '5';

  temp.StringToBitstr("0110");
  if(temp == b)
    return '6';

  temp.StringToBitstr("0111");
  if(temp == b)
    return '7';

  temp.StringToBitstr("1000");
  if(temp == b)
    return '8';

  temp.StringToBitstr("1001");
  if(temp == b)
    return '9';

  temp.StringToBitstr("1010");
  if(temp == b)
    return 'A';

  temp.StringToBitstr("1011");
  if(temp == b)
    return 'B';

  temp.StringToBitstr("1100") ;
  if(temp == b)
    return 'C';

  temp.StringToBitstr("1101");
  if(temp == b)
    return 'D';

  temp.StringToBitstr("1110");
  if(temp == b)
    return 'E';

  temp.StringToBitstr("1111");
  if(temp == b)
    return 'F';

  return '\0';

}


/***********************************************************************
* Member:  Hex()  Constructor
* Purpose: Create empty hexadecimal bitstring
* Return:  Nothing
***********************************************************************/
Hex::Hex()
{ }


/***********************************************************************
* Member:  Bitstring(unsigned long size)  Constructor
* Purpose: Create a hexadecimal bitstring of size "size"*4
* Return:  Nothing
***********************************************************************/
Hex::Hex(unsigned long size)
{
  Resize(size);
}


/***********************************************************************
* Member:  Hex(char * str)  Constructor
* Purpose: Create hexadecimal bitstring with "str" as initiale value
* Return:  Nothing
***********************************************************************/
Hex::Hex(char *hstr)
{
  long i,j;
  Hex t1(4),t2(4);

  length = strlen(hstr)*4;
  lengthT= sizeof(unsigned long) * 8;
  lengthBlock = (length/lengthT)+1;

  bitstr = new unsigned long [lengthBlock];

  for (i=0;i<lengthBlock;i++)
    bitstr[i]=0;

  j=0;
  for (i= (length/4) -1; i >= 0; i--)
    {
      t1.GetBin(hstr[i--]);
      if (i >= 0)
  {
    t2.GetBin(hstr[i]);
          t2.Concat(t1);
    PutByte(j,t2);
  }
      else
    PutByte(j,t1);
      j++;
    }
}


/***********************************************************************
* Member:  Hex(Bitstring & copy)
* Purpose: Create a hexadecimal bitstring with initiale value the bitstring
*          "copy"
* Return:  Nothing
***********************************************************************/
Hex::Hex(const Bitstring &copy)
{
  int i=0;

  length = copy.Length();
  lengthT = sizeof(unsigned long)*8;
  lengthBlock = (length /lengthT) +1;

  bitstr = new unsigned long [lengthBlock];

  for (i=0 ; i< lengthBlock; i++)
    bitstr[i]= copy[i];

  if ((length%4)!= 0)
    Resize(length/4 +1);

}


/***********************************************************************
* Member:  Resize (unsigned long len)
* Purpose: Change the length of the bitstring to "len"*4. If the new length is
*          larger then the length, the bitstring is appended with 0, else
*          the bitstring is truncated to the new length "len"*4
* Return:  Previous length
***********************************************************************/
unsigned long Hex::Resize(unsigned long len)
{
  return Bitstring::Resize(len*4);
}

/***********************************************************************
* Member:  BitstrToString()
* Purpose: Converts a hexadecimal bitstring to a C-string
* Return:  length >0   ->  The C-string
*          else        ->  "Bitstring is empty"
***********************************************************************/
void Hex::BitstrToString(char* out) const
{
  Bitstring temp(4);
  int i;

  out= new char[(length/4)+2];
  for (i = 0 ;i< length/4; i++)
    {
      // cout << "i = " << i << endl << flush;
      SubBitstring(i*4,temp);
      // cout << "temp = " << temp << endl << flush;
      out[length/4 -i -1]=GetHex(temp);
    }
  out[(length/4)]='\0';
}

/***********************************************************************
* Member:  StringToBitstr(char *str)
* Purpose: Converts a C-string to a hexadecimal bitstring
* Return:  converted bitstring
***********************************************************************/
void Hex::StringToBitstr(char *hstr)
{
  long i,j,k;
  Bitstring t1(4),t2(4);
  unsigned long tmp;

  length = strlen(hstr)*4;
  lengthT= sizeof(unsigned long) * 8;
  if (length % lengthT == 0)
     lengthBlock = length/lengthT;
  else
     lengthBlock = (length/lengthT)+1;
  if (bitstr)
    delete [] bitstr;
  // cout << "lengthBlock = " << lengthBlock << endl << flush;
  bitstr = new unsigned long [lengthBlock];
  for (i=0;i<lengthBlock;i++)
    bitstr[i]=0;
  j=0;
  for (i= 0; i < length/4;)  {
    // cout << "hstr[i] = " << hstr[i] << endl << flush;
    tmp = 0;
    for (k = 0; k < 7;k++) {
        if (hstr[i] <= '9')
      tmp += hstr[i++] - '0';
        else
            tmp += hstr[i++] - 'a' + 10;
        // cout << hex << tmp << endl;
        tmp *= 16;
    }
    if (hstr[i] <= '9')
        tmp += hstr[i++] - '0';
    else
        tmp += hstr[i++] - 'a' + 10;
    // cout << "tmp = " << hex << tmp << endl;
    bitstr[j] = tmp;
    // cout << hex << bitstr[j] << " " << flush;
    j++;
  }
/*
for (i= (length/4) -1; i >= 0; i--)
    {
      t1.GetBin(hstr[i--]);
      // cout << "t1 = " << t1 << endl << flush;
      if (i >= 0)
  {
    t2.GetBin(hstr[i]);
          // cout << "t2 = " << t2 << endl << flush;
          t2.Concat(t1);
          // cout << "t2 = " << t2 << endl << flush;
    PutByte(j,t2);
  }
      else
    PutByte(j,t1);
      j++;
    }
*/
}
