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
#         Tarun Agarwal(05-07/2002)                                      #
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
#include <string.h>
#include <Bitstring.h>
#include <stdio.h>

/*class implementation*/

/***********************************************************************
* Member:  GetPattern(unsigned long res)
* Purpose: returns a Bitstring with the first "res" bits set.
* Return:  Bitstring with first "res" bits set.
***********************************************************************/
unsigned long Bitstring::GetPattern(unsigned long rest)
{
  unsigned long pattern =(unsigned long) ((unsigned long) 1 << lengthT) -1;
  pattern = pattern >> ((lengthT) -rest);
  return pattern;
}


/***********************************************************************
* Member:  Convert()
* Purpose: returns the first block of bitstr. Its only defined to be used
*          in PutByte so the ^= operator would work.
* Return:  first block of bitstr -> bitstr[0].
***********************************************************************/
unsigned long Bitstring::Convert()
{
  return bitstr[0];
}



/***********************************************************************
* Member:  Bitstring()  Constructor
* Purpose: Create empty bitstring
* Return:  Nothing
***********************************************************************/
Bitstring::Bitstring()
{
  length=lengthBlock=0;
  lengthT = sizeof(unsigned long)*8;
  bitstr = NULL;
}

/***********************************************************************
* Member:  Bitstring(unsigned long size)  Constructor
* Purpose: Create bitstring of size "size" and initiate it with zeros
* Return:  Nothing
***********************************************************************/
Bitstring::Bitstring(unsigned long size)
{
  int i;
  length= size;
  lengthT = sizeof(unsigned long )*8;
  if (size % lengthT == 0)
    lengthBlock = (size /lengthT);
  else
    lengthBlock= (size /lengthT) + 1;
  bitstr = new unsigned long [lengthBlock];
  for (i=0;i<lengthBlock;i++)
    bitstr[i]=0;
}

/***********************************************************************
* Member:  Bitstring(char * str)  Constructor
* Purpose: Create bitstring with "str" as initiale value
* Return:  Nothing
***********************************************************************/
Bitstring::Bitstring(char * str)
{

  long i;

  length = strlen(str);
  lengthT =sizeof(unsigned long )*8;

  if (length % lengthT == 0)
    lengthBlock = (length /lengthT);
  else
    lengthBlock= (length /lengthT) + 1;

  bitstr = new unsigned long [lengthBlock];

  for (i=0;i<lengthBlock;i++)
    bitstr[i]=0;

  for (i= length -1; i >= 0; i--)
    {
      if (str[i]=='1')
  bitstr [(length -1 -i)/lengthT] |= 1 << ((length-1 -i)%lengthT);
    }
}

/***********************************************************************
* Member:  Bitstring(Bitstring & copy)  Copy constructor
* Purpose: Create a bitstring with initiale value the bitstring "copy"
* Return:  Nothing
***********************************************************************/
Bitstring::Bitstring(const Bitstring & copy)
{
  length = copy.length;
  lengthT = copy.lengthT;
  lengthBlock = copy.lengthBlock;
  if (length > 0) {
    bitstr = new unsigned long [lengthBlock];
    for (int i=0 ; i< lengthBlock; i++)
      bitstr[i]= copy.bitstr[i];
  }
  else
    bitstr = NULL;
}


/***********************************************************************
* Member:  ~Bitstring()  Deconstructor
* Purpose: Remove bitstr from memory
* Return:  Nothing
***********************************************************************/
Bitstring::~Bitstring()
{
  delete [] bitstr;
}

/***********************************************************************
* Member:  Length()
* Purpose: Get the length of the bitstring
* Return:  Length of bitstring
***********************************************************************/
unsigned long Bitstring::Length() const
{
  return length;
}

/***********************************************************************
* Member:  Resize (unsigned long len)
* Purpose: Change the length of the bitstring to "len". If "len" is
*          larger then the length, the bitstring is appended with 0, else
*          the bitstring is truncated to the new length "len"
* Return:  Previous length
***********************************************************************/
unsigned long Bitstring::Resize(unsigned long len)
{
 unsigned long * temp;
 long tmplenBlock,tmpMinlen,i,rest,prevLen;
 unsigned long value;

 if (len > 0) {
 /*create bitstr with new size*/
 if (len % lengthT == 0)
   tmplenBlock = (len /lengthT);
 else
   tmplenBlock= (len /lengthT) + 1;

 temp = new unsigned long [tmplenBlock];

 /*get length to copy*/
 if (lengthBlock <= tmplenBlock)
   tmpMinlen = lengthBlock;
 else
   tmpMinlen = tmplenBlock;

 /*copy bitstr*/
 for (i=0; i < tmpMinlen; i++)
   temp[i] = bitstr[i] ;


 /*delete old bitstr*/
 if (bitstr != NULL)
     delete [] bitstr;

 if (len % lengthT != 0) {
   rest = len % lengthT;
   value = GetPattern (rest);
   temp[len/lengthT] &= value;
 }

 /*append with 0*/
 if(lengthBlock < tmplenBlock)
   {
     for (i = lengthBlock; i < tmplenBlock; i++)
       temp[i]=0;
   }

 /*change size */
 bitstr = temp;
 prevLen = length;
 length= len ;
 lengthBlock=tmplenBlock;
}
else {
  length=lengthBlock=0;
  lengthT = sizeof(unsigned long)*8;
  delete [] bitstr;
  bitstr = NULL;
}
 return prevLen;
}

/***********************************************************************
* Member:  GetBit(unsigned long bit)
* Purpose: Get the bit value at position "bit"
* Return:  0 <= "bit" < length -> The bit value
*          else           -> 0
***********************************************************************/
int Bitstring::GetBit(long bit) const
{ unsigned long  tmp;

  if ((0 <= bit) && (bit < length)) {
    tmp = bitstr[bit/lengthT];
    if (tmp & (1L << lengthT - 1 - (bit% lengthT)))
       return 1;
  }
  return 0;
}

/***********************************************************************
* Member:  PutBit (unsigned long bit, unsigned int bitvalue)
* Purpose: Replace the bit value at position "bit" with the new "bitvalue"
* Return:  Previous bit value
***********************************************************************/
int Bitstring::PutBit(unsigned long bit,unsigned int bitvalue)
{
  int prevBit;

  /*get current bit value*/
  prevBit = GetBit(bit);
  // cout << "bit = " << bit << " " << bitvalue << endl;
  // cout << "bitstr = " << bitstr[bit/lengthT] << endl;
  if (bitvalue == 0) {
      if (bitstr[bit/lengthT] & (1L << (lengthT - 1 - (bit%lengthT))))
          bitstr[bit/lengthT] -= (1L << (lengthT - 1 - (bit%lengthT)));
  }
  else
      bitstr[bit/lengthT] |= (1L << (lengthT - 1 - (bit%lengthT)));
  // cout << "bitstr = " << bitstr[bit/lengthT] << endl;
  return prevBit;
}

/***********************************************************************
* Member:  Set (unsigned long bit)
* Purpose: Replace the bit value at position "bit" with the 1
* Return:  Previous bit value
***********************************************************************/
int Bitstring::Set(long bit)
{
  return PutBit(bit,1);
}
/***********************************************************************
* Member:  Clear (unsigned long bit)
* Purpose: Replace the bit value at position "bit" with the 0
* Return:  Previous bit value
***********************************************************************/
int Bitstring::Clear(long bit)
{
  return PutBit(bit,0);
}

/***********************************************************************
* Member:  ClearBitString ()
* Purpose: Clears the bitstring
* Return:  Cleared bitstring
***********************************************************************/
Bitstring Bitstring::ClearBitString()
{
  int i ;
  for (i =0; i< lengthBlock; i++)
    bitstr[i] = 0;

  return *this;
}

/***********************************************************************
* Member:  GetByte (unsigned long byte)
* Purpose: Get the byte value at position "byte"
* Return:  The byte value
***********************************************************************/
void Bitstring::GetByte(unsigned long byte, Bitstring sub) const
{
  SubBitstring(byte*8,sub);
}

/***********************************************************************
* Member:  PutByte (unsigned long byte, Bitstring bytevalue)
* Purpose: Replace the byte value at position "byte" with the new "bytevalue"
* Return:  Previous byte value
***********************************************************************/
void Bitstring::PutByte(unsigned long byte,Bitstring bytevalue)
{
  long sizeT;

  sizeT=sizeof(unsigned long);

  // GetByte(byte,prevByte);
  // prevByte.Resize(lengthT);
  // bytevalue.Resize(8);
  // bytevalue.Resize(lengthT);

  /* replace value by "bytevalue" using xor*/
  // cout << bitstr << endl;
  // cout << bytevalue << endl << flush;
  bitstr[byte/sizeT] ^= (bytevalue.Convert() << ((byte%sizeT)*8));
  // bitstr[byte/sizeT] ^= (prevByte.Convert() << ((byte%sizeT)*8));
  // cout << bitstr << endl << flush;
}

/***********************************************************************
* Member:  Inc ()
* Purpose: Increase the bitstring value by 1
* Return:  Carry after most significant bit
***********************************************************************/
int Bitstring::Inc()
{
  int i = 0;

  while ((i <length) && (GetBit(i)== 1))
    Clear(i++);

  if (i < length)
    Set(i);
  else
    return 1;

  return 0;

}

/***********************************************************************
* Member:  Dec ()
* Purpose: Decrease the bitstring value by 1
* Return:  1 -> wrap around (negative)
*          0 -> OK
***********************************************************************/
int Bitstring:: Dec()
{
  int i = 0;

  while ((i<length) && (GetBit(i) == 0))
    Set(i++);

  if (i< length)
    Clear(i);
  else
    return 1;

  return 0;
}

/***********************************************************************
* Member:  BitstrToString()
* Purpose: Converts a bitstring to a C-string
* Return:  length >0   ->  The C-string
*          else        ->  "Bitstring is empty"
***********************************************************************/
void Bitstring::BitstrToString(char str[256]) const
{
  long place, i;
  unsigned long block;


  if (length > 0)
    {
      /*convert bitstring*/
      for (i=0; i <length;i++)
  {
    place = i%lengthT;
    block = 1 << place;
    if ((bitstr[i/lengthT] & block) == 0)
      str[length-1 -i]='0';
    else
      str[length-1 -i]='1';
  }
      str[length]='\0';
    }
  else
    {
      /*empty bitstring*/
      //      str = new char [25];
      // strcpy(str,"Bitstring is empty!\n");
      str[0]='\0';
    }
}


/***********************************************************************
* Member:  StringToBitstr(char *str)
* Purpose: Converts a C-string to a bitstring
* Return:  converted bitstring
***********************************************************************/
void Bitstring::StringToBitstr(char *str)
{

  long i;

  length = strlen(str);
  lengthT =sizeof(unsigned long)*8;
  lengthBlock = (length/lengthT)+1;
  if(bitstr)
    delete [] bitstr;
  bitstr = new unsigned long [lengthBlock];

  for (i=0;i<lengthBlock;i++)
    bitstr[i]=0;

  for (i= length -1; i >= 0; i--)
    {
      if (str[i]=='1')
  bitstr [(length -1 -i)/lengthT] |= 1 << ((length-1 -i)%lengthT);
    }
}



/***********************************************************************
* Member:  Concat(const Bitstring &b2)
* Purpose: Puts bitstring *this in front of b2 -> so the first bit of b2
*          becomes the first bit of *thisb2
* Return:  *this -> *thisb2
***********************************************************************/
void Bitstring::Concat(const Bitstring &b2)
{
  Resize(length+b2.length);
  for (int i = length;i < length + b2.length;i++) {
     bitstr[i] = b2[i-length];
  }
}


/***********************************************************************
* Member:  SubBitstring(unsigned long begin, unsigned long count)
* Purpose: Returns the substring defined by position "begin" to "count"
* Return:  substring from bitstring
***********************************************************************/
void Bitstring::SubBitstring(unsigned long begin, Bitstring &sub) const
{
  Bitstring temp(*this);
  int i;
  // cout << "temp = " << temp << endl << flush;
  temp << begin;
  // cout << "temp = " << temp << endl << flush;
  for (i = 0;i < sub.lengthBlock-1;i++)
    sub.bitstr[i] = temp[i];
  sub.bitstr[i] = ((temp[i] >> (32 - (sub.length % lengthT))) << (32 - (sub.length % lengthT)));
  // cout << sub << endl << flush;
}

/***********************************************************************
* Member:  operator = (const Bitstring &copy)
* Purpose: Overloads the assign operator. Makes the bistring equal to "copy"
* Return:  The changed bitstring
***********************************************************************/
Bitstring& Bitstring::operator = (const Bitstring &copy)
{
  int i=0;
  //  unsigned long * tmp;

  /*change lengths*/
  length = copy.length;
  lengthT = copy.lengthT;
  lengthBlock = copy.lengthBlock;

  if(bitstr)
    // tmp = bitstr;
    delete [] bitstr;


  //check length of copy
  if (length >0)
    {
      bitstr = new unsigned long [lengthBlock];

      /*copy the bitstring*/
      for (i=0 ; i< lengthBlock; i++)
  bitstr[i]= copy.bitstr[i];
    }
  else
    bitstr= NULL;

  //  if (tmp)
  //  delete [] tmp;

  return *this;
}

/***********************************************************************
* Member:  operator == (const Bitstring & b) const
* Purpose: Overloads the equal operator.
* Return:  equal     -> 1
*          not equal -> 0
***********************************************************************/
int Bitstring::operator == (const Bitstring &b) const
{
  int i=0, value = 1;

  /*if lengths not equal than the bitstrings not equal */
  if (length != b.length)
    value = 0;


  while ((i<lengthBlock ) && (value))
    {
      if (bitstr[i] != b[i])
  value =0;
      i++;
    }
  return value;
}

/***********************************************************************
* Member:  operator != (const Bitstring &b) const
* Purpose: Overloads the not equal operator.
* Return:  not equal -> 1
*          equal     -> 0
***********************************************************************/
int Bitstring::operator != (const Bitstring &b) const
{

  int i=0, value = 0;

  if (length != b.length) {
    value = 1;
  }
  while ((i<lengthBlock ) && (!value))
    {
      if (bitstr[i] != b[i]) {
  value = 1;
      }
      i++;
    }

  // cout << *this << endl;
  // cout << b << endl;
  return value;
}

/***********************************************************************
* Member:  operator > (const Bitstring &b) const
* Purpose: Overloads the greater than operator.
* Return:  greater       -> 1
*          not greater   -> 0
***********************************************************************/
int Bitstring::operator > (const Bitstring &b) const
{
  Bitstring temp;
  int i;

  if (*this == b)
    return 0;

  if (Length() > b.Length())
    {
      temp = b;
      temp.Resize(Length());
    }

  if (Length() < b.Length())
    {
      temp = *this;
      temp.Resize(b.Length());
    }

  i = Length()-1;

  while (i>=0)
    {
      if (GetBit(i)!= b.GetBit(i))
  {
    if (GetBit(i) == 1)
      return 1;
    else
      return 0;
  }
      i--;
    }
  return 0;
}

/***********************************************************************
* Member:  operator < (const Bitstring &b) const
* Purpose: Overloads the smaller than operator.
* Return:  smaller      -> 1
*          not smaller  -> 0
***********************************************************************/
int Bitstring::operator < (const Bitstring &b) const
{
  Bitstring temp;
  int i;

  if (*this == b)
    return 0;

  if (Length() > b.Length())
    {
      temp = b;
      temp.Resize(Length());
    }

  if (Length() < b.Length())
    {
      temp = *this;
      temp.Resize(b.Length());
    }

  i = Length()-1;

  while (i>=0)
    {
      if (GetBit(i)!= b.GetBit(i))
  {
    if (GetBit(i) == 1)
      return 0;
    else
      return 1;
  }
      i--;
    }
  return 0;
}



/***********************************************************************
* Member:  operator << (unsigned long count)
* Purpose: Overloads the shift left operator. Shifts the bitstring "count"
*          bits to the left
* Return:  shifted bitstring
***********************************************************************/
void Bitstring::operator <<(long count)
{
  int i,j, rest;

  /*if count > lengthT -> copy blocks*/
  if ((count/lengthT) > 0)
    {
      for (j=0; j  <lengthBlock ; j++)
        {
          if (j+(count/lengthT) < (unsigned long)(lengthBlock))
            bitstr[j]=bitstr[j+(count/lengthT)];
          else
            bitstr[j]= 0;
        }

      //make lastblocks = 0
      for (j= lengthBlock - (count/lengthT); j < lengthBlock ; j++)
        bitstr[j]= 0;
    }
  /* if count < lengthT -> shift bitwise*/
  rest = count % lengthT;

  unsigned long leading = (1L << lengthT-1);
  for (i=0; i < rest ; i++)
    {
      for (j=0; j < lengthBlock-1; j++)
  {
          bitstr[j] <<= 1;
    //check for carry
    if (bitstr[j+1] & leading)
        bitstr[j] += 1L;
  }
        bitstr[lengthBlock-1] <<=1;
    }
}

/***********************************************************************
* Member:  operator >> (unsigned long count)
* Purpose: Overloads the shift right operator. Shifts the bitstring "count"
*          bits to the right
* Return:  shifted bitstring
***********************************************************************/
void Bitstring::operator >>(unsigned long count)
{
  int i,j, rest,carry = 0;

  /*if count > lengthT -> copy blocks*/
  if ((count/lengthT) > 0)
    {
      // cout << "copy blocks" << endl << flush;
      for (j=0; j  <lengthBlock ; j++)
  {
    if (j+(count/lengthT) < (unsigned long)(lengthBlock))
      bitstr[j]=bitstr[j+(count/lengthT)];
    else
      bitstr[j]= 0;
  }

      //make lastblocks = 0
      for (j= lengthBlock - (count/lengthT); j < lengthBlock ; j++)
  bitstr[j]= 0;

    }


  /* if count < lengthT -> shift bitwise*/
  rest = count % lengthT;
  // cout << "rest = " << rest << endl << flush;
  for (i=0; i < rest; i++)
    {
      carry = 0;
      for (j=lengthBlock-1;j > 0; j--)
  {
          bitstr[j] >>= 1;
          if ((bitstr[j-1] & 1L) == 1L)
            bitstr[j] |= (1 <<( lengthT-1));
  }
        bitstr[0] >>= 0;
    }
}


/***********************************************************************
* Member:  operator [] (unsigned long n)
* Purpose: Overloads the array operator. Returns/change block "n"
* Return:  block "n"
***********************************************************************/
unsigned long& Bitstring::operator [](unsigned long n) const
{
  //if(( n < 0) || (n > lengthBlock))
    return bitstr[n];
}

/***********************************************************************
* Member:  operator &(const Bitstring &bitst)
* Purpose: Overloads the bitwise and operator. Does a bitwise and with
*          "bitstr" and "bitst". Makes the size of bitst equal to the
*          bitstring.
* Return:  bitwised and -> *this & bitst
***********************************************************************/
Bitstring Bitstring::operator &(const Bitstring &bitst)
{

  long i;
  Bitstring temp1(*this),temp2(bitst);

  //make size bitst equal to *this
  if (lengthBlock > bitst.lengthBlock)
    temp2.Resize(length);

  //bitwise and
  for (i = 0; i<lengthBlock ; i++ )
    temp1[i] &= temp2[i];

  return temp1;

}

/***********************************************************************
* Member:  operator |(const Bitstring &bitst)
* Purpose: Overloads the bitwise or operator. Does a bitwise or with
*          "bitstr" and "bitst". Makes the size of bitst equal to the
*          bitstring.
* Return:  bitwised or -> *this | bitst
***********************************************************************/
Bitstring Bitstring::operator |(const Bitstring &bitst)
{
  long i;
  Bitstring temp1(*this),temp2(bitst);

 //make size bitst equal to *this
  if (lengthBlock > bitst.lengthBlock)
    temp2.Resize(length);

  //bitwise or
  for (i = 0; i<lengthBlock ; i++ )
    temp1[i] |= temp2[i];

  return temp1;
}


/***********************************************************************
* Member:  operator ^ (const Bitstring &bitst)
* Purpose: Overloads the bitwise xor operator. Does a bitwise xor with
*          "bitstr" and "bitst". Makes the size of bitst equal to the
*          bitstring.
* Return:  bitwised xor -> *this ^ bitst
***********************************************************************/
Bitstring Bitstring::operator ^( const Bitstring &bitst)
{
  long i;
  Bitstring temp1(*this),temp2(bitst);

 //make size bitst equal to *this
  if (lengthBlock > bitst.lengthBlock)
    temp2.Resize(length);

  //bitwise xor
  for (i = 0; i<lengthBlock ; i++ )
    temp1[i] ^= temp2[i];

  return temp1;
}

/***********************************************************************
* Member:  operator ~ ()
* Purpose: Overloads the bitwise not operator. Does a bitwise not with
*          "bitstr".
* Return:  bitwised not -> ~(*this)
***********************************************************************/
Bitstring Bitstring::operator ~()
{
  long i;
  Bitstring temp1(*this);


  //bitwise not
  for (i = 0; i<lengthBlock ; i++ )
    temp1[i] = ~temp1[i];

  temp1.Resize(this->Length());
  return temp1;

}



/***********************************************************************
* Member:  operator << (ostream& outs, Bitstring &strout)
* Purpose: Overloads the stream output operator. Converts the bitstring to a
*          C-string and returns it to "ostream".
* Return:  outs -> converted bitstring
***********************************************************************/
ostream& operator << (ostream& outs, const Bitstring &strout)
{
  // convert bitstring

/*
  char tmp[256];
  strout.BitstrToString(tmp);
  outs << tmp;
*/
  char tmp[9];
  for (int i = 0; i < strout.lengthBlock;i++) {
    sprintf(tmp,"%08x",strout[i]);
    outs << tmp;
  }
  // outs << hex << setw(8) << setfill('0') << strout[i];
  return outs;
}


/***********************************************************************
* Member:  operator >> (istream& ins, Bitstring &instr)
* Purpose: Overloads the stream input operator. Converts the C-string to a
*          bitstring.
* Return:  ins
***********************************************************************/
istream& operator >> (istream& ins, Bitstring &instr)
{
  char str[255];

  cin>> str;
  instr.StringToBitstr(str);

  return ins;
}
