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

DriverFloatRepr DriverFloatRepr::operator + (DriverFloatRepr& fl)
{
  void *res = alloca(::fc_get_buffer_length());
  void *op1 = alloca(::fc_get_buffer_length());
  void *op2 = alloca(::fc_get_buffer_length());

  to(op1);
  fl.to(op2);

  SetLibRound();

  // cout << "op1 : " << endl << op1 << endl << flush;
  // cout << "op2 : " << endl << op2 << endl << flush;
  GetLibExceptions();
  ::fc_add(op1, op2, res);
  // cout << "res: " << res << endl << flush; // debug

  GetLibExceptions();
  DriverFloatRepr r(res);
  return r;
}
DriverFloatRepr DriverFloatRepr::operator - (DriverFloatRepr &fl)
{
  void *res = alloca(::fc_get_buffer_length());
  void *op1 = alloca(::fc_get_buffer_length());
  void *op2 = alloca(::fc_get_buffer_length());

  to(op1);
  fl.to(op2);

  SetLibRound();

  // cout << "op1 : " << endl << op1 << endl << flush;
  // cout << "op2 : " << endl << op2 << endl << flush;
  ::fc_sub(op1, op2, res);
  // cout << "res: " << res << endl << flush; // debug

  GetLibExceptions();
  DriverFloatRepr r(res);
  return r;
}

DriverFloatRepr DriverFloatRepr::operator * (DriverFloatRepr &fl)
{
  void *res = alloca(::fc_get_buffer_length());
  void *op1 = alloca(::fc_get_buffer_length());
  void *op2 = alloca(::fc_get_buffer_length());

  to(op1);
  fl.to(op2);

  SetLibRound();

  // cout << "op1 : " << endl << op1 << endl << flush;
  // cout << "op2 : " << endl << op2 << endl << flush;
  ::fc_mul(op1, op2, res);
  // cout << "res: " << res << endl << flush; // debug

  GetLibExceptions();
  DriverFloatRepr r(res);
  return r;
}

DriverFloatRepr DriverFloatRepr::operator / (DriverFloatRepr &fl)
{
  void *res = alloca(::fc_get_buffer_length());
  void *op1 = alloca(::fc_get_buffer_length());
  void *op2 = alloca(::fc_get_buffer_length());

  to(op1);
  fl.to(op2);

  SetLibRound();

  // cout << "op1 : " << endl << op1 << endl << flush;
  // cout << "op2 : " << endl << op2 << endl << flush;
  ::fc_div(op1, op2, res);
  // cout << "res: " << res << endl << flush; // debug

  GetLibExceptions();
  DriverFloatRepr r(res);
  // DriverFloatRepr r(op1); // debug
  return r;
}


DriverFloatRepr DriverFloatRepr::operator % (DriverFloatRepr &fl)
{
  DriverFloatRepr r(0l);
  return r;
}

DriverFloatRepr DriverFloatRepr::sqrt()
{
  DriverFloatRepr r(0l);
  return r;
}
