/* -*- c -*- */
/*
 * Project:     libFIRM
 * File name:   ir/external/read.h
 * Purpose:     Read descriptions of external effects
 * Author:      Florian
 * Modified by: Boris Boesler
 * Created:     11.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _READ_H_
#define _READ_H_

/*
  The public interface
*/
/** read the file and build the graphs */
void create_abstraction(const char *filename);


#endif /* defined _READ_H_ */

/*
  $Log$
  Revision 1.7  2004/10/25 13:52:24  boesler
  seperated read.h (public interface) and read_t.h (types)

  Revision 1.6  2004/10/22 13:13:27  boesler
  replaced char* by idents, minor fix in Firm codegen for call

  Revision 1.5  2004/10/21 15:31:55  boesler
  added lots of stuff:
  - build abstract syntax trees
  - build Firm graphs for many effects, still todos

  Revision 1.1  2004/10/11 09:31:06  liekweg
  First Import of XML reading procs --flo

*/
