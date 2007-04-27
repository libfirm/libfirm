/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

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
 */

#ifndef _READ_H_
#define _READ_H_

/*
  The public interface
*/
/**
 * read the file and build the graphs
 *
 * @return 0 on I/O error, non-zero else
 */
int create_abstraction(const char *filename);

void free_abstraction(void);


#endif /* defined _READ_H_ */

/*
  $Log$
  Revision 1.9  2005/08/16 10:18:35  beck
  create_abstraction() now returns an error code if the file could not
  be opened.

  Revision 1.8  2004/11/11 09:28:32  goetz
  treat pseudo irgs special
  parse 'local' from xml files

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
