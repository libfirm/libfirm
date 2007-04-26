/* -*- c -*- */

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

/**
 * @file
 * @brief    Provide some GNU CC extensions to the rest of the world
 * @author   Florian
 * @date     Sat Nov 13 19:35:27 CET 2004
 * @version  $Id$
 */
# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
  gnu_ext: Provide some GNU CC extensions to the rest of the world
*/

/* Includes */

/* Local Defines: */
# if !defined (__GNUC__)
#  if !defined(__FUNCTION__)
#    define __FUNCTION__  "::"
#  endif
#  if !defined(__PRETTY_FUNCTION__)
#    define __PRETTY_FUNCTION__ ":::"
#  endif
# endif /* !define __GNUC__ */

/* Local Data Types: */

/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */

/* ===================================================
   Exported Implementation:
   =================================================== */


/*
  $Log$
  Revision 1.2  2006/07/02 16:30:17  beck
  Fixed warnings on newer VC

  Revision 1.1  2005/01/14 14:15:19  liekweg
  Support GNU extensions on non-GNU platforms


*/
