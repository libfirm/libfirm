/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/gnu_ext.c
   Purpose:     Provide some GNU CC extensions to the rest of the world
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2005 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
  gnu_ext: Provide some GNU CC extensions to the rest of the world
*/

/* Includes */

/* Local Defines: */
# if defined (__GNUC__)
/* then we're all set */
# else /* defined __GNUC__ */
#  define __FUNCTION__  "::"
#  define __PRETTY_FUNCTION__ ":::"
# endif /* define __GNUC__ */

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
  Revision 1.1  2005/01/14 14:15:19  liekweg
  Support GNU extensions on non-GNU platforms


*/
