/*
 * Project:     libFIRM
 * File name:   ir/adt/obst.h
 * Purpose:
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# include <obstack.h>
# include <stdlib.h>
# include <strings.h>

# include "xmalloc.h"

# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free  free
