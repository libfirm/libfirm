/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

# include <obstack.h>
# include <malloc.h>

# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free
