/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

typedef unsigned long label;
extern label last_label;
#define new_label() (++last_label)
