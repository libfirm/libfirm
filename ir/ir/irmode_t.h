
/* $Id$ */

/**
 * @file irmode_t.h
 */

# ifndef _IRMODE_T_H_
# define _IRMODE_T_H_

# include "irmode.h"

/** this struct is supposed to completely define a mode **/
struct ir_mode {
  modecode code;          /* unambiguous identifier of a mode */
  ident *name;            /* Name ident of this mode */
  mode_sort sort;         /* coarse classification of this mode:
                             int, float, reference ...
                             (see irmode.h) */
  int    size;            /* size of the mode in Bits. */
  int    align;           /* byte alignment */
  unsigned sign:1;        /* signedness of this mode */
};

#endif
