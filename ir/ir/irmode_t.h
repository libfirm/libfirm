
/* $Id$ */

# ifndef _IRMODE_T_H_
# define _IRMODE_T_H_

# include "irmode.h"

struct ir_mode {
  modecode code;
  ident *name;            /**< Name of this mode */
  int    size;            /**< size of the mode in Bytes. */
  int    ld_align;        /**< ld means log2 */
  tarval *min;            /**< largest value to be represented by this mode */
  tarval *max;            /**< smallest value to be represented by this mode */
  tarval *null;           /**< Representation of zero in this mode */
  unsigned fsigned:1;     /**< signedness of this mode */
  unsigned ffloat:1;      /**< true if this is a float */
};


void init_mode (void);

#endif
