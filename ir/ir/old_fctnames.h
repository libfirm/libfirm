
/* Copyright (C) 2001 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** Some makros supporting old function names.
*/


#ifndef __OLD_FCTNAMES_H__
#define __OLD_FCTNAMES_H__

/* irgraph */
#define get_irg_params     get_irg_n_loc
#define set_irg_params     set_irg_n_loc


/* irmode.h */
#define get_ident_of_mode     get_mode_ident
#define get_size_of_mode      get_mode_size
#define get_ld_align_of_mode  get_mode_ld_align
#define get_min_of_mode	      get_mode_min
#define get_max_of_mode	      get_mode_max
#define get_null_of_mode      get_mode_null
#define get_fsigned_of_mode   get_mode_fsigned
#define get_ffloat_of_mode    get_mode_ffloat

/* type.h */
#define get_type_nameid(_t_)     get_type_ident(_t_)
#define set_type_nameid(_t_,_i_) set_type_ident(_t_,_i_)

#endif
