/*
 * Project:     libFIRM
 * File name:   ir/ir/old_fctnames.h
 * Purpose:     Some makros supporting old function names.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef __OLD_FCTNAMES_H__
#define __OLD_FCTNAMES_H__

/* firmstat */
#define stat_init init_stat

/* ircons */
#define add_in_edge(X, Y)     add_immBlock_pred(X, Y)
#define mature_block(X)	      mature_immBlock(X)
#define switch_block(X)	      set_cur_block(X)


/* irgraph */
#define get_irg_ent(X)        get_irg_entity(X)
#define get_irg_params        get_irg_n_locs
#define get_irg_n_loc         get_irg_n_locs
#define set_irg_ent(X, Y)     set_irg_entity(X, Y)
#define set_irg_params        set_irg_n_loc
#define no_dom                dom_none
#define no_outs               outs_none

/* irnode.h */
#define get_Return_n_res      get_Return_n_ress
#define get_Sel_n_index       get_Sel_n_indexs
#define get_SymConst_ptrinfo  get_SymConst_name
#define set_SymConst_ptrinfo  set_SymConst_name
#define get_nodes_Block(X)    get_nodes_block(X)
#define set_nodes_Block(X, Y) set_nodes_block(X, Y)
#define get_Start_irg(X)      get_irn_irg(X)
#define get_EndReg_irg(X)     get_irn_irg(X)
#define get_EndExcept_irg(X)  get_irn_irg(X)
#define get_CallBegin_irg(X)  get_irn_irg(X)
#define get_ip_cfop_irg(X)    get_irn_irg(X)
#define skip_nop(X)           skip_Id(X)


/* irmode.h */
#define get_ident_of_mode        get_mode_ident
#define get_size_of_mode         get_mode_size
#define get_ld_align_of_mode     get_mode_ld_align
#define get_min_of_mode          get_mode_min
#define get_max_of_mode          get_mode_max
#define get_mode_vector_elems(X) get_mode_n_vector_elems(X)
#define get_null_of_mode         get_mode_null
#define get_fsigned_of_mode      get_mode_fsigned
#define get_ffloat_of_mode       get_mode_ffloat
#define get_mode_size(X)         { assert(get_mode_size_bytes(X) != -1); get_mode_size_bytes(X); }


/* irop */
#define floats                   op_pin_state_floats
#define pinned    	   	 op_pin_state_pinned
#define op_pinned		 op_pin_state

/* type.h */
#define get_type_nameid(_t_)     get_type_ident(_t_)
#define set_type_nameid(_t_,_i_) set_type_ident(_t_,_i_)
#define get_class_n_member    get_class_n_members
#define get_class_n_subtype   get_class_n_subtypes
#define get_class_n_supertype get_class_n_supertypes
#define get_struct_n_member   get_struct_n_members

#define get_method_n_res(X) get_method_n_ress(X)

/* tv.h */
#define tarval_from_long(X, Y) new_tarval_from_long(Y, X)
#define tarval_P_from_entity(X) new_tarval_from_entity(X, mode_P_mach)
#define tarval_to_entity(X) get_tarval_entity(X)
#define tarval_to_long(X) get_tarval_long(X)
#define tarval_to_double(X) get_tarval_double(X)
#define tarval_set_mode_output_option(X, Y) set_tarval_mode_output_option(X, Y)
#define tarval_get_mode_output_option(X) get_tarval_mode_output_option(X)
#define tarval_bitpattern(X) get_tarval_bitpattern(X)
#define tarval_sub_bits(X, Y) get_tarval_sub_bits(X, Y)
#define tarval_classify(X) classify_tarval(X)

/* ident.h */
#define id_to_strlen(X) get_id_strlen(X)
#define id_to_str(X)    get_id_str(X)
#endif
