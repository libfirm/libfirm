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
 * @brief     Some makros supporting old function names.
 * @author    Goetz Lindenmaier
 * @version   $Id$
 */
#ifndef FIRM_COMMON_OLD_FCTNAMES_H
#define FIRM_COMMON_OLD_FCTNAMES_H

/* firmstat */
#define stat_init init_stat

/* ircons */
#define add_in_edge(X, Y)     add_immBlock_pred(X, Y)
#define mature_block(X)       mature_immBlock(X)
#define switch_block(X)       set_cur_block(X)
#define finalize_cons(X)      irg_finalize_cons(X)

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
#define type_tag              symconst_type_tag
#define size                  symconst_type_size
#define symconst_size         symconst_type_size
#define linkage_ptr_info      symconst_addr_name

#define get_nodes_Block(X)    get_nodes_block(X)
#define set_nodes_Block(X, Y) set_nodes_block(X, Y)
#define get_Start_irg(X)      get_irn_irg(X)
#define get_EndReg_irg(X)     get_irn_irg(X)
#define get_EndExcept_irg(X)  get_irn_irg(X)
#define get_CallBegin_irg(X)  get_irn_irg(X)
#define get_ip_cfop_irg(X)    get_irn_irg(X)
#define skip_nop(X)           skip_Id(X)

#define pns_initial_exec   pn_Start_X_initial_exec
#define pns_global_store   pn_Start_M
#define pns_frame_base     pn_Start_P_frame_base
#define pns_args           pn_Start_T_args
#define pns_value_arg_base pn_Start_P_value_arg_base

#define pnc_number pn_Cmp
#define False pn_Cmp_False
#define Eq    pn_Cmp_Eq
#define Lt    pn_Cmp_Lt
#define Le    pn_Cmp_Le
#define Gt    pn_Cmp_Gt
#define Ge    pn_Cmp_Ge
#define Lg    pn_Cmp_Lg
#define Leg   pn_Cmp_Leg
#define Uo    pn_Cmp_Uo
#define Ue    pn_Cmp_Ue
#define Ul    pn_Cmp_Ul
#define Ule   pn_Cmp_Ule
#define Ug    pn_Cmp_Ug
#define Uge   pn_Cmp_Uge
#define Ne    pn_Cmp_Ne
#define True  pn_Cmp_True

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
#define get_mode_size(X)         (assert(get_mode_size_bytes(X) != -1), get_mode_size_bytes(X))


/* irop */
#define floats                   op_pin_state_floats
#define pinned                   op_pin_state_pinned
#define op_pinned                op_pin_state

/* irdump */
#define dump_cg_graph                dump_ir_graph
#define dump_cg_block_graph          dump_ir_block_graph
#define dont_dump_loop_information() dump_loop_information(0)

/* type.h */
typedef ir_type type;
#define get_type_nameid(_t_)     get_type_ident(_t_)
#define set_type_nameid(_t_,_i_) set_type_ident(_t_,_i_)
#define get_class_n_member    get_class_n_members
#define get_class_n_subtype   get_class_n_subtypes
#define get_class_n_supertype get_class_n_supertypes
#define get_struct_n_member   get_struct_n_members

#define get_method_n_res(X) get_method_n_ress(X)

/* entity.h */
#define ent_visibility ir_visibility
#define ent_allocation ir_allocation
#define ent_stickyness ir_stickyness
#define ent_volatility ir_volatility
#define peculiarity    ir_peculiarity
#define entity         ir_entity
#define get_entity_offset_bytes(ent)      get_entity_offset(ent)
#define set_entity_offset_bytes(ent, ofs) set_entity_offset(ent, ofs)

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
#define get_tarval_P_void() get_tarval_null(mode_P)
#define tarval_P_void       get_tarval_null(mode_P)

#define tarval_is_entity(X) 0
#define get_tarval_entity(X) ((ir_entity *)NULL)

/* ident.h */
#define id_to_strlen(X)   get_id_strlen(X)
#define id_to_str(X)      get_id_str(X)
#define id_from_str(X, Y) new_id_from_chars(X, Y)

/* irouts.h */
#define compute_outs(X)   compute_irg_outs(X)

/* tr_inheritance.h */
#define is_subclass_of(low, high)       is_SubClass_of(low, high)
#define is_subclass_ptr_of(low, high)   is_SubClass_ptr_of(low, high)
#define is_superclass_of(high, low)     is_SuperClass_of(high, low)
#define is_superclass_ptr_of(low, high) is_SuperClass_ptr_of(low, high)

/* previously in irvrfy.h, now in irflag.h */
#define NODE_VERIFICATION_OFF        FIRM_VERIFICATION_OFF
#define NODE_VERIFICATION_ON         FIRM_VERIFICATION_ON
#define NODE_VERIFICATION_REPORT     FIRM_VERIFICATION_REPORT
#define NODE_VERIFICATION_ERROR_ONLY FIRM_VERIFICATION_ERROR_ONLY

/* execfreq.h */
#define exec_freq_t ir_exec_freq

#endif
