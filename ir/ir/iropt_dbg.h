

/* This file contains makros that generate the calls to
   update the debug information after a transformation. */

#define DBG_OPT_STG                                                 \
        {                                                           \
	  ir_node *ons[2];                                          \
	  ons[0] = oldn;                                               \
	  ons[1] = get_Block_cfgpred(n, 0);                         \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_straightening); \
	}

#define DBG_OPT_IFSIM                                                 \
        {                                                             \
	  ir_node *ons[4];                                            \
	  ons[0] = oldn;                                                 \
	  ons[1] = a;                                                 \
	  ons[2] = b;                                                 \
	  ons[3] = get_Proj_pred(a);                                  \
	  __dbg_info_merge_sets(&n, 1, ons, 4, dbg_if_simplification); \
	}

#define DBG_OPT_ALGSIM1                                               \
        {                                                             \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  ons[2] = b;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_algebraic_simplification); \
	}

#define DBG_OPT_ALGSIM2                                               \
        {                                                             \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = get_unop_op(n);                                    \
          ons[2] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_algebraic_simplification); \
	}

#define DBG_OPT_ALGSIM3                                               \
        {                                                             \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_algebraic_simplification); \
	}

#define DBG_OPT_PHI                                                   \
        {                                                             \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = first_val;                                         \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_opt_ssa);          \
	}


#define DBG_OPT_WAW                                                   \
        {                                                             \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_write_after_write);\
	}

#define DBG_OPT_WAR                                                   \
        {                                                             \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = c;                                                 \
	  __dbg_info_merge_sets(&c, 1, ons, 2, dbg_write_after_read); \
	}

#define DBG_OPT_TUPLE                                                 \
        {                                                             \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  ons[2] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_opt_auxnode);      \
	}
#define DBG_OPT_ID                                                \
        {                                                             \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_opt_auxnode);      \
	}
