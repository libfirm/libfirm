

/* This file contains makros that generate the calls to
   update the debug information after a transformation. */

#define DBG_OPT_STG                                                \
  do {                                                       \
	  ir_node *ons[2];                                         \
	  ons[0] = oldn;                                           \
	  ons[1] = get_Block_cfgpred(oldn, 0);                     \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_straightening); \
	} while(0)

#define DBG_OPT_IFSIM                                                 \
  do {                                                          \
	  ir_node *ons[4];                                            \
	  ons[0] = oldn;                                                 \
	  ons[1] = a;                                                 \
	  ons[2] = b;                                                 \
	  ons[3] = get_Proj_pred(a);                                  \
	  __dbg_info_merge_sets(&n, 1, ons, 4, dbg_if_simplification); \
	} while(0)

#define DBG_OPT_ALGSIM1                                               \
  do {                                                          \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  ons[2] = b;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_algebraic_simplification); \
	} while(0)

#define DBG_OPT_ALGSIM2                                               \
  do {                                                          \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = get_unop_op(n);                                    \
          ons[2] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_algebraic_simplification); \
	} while(0)

#define DBG_OPT_ALGSIM3                                               \
  do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_algebraic_simplification); \
	} while(0)

#define DBG_OPT_PHI                                                   \
  do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = first_val;                                         \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_opt_ssa);          \
	} while(0)


#define DBG_OPT_WAW                                                   \
  do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_write_after_write);\
	} while(0)

#define DBG_OPT_WAR                                                   \
  do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = c;                                                 \
	  __dbg_info_merge_sets(&c, 1, ons, 2, dbg_write_after_read); \
	} while(0)

#define DBG_OPT_TUPLE                                                 \
  do {                                                          \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  ons[2] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 3, dbg_opt_auxnode);      \
	} while(0)

#define DBG_OPT_ID                                                \
  do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, 2, dbg_opt_auxnode);      \
	} while(0)
